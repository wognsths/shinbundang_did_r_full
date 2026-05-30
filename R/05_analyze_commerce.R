# ------------------------------------------------------------
# 05_analyze_commerce.R
# DID, event study, robustness, SCM, heterogeneity for commerce
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing commerce panel...")
commerce_panel <- readr::read_csv(file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv"), show_col_types = FALSE)

add_flags <- function(df, treated_codes, control_codes = NULL, mode = "explicit") {
  out <- df %>% mutate(treated = as.integer(dong_code %in% treated_codes))
  if (mode == "explicit") {
    out <- out %>% filter(dong_code %in% c(treated_codes, control_codes))
  } else if (mode == "gangnam_seocho") {
    out <- out %>% filter(district_code %in% c(11680, 11650)) %>% filter(treated == 1 | !(dong_code %in% CONTAMINATED_DONGS))
  } else if (mode == "all_seoul") {
    out <- out %>% filter(treated == 1 | !(dong_code %in% CONTAMINATED_DONGS))
  } else {
    stop("Unknown mode: ", mode)
  }
  out %>% mutate(unit = paste0(dong_code, "_", dong_name), did = treated * post)
}

main <- add_flags(commerce_panel, as.integer(names(TREATED_CORE)), as.integer(names(CONTROL_MAIN)), "explicit")
save_csv(main, file.path(OUT_PROCESSED, "commerce_main_panel_core_local_2019_2024.csv"))
main_no_transition <- main %>% filter(transition == 0)

commerce_outcomes <- c("log_sales", "log_transactions", "log_avg_ticket", "weekend_share", "night_share", "night_tx_share")
main_results <- purrr::map_dfr(commerce_outcomes, function(y) {
  mod <- fixest::feols(as.formula(paste0(y, " ~ did | unit + qstr")), cluster = ~ unit, data = main_no_transition)
  r <- fixest_term_row(mod, "did")
  tibble(
    outcome = y,
    coef = r$estimate,
    se = r$se,
    p_value = r$p_value,
    ci_low = r$ci_low,
    ci_high = r$ci_high,
    pct_effect_if_log = if_else(startsWith(y, "log"), pct_from_log(r$estimate), NA_real_),
    effect_points_if_share = if_else(str_detect(y, "share"), r$estimate, NA_real_),
    n_obs = nobs(mod),
    n_units = n_distinct(main_no_transition$unit)
  )
})
save_csv(main_results, file.path(OUT_TABLES, "commerce_did_main_2019_2024.csv"))

# Event study
base_rel <- -1 # 2022Q1
rel_vals <- sort(unique(main$rel_q))
event_results <- list()
pretrend_rows <- list()
for (y in c("log_sales", "log_transactions", "night_share", "night_tx_share")) {
  es <- main
  terms <- character(0)
  for (r in rel_vals) {
    if (r == base_rel) next
    nm <- if (r < 0) paste0("ev_m", abs(r)) else paste0("ev_p", r)
    es[[nm]] <- as.integer(es$treated == 1 & es$rel_q == r)
    terms <- c(terms, nm)
  }
  mod <- fixest::feols(as.formula(paste0(y, " ~ ", paste(terms, collapse = " + "), " | unit + qstr")), cluster = ~ unit, data = es)
  ct <- fixest::coeftable(mod)
  ci <- confint(mod)
  event_results[[y]] <- purrr::map_dfr(rel_vals, function(r) {
    if (r == base_rel) {
      return(tibble(outcome = y, rel_q = r, qstr = "2022Q1/ref", coef = 0, se = 0, ci_low = 0, ci_high = 0))
    }
    nm <- if (r < 0) paste0("ev_m", abs(r)) else paste0("ev_p", r)
    tibble(outcome = y, rel_q = r, qstr = es$qstr[match(r, es$rel_q)], coef = ct[nm, "Estimate"], se = ct[nm, "Std. Error"], ci_low = ci[nm, 1], ci_high = ci[nm, 2])
  })
  pre_p <- tryCatch({ as.numeric(fixest::wald(mod, keep = "^ev_m")$p) }, error = function(e) NA_real_)
  pretrend_rows[[y]] <- tibble(outcome = y, event_pretrend_wald_p = pre_p)
}
save_csv(bind_rows(event_results), file.path(OUT_TABLES, "commerce_event_study_coefficients_2019_2024.csv"))
save_csv(bind_rows(pretrend_rows), file.path(OUT_TABLES, "commerce_pretrend_tests_2019_2024.csv"))

# Robustness specs
specs <- tibble(
  spec = c("core_local_controls", "core_gangnam_seocho_controls", "core_all_seoul_controls", "extended_local_controls", "extended_gangnam_seocho_controls"),
  mode = c("explicit", "gangnam_seocho", "all_seoul", "explicit", "gangnam_seocho"),
  treated_type = c("core", "core", "core", "extended", "extended")
)

robust <- purrr::map_dfr(seq_len(nrow(specs)), function(i) {
  treated <- if (specs$treated_type[i] == "core") as.integer(names(TREATED_CORE)) else as.integer(names(TREATED_EXTENDED))
  controls <- if (specs$mode[i] == "explicit") as.integer(names(CONTROL_MAIN)) else NULL
  dat <- add_flags(commerce_panel, treated, controls, specs$mode[i]) %>% filter(transition == 0)
  purrr::map_dfr(c("log_sales", "log_transactions", "night_share", "night_tx_share"), function(y) {
    mod <- fixest::feols(as.formula(paste0(y, " ~ did | unit + qstr")), cluster = ~ unit, data = dat)
    r <- fixest_term_row(mod, "did")
    tibble(
      spec = specs$spec[i],
      outcome = y,
      coef = r$estimate,
      se = r$se,
      p_value = r$p_value,
      pct_effect_if_log = if_else(startsWith(y, "log"), pct_from_log(r$estimate), NA_real_),
      effect_points_if_share = if_else(str_detect(y, "share"), r$estimate, NA_real_),
      n_obs = nobs(mod),
      n_units = n_distinct(dat$unit)
    )
  })
})
save_csv(robust, file.path(OUT_TABLES, "commerce_did_robustness_2019_2024.csv"))

# Heterogeneity. Uses service-level panel produced in 04_prepare_commerce.R.
service_file <- file.path(OUT_PROCESSED, "commerce_service_dong_quarter_panel_2019_2024.csv")
if (file.exists(service_file)) {
  service_panel <- readr::read_csv(service_file, show_col_types = FALSE)
  agg_cols <- c("sales", "transactions", "weekday_sales", "weekend_sales",
                "sales_00_06", "sales_06_11", "sales_11_14", "sales_14_17", "sales_17_21", "sales_21_24",
                "cnt_00_06", "cnt_06_11", "cnt_11_14", "cnt_14_17", "cnt_17_21", "cnt_21_24")

  add_outcomes <- function(df) {
    df %>%
      mutate(
        log_sales = log(sales + 1),
        log_transactions = log(transactions + 1),
        night_sales = sales_21_24 + sales_00_06,
        night_share = night_sales / if_else(sales == 0, NA_real_, sales),
        district_code = dong_code %/% 1000
      )
  }

  agg_service <- function(filter_expr, label) {
    service_panel %>%
      filter({{ filter_expr }}) %>%
      group_by(dong_code, dong_name, quarter_code, year, quarter, qstr, q_index, rel_q, post, transition) %>%
      summarise(across(all_of(agg_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
      add_outcomes() %>%
      mutate(category = label)
  }

  sector_panels <- bind_rows(
    agg_service(str_detect(service_name, "음식점|커피|분식|제과|패스트푸드|호프|치킨|일식|중식|양식|주점"), "food_beverage"),
    agg_service(str_detect(service_name, "편의점|슈퍼마켓|화장품|의류|신발|문구|가전|핸드폰|약국"), "retail_convenience"),
    agg_service(str_detect(service_name, "의원|치과|한의원|의료|피부관리|미용|네일"), "medical_service")
  )

  key_services <- c("한식음식점", "커피-음료", "편의점", "일반의원", "호프-간이주점", "분식전문점", "제과점", "패스트푸드점")
  service_panels <- purrr::map_dfr(key_services, function(svc) agg_service(service_name == svc, svc))
  heter_panel <- bind_rows(sector_panels, service_panels)

  heter <- heter_panel %>%
    group_by(category) %>%
    group_modify(~ {
      dat <- add_flags(.x, as.integer(names(TREATED_CORE)), as.integer(names(CONTROL_MAIN)), "explicit") %>% filter(transition == 0)
      purrr::map_dfr(c("log_sales", "log_transactions", "night_share"), function(y) {
        mod <- fixest::feols(as.formula(paste0(y, " ~ did | unit + qstr")), cluster = ~ unit, data = dat)
        r <- fixest_term_row(mod, "did")
        tibble(
          outcome = y,
          coef = r$estimate,
          se = r$se,
          p_value = r$p_value,
          pct_effect_if_log = if_else(startsWith(y, "log"), pct_from_log(r$estimate), NA_real_),
          effect_points_if_share = if_else(str_detect(y, "share"), r$estimate, NA_real_)
        )
      })
    }) %>% ungroup()
  save_csv(heter, file.path(OUT_TABLES, "commerce_heterogeneity_2019_2024.csv"))
}

# SCM for commerce log sales, local controls.
wide <- main %>%
  filter(transition == 0) %>%
  select(quarter_code, unit, log_sales, treated) %>%
  tidyr::pivot_wider(names_from = unit, values_from = log_sales) %>%
  arrange(quarter_code)

treat_units <- main %>% filter(treated == 1) %>% distinct(unit) %>% pull(unit)
control_units <- main %>% filter(treated == 0) %>% distinct(unit) %>% pull(unit)
y1 <- rowMeans(wide[, treat_units], na.rm = TRUE)
x0 <- as.matrix(wide[, control_units])
pre_idx <- wide$quarter_code <= 20221 & complete.cases(x0) & !is.na(y1)
if (sum(pre_idx) >= 4) {
  w <- fit_scm_weights(y1[pre_idx], x0[pre_idx, , drop = FALSE])
  y_syn <- as.numeric(x0 %*% w)
  scm_ts <- tibble(quarter_code = wide$quarter_code, treated_log_sales = y1, synthetic_log_sales = y_syn, gap_log = y1 - y_syn) %>%
    mutate(period = case_when(quarter_code <= 20221 ~ "pre", quarter_code >= POST_Q ~ "post", TRUE ~ "transition"))
  save_csv(tibble(unit = control_units, weight = w) %>% arrange(desc(weight)), file.path(OUT_TABLES, "commerce_scm_weights.csv"))
  save_csv(scm_ts, file.path(OUT_TABLES, "commerce_scm_timeseries.csv"))
  save_csv(
    tibble(
      analysis = "commerce_scm_log_sales",
      pre_rmspe = sqrt(mean(scm_ts$gap_log[scm_ts$period == "pre"]^2, na.rm = TRUE)),
      post_average_gap_log = mean(scm_ts$gap_log[scm_ts$period == "post"], na.rm = TRUE),
      post_average_gap_pct = pct_from_log(mean(scm_ts$gap_log[scm_ts$period == "post"], na.rm = TRUE)),
      n_controls = length(control_units)
    ),
    file.path(OUT_TABLES, "commerce_scm_summary.csv")
  )
}

# Plots
trend <- main %>% filter(transition == 0) %>% group_by(quarter_code, qstr, treated) %>% summarise(sales = mean(sales), .groups = "drop") %>% mutate(group = if_else(treated == 1, "Treated", "Control"))
ggplot(trend, aes(qstr, sales, group = group, linetype = group)) +
  geom_line() + geom_point() + geom_vline(xintercept = "2022Q2", linetype = "dashed") +
  labs(title = "Commercial sales: treated vs control dongs (2019–2024)", x = "Quarter", y = "Mean sales") +
  theme_minimal() + theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(file.path(OUT_FIGURES, "commerce_trend_sales_2019_2024.png"), width = 10, height = 5, dpi = 200)

event_df <- readr::read_csv(file.path(OUT_TABLES, "commerce_event_study_coefficients_2019_2024.csv"), show_col_types = FALSE)
for (y in c("log_sales", "night_share")) {
  p <- event_df %>% filter(outcome == y)
  ggplot(p, aes(rel_q, coef)) +
    geom_hline(yintercept = 0) + geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point() + geom_errorbar(aes(ymin = coef - 1.96 * se, ymax = coef + 1.96 * se), width = 0.15) +
    labs(title = paste("Commercial event study:", y), x = "Quarters relative to 2022Q2 transition", y = "Effect") +
    theme_minimal()
  ggsave(file.path(OUT_FIGURES, paste0("commerce_event_study_", y, "_2019_2024.png")), width = 10, height = 5, dpi = 200)
}

message("Commerce analysis complete.")
