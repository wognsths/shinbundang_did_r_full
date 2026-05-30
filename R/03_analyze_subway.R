# ------------------------------------------------------------
# 03_analyze_subway.R
# DID, event study, robustness, SCM for subway outcomes
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing subway panel...")
subway_m <- readr::read_csv(file.path(OUT_PROCESSED, "subway_monthly_panel_2018_2024.csv"), show_col_types = FALSE) %>%
  mutate(month = as.Date(month))

main <- subway_m %>% filter(transition == 0)

m_did <- fixest::feols(
  log_avg_daily_riders ~ did | station_line + month_str,
  cluster = ~ station_line,
  data = main
)

main_row <- fixest_term_row(m_did, "did") %>%
  transmute(
    analysis = "subway_monthly_DID_2018_2024",
    coef_log = estimate,
    se = se,
    p_value = p_value,
    ci_low_log = ci_low,
    ci_high_log = ci_high,
    pct_effect = pct_from_log(estimate),
    ci_low_pct = pct_from_log(ci_low),
    ci_high_pct = pct_from_log(ci_high),
    n_obs = nobs(m_did),
    n_units = n_distinct(main$station_line)
  )
save_csv(main_row, file.path(OUT_TABLES, "subway_did_main.csv"))

save_csv(
  main %>% group_by(treated, post) %>% summarise(avg_daily_riders = mean(avg_daily_riders), .groups = "drop"),
  file.path(OUT_TABLES, "subway_group_means.csv")
)

# Event study: reference is rel_month = -2, i.e., 2022-04. 2022-05 is transition month.
ref <- -2
es <- subway_m
rel_vals <- sort(unique(es$rel_month))
event_terms <- character(0)
for (r in rel_vals) {
  if (r == ref) next
  nm <- if (r < 0) paste0("es_m", abs(r)) else paste0("es_p", r)
  es[[nm]] <- as.integer(es$treated == 1 & es$rel_month == r)
  event_terms <- c(event_terms, nm)
}

es_formula <- as.formula(paste0("log_avg_daily_riders ~ ", paste(event_terms, collapse = " + "), " | station_line + month_str"))
m_es <- fixest::feols(es_formula, cluster = ~ station_line, data = es)
ct <- fixest::coeftable(m_es)
ci <- confint(m_es)

event_rows <- purrr::map_dfr(rel_vals, function(r) {
  if (r == ref) {
    return(tibble(rel_month = r, month_str = "2022-04/ref", coef_log = 0, se = 0, ci_low_log = 0, ci_high_log = 0, pct_effect = 0))
  }
  nm <- if (r < 0) paste0("es_m", abs(r)) else paste0("es_p", r)
  tibble(
    rel_month = r,
    month_str = format(POST_MONTH %m+% months(r), "%Y-%m"),
    coef_log = ct[nm, "Estimate"],
    se = ct[nm, "Std. Error"],
    ci_low_log = ci[nm, 1],
    ci_high_log = ci[nm, 2],
    pct_effect = pct_from_log(ct[nm, "Estimate"])
  )
})
save_csv(event_rows, file.path(OUT_TABLES, "subway_event_study_coefficients.csv"))

# Pretrend checks
pre <- main %>%
  filter(month <= as.Date("2022-04-01")) %>%
  mutate(month_index = 12 * (year(month) - min(year(month))) + month(month), treat_time = treated * month_index)

m_pre <- fixest::feols(
  log_avg_daily_riders ~ treat_time | station_line + month_str,
  cluster = ~ station_line,
  data = pre
)
pre_row <- fixest_term_row(m_pre, "treat_time")
pre_event_p <- tryCatch({
  as.numeric(fixest::wald(m_es, keep = "^es_m")$p)
}, error = function(e) NA_real_)

save_csv(
  tibble(
    analysis = "subway_pretrend",
    event_pretrend_wald_p = pre_event_p,
    linear_treat_time_coef = pre_row$estimate,
    linear_p_value = pre_row$p_value
  ),
  file.path(OUT_TABLES, "subway_pretrend_tests.csv")
)

# Robustness
windows <- tibble(
  window = c("2018_2024", "2019_2024", "2020_2024", "2021_2024"),
  start = as.Date(c("2018-01-01", "2019-01-01", "2020-01-01", "2021-01-01")),
  end = as.Date("2024-12-01")
)
control_sets <- list(
  all_controls = CONTROL_STATIONS,
  drop_9line_phase_controls = CONTROL_STATIONS[!startsWith(CONTROL_STATIONS, "9호선2~3단계")],
  core_area_controls = c(
    "2호선_역삼", "2호선_선릉", "2호선_삼성(무역센터)",
    "3호선_압구정", "3호선_매봉",
    "7호선_학동", "7호선_강남구청", "7호선_청담",
    "9호선_사평", "9호선_고속터미널",
    "9호선2~3단계_언주", "9호선2~3단계_선정릉", "9호선2~3단계_봉은사"
  )
)

robust <- purrr::map_dfr(seq_len(nrow(windows)), function(i) {
  purrr::imap_dfr(control_sets, function(ctrl, cname) {
    dat <- subway_m %>%
      filter(month >= windows$start[i], month <= windows$end[i], transition == 0, station_line %in% c(TREATED_STATIONS, ctrl)) %>%
      mutate(treated = as.integer(station_line %in% TREATED_STATIONS), post = as.integer(month >= POST_MONTH), did = treated * post)
    mod <- fixest::feols(log_avg_daily_riders ~ did | station_line + month_str, cluster = ~ station_line, data = dat)
    r <- fixest_term_row(mod, "did")
    tibble(
      window = windows$window[i],
      control_set = cname,
      coef_log = r$estimate,
      se = r$se,
      p_value = r$p_value,
      pct_effect = pct_from_log(r$estimate),
      ci_low_pct = pct_from_log(r$ci_low),
      ci_high_pct = pct_from_log(r$ci_high),
      n_obs = nobs(mod),
      n_units = n_distinct(dat$station_line)
    )
  })
})
save_csv(robust, file.path(OUT_TABLES, "subway_did_robustness.csv"))

# Synthetic control: aggregate treated mean vs weighted controls in log outcome.
wide <- subway_m %>%
  filter(transition == 0) %>%
  select(month, station_line, log_avg_daily_riders) %>%
  tidyr::pivot_wider(names_from = station_line, values_from = log_avg_daily_riders) %>%
  arrange(month)

y1 <- rowMeans(wide[, TREATED_STATIONS], na.rm = TRUE)
x0 <- as.matrix(wide[, CONTROL_STATIONS])
pre_idx <- wide$month <= as.Date("2022-04-01") & complete.cases(x0) & !is.na(y1)
w <- fit_scm_weights(y1[pre_idx], x0[pre_idx, , drop = FALSE])
y_syn <- as.numeric(x0 %*% w)
scm_ts <- tibble(month = wide$month, treated_log = y1, synthetic_log = y_syn, gap_log = y1 - y_syn) %>%
  mutate(period = case_when(month <= as.Date("2022-04-01") ~ "pre", month >= POST_MONTH ~ "post", TRUE ~ "transition"))

save_csv(tibble(station_line = CONTROL_STATIONS, weight = w) %>% arrange(desc(weight)), file.path(OUT_TABLES, "subway_scm_weights.csv"))
save_csv(scm_ts, file.path(OUT_TABLES, "subway_scm_timeseries.csv"))
save_csv(
  tibble(
    analysis = "subway_scm_log_riders",
    pre_rmspe = sqrt(mean(scm_ts$gap_log[scm_ts$period == "pre"]^2, na.rm = TRUE)),
    post_average_gap_log = mean(scm_ts$gap_log[scm_ts$period == "post"], na.rm = TRUE),
    post_average_gap_pct = pct_from_log(mean(scm_ts$gap_log[scm_ts$period == "post"], na.rm = TRUE)),
    n_controls = length(CONTROL_STATIONS)
  ),
  file.path(OUT_TABLES, "subway_scm_summary.csv")
)

# Time-band DID, if prepared.
timeband_file <- file.path(OUT_PROCESSED, "subway_timeband_monthly_panel_2018_2024.csv")
if (file.exists(timeband_file)) {
  tb <- readr::read_csv(timeband_file, show_col_types = FALSE) %>% mutate(month = as.Date(month))
  tb_main <- tb %>% filter(transition == 0)
  outcomes <- c("total", "morning_peak", "evening_peak", "late_night", "daytime")
  tb_results <- purrr::map_dfr(outcomes, function(y) {
    mod <- fixest::feols(as.formula(paste0("log_", y, " ~ did | station_line + month_str")), cluster = ~ station_line, data = tb_main)
    r <- fixest_term_row(mod, "did")
    tibble(
      outcome = y,
      coef_log = r$estimate,
      se = r$se,
      p_value = r$p_value,
      pct_effect = pct_from_log(r$estimate),
      ci_low_pct = pct_from_log(r$ci_low),
      ci_high_pct = pct_from_log(r$ci_high),
      n_obs = nobs(mod),
      n_units = n_distinct(tb_main$station_line)
    )
  })
  save_csv(tb_results, file.path(OUT_TABLES, "subway_timeband_did.csv"))
}

# Plots
trend <- main %>% group_by(month, group) %>% summarise(avg_daily_riders = mean(avg_daily_riders), .groups = "drop")
ggplot(trend, aes(month, avg_daily_riders, linetype = group)) +
  geom_line() +
  geom_vline(xintercept = OPEN_DATE, linetype = "dashed") +
  labs(title = "Subway ridership: treated vs control stations (2018–2024)", x = "Month", y = "Average daily riders") +
  theme_minimal()
ggsave(file.path(OUT_FIGURES, "subway_trend_treated_control_2018_2024.png"), width = 11, height = 5, dpi = 200)

plot_es <- event_rows %>% filter(rel_month >= -36, rel_month <= 30)
ggplot(plot_es, aes(rel_month, coef_log)) +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 1.6) +
  geom_errorbar(aes(ymin = coef_log - 1.96 * se, ymax = coef_log + 1.96 * se), width = 0.2) +
  labs(title = "Subway event study (reference: Apr 2022)", x = "Months relative to Jun 2022", y = "Log-point effect") +
  theme_minimal()
ggsave(file.path(OUT_FIGURES, "subway_event_study_2018_2024.png"), width = 12, height = 5, dpi = 200)

ggplot(scm_ts, aes(month)) +
  geom_line(aes(y = treated_log, linetype = "Treated mean")) +
  geom_line(aes(y = synthetic_log, linetype = "Synthetic control")) +
  geom_vline(xintercept = OPEN_DATE, linetype = "dashed") +
  labs(title = "Subway synthetic control", x = "Month", y = "Log average daily riders", linetype = NULL) +
  theme_minimal()
ggsave(file.path(OUT_FIGURES, "subway_scm_2018_2024.png"), width = 11, height = 5, dpi = 200)

message("Subway analysis complete.")
