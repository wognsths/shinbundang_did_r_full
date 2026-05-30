# ------------------------------------------------------------
# 07_analyze_less_central.R
# Anchor vs secondary-corridor commerce add-on analysis
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing less-central commerce split...")

LESS_CENTRAL_DIR <- file.path(OUT_DIR, "less_central")
LESS_CENTRAL_TABLES <- file.path(LESS_CENTRAL_DIR, "tables")
LESS_CENTRAL_FIGURES <- file.path(LESS_CENTRAL_DIR, "figures")
for (d in c(LESS_CENTRAL_DIR, LESS_CENTRAL_TABLES, LESS_CENTRAL_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

commerce_panel_path <- file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv")
service_panel_path <- file.path(OUT_PROCESSED, "commerce_service_dong_quarter_panel_2019_2024.csv")
if (!file.exists(commerce_panel_path) || !file.exists(service_panel_path)) {
  source("R/04_prepare_commerce.R")
}

commerce_panel <- readr::read_csv(commerce_panel_path, show_col_types = FALSE)

ANCHOR_TREATED <- c(
  `11680510` = "신사동",
  `11680640` = "역삼1동"
)

SECONDARY_TREATED <- c(
  `11680521` = "논현1동",
  `11650531` = "서초4동"
)

LOCAL_CONTROLS <- CONTROL_MAIN

ANCHOR_CODES <- as.integer(names(ANCHOR_TREATED))
SECONDARY_CODES <- as.integer(names(SECONDARY_TREATED))
CONTROL_CODES <- as.integer(names(LOCAL_CONTROLS))
ALL_SPLIT_CODES <- c(ANCHOR_CODES, SECONDARY_CODES, CONTROL_CODES)

COMMERCE_OUTCOMES <- c(
  "log_sales", "log_transactions", "log_avg_ticket",
  "weekend_share", "night_share", "night_tx_share"
)

add_unit <- function(df) {
  df %>% mutate(unit = paste0(dong_code, "_", dong_name))
}

effect_row <- function(model, term, outcome, extra = list()) {
  r <- fixest_term_row(model, term)
  tibble(
    outcome = outcome,
    term = term,
    coef = r$estimate,
    se = r$se,
    p_value = r$p_value,
    ci_low = r$ci_low,
    ci_high = r$ci_high,
    pct_effect_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$estimate), NA_real_),
    ci_low_pct_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$ci_low), NA_real_),
    ci_high_pct_if_log = if_else(startsWith(outcome, "log"), pct_from_log(r$ci_high), NA_real_),
    effect_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$estimate, NA_real_),
    ci_low_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$ci_low, NA_real_),
    ci_high_pp_if_share = if_else(str_detect(outcome, "share"), 100 * r$ci_high, NA_real_)
  ) %>% bind_cols(as_tibble(extra))
}

fit_did <- function(dat, outcomes, term = "did", extra = list()) {
  purrr::map_dfr(outcomes, function(y) {
    mod <- fixest::feols(
      as.formula(paste0(y, " ~ ", term, " | unit + qstr")),
      cluster = ~ unit,
      data = dat
    )
    effect_row(
      mod, term, y,
      c(extra, list(n_obs = nobs(mod), n_units = n_distinct(dat$unit)))
    )
  })
}

# 1) Secondary corridor only: 논현1동+서초4동 vs local controls.
secondary_only <- commerce_panel %>%
  filter(dong_code %in% c(SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
  add_unit() %>%
  mutate(
    secondary = as.integer(dong_code %in% SECONDARY_CODES),
    did = secondary * post
  )

secondary_only_results <- fit_did(
  secondary_only,
  COMMERCE_OUTCOMES,
  "did",
  list(spec = "secondary_only_vs_local_controls", treated_group = "secondary")
) %>%
  select(spec, treated_group, everything())

save_csv(
  secondary_only_results,
  file.path(LESS_CENTRAL_TABLES, "commerce_secondary_only_did.csv")
)

# 2) Anchor and secondary effects in one model.
split_panel <- commerce_panel %>%
  filter(dong_code %in% ALL_SPLIT_CODES, transition == 0) %>%
  add_unit() %>%
  mutate(
    group = case_when(
      dong_code %in% ANCHOR_CODES ~ "anchor",
      dong_code %in% SECONDARY_CODES ~ "secondary",
      TRUE ~ "control"
    ),
    anchor_post = as.integer(group == "anchor") * post,
    secondary_post = as.integer(group == "secondary") * post
  )

split_results <- purrr::map_dfr(COMMERCE_OUTCOMES, function(y) {
  mod <- fixest::feols(
    as.formula(paste0(y, " ~ anchor_post + secondary_post | unit + qstr")),
    cluster = ~ unit,
    data = split_panel
  )
  bind_rows(
    effect_row(mod, "anchor_post", y, list(group = "anchor", n_obs = nobs(mod), n_units = n_distinct(split_panel$unit))),
    effect_row(mod, "secondary_post", y, list(group = "secondary", n_obs = nobs(mod), n_units = n_distinct(split_panel$unit)))
  )
}) %>%
  select(group, everything())

save_csv(
  split_results,
  file.path(LESS_CENTRAL_TABLES, "commerce_anchor_vs_secondary_split_did.csv")
)

# 3) Single-dong DID against the same local controls.
treated_lookup <- bind_rows(
  tibble(dong_code = ANCHOR_CODES, dong_name_config = unname(ANCHOR_TREATED), treated_group = "anchor"),
  tibble(dong_code = SECONDARY_CODES, dong_name_config = unname(SECONDARY_TREATED), treated_group = "secondary")
)

single_dong_results <- purrr::map_dfr(seq_len(nrow(treated_lookup)), function(i) {
  code <- treated_lookup$dong_code[i]
  dat <- commerce_panel %>%
    filter(dong_code %in% c(code, CONTROL_CODES), transition == 0) %>%
    add_unit() %>%
    mutate(did = as.integer(dong_code == code) * post)

  fit_did(
    dat,
    COMMERCE_OUTCOMES,
    "did",
    list(
      treated_group = treated_lookup$treated_group[i],
      treated_dong_code = code,
      treated_dong_name = treated_lookup$dong_name_config[i]
    )
  )
}) %>%
  select(treated_group, treated_dong_code, treated_dong_name, everything())

save_csv(
  single_dong_results,
  file.path(LESS_CENTRAL_TABLES, "commerce_single_dong_did.csv")
)

# 4) Secondary-corridor heterogeneity by key service/category.
service_panel <- readr::read_csv(service_panel_path, show_col_types = FALSE)

agg_cols <- c(
  "sales", "transactions", "weekday_sales", "weekend_sales",
  "sales_00_06", "sales_06_11", "sales_11_14", "sales_14_17", "sales_17_21", "sales_21_24",
  "cnt_00_06", "cnt_06_11", "cnt_11_14", "cnt_14_17", "cnt_17_21", "cnt_21_24"
)

add_commerce_outcomes_lite <- function(df) {
  df %>%
    mutate(
      log_sales = log(sales + 1),
      log_transactions = log(transactions + 1),
      avg_ticket = sales / if_else(transactions == 0, NA_real_, transactions),
      log_avg_ticket = log(coalesce(avg_ticket, 0) + 1),
      weekend_share = weekend_sales / if_else(weekday_sales + weekend_sales == 0, NA_real_, weekday_sales + weekend_sales),
      night_sales = sales_21_24 + sales_00_06,
      night_share = night_sales / if_else(sales == 0, NA_real_, sales),
      night_transactions = cnt_21_24 + cnt_00_06,
      night_tx_share = night_transactions / if_else(transactions == 0, NA_real_, transactions)
    )
}

make_service_panel <- function(df, label) {
  df %>%
    group_by(dong_code, dong_name, quarter_code, year, quarter, qstr, q_index, rel_q, post, transition) %>%
    summarise(across(all_of(agg_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    add_commerce_outcomes_lite() %>%
    mutate(category = label)
}

food_pattern <- "음식점|커피|음료|분식|제과|패스트푸드|호프|치킨|일식|중식|양식|주점"
key_services <- c(
  "한식음식점", "커피-음료", "편의점", "호프-간이주점",
  "분식전문점", "제과점", "패스트푸드점"
)

service_category_panel <- bind_rows(
  make_service_panel(
    service_panel %>% filter(str_detect(service_name, food_pattern)),
    "음식·음료 전체"
  ),
  purrr::map_dfr(
    key_services,
    ~ make_service_panel(service_panel %>% filter(service_name == .x), .x)
  )
)

secondary_service_results <- service_category_panel %>%
  group_by(category) %>%
  group_modify(~ {
    dat <- .x %>%
      filter(dong_code %in% c(SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
      add_unit() %>%
      mutate(
        secondary = as.integer(dong_code %in% SECONDARY_CODES),
        did = secondary * post
      )
    fit_did(
      dat,
      c("log_sales", "log_transactions", "night_share"),
      "did",
      list(spec = "secondary_service_vs_local_controls")
    )
  }) %>%
  ungroup() %>%
  select(category, spec, everything())

save_csv(
  secondary_service_results,
  file.path(LESS_CENTRAL_TABLES, "commerce_secondary_key_service_did.csv")
)

# Figures.
three_group_trend <- split_panel %>%
  group_by(quarter_code, qstr, group) %>%
  summarise(mean_log_sales = mean(log_sales, na.rm = TRUE), .groups = "drop")

ggplot(three_group_trend, aes(qstr, mean_log_sales, group = group, linetype = group)) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.7) +
  geom_vline(xintercept = "2022Q2", linetype = "dashed") +
  labs(
    title = "Commercial sales trend: anchor, secondary corridor, controls",
    x = "Quarter",
    y = "Mean log sales"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave(
  file.path(LESS_CENTRAL_FIGURES, "commerce_three_group_sales_trend.png"),
  width = 10,
  height = 5,
  dpi = 200
)

service_sales_plot <- secondary_service_results %>%
  filter(outcome == "log_sales") %>%
  mutate(
    category = factor(category, levels = rev(category[order(pct_effect_if_log)]))
  )

ggplot(service_sales_plot, aes(pct_effect_if_log, category)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(
    aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log),
    orientation = "y",
    width = 0.18
  ) +
  labs(
    title = "Secondary corridor service-level DID: log sales",
    x = "Percent effect",
    y = NULL
  ) +
  theme_minimal()
ggsave(
  file.path(LESS_CENTRAL_FIGURES, "commerce_secondary_key_service_log_sales_effects.png"),
  width = 9,
  height = 5,
  dpi = 200
)

# Lightweight report.
lookup_result <- function(df, target_outcome, group = NULL, dong = NULL, category = NULL) {
  out <- df %>% filter(.data$outcome == target_outcome)
  if (!is.null(group)) out <- out %>% filter(.data$group == group | .data$treated_group == group)
  if (!is.null(dong)) out <- out %>% filter(.data$treated_dong_name == dong)
  if (!is.null(category)) out <- out %>% filter(.data$category == category)
  out %>% slice(1)
}

fmt_pct <- function(x) if_else(is.na(x), "NA", sprintf("%.2f%%", x))
fmt_pp <- function(x) if_else(is.na(x), "NA", sprintf("%.2f p.p.", x))
fmt_p <- function(x) if_else(is.na(x), "NA", sprintf("%.3f", x))

sec_sales <- lookup_result(secondary_only_results, "log_sales")
sec_tx <- lookup_result(secondary_only_results, "log_transactions")
sec_night <- lookup_result(secondary_only_results, "night_share")
anchor_sales <- split_results %>% filter(group == "anchor", outcome == "log_sales") %>% slice(1)
split_sec_sales <- split_results %>% filter(group == "secondary", outcome == "log_sales") %>% slice(1)
korean_food <- secondary_service_results %>% filter(category == "한식음식점", outcome == "log_sales") %>% slice(1)

report_lines <- c(
  "# Less-central commerce add-on",
  "",
  "## Setup",
  "- Anchor treated: 신사동, 역삼1동.",
  "- Secondary corridor treated: 논현1동, 서초4동.",
  "- Local controls: 강남/서초 내 직접 영향권 밖 14개 행정동.",
  "- Transition quarter 2022Q2 is excluded; post starts in 2022Q3.",
  "",
  "## Main takeaways",
  paste0("- Secondary-only total sales DID: ", fmt_pct(sec_sales$pct_effect_if_log), ", p = ", fmt_p(sec_sales$p_value), "."),
  paste0("- Secondary-only transactions DID: ", fmt_pct(sec_tx$pct_effect_if_log), ", p = ", fmt_p(sec_tx$p_value), "."),
  paste0("- Secondary-only night-sales share DID: ", fmt_pp(sec_night$effect_pp_if_share), ", p = ", fmt_p(sec_night$p_value), "."),
  paste0("- Anchor total sales DID in split model: ", fmt_pct(anchor_sales$pct_effect_if_log), ", p = ", fmt_p(anchor_sales$p_value), "."),
  paste0("- Secondary total sales DID in split model: ", fmt_pct(split_sec_sales$pct_effect_if_log), ", p = ", fmt_p(split_sec_sales$p_value), "."),
  paste0("- Secondary corridor Korean-restaurant sales DID: ", fmt_pct(korean_food$pct_effect_if_log), ", p = ", fmt_p(korean_food$p_value), "."),
  "",
  "## Interpretation",
  "Secondary corridor results do not support a strong claim that total commercial scale clearly increased.",
  "The safer interpretation is that the Shinbundang extension is associated with changes in time-of-day and sector composition, with the Korean-restaurant result best treated as exploratory heterogeneity evidence."
)

writeLines(report_lines, file.path(LESS_CENTRAL_DIR, "LESS_CENTRAL_REPORT.md"))

message("Less-central commerce split complete.")
