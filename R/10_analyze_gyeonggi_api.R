# ------------------------------------------------------------
# 10_analyze_gyeonggi_api.R
# Gyeonggi API extension: original Shinbundang south-corridor activity population
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing Gyeonggi API activity-population extension...")

GG_DIR <- file.path(OUT_DIR, "gyeonggi_api")
GG_TABLES <- file.path(GG_DIR, "tables")
GG_FIGURES <- file.path(GG_DIR, "figures")
for (d in c(GG_DIR, GG_TABLES, GG_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

GYEONGGI_DAY_RAW_PATH <- file.path(OUT_PROCESSED, "gyeonggi_flow_day_dong_api_raw.csv")
GYEONGGI_TIME_RAW_PATH <- file.path(OUT_PROCESSED, "gyeonggi_flow_time_dong_api_raw.csv")

if (!file.exists(GYEONGGI_DAY_RAW_PATH)) {
  stop("Missing Gyeonggi day-dong API raw file: ", GYEONGGI_DAY_RAW_PATH)
}
if (!file.exists(GYEONGGI_TIME_RAW_PATH)) {
  stop("Missing Gyeonggi time-dong API raw file: ", GYEONGGI_TIME_RAW_PATH)
}

day_raw <- readr::read_csv(GYEONGGI_DAY_RAW_PATH, show_col_types = FALSE)
time_raw <- readr::read_csv(GYEONGGI_TIME_RAW_PATH, show_col_types = FALSE)

dong_names <- time_raw %>%
  transmute(
    admdong_cd = as.character(.data$adstrd_cd),
    admdong_nm = as.character(.data$adstrd_nm)
  ) %>%
  distinct()

# Original Shinbundang Line south corridor around Pangyo, Jeongja, and Migeum.
# The design intentionally compares within Bundang-gu to avoid mixing in Seoul-side changes.
bundang_treated <- c(
  `4113565000` = "판교동",
  `4113565500` = "삼평동",
  `4113565700` = "백현동",
  `4113554500` = "정자동",
  `4113555000` = "정자1동",
  `4113556000` = "정자2동",
  `4113557000` = "정자3동",
  `4113566200` = "금곡동",
  `4113566500` = "구미1동",
  `4113567000` = "구미동"
)

bundang_controls <- c(
  `4113551000` = "분당동",
  `4113552000` = "수내1동",
  `4113553000` = "수내2동",
  `4113554000` = "수내3동",
  `4113558000` = "서현1동",
  `4113559000` = "서현2동",
  `4113560000` = "이매1동",
  `4113561000` = "이매2동",
  `4113562000` = "야탑1동",
  `4113563000` = "야탑2동",
  `4113564000` = "야탑3동",
  `4113568000` = "운중동"
)

study_codes <- c(names(bundang_treated), names(bundang_controls))

gyeonggi_day_panel <- day_raw %>%
  transmute(
    month = as.Date(paste0(as.character(.data$std_ym), "01"), format = "%Y%m%d"),
    month_str = format(month, "%Y-%m"),
    admdong_cd = as.character(.data$admdong_cd),
    wday_cd = as.character(.data$wday_cd),
    dynmc_popltn_cnt = parse_num(.data$dynmc_popltn_cnt)
  ) %>%
  left_join(dong_names, by = "admdong_cd") %>%
  filter(admdong_cd %in% study_codes, wday_cd != "TOT") %>%
  mutate(
    group = if_else(admdong_cd %in% names(bundang_treated), "treated_south_corridor", "bundang_controls"),
    treated = as.integer(group == "treated_south_corridor"),
    post = as.integer(month >= POST_MONTH),
    transition = as.integer(month == TRANSITION_MONTH),
    weekday_type = if_else(wday_cd %in% c("SAT", "SUN"), "weekend", "weekday")
  ) %>%
  group_by(admdong_cd, admdong_nm, group, treated, month, month_str, post, transition) %>%
  summarise(
    total_avg_pop = mean(dynmc_popltn_cnt, na.rm = TRUE),
    weekday_avg_pop = mean(dynmc_popltn_cnt[weekday_type == "weekday"], na.rm = TRUE),
    weekend_avg_pop = mean(dynmc_popltn_cnt[weekday_type == "weekend"], na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    did = treated * post,
    weekend_share = weekend_avg_pop / (weekday_avg_pop + weekend_avg_pop),
    log_total_avg_pop = log(total_avg_pop + 1),
    log_weekday_avg_pop = log(weekday_avg_pop + 1),
    log_weekend_avg_pop = log(weekend_avg_pop + 1)
  ) %>%
  filter(transition == 0) %>%
  group_by(admdong_cd) %>%
  filter(any(post == 0), any(post == 1), n_distinct(month) >= 24) %>%
  ungroup()

save_csv(gyeonggi_day_panel, file.path(OUT_PROCESSED, "gyeonggi_bundang_day_dong_panel_2018_2025.csv"))

gg_effect_row <- function(model, term, outcome) {
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
  )
}

gg_fit_one <- function(dat, outcome) {
  mod <- fixest::feols(
    as.formula(paste0(outcome, " ~ did | admdong_cd + month_str")),
    cluster = ~ admdong_cd,
    data = dat
  )
  gg_effect_row(mod, "did", outcome) %>%
    mutate(n_obs = nobs(mod), n_units = n_distinct(dat$admdong_cd))
}

gyeonggi_day_did <- purrr::map_dfr(
  c("log_total_avg_pop", "log_weekday_avg_pop", "log_weekend_avg_pop", "weekend_share"),
  ~ gg_fit_one(gyeonggi_day_panel, .x)
) %>%
  mutate(
    hypothesis = "original_shinbundang_south_corridor_activity",
    treated_definition = "Pangyo/Jeongja/Migeum corridor dongs",
    control_definition = "Other Bundang-gu dongs"
  )

save_csv(gyeonggi_day_did, file.path(GG_TABLES, "gyeonggi_bundang_day_dong_did.csv"))

window_specs <- tibble::tribble(
  ~window, ~start_month, ~end_month, ~note,
  "2021_2022", as.Date("2021-01-01"), as.Date("2022-12-01"), "closest two-year window around opening; avoids the 2023 data-level break",
  "2020_2022", as.Date("2020-01-01"), as.Date("2022-12-01"), "three-year pre/opening window; avoids the 2023 data-level break",
  "2021_2023", as.Date("2021-01-01"), as.Date("2023-12-01"), "includes 2023 and is more exposed to the data-level break",
  "full", min(gyeonggi_day_panel$month), max(gyeonggi_day_panel$month), "full available API period"
)

gyeonggi_window_robustness <- purrr::pmap_dfr(window_specs, function(window, start_month, end_month, note) {
  dat <- gyeonggi_day_panel %>%
    filter(month >= start_month, month <= end_month)

  purrr::map_dfr(c("log_total_avg_pop", "log_weekday_avg_pop", "log_weekend_avg_pop"), function(outcome) {
    gg_fit_one(dat, outcome) %>%
      mutate(window = window, start_month = start_month, end_month = end_month, note = note, .before = 1)
  })
})

save_csv(gyeonggi_window_robustness, file.path(GG_TABLES, "gyeonggi_bundang_window_robustness.csv"))

dong_level_did <- purrr::map_dfr(names(bundang_treated), function(code) {
  dat <- gyeonggi_day_panel %>%
    filter(admdong_cd %in% c(code, names(bundang_controls))) %>%
    mutate(single_treated = as.integer(admdong_cd == code), single_did = single_treated * post)

  mod <- fixest::feols(
    log_total_avg_pop ~ single_did | admdong_cd + month_str,
    cluster = ~ admdong_cd,
    data = dat
  )

  r <- fixest_term_row(mod, "single_did")
  tibble(
    admdong_cd = code,
    admdong_nm = unname(bundang_treated[code]),
    coef = r$estimate,
    se = r$se,
    p_value = r$p_value,
    pct_effect = pct_from_log(r$estimate),
    ci_low_pct = pct_from_log(r$ci_low),
    ci_high_pct = pct_from_log(r$ci_high),
    n_obs = nobs(mod)
  )
})

save_csv(dong_level_did, file.path(GG_TABLES, "gyeonggi_bundang_single_dong_did.csv"))

trend <- gyeonggi_day_panel %>%
  group_by(month, group) %>%
  summarise(mean_log_total = mean(log_total_avg_pop, na.rm = TRUE), .groups = "drop")

ggplot(trend, aes(month, mean_log_total, linetype = group)) +
  geom_vline(xintercept = POST_MONTH, linetype = "dashed") +
  geom_line(linewidth = 0.5) +
  labs(
    title = "Gyeonggi activity population trend: original Shinbundang corridor",
    x = "Month",
    y = "Mean log daily activity population",
    linetype = NULL
  ) +
  theme_minimal()
ggsave(file.path(GG_FIGURES, "gyeonggi_bundang_activity_trend.png"), width = 9, height = 4.8, dpi = 200)

plot_did <- gyeonggi_day_did %>%
  mutate(
    label = recode(
      outcome,
      log_total_avg_pop = "Total daily average",
      log_weekday_avg_pop = "Weekday average",
      log_weekend_avg_pop = "Weekend average",
      weekend_share = "Weekend share"
    ),
    effect = if_else(is.na(pct_effect_if_log), effect_pp_if_share, pct_effect_if_log),
    lo = if_else(is.na(ci_low_pct_if_log), ci_low_pp_if_share, ci_low_pct_if_log),
    hi = if_else(is.na(ci_high_pct_if_log), ci_high_pp_if_share, ci_high_pct_if_log),
    effect_type = if_else(str_detect(outcome, "share"), "p.p.", "%")
  )

ggplot(plot_did, aes(reorder(label, effect), effect)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_errorbar(aes(ymin = lo, ymax = hi), width = 0.2) +
  geom_point(size = 2) +
  coord_flip() +
  facet_wrap(~ effect_type, scales = "free_x") +
  labs(
    title = "DID estimates for original Shinbundang south corridor",
    x = NULL,
    y = "Effect"
  ) +
  theme_minimal()
ggsave(file.path(GG_FIGURES, "gyeonggi_bundang_did_effects.png"), width = 8, height = 4.8, dpi = 200)

single_plot <- dong_level_did %>%
  arrange(pct_effect) %>%
  mutate(
    admdong_label = recode(
      admdong_nm,
      "판교동" = "Pangyo",
      "삼평동" = "Sampyeong",
      "백현동" = "Baekhyeon",
      "정자동" = "Jeongja",
      "정자1동" = "Jeongja 1",
      "정자2동" = "Jeongja 2",
      "정자3동" = "Jeongja 3",
      "금곡동" = "Geumgok",
      "구미1동" = "Gumi 1",
      "구미동" = "Gumi"
    ),
    admdong_label = factor(admdong_label, levels = admdong_label)
  )

ggplot(single_plot, aes(admdong_label, pct_effect)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_errorbar(aes(ymin = ci_low_pct, ymax = ci_high_pct), width = 0.2) +
  geom_point(size = 2) +
  coord_flip() +
  labs(
    title = "Single-dong DID: total daily activity population",
    x = NULL,
    y = "Percent effect"
  ) +
  theme_minimal()
ggsave(file.path(GG_FIGURES, "gyeonggi_bundang_single_dong_effects.png"), width = 8, height = 5.4, dpi = 200)

fmt_pct_gg <- function(x) if_else(is.na(x), "NA", sprintf("%.2f%%", x))
fmt_pp_gg <- function(x) if_else(is.na(x), "NA", sprintf("%.2f p.p.", x))
fmt_p_gg <- function(x) if_else(is.na(x), "NA", if_else(x < 0.001, "<0.001", sprintf("%.3f", x)))

main_lines <- gyeonggi_day_did %>%
  transmute(
    outcome,
    effect = if_else(is.na(pct_effect_if_log), fmt_pp_gg(effect_pp_if_share), fmt_pct_gg(pct_effect_if_log)),
    p_value = fmt_p_gg(p_value)
  )

robust_lines <- gyeonggi_window_robustness %>%
  filter(outcome == "log_total_avg_pop") %>%
  transmute(
    window,
    effect = fmt_pct_gg(pct_effect_if_log),
    p_value = fmt_p_gg(p_value),
    note
  )

report <- c(
  "# Gyeonggi API Extension: Original Shinbundang Corridor",
  "",
  "This extension uses the Gyeonggi Data Dream OpenAPI endpoint `TB25BPTPOPDAYDONGM`.",
  "The related time-zone endpoint was checked, but its dong-level rows repeat the same district-level value within each district, so the DID uses the weekday-by-dong endpoint where values vary by administrative dong.",
  "",
  "## Design",
  "",
  "- Treated: Pangyo, Jeongja, and Migeum corridor dongs in Bundang-gu.",
  "- Controls: other Bundang-gu administrative dongs outside the direct Shinbundang corridor.",
  "- Post period: June 2022 onward; May 2022 is excluded as transition.",
  "- Unit: administrative dong by month.",
  "",
  "## Main DID Results",
  "",
  "The full-period result is reported for completeness, but the API has a visible level break around early 2023. The closest 2021-2022 window is therefore the safer causal specification.",
  "",
  paste0("- ", main_lines$outcome, ": ", main_lines$effect, ", p=", main_lines$p_value),
  "",
  "## Window Robustness: Total Daily Activity Population",
  "",
  paste0("- ", robust_lines$window, ": ", robust_lines$effect, ", p=", robust_lines$p_value, " (", robust_lines$note, ")"),
  "",
  "## Key Outputs",
  "",
  "- `outputs/gyeonggi_api/tables/gyeonggi_bundang_day_dong_did.csv`",
  "- `outputs/gyeonggi_api/tables/gyeonggi_bundang_window_robustness.csv`",
  "- `outputs/gyeonggi_api/tables/gyeonggi_bundang_single_dong_did.csv`",
  "- `outputs/gyeonggi_api/figures/gyeonggi_bundang_activity_trend.png`",
  "- `outputs/gyeonggi_api/figures/gyeonggi_bundang_did_effects.png`",
  "- `outputs/gyeonggi_api/figures/gyeonggi_bundang_single_dong_effects.png`"
)

writeLines(report, file.path(GG_DIR, "GYEONGGI_API_EXTENSION_REPORT.md"))

message("Gyeonggi API extension complete.")
