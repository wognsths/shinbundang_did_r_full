# ------------------------------------------------------------
# 08_analyze_commute_composition.R
# Commute-flow and consumer-composition analysis
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing commute and composition patterns...")

COMP_DIR <- file.path(OUT_DIR, "commute_composition")
COMP_TABLES <- file.path(COMP_DIR, "tables")
COMP_FIGURES <- file.path(COMP_DIR, "figures")
for (d in c(COMP_DIR, COMP_TABLES, COMP_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

subway_daily_path <- file.path(OUT_PROCESSED, "subway_daily_panel_2018_2024.csv")
subway_timeband_path <- file.path(OUT_PROCESSED, "subway_timeband_monthly_panel_2018_2024.csv")
commerce_panel_path <- file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv")
service_panel_path <- file.path(OUT_PROCESSED, "commerce_service_dong_quarter_panel_2019_2024.csv")

if (!file.exists(subway_daily_path) || !file.exists(subway_timeband_path)) source("R/02_prepare_subway.R")
if (!file.exists(commerce_panel_path) || !file.exists(service_panel_path)) source("R/04_prepare_commerce.R")

ANCHOR_TREATED <- c(
  `11680510` = "신사동",
  `11680640` = "역삼1동"
)

SECONDARY_TREATED <- c(
  `11680521` = "논현1동",
  `11650531` = "서초4동"
)

ANCHOR_CODES <- as.integer(names(ANCHOR_TREATED))
SECONDARY_CODES <- as.integer(names(SECONDARY_TREATED))
CONTROL_CODES <- as.integer(names(CONTROL_MAIN))

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

fit_one_term <- function(dat, outcomes, term = "did", fe = "unit + qstr", cluster = "unit", extra = list()) {
  purrr::map_dfr(outcomes, function(y) {
    mod <- fixest::feols(
      as.formula(paste0(y, " ~ ", term, " | ", fe)),
      cluster = as.formula(paste0("~ ", cluster)),
      data = dat
    )
    effect_row(mod, term, y, c(extra, list(n_obs = nobs(mod), n_units = n_distinct(dat[[cluster]]))))
  })
}

safe_divide <- function(num, den) {
  num / if_else(den == 0, NA_real_, den)
}

# Hypothesis A: direction/time-band subway reallocation.
tb <- readr::read_csv(subway_timeband_path, show_col_types = FALSE)

time_cols <- function(hours, direction) {
  prefixes <- sprintf("%02d시-", hours)
  names(tb)[purrr::map_lgl(names(tb), function(nm) {
    any(startsWith(nm, prefixes)) && str_detect(nm, direction)
  })]
}

make_timeband_outcome <- function(hours, direction, label) {
  cols <- time_cols(hours, direction)
  tb[[label]] <<- rowSums(as.data.frame(lapply(tb[cols], parse_num)), na.rm = TRUE)
  tb[[paste0("log_", label)]] <<- log(tb[[label]] + 1)
  paste0("log_", label)
}

direction_specs <- tibble(
  hypothesis = "A_commute_direction",
  label = c("morning_alight_07_10", "evening_board_17_20", "late_night_board_21_24", "late_night_alight_21_24"),
  display = c("출근시간 하차 07-10시", "퇴근시간 승차 17-20시", "야간 승차 21-24시", "야간 하차 21-24시"),
  hours = list(c(7, 8, 9), c(17, 18, 19), c(21, 22, 23), c(21, 22, 23)),
  direction = c("하차인원", "승차인원", "승차인원", "하차인원")
)

for (i in seq_len(nrow(direction_specs))) {
  make_timeband_outcome(direction_specs$hours[[i]], direction_specs$direction[i], direction_specs$label[i])
}

tb_main <- tb %>%
  filter(transition == 0) %>%
  mutate(station_cluster = station_line)

subway_direction_results <- purrr::map_dfr(seq_len(nrow(direction_specs)), function(i) {
  y <- paste0("log_", direction_specs$label[i])
  mod <- fixest::feols(
    as.formula(paste0(y, " ~ did | station_line + month_str")),
    cluster = ~ station_line,
    data = tb_main
  )
  effect_row(
    mod,
    "did",
    y,
    list(
      hypothesis = direction_specs$hypothesis[i],
      display = direction_specs$display[i],
      n_obs = nobs(mod),
      n_units = n_distinct(tb_main$station_line)
    )
  )
}) %>%
  select(hypothesis, display, everything())

save_csv(subway_direction_results, file.path(COMP_TABLES, "subway_commute_direction_did.csv"))

# Hypothesis B: weekday vs weekend subway reallocation.
daily <- readr::read_csv(subway_daily_path, show_col_types = FALSE) %>%
  mutate(
    date = as.Date(date),
    month = as.Date(month),
    date_str = format(date, "%Y-%m-%d"),
    weekday_type = if_else(lubridate::wday(date, week_start = 1) <= 5, "weekday", "weekend"),
    post = as.integer(month >= POST_MONTH),
    transition = as.integer(month == TRANSITION_MONTH),
    did = treated * post,
    station_cluster = station_line
  ) %>%
  filter(transition == 0)

subway_weekday_weekend_results <- purrr::map_dfr(c("weekday", "weekend"), function(sample_name) {
  dat <- daily %>% filter(weekday_type == sample_name)
  mod <- fixest::feols(log_riders ~ did | station_line + date_str, cluster = ~ station_line, data = dat)
  effect_row(
    mod,
    "did",
    "log_riders",
    list(
      hypothesis = "B_weekday_weekend",
      sample = sample_name,
      n_obs = nobs(mod),
      n_units = n_distinct(dat$station_line)
    )
  )
}) %>%
  select(hypothesis, sample, everything())

save_csv(subway_weekday_weekend_results, file.path(COMP_TABLES, "subway_weekday_weekend_did.csv"))

# Commerce base panels.
commerce_panel <- readr::read_csv(commerce_panel_path, show_col_types = FALSE) %>%
  mutate(
    female_sales_share = if ("female_sales_share" %in% names(.)) {
      .data$female_sales_share
    } else {
      safe_divide(.data$female_sales, .data$male_sales + .data$female_sales)
    }
  )

secondary_panel <- commerce_panel %>%
  filter(dong_code %in% c(SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
  add_unit() %>%
  mutate(
    secondary = as.integer(dong_code %in% SECONDARY_CODES),
    did = secondary * post
  )

split_panel <- commerce_panel %>%
  filter(dong_code %in% c(ANCHOR_CODES, SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
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

# Hypothesis C: secondary corridor composition rather than total scale.
composition_outcomes <- c(
  "log_sales", "log_transactions", "log_avg_ticket",
  "weekend_share", "after_work_share", "night_share"
)

secondary_composition_results <- fit_one_term(
  secondary_panel,
  composition_outcomes,
  "did",
  "unit + qstr",
  "unit",
  list(hypothesis = "C_secondary_consumption_composition", treated_group = "secondary")
) %>%
  select(hypothesis, treated_group, everything())

save_csv(secondary_composition_results, file.path(COMP_TABLES, "commerce_secondary_consumption_composition_did.csv"))

# Hypothesis D: age/gender composition in the secondary corridor.
age_gender_outcomes <- c("age20_30_share", "age40_50_share", "age50_60_share", "male_sales_share", "female_sales_share")

secondary_age_gender_results <- fit_one_term(
  secondary_panel,
  age_gender_outcomes,
  "did",
  "unit + qstr",
  "unit",
  list(hypothesis = "D_secondary_age_gender_composition", treated_group = "secondary")
) %>%
  select(hypothesis, treated_group, everything())

save_csv(secondary_age_gender_results, file.path(COMP_TABLES, "commerce_secondary_age_gender_did.csv"))

# Hypothesis E: sector-specific secondary-corridor heterogeneity.
service_panel <- readr::read_csv(service_panel_path, show_col_types = FALSE)
service_agg_cols <- c(
  "sales", "transactions", "weekday_sales", "weekend_sales",
  "sales_00_06", "sales_06_11", "sales_11_14", "sales_14_17", "sales_17_21", "sales_21_24",
  "cnt_00_06", "cnt_06_11", "cnt_11_14", "cnt_14_17", "cnt_17_21", "cnt_21_24"
)

service_outcomes <- function(df) {
  df %>%
    mutate(
      log_sales = log(sales + 1),
      log_transactions = log(transactions + 1),
      after_work_share = safe_divide(sales_17_21 + sales_21_24, sales),
      night_share = safe_divide(sales_21_24 + sales_00_06, sales)
    )
}

make_service_group <- function(df, category) {
  df %>%
    group_by(dong_code, dong_name, quarter_code, year, quarter, qstr, q_index, rel_q, post, transition) %>%
    summarise(across(all_of(service_agg_cols), ~sum(.x, na.rm = TRUE)), .groups = "drop") %>%
    service_outcomes() %>%
    mutate(category = category)
}

food_all_pattern <- "음식점|커피|음료|분식|제과|패스트푸드|호프|치킨|일식|중식|양식|주점"
food_ex_cafe_pattern <- "음식점|분식|패스트푸드|치킨|일식|중식|양식"

service_groups <- bind_rows(
  make_service_group(service_panel %>% filter(service_name == "한식음식점"), "한식음식점"),
  make_service_group(service_panel %>% filter(str_detect(service_name, food_ex_cafe_pattern), !str_detect(service_name, "커피|제과")), "카페 제외 음식점"),
  make_service_group(service_panel %>% filter(str_detect(service_name, food_all_pattern)), "음식·음료 전체"),
  make_service_group(service_panel %>% filter(str_detect(service_name, "커피|제과")), "카페·제과"),
  make_service_group(service_panel %>% filter(str_detect(service_name, "편의점|슈퍼마켓|반찬가게")), "편의점·슈퍼·반찬"),
  make_service_group(service_panel %>% filter(str_detect(service_name, "호프|노래방")), "호프·노래방"),
  make_service_group(service_panel %>% filter(str_detect(service_name, "의원|의료|의약품|피부관리|미용|네일")), "의료")
)

secondary_service_group_results <- service_groups %>%
  group_by(category) %>%
  group_modify(~ {
    dat <- .x %>%
      filter(dong_code %in% c(SECONDARY_CODES, CONTROL_CODES), transition == 0) %>%
      add_unit() %>%
      mutate(
        secondary = as.integer(dong_code %in% SECONDARY_CODES),
        did = secondary * post
      )
    fit_one_term(
      dat,
      c("log_sales", "log_transactions", "after_work_share", "night_share"),
      "did",
      "unit + qstr",
      "unit",
      list(hypothesis = "E_secondary_service_group")
    )
  }) %>%
  ungroup() %>%
  select(hypothesis, category, everything())

save_csv(secondary_service_group_results, file.path(COMP_TABLES, "commerce_secondary_service_group_did.csv"))

# Hypothesis F: split anchor vs secondary composition in one model.
split_outcomes <- c("log_sales", "log_transactions", "log_avg_ticket", "age20_30_share", "male_sales_share", "female_sales_share")

anchor_secondary_split_results <- purrr::map_dfr(split_outcomes, function(y) {
  mod <- fixest::feols(
    as.formula(paste0(y, " ~ anchor_post + secondary_post | unit + qstr")),
    cluster = ~ unit,
    data = split_panel
  )
  bind_rows(
    effect_row(mod, "anchor_post", y, list(hypothesis = "F_anchor_secondary_split", group = "anchor", n_obs = nobs(mod), n_units = n_distinct(split_panel$unit))),
    effect_row(mod, "secondary_post", y, list(hypothesis = "F_anchor_secondary_split", group = "secondary", n_obs = nobs(mod), n_units = n_distinct(split_panel$unit)))
  )
}) %>%
  select(hypothesis, group, everything())

save_csv(anchor_secondary_split_results, file.path(COMP_TABLES, "commerce_anchor_secondary_composition_split_did.csv"))

# Combined key estimates table.
key_estimates <- bind_rows(
  subway_direction_results %>%
    transmute(hypothesis, group = display, outcome, estimate = pct_effect_if_log, effect_scale = "percent", p_value, source_table = "subway_commute_direction_did"),
  subway_weekday_weekend_results %>%
    transmute(hypothesis, group = sample, outcome, estimate = pct_effect_if_log, effect_scale = "percent", p_value, source_table = "subway_weekday_weekend_did"),
  secondary_composition_results %>%
    transmute(hypothesis, group = treated_group, outcome, estimate = coalesce(pct_effect_if_log, effect_pp_if_share), effect_scale = if_else(startsWith(outcome, "log"), "percent", "percentage points"), p_value, source_table = "commerce_secondary_consumption_composition_did"),
  secondary_age_gender_results %>%
    transmute(hypothesis, group = treated_group, outcome, estimate = effect_pp_if_share, effect_scale = "percentage points", p_value, source_table = "commerce_secondary_age_gender_did"),
  secondary_service_group_results %>%
    filter(outcome == "log_sales") %>%
    transmute(hypothesis, group = category, outcome, estimate = pct_effect_if_log, effect_scale = "percent", p_value, source_table = "commerce_secondary_service_group_did"),
  anchor_secondary_split_results %>%
    transmute(hypothesis, group, outcome, estimate = coalesce(pct_effect_if_log, effect_pp_if_share), effect_scale = if_else(startsWith(outcome, "log"), "percent", "percentage points"), p_value, source_table = "commerce_anchor_secondary_composition_split_did")
)

save_csv(key_estimates, file.path(COMP_TABLES, "commute_composition_key_estimates.csv"))

# Figures.
direction_plot <- subway_direction_results %>%
  mutate(
    plot_label = recode(
      display,
      "출근시간 하차 07-10시" = "AM alighting 07-10",
      "퇴근시간 승차 17-20시" = "PM boarding 17-20",
      "야간 승차 21-24시" = "Late-night boarding 21-24",
      "야간 하차 21-24시" = "Late-night alighting 21-24"
    )
  )

ggplot(direction_plot, aes(pct_effect_if_log, reorder(plot_label, pct_effect_if_log))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(
    aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log),
    orientation = "y",
    width = 0.16
  ) +
  labs(title = "Subway DID by direction and time band", x = "Percent effect", y = NULL) +
  theme_minimal()
ggsave(file.path(COMP_FIGURES, "subway_direction_timeband_effects.png"), width = 9, height = 4.8, dpi = 200)

ggplot(secondary_age_gender_results, aes(effect_pp_if_share, reorder(outcome, effect_pp_if_share))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(
    aes(xmin = ci_low_pp_if_share, xmax = ci_high_pp_if_share),
    orientation = "y",
    width = 0.16
  ) +
  labs(title = "Secondary corridor DID: consumer composition", x = "Percentage-point effect", y = NULL) +
  theme_minimal()
ggsave(file.path(COMP_FIGURES, "commerce_secondary_age_gender_effects.png"), width = 9, height = 4.8, dpi = 200)

service_sales_plot <- secondary_service_group_results %>%
  filter(outcome == "log_sales") %>%
  mutate(
    plot_label = recode(
      category,
      "한식음식점" = "Korean restaurants",
      "카페 제외 음식점" = "Restaurants excl. cafes",
      "음식·음료 전체" = "Food & beverage",
      "카페·제과" = "Cafes & bakery",
      "편의점·슈퍼·반찬" = "Convenience/grocery",
      "호프·노래방" = "Pubs & karaoke",
      "의료" = "Medical services"
    )
  )
ggplot(service_sales_plot, aes(pct_effect_if_log, reorder(plot_label, pct_effect_if_log))) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(size = 2) +
  geom_errorbar(
    aes(xmin = ci_low_pct_if_log, xmax = ci_high_pct_if_log),
    orientation = "y",
    width = 0.16
  ) +
  labs(title = "Secondary corridor DID by service group: sales", x = "Percent effect", y = NULL) +
  theme_minimal()
ggsave(file.path(COMP_FIGURES, "commerce_secondary_service_group_sales_effects.png"), width = 9, height = 5.2, dpi = 200)

# Report.
row_one <- function(df, target_outcome, group_col = NULL, group_value = NULL) {
  out <- df %>% filter(.data$outcome == target_outcome)
  if (!is.null(group_col)) out <- out %>% filter(.data[[group_col]] == group_value)
  out %>% slice(1)
}

fmt_pct <- function(x) if_else(is.na(x), "NA", sprintf("%.2f%%", x))
fmt_pp <- function(x) if_else(is.na(x), "NA", sprintf("%.2f p.p.", x))
fmt_p <- function(x) if_else(is.na(x), "NA", sprintf("%.3f", x))

a_morn <- row_one(subway_direction_results, "log_morning_alight_07_10")
a_eve <- row_one(subway_direction_results, "log_evening_board_17_20")
b_weekday <- row_one(subway_weekday_weekend_results, "log_riders", "sample", "weekday")
c_sales <- row_one(secondary_composition_results, "log_sales")
c_tx <- row_one(secondary_composition_results, "log_transactions")
d_young <- row_one(secondary_age_gender_results, "age20_30_share")
d_older <- row_one(secondary_age_gender_results, "age40_50_share")
e_korean <- row_one(secondary_service_group_results, "log_sales", "category", "한식음식점")
f_anchor_young <- row_one(anchor_secondary_split_results, "age20_30_share", "group", "anchor")
f_secondary_young <- row_one(anchor_secondary_split_results, "age20_30_share", "group", "secondary")

report_lines <- c(
  "# Commute and composition analysis",
  "",
  "## Framing",
  "The additional analyses shift the commerce question from a simple total-sales effect to a reallocation question: did the Shinbundang extension rearrange existing subway flows and the internal composition of Gangnam-area commerce?",
  "",
  "## Key results",
  paste0("- Commute-direction subway DID, morning alight 07-10: ", fmt_pct(a_morn$pct_effect_if_log), ", p = ", fmt_p(a_morn$p_value), "."),
  paste0("- Commute-direction subway DID, evening board 17-20: ", fmt_pct(a_eve$pct_effect_if_log), ", p = ", fmt_p(a_eve$p_value), "."),
  paste0("- Weekday subway DID: ", fmt_pct(b_weekday$pct_effect_if_log), ", p = ", fmt_p(b_weekday$p_value), "."),
  paste0("- Secondary corridor total sales DID: ", fmt_pct(c_sales$pct_effect_if_log), ", p = ", fmt_p(c_sales$p_value), "."),
  paste0("- Secondary corridor transactions DID: ", fmt_pct(c_tx$pct_effect_if_log), ", p = ", fmt_p(c_tx$p_value), "."),
  paste0("- Secondary corridor 20-30s sales share DID: ", fmt_pp(d_young$effect_pp_if_share), ", p = ", fmt_p(d_young$p_value), "."),
  paste0("- Secondary corridor 40-50s sales share DID: ", fmt_pp(d_older$effect_pp_if_share), ", p = ", fmt_p(d_older$p_value), "."),
  paste0("- Secondary corridor Korean-restaurant sales DID: ", fmt_pct(e_korean$pct_effect_if_log), ", p = ", fmt_p(e_korean$p_value), "."),
  paste0("- Split model 20-30s sales share, anchor: ", fmt_pp(f_anchor_young$effect_pp_if_share), ", p = ", fmt_p(f_anchor_young$p_value), "."),
  paste0("- Split model 20-30s sales share, secondary: ", fmt_pp(f_secondary_young$effect_pp_if_share), ", p = ", fmt_p(f_secondary_young$p_value), "."),
  "",
  "## Recommended interpretation",
  "The strongest additional result is the subway reallocation pattern: reductions are concentrated in commute-direction flows rather than late-night flows.",
  "Commerce results should remain secondary and exploratory. They do not support a strong total-sales-growth claim, but they do suggest changes in consumer composition and service mix in the secondary corridor.",
  "",
  "## Main caveat",
  "Age, gender, and service-group analyses are heterogeneity checks over multiple outcomes. They should be reported as exploratory evidence, not as the main causal claim."
)

writeLines(report_lines, file.path(COMP_DIR, "COMMUTE_COMPOSITION_REPORT.md"))

message("Commute and composition analysis complete.")
