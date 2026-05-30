# ------------------------------------------------------------
# 11_analyze_control_robustness.R
# DAG-based control selection and DID robustness checks
# ------------------------------------------------------------

source("R/01_utils.R")

message("Analyzing DAG-based control robustness...")

DAG_DIR <- file.path(OUT_DIR, "control_robustness")
DAG_TABLES <- file.path(DAG_DIR, "tables")
DAG_FIGURES <- file.path(DAG_DIR, "figures")
for (d in c(DAG_DIR, DAG_TABLES, DAG_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

effect_row <- function(model, term, outcome, spec, domain, n_units, n_obs, notes = "") {
  r <- fixest_term_row(model, term)
  is_log <- startsWith(outcome, "log_")
  is_share <- str_detect(outcome, "share")

  tibble(
    domain = domain,
    spec = spec,
    outcome = outcome,
    term = term,
    coef = r$estimate,
    se = r$se,
    p_value = r$p_value,
    ci_low = r$ci_low,
    ci_high = r$ci_high,
    effect = case_when(
      is_log ~ pct_from_log(r$estimate),
      is_share ~ 100 * r$estimate,
      TRUE ~ r$estimate
    ),
    ci_low_effect = case_when(
      is_log ~ pct_from_log(r$ci_low),
      is_share ~ 100 * r$ci_low,
      TRUE ~ r$ci_low
    ),
    ci_high_effect = case_when(
      is_log ~ pct_from_log(r$ci_high),
      is_share ~ 100 * r$ci_high,
      TRUE ~ r$ci_high
    ),
    effect_scale = case_when(
      is_log ~ "percent",
      is_share ~ "percentage points",
      TRUE ~ "raw"
    ),
    n_obs = n_obs,
    n_units = n_units,
    notes = notes
  )
}

fit_did_spec <- function(dat, outcome, formula_rhs, fe_rhs, cluster_rhs, spec, domain, unit_col, notes = "") {
  mod <- fixest::feols(
    as.formula(paste0(outcome, " ~ ", formula_rhs, " | ", fe_rhs)),
    cluster = as.formula(paste0("~ ", cluster_rhs)),
    data = dat
  )
  effect_row(
    model = mod,
    term = "did",
    outcome = outcome,
    spec = spec,
    domain = domain,
    n_units = n_distinct(dat[[unit_col]]),
    n_obs = nobs(mod),
    notes = notes
  )
}

fmt_effect <- function(x, scale) {
  if_else(
    is.na(x),
    "NA",
    if_else(scale == "percentage points", sprintf("%.2f p.p.", x), sprintf("%.2f%%", x))
  )
}

fmt_p <- function(x) {
  if_else(is.na(x), "NA", if_else(x < 0.001, "<0.001", sprintf("%.3f", x)))
}

control_decisions <- tibble::tribble(
  ~variable_or_design, ~dag_role, ~use_in_main_did, ~use_in_robustness, ~implementation, ~reason,
  "Unit fixed effects", "Time-invariant station or dong differences", "yes", "yes", "station_line, unit, or admdong fixed effects", "Blocks permanent differences such as anchor strength, land use, and baseline transit centrality.",
  "Month or quarter fixed effects", "Common time shock", "yes", "yes", "month_str or qstr fixed effects", "Absorbs seasonality, macro shocks, and common COVID-period changes.",
  "Unit-specific linear trend", "Pre-existing differential trend", "no", "yes", "unit-by-time linear slope", "Useful as sensitivity check, but can absorb part of the treatment path if used as the only preferred specification.",
  "Baseline outcome x post", "Pre-treatment scale heterogeneity", "no", "yes", "pre-opening baseline log outcome interacted with post", "Checks whether results are driven by large anchor stations or dongs responding differently after 2022.",
  "Differential COVID shock", "Pre-treatment differential shock", "no", "yes", "treated x COVID-period indicator", "Allows treated units to have a different pandemic-period level before opening.",
  "Post-opening bus ridership", "Mediator or affected outcome", "no", "no", "analyzed as a separate outcome", "Controlling for it would block part of route-reallocation effects.",
  "Post-opening living population or visitor composition", "Mediator", "no", "no", "analyzed as a separate outcome", "This is likely on the path from transport change to commerce composition.",
  "Post-opening store count or service mix", "Mediator or collider risk", "no", "no", "not controlled in main DID", "Store mix can respond to the opening and to unobserved demand shocks.",
  "Weather, event, and local construction shocks", "Time-varying confounder candidate", "not available", "not implemented", "would require external event-level data", "Could improve precision if measured, but is not available in the current project files."
)

save_csv(control_decisions, file.path(DAG_TABLES, "dag_control_decisions.csv"))

draw_dag <- function(path) {
  grDevices::png(path, width = 2400, height = 1400, res = 220)
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  node <- function(x, y, label, w, h, fill, border = "#30343B", fontsize = 9.2, text_col = "#202124", lwd = 1.0) {
    grid::grid.roundrect(
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      width = grid::unit(w, "npc"),
      height = grid::unit(h, "npc"),
      r = grid::unit(0.018, "npc"),
      gp = grid::gpar(fill = fill, col = border, lwd = lwd)
    )
    grid::grid.text(
      label,
      x = grid::unit(x, "npc"),
      y = grid::unit(y, "npc"),
      gp = grid::gpar(col = text_col, fontsize = fontsize, lineheight = 0.92)
    )
  }

  grid::grid.rect(gp = grid::gpar(fill = "#FFFFFF", col = NA))
  grid::grid.text(
    "DAG-Based Control Logic",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.94, "npc"),
    gp = grid::gpar(fontsize = 18, fontface = "bold", col = "#171A1F")
  )
  grid::grid.text(
    "Pre-treatment sources are adjusted; post-treatment variables are analyzed as mechanisms, not controls.",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.90, "npc"),
    gp = grid::gpar(fontsize = 9.5, col = "#5F6368")
  )

  col_header <- function(x, label) {
    grid::grid.text(
      label,
      x = grid::unit(x, "npc"),
      y = grid::unit(0.80, "npc"),
      gp = grid::gpar(fontsize = 11, fontface = "bold", col = "#3C4043")
    )
  }

  col_header(0.20, "Pre-Treatment Sources")
  col_header(0.50, "Treatment")
  col_header(0.80, "Post-Treatment Interpretation")

  grid::grid.lines(
    x = grid::unit(c(0.35, 0.35), "npc"),
    y = grid::unit(c(0.22, 0.76), "npc"),
    gp = grid::gpar(col = "#E0E3E7", lwd = 1)
  )
  grid::grid.lines(
    x = grid::unit(c(0.65, 0.65), "npc"),
    y = grid::unit(c(0.22, 0.76), "npc"),
    gp = grid::gpar(col = "#E0E3E7", lwd = 1)
  )

  node(
    0.20, 0.62,
    "Potential backdoor factors\n\n- Baseline place strength\n- Pre-existing trends\n- COVID / macro shocks\n- Seasonality",
    0.25, 0.24,
    "#EAF1FF",
    border = "#5B6F95",
    fontsize = 8.8
  )
  node(
    0.20, 0.36,
    "Adjusted or checked\n\nUnit FE, time FE,\nbaseline x post,\nCOVID shift,\nunit trend robustness",
    0.25, 0.22,
    "#E2F0DA",
    border = "#5E7F56",
    fontsize = 8.6
  )

  node(
    0.50, 0.54,
    "Shinbundang\nGangnam-Sinsa\nextension opening\n\nPost = 2022-06 onward\nTransition = 2022-05 excluded",
    0.23, 0.28,
    "#FFF0BF",
    border = "#9C6F19",
    fontsize = 9
  )

  node(
    0.80, 0.64,
    "Main outcomes\n\nExisting-line ridership\nCommerce sales\nCommerce composition",
    0.25, 0.21,
    "#F8E0CF",
    border = "#986044",
    fontsize = 8.8
  )
  node(
    0.80, 0.38,
    "Mechanisms, not controls\n\nBus ridership\nLiving population\nVisitor mix\nStore mix",
    0.25, 0.22,
    "#F5D0D0",
    border = "#A33A3A",
    fontsize = 8.8
  )

  grid::grid.roundrect(
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.16, "npc"),
    width = grid::unit(0.74, "npc"),
    height = grid::unit(0.08, "npc"),
    r = grid::unit(0.012, "npc"),
    gp = grid::gpar(fill = "#FAFAFA", col = "#DADCE0", lwd = 0.8)
  )
  grid::grid.text(
    "Rule used in the report: do not control for variables that may be caused by the opening.",
    x = grid::unit(0.5, "npc"),
    y = grid::unit(0.16, "npc"),
    gp = grid::gpar(fontsize = 9.4, col = "#3C4043")
  )
}

draw_dag(file.path(DAG_FIGURES, "dag_identification.png"))

# Subway robustness ---------------------------------------------------------
subway_m <- readr::read_csv(file.path(OUT_PROCESSED, "subway_monthly_panel_2018_2024.csv"), show_col_types = FALSE) %>%
  mutate(
    month = as.Date(month),
    month_index = 12 * (year(month) - min(year(month), na.rm = TRUE)) + month(month),
    covid_period = as.integer(month >= as.Date("2020-02-01") & month <= as.Date("2022-04-01")),
    treated_covid = treated * covid_period
  )

subway_baseline <- subway_m %>%
  filter(month < as.Date("2020-02-01")) %>%
  group_by(station_line) %>%
  summarise(baseline_log = mean(log_avg_daily_riders, na.rm = TRUE), .groups = "drop")

subway_dat <- subway_m %>%
  filter(transition == 0) %>%
  left_join(subway_baseline, by = "station_line") %>%
  mutate(baseline_post = baseline_log * post)

subway_specs <- tibble::tribble(
  ~spec, ~rhs, ~notes,
  "base_two_way_fe", "did", "Unit and month fixed effects.",
  "plus_covid_diff", "did + treated_covid", "Adds treated-by-COVID-period level shift.",
  "plus_baseline_x_post", "did + baseline_post", "Adds pre-COVID baseline ridership interacted with post.",
  "plus_unit_linear_trend", "did + station_line:month_index", "Adds station-line-specific linear trends.",
  "combined_controls", "did + treated_covid + baseline_post + station_line:month_index", "Combines COVID shift, baseline-post, and unit trends."
)

subway_control_robustness <- purrr::pmap_dfr(subway_specs, function(spec, rhs, notes) {
  fit_did_spec(
    dat = subway_dat,
    outcome = "log_avg_daily_riders",
    formula_rhs = rhs,
    fe_rhs = "station_line + month_str",
    cluster_rhs = "station_line",
    spec = spec,
    domain = "subway_existing_lines",
    unit_col = "station_line",
    notes = notes
  )
})

save_csv(subway_control_robustness, file.path(DAG_TABLES, "subway_control_robustness.csv"))

# Commerce robustness -------------------------------------------------------
commerce_main <- readr::read_csv(file.path(OUT_PROCESSED, "commerce_main_panel_core_local_2019_2024.csv"), show_col_types = FALSE) %>%
  filter(transition == 0) %>%
  mutate(
    covid_period = as.integer(quarter_code >= 20201 & quarter_code <= 20221),
    treated_covid = treated * covid_period
  )

commerce_baseline <- commerce_main %>%
  filter(year == 2019) %>%
  group_by(unit) %>%
  summarise(baseline_log_sales = mean(log_sales, na.rm = TRUE), .groups = "drop")

commerce_main <- commerce_main %>%
  left_join(commerce_baseline, by = "unit") %>%
  mutate(baseline_post = baseline_log_sales * post)

commerce_specs <- tibble::tribble(
  ~spec, ~rhs, ~notes,
  "base_two_way_fe", "did", "Dong and quarter fixed effects.",
  "plus_covid_diff", "did + treated_covid", "Adds treated-by-COVID-period level shift.",
  "plus_baseline_x_post", "did + baseline_post", "Adds 2019 baseline sales interacted with post.",
  "plus_unit_linear_trend", "did + unit:q_index", "Adds dong-specific linear trends.",
  "combined_controls", "did + treated_covid + baseline_post + unit:q_index", "Combines COVID shift, baseline-post, and unit trends."
)

commerce_control_robustness <- purrr::pmap_dfr(commerce_specs, function(spec, rhs, notes) {
  purrr::map_dfr(c("log_sales", "log_transactions", "night_share", "age20_30_share"), function(outcome) {
    fit_did_spec(
      dat = commerce_main,
      outcome = outcome,
      formula_rhs = rhs,
      fe_rhs = "unit + qstr",
      cluster_rhs = "unit",
      spec = spec,
      domain = "commerce_core_treated",
      unit_col = "unit",
      notes = notes
    )
  })
})

save_csv(commerce_control_robustness, file.path(DAG_TABLES, "commerce_control_robustness.csv"))

# Secondary corridor robustness --------------------------------------------
commerce_panel <- readr::read_csv(file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv"), show_col_types = FALSE)
secondary_codes <- c(11680521L, 11650531L)
secondary_controls <- as.integer(names(CONTROL_MAIN))

secondary_dat <- commerce_panel %>%
  filter(dong_code %in% c(secondary_codes, secondary_controls), transition == 0) %>%
  mutate(
    treated = as.integer(dong_code %in% secondary_codes),
    unit = paste0(dong_code, "_", dong_name),
    did = treated * post,
    covid_period = as.integer(quarter_code >= 20201 & quarter_code <= 20221),
    treated_covid = treated * covid_period
  )

secondary_baseline <- secondary_dat %>%
  filter(year == 2019) %>%
  group_by(unit) %>%
  summarise(baseline_log_sales = mean(log_sales, na.rm = TRUE), .groups = "drop")

secondary_dat <- secondary_dat %>%
  left_join(secondary_baseline, by = "unit") %>%
  mutate(baseline_post = baseline_log_sales * post)

secondary_control_robustness <- purrr::pmap_dfr(commerce_specs, function(spec, rhs, notes) {
  purrr::map_dfr(c("log_sales", "log_transactions", "night_share", "age20_30_share"), function(outcome) {
    fit_did_spec(
      dat = secondary_dat,
      outcome = outcome,
      formula_rhs = rhs,
      fe_rhs = "unit + qstr",
      cluster_rhs = "unit",
      spec = spec,
      domain = "commerce_secondary_corridor",
      unit_col = "unit",
      notes = notes
    )
  })
})

save_csv(secondary_control_robustness, file.path(DAG_TABLES, "commerce_secondary_control_robustness.csv"))

# Gyeonggi activity-population robustness -----------------------------------
gyeonggi_dat <- readr::read_csv(file.path(OUT_PROCESSED, "gyeonggi_bundang_day_dong_panel_2018_2025.csv"), show_col_types = FALSE) %>%
  mutate(
    month = as.Date(month),
    month_index = 12 * (year(month) - min(year(month), na.rm = TRUE)) + month(month)
  )

gyeonggi_baseline <- gyeonggi_dat %>%
  filter(month < as.Date("2020-02-01")) %>%
  group_by(admdong_cd) %>%
  summarise(baseline_log_pop = mean(log_total_avg_pop, na.rm = TRUE), .groups = "drop")

gyeonggi_short <- gyeonggi_dat %>%
  filter(month >= as.Date("2021-01-01"), month <= as.Date("2022-12-01"), transition == 0) %>%
  left_join(gyeonggi_baseline, by = "admdong_cd") %>%
  mutate(baseline_post = baseline_log_pop * post)

gyeonggi_specs <- tibble::tribble(
  ~spec, ~rhs, ~notes,
  "base_two_way_fe", "did", "Dong and month fixed effects in the 2021-2022 window.",
  "plus_baseline_x_post", "did + baseline_post", "Adds pre-COVID baseline activity population interacted with post.",
  "plus_unit_linear_trend", "did + admdong_cd:month_index", "Adds dong-specific linear trends.",
  "combined_controls", "did + baseline_post + admdong_cd:month_index", "Combines baseline-post and dong trends."
)

gyeonggi_control_robustness <- purrr::pmap_dfr(gyeonggi_specs, function(spec, rhs, notes) {
  fit_did_spec(
    dat = gyeonggi_short,
    outcome = "log_total_avg_pop",
    formula_rhs = rhs,
    fe_rhs = "admdong_cd + month_str",
    cluster_rhs = "admdong_cd",
    spec = spec,
    domain = "gyeonggi_original_corridor",
    unit_col = "admdong_cd",
    notes = notes
  )
})

save_csv(gyeonggi_control_robustness, file.path(DAG_TABLES, "gyeonggi_control_robustness.csv"))

all_control_robustness <- bind_rows(
  subway_control_robustness,
  commerce_control_robustness,
  secondary_control_robustness,
  gyeonggi_control_robustness
)
save_csv(all_control_robustness, file.path(DAG_TABLES, "all_control_robustness.csv"))

plot_rows <- all_control_robustness %>%
  filter(
    (domain == "subway_existing_lines" & outcome == "log_avg_daily_riders") |
      (domain == "commerce_core_treated" & outcome == "log_sales") |
      (domain == "commerce_secondary_corridor" & outcome == "log_sales") |
      (domain == "gyeonggi_original_corridor" & outcome == "log_total_avg_pop")
  ) %>%
  mutate(
    domain_label = recode(
      domain,
      subway_existing_lines = "Subway existing lines",
      commerce_core_treated = "Commerce core dongs",
      commerce_secondary_corridor = "Commerce secondary corridor",
      gyeonggi_original_corridor = "Gyeonggi original corridor"
    ),
    spec_label = recode(
      spec,
      base_two_way_fe = "Base",
      plus_covid_diff = "COVID shift",
      plus_baseline_x_post = "Baseline x post",
      plus_unit_linear_trend = "Unit trend",
      combined_controls = "Combined"
    ),
    spec_label = factor(spec_label, levels = c("Base", "COVID shift", "Baseline x post", "Unit trend", "Combined"))
  )

ggplot(plot_rows, aes(spec_label, effect)) +
  geom_hline(yintercept = 0, linewidth = 0.25) +
  geom_errorbar(aes(ymin = ci_low_effect, ymax = ci_high_effect), width = 0.18) +
  geom_point(size = 1.8) +
  coord_flip() +
  facet_wrap(~ domain_label, scales = "free_x") +
  labs(
    title = "DID robustness with DAG-motivated controls",
    x = NULL,
    y = "Percent effect"
  ) +
  theme_minimal() +
  theme(panel.grid.minor = element_blank())
ggsave(file.path(DAG_FIGURES, "did_control_robustness.png"), width = 10.5, height = 6.2, dpi = 200)

summary_rows <- plot_rows %>%
  select(domain_label, spec_label, effect, ci_low_effect, ci_high_effect, p_value, effect_scale) %>%
  arrange(domain_label, spec_label)

save_csv(summary_rows, file.path(DAG_TABLES, "control_robustness_summary_for_report.csv"))

subway_main <- summary_rows %>% filter(domain_label == "Subway existing lines", spec_label == "Base")
subway_combined <- summary_rows %>% filter(domain_label == "Subway existing lines", spec_label == "Combined")
commerce_main_base <- summary_rows %>% filter(domain_label == "Commerce core dongs", spec_label == "Base")
commerce_secondary_base <- summary_rows %>% filter(domain_label == "Commerce secondary corridor", spec_label == "Base")
gyeonggi_base <- summary_rows %>% filter(domain_label == "Gyeonggi original corridor", spec_label == "Base")

report_lines <- c(
  "# DAG Controls and Robustness Report",
  "",
  "This add-on separates valid pre-treatment controls from post-treatment mediators using a DAG logic.",
  "The key rule is that post-opening bus ridership, living population, visitor composition, and store mix are not included as controls in the main DID because they can lie on the causal path.",
  "",
  "## Main Robustness Checks",
  "",
  sprintf(
    "- Subway existing-line effect: base %s (p=%s); combined-control spec %s (p=%s).",
    fmt_effect(subway_main$effect, subway_main$effect_scale), fmt_p(subway_main$p_value),
    fmt_effect(subway_combined$effect, subway_combined$effect_scale), fmt_p(subway_combined$p_value)
  ),
  sprintf(
    "- Commerce core log-sales effect remains non-positive in the base spec: %s (p=%s).",
    fmt_effect(commerce_main_base$effect, commerce_main_base$effect_scale), fmt_p(commerce_main_base$p_value)
  ),
  sprintf(
    "- Secondary corridor log-sales effect remains non-positive in the base spec: %s (p=%s).",
    fmt_effect(commerce_secondary_base$effect, commerce_secondary_base$effect_scale), fmt_p(commerce_secondary_base$p_value)
  ),
  sprintf(
    "- Gyeonggi original-corridor activity population, 2021-2022 window: %s (p=%s).",
    fmt_effect(gyeonggi_base$effect, gyeonggi_base$effect_scale), fmt_p(gyeonggi_base$p_value)
  ),
  "",
  "## Outputs",
  "",
  "- `outputs/control_robustness/tables/dag_control_decisions.csv`",
  "- `outputs/control_robustness/tables/all_control_robustness.csv`",
  "- `outputs/control_robustness/tables/control_robustness_summary_for_report.csv`",
  "- `outputs/control_robustness/figures/dag_identification.png`",
  "- `outputs/control_robustness/figures/did_control_robustness.png`"
)

writeLines(report_lines, file.path(DAG_DIR, "CONTROL_ROBUSTNESS_REPORT.md"), useBytes = TRUE)

message("DAG control robustness complete.")
