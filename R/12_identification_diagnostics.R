# ------------------------------------------------------------
# 12_identification_diagnostics.R
# Pretrend diagnostics, permutation inference, unit heterogeneity
# ------------------------------------------------------------

source("R/01_utils.R")

message("Running identification diagnostics...")

ID_DIR <- file.path(OUT_DIR, "identification")
ID_TABLES <- file.path(ID_DIR, "tables")
ID_FIGURES <- file.path(ID_DIR, "figures")
for (d in c(ID_DIR, ID_TABLES, ID_FIGURES)) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ============================================================
# A. Pretrend diagnostic summary
# ============================================================

message("A. Compiling pretrend diagnostics...")

subway_pretrend_path <- file.path(OUT_TABLES, "subway_pretrend_tests.csv")
commerce_pretrend_path <- file.path(OUT_TABLES, "commerce_pretrend_tests_2019_2024.csv")
subway_es_path <- file.path(OUT_TABLES, "subway_event_study_coefficients.csv")
commerce_es_path <- file.path(OUT_TABLES, "commerce_event_study_coefficients_2019_2024.csv")

pretrend_rows <- list()

if (file.exists(subway_pretrend_path)) {
  sp <- readr::read_csv(subway_pretrend_path, show_col_types = FALSE)
  pretrend_rows[["subway"]] <- tibble(
    domain = "subway",
    outcome = "log_avg_daily_riders",
    wald_joint_p = sp$event_pretrend_wald_p,
    linear_pretrend_p = sp$linear_p_value
  )
}

if (file.exists(commerce_pretrend_path)) {
  cp <- readr::read_csv(commerce_pretrend_path, show_col_types = FALSE)
  pretrend_rows[["commerce"]] <- cp %>%
    transmute(
      domain = "commerce",
      outcome = outcome,
      wald_joint_p = event_pretrend_wald_p,
      linear_pretrend_p = NA_real_
    )
}

if (file.exists(subway_es_path)) {
  ses <- readr::read_csv(subway_es_path, show_col_types = FALSE)
  pre_coefs <- ses %>% filter(rel_month < 0)
  pretrend_rows[["subway"]] <- pretrend_rows[["subway"]] %>%
    mutate(
      pre_coef_mean = mean(pre_coefs$coef_log, na.rm = TRUE),
      pre_coef_min = min(pre_coefs$coef_log, na.rm = TRUE),
      pre_coef_max = max(pre_coefs$coef_log, na.rm = TRUE),
      pre_coef_abs_mean = mean(abs(pre_coefs$coef_log), na.rm = TRUE),
      n_pre_periods = nrow(pre_coefs)
    )
}

if (file.exists(commerce_es_path)) {
  ces <- readr::read_csv(commerce_es_path, show_col_types = FALSE)
  commerce_pre_summary <- ces %>%
    filter(rel_q < 0) %>%
    group_by(outcome) %>%
    summarise(
      pre_coef_mean = mean(coef, na.rm = TRUE),
      pre_coef_min = min(coef, na.rm = TRUE),
      pre_coef_max = max(coef, na.rm = TRUE),
      pre_coef_abs_mean = mean(abs(coef), na.rm = TRUE),
      n_pre_periods = n(),
      .groups = "drop"
    )
  if (!is.null(pretrend_rows[["commerce"]])) {
    pretrend_rows[["commerce"]] <- pretrend_rows[["commerce"]] %>%
      left_join(commerce_pre_summary, by = "outcome")
  }
}

pretrend_summary <- bind_rows(pretrend_rows)
save_csv(pretrend_summary, file.path(ID_TABLES, "pretrend_diagnostic_summary.csv"))

# ============================================================
# B. Subway permutation inference
# ============================================================

message("B. Running subway permutation inference...")

subway_m <- readr::read_csv(
  file.path(OUT_PROCESSED, "subway_monthly_panel_2018_2024.csv"),
  show_col_types = FALSE
) %>% mutate(month = as.Date(month))

main <- subway_m %>% filter(transition == 0)

actual_mod <- fixest::feols(
  log_avg_daily_riders ~ did | station_line + month_str,
  cluster = ~ station_line,
  data = main
)
actual_beta <- fixest::coeftable(actual_mod)["did", "Estimate"]

set.seed(20260601)
n_perm <- 999
all_stations <- unique(main$station_line)
n_treated <- length(TREATED_STATIONS)
perm_betas <- numeric(n_perm)

for (i in seq_len(n_perm)) {
  fake_treated <- sample(all_stations, n_treated)
  perm_dat <- main %>%
    mutate(
      treated_perm = as.integer(station_line %in% fake_treated),
      did_perm = treated_perm * post
    )
  perm_mod <- fixest::feols(
    log_avg_daily_riders ~ did_perm | station_line + month_str,
    cluster = ~ station_line,
    data = perm_dat
  )
  perm_betas[i] <- fixest::coeftable(perm_mod)["did_perm", "Estimate"]
}

perm_p_twosided <- mean(abs(perm_betas) >= abs(actual_beta))
perm_p_onesided <- mean(perm_betas <= actual_beta)

save_csv(
  tibble(
    actual_beta = actual_beta,
    actual_pct = pct_from_log(actual_beta),
    n_permutations = n_perm,
    perm_p_twosided = perm_p_twosided,
    perm_p_onesided = perm_p_onesided,
    perm_mean = mean(perm_betas),
    perm_sd = sd(perm_betas),
    conventional_p = fixest::coeftable(actual_mod)["did", "Pr(>|t|)"]
  ),
  file.path(ID_TABLES, "subway_permutation_pvalue.csv")
)

ggplot(tibble(beta = perm_betas), aes(beta)) +
  geom_histogram(bins = 50, fill = "grey70", color = "grey40") +
  geom_vline(xintercept = actual_beta, color = "red", linewidth = 1) +
  labs(
    title = "Permutation distribution of subway DID coefficient",
    subtitle = sprintf(
      "Actual = %.4f (red line). Two-sided p = %.3f, one-sided p = %.3f",
      actual_beta, perm_p_twosided, perm_p_onesided
    ),
    x = "Permuted DID coefficient (log-points)",
    y = "Count"
  ) +
  theme_minimal()
ggsave(
  file.path(ID_FIGURES, "subway_permutation_histogram.png"),
  width = 9, height = 5, dpi = 200
)

# ============================================================
# C. Control-set sensitivity summary
# ============================================================

message("C. Compiling control-set sensitivity...")

robust_path <- file.path(OUT_TABLES, "subway_did_robustness.csv")
if (file.exists(robust_path)) {
  robust <- readr::read_csv(robust_path, show_col_types = FALSE)
  control_set_summary <- robust %>%
    filter(window == "2018_2024") %>%
    select(control_set, pct_effect, p_value, n_units)
  save_csv(control_set_summary, file.path(ID_TABLES, "subway_control_set_sensitivity.csv"))
}

# ============================================================
# D. Individual dong heterogeneity
# ============================================================

message("D. Compiling individual dong heterogeneity...")

single_dong_path <- file.path(OUT_DIR, "anchor_secondary", "tables", "commerce_single_dong_did.csv")
if (file.exists(single_dong_path)) {
  single_dong <- readr::read_csv(single_dong_path, show_col_types = FALSE) %>%
    filter(outcome %in% c("log_sales", "log_transactions")) %>%
    select(
      treated_group, treated_dong_name, outcome,
      pct_effect_if_log, p_value, n_obs
    ) %>%
    arrange(treated_group, treated_dong_name, outcome)
  save_csv(single_dong, file.path(ID_TABLES, "commerce_dong_heterogeneity.csv"))
}

message("Identification diagnostics complete.")
