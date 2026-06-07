# ------------------------------------------------------------
# 13_balance_descriptive.R
# Pre-treatment descriptive statistics & covariates balance check
# ------------------------------------------------------------

source("R/01_utils.R")

BAL_DIR <- file.path(OUT_DIR, "balance")
dir.create(BAL_DIR, recursive = TRUE, showWarnings = FALSE)

compute_balance <- function(data, group_var, unit_var, covariates) {
  rows <- lapply(covariates, function(v) {
    treated <- data[[v]][data[[group_var]] == 1]
    control <- data[[v]][data[[group_var]] == 0]
    treated <- treated[!is.na(treated)]
    control <- control[!is.na(control)]

    n_t <- length(unique(data[[unit_var]][data[[group_var]] == 1]))
    n_c <- length(unique(data[[unit_var]][data[[group_var]] == 0]))
    m_t <- mean(treated)
    m_c <- mean(control)
    s_t <- sd(treated)
    s_c <- sd(control)
    pooled_sd <- sqrt((s_t^2 + s_c^2) / 2)
    std_diff <- if (pooled_sd > 0) (m_t - m_c) / pooled_sd else NA_real_

    tibble(
      variable    = v,
      N_treated   = n_t,
      N_obs_treated = length(treated),
      mean_treated  = round(m_t, 4),
      sd_treated    = round(s_t, 4),
      N_control   = n_c,
      N_obs_control = length(control),
      mean_control  = round(m_c, 4),
      sd_control    = round(s_c, 4),
      std_diff      = round(std_diff, 4),
      balance_ok    = abs(std_diff) < 0.25
    )
  })
  bind_rows(rows)
}

# ---- Subway balance ----

subway_path <- file.path(OUT_PROCESSED, "subway_monthly_panel_2018_2024.csv")
if (file.exists(subway_path)) {
  sub <- read_csv(subway_path, show_col_types = FALSE) %>%
    filter(month < as.Date("2022-05-01"), transition == 0)

  sub <- sub %>%
    mutate(boarding_share = avg_daily_board / avg_daily_riders)

  subway_vars <- c(
    "avg_daily_riders", "avg_daily_board", "avg_daily_alight",
    "log_avg_daily_riders", "boarding_share"
  )

  subway_bal <- compute_balance(sub, "treated", "station_line", subway_vars)
  save_csv(subway_bal, file.path(BAL_DIR, "subway_pretreatment_balance.csv"))
  message("Subway balance table saved.")
} else {
  message("WARNING: subway panel not found at ", subway_path)
}

# ---- Commerce balance ----

commerce_path <- file.path(OUT_PROCESSED, "commerce_main_panel_core_local_2019_2024.csv")
if (!file.exists(commerce_path)) {
  commerce_path <- file.path(OUT_PROCESSED, "commerce_dong_quarter_panel_2019_2024.csv")
}

if (file.exists(commerce_path)) {
  com <- read_csv(commerce_path, show_col_types = FALSE) %>%
    filter(quarter_code < 20222, transition == 0)

  unit_var <- if ("unit" %in% names(com)) "unit" else "dong_code"

  commerce_vars <- c(
    "sales", "transactions",
    "log_sales", "log_transactions",
    "weekend_share", "night_share", "after_work_share",
    "age20_30_share", "age50_60_share",
    "male_sales_share"
  )
  commerce_vars <- commerce_vars[commerce_vars %in% names(com)]

  commerce_bal <- compute_balance(com, "treated", unit_var, commerce_vars)
  save_csv(commerce_bal, file.path(BAL_DIR, "commerce_pretreatment_balance.csv"))
  message("Commerce balance table saved.")
} else {
  message("WARNING: commerce panel not found.")
}

message("Balance and descriptive statistics complete. See outputs/balance/")
