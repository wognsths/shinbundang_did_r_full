# ------------------------------------------------------------
# 06_make_summary.R
# Build combined tables and short markdown report
# ------------------------------------------------------------

source("R/01_utils.R")

message("Building combined summary...")
rows <- list()

subway_main_path <- file.path(OUT_TABLES, "subway_did_main.csv")
if (file.exists(subway_main_path)) {
  s <- readr::read_csv(subway_main_path, show_col_types = FALSE)
  rows[[length(rows) + 1]] <- tibble(domain = "subway", outcome = "log average daily riders", estimate = s$pct_effect[1], effect_scale = "percent", p_value = s$p_value[1])
}

timeband_path <- file.path(OUT_TABLES, "subway_timeband_did.csv")
if (file.exists(timeband_path)) {
  tb <- readr::read_csv(timeband_path, show_col_types = FALSE)
  rows[[length(rows) + 1]] <- tb %>% transmute(domain = "subway_timeband", outcome, estimate = pct_effect, effect_scale = "percent", p_value)
}

commerce_path <- file.path(OUT_TABLES, "commerce_did_main_2019_2024.csv")
if (file.exists(commerce_path)) {
  c <- readr::read_csv(commerce_path, show_col_types = FALSE)
  rows[[length(rows) + 1]] <- c %>%
    mutate(estimate = if_else(startsWith(outcome, "log"), pct_effect_if_log, effect_points_if_share)) %>%
    transmute(domain = "commerce", outcome, estimate, effect_scale = "percent if log; share-point if share", p_value)
}

combined <- bind_rows(rows)
save_csv(combined, file.path(OUT_TABLES, "combined_key_estimates_2019_2024.csv"))

# Lightweight markdown report
report_path <- file.path(OUT_DIR, "REPORT_SUMMARY.md")
subway <- if (file.exists(subway_main_path)) readr::read_csv(subway_main_path, show_col_types = FALSE) else NULL
commerce <- if (file.exists(commerce_path)) readr::read_csv(commerce_path, show_col_types = FALSE) else NULL
subway_pre <- if (file.exists(file.path(OUT_TABLES, "subway_pretrend_tests.csv"))) readr::read_csv(file.path(OUT_TABLES, "subway_pretrend_tests.csv"), show_col_types = FALSE) else NULL
commerce_pre <- if (file.exists(file.path(OUT_TABLES, "commerce_pretrend_tests_2019_2024.csv"))) readr::read_csv(file.path(OUT_TABLES, "commerce_pretrend_tests_2019_2024.csv"), show_col_types = FALSE) else NULL

lines <- c(
  "# Shinbundang extension DID analysis summary",
  "",
  "## Identification setup",
  "- Treatment shock: opening of the Shinbundang Line Gangnam–Sinsa extension on 2022-05-28.",
  "- Subway post period: 2022-06 onward; 2022-05 excluded as transition month.",
  "- Commerce post period: 2022Q3 onward; 2022Q2 excluded as transition quarter.",
  "- Main subway units: existing station-line units around Gangnam, Sinnonhyeon, Nonhyeon, and Sinsa.",
  "- Main commerce units: core treated administrative dongs around the extension stations vs local Gangnam/Seocho controls.",
  "",
  "## Main results"
)
if (!is.null(subway)) {
  lines <- c(lines, sprintf("- Subway DID: %.2f%%, p = %.3f.", subway$pct_effect[1], subway$p_value[1]))
}
if (!is.null(commerce)) {
  sales <- commerce %>% filter(outcome == "log_sales")
  night <- commerce %>% filter(outcome == "night_share")
  lines <- c(lines, sprintf("- Commerce total sales DID: %.2f%%, p = %.3f.", sales$pct_effect_if_log[1], sales$p_value[1]))
  lines <- c(lines, sprintf("- Commerce night-sales share DID: %.3f share-point, p = %.3f.", night$effect_points_if_share[1], night$p_value[1]))
}
lines <- c(lines, "", "## Main caveat")
if (!is.null(subway_pre)) {
  lines <- c(lines, sprintf("- Subway linear pretrend p-value: %.3f; event-study joint pretrend can be stricter and should be reported separately.", subway_pre$linear_p_value[1]))
}
if (!is.null(commerce_pre)) {
  lines <- c(lines, "- Commerce event-study pretrend tests are generally unfavorable, so commerce results should be interpreted as exploratory rather than strong causal evidence.")
}
writeLines(lines, report_path)
message("Summary written to: ", report_path)
