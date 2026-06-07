# Run the full analysis pipeline from the project root:
#   Rscript run_all.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/02_prepare_subway.R")
source("R/03_analyze_subway.R")
source("R/04_prepare_commerce.R")
source("R/05_analyze_commerce.R")
source("R/06_make_summary.R")
source("R/07_analyze_anchor_secondary.R")
source("R/08_analyze_commute_composition.R")
source("R/09_analyze_supplementary_flows.R")
source("R/10_analyze_gyeonggi_activity.R")
source("R/11_analyze_control_robustness.R")
source("R/12_identification_diagnostics.R")
source("R/13_balance_descriptive.R")

message("All done. See outputs/tables, outputs/figures, outputs/anchor_secondary, outputs/commute_composition, outputs/supplementary_flows, outputs/gyeonggi_activity, outputs/control_robustness, outputs/identification, outputs/balance, and outputs/REPORT_SUMMARY.md")
