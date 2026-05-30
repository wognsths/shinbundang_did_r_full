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
source("R/07_analyze_less_central.R")
source("R/08_analyze_creative_hypotheses.R")
source("R/09_analyze_reallocation_extensions.R")
source("R/10_analyze_gyeonggi_api.R")
source("R/11_analyze_dag_controls.R")

message("All done. See outputs/tables, outputs/figures, outputs/less_central, outputs/creative_hypotheses, outputs/reallocation_extensions, outputs/gyeonggi_api, outputs/dag_controls, and outputs/REPORT_SUMMARY.md")
