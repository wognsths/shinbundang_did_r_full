# Run the additional reallocation extension analyses:
#   Rscript run_supplementary_flows.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/09_analyze_supplementary_flows.R")

message("All done. See outputs/supplementary_flows/tables, outputs/supplementary_flows/figures, and outputs/supplementary_flows/SUPPLEMENTARY_FLOWS_REPORT.md")
