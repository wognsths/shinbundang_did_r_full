# Run the anchor vs secondary-corridor commerce add-on:
#   Rscript run_less_central.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/07_analyze_less_central.R")

message("All done. See outputs/less_central/tables, outputs/less_central/figures, and outputs/less_central/LESS_CENTRAL_REPORT.md")
