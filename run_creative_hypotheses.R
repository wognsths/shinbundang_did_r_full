# Run the creative reallocation hypotheses add-on:
#   Rscript run_creative_hypotheses.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/08_analyze_creative_hypotheses.R")

message("All done. See outputs/creative_hypotheses/tables, outputs/creative_hypotheses/figures, and outputs/creative_hypotheses/CREATIVE_HYPOTHESES_REPORT.md")
