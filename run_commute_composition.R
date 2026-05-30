# Run commute and consumer-composition analyses:
#   Rscript run_commute_composition.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/08_analyze_commute_composition.R")

message("All done. See outputs/commute_composition/tables, outputs/commute_composition/figures, and outputs/commute_composition/COMMUTE_COMPOSITION_REPORT.md")
