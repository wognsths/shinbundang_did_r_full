# Run the additional reallocation extension analyses:
#   Rscript run_reallocation_extensions.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/09_analyze_reallocation_extensions.R")

message("All done. See outputs/reallocation_extensions/tables, outputs/reallocation_extensions/figures, and outputs/reallocation_extensions/REALLOCATION_EXTENSIONS_REPORT.md")
