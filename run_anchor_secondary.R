# Run the anchor vs secondary-corridor commerce add-on:
#   Rscript run_anchor_secondary.R

for (loc in c("en_US.UTF-8", "ko_KR.UTF-8", "C.UTF-8")) {
  updated <- suppressWarnings(Sys.setlocale("LC_CTYPE", loc))
  if (!is.na(updated) && grepl("UTF-8|UTF8", updated, ignore.case = TRUE)) break
}

source("R/07_analyze_anchor_secondary.R")

message("All done. See outputs/anchor_secondary/tables, outputs/anchor_secondary/figures, and outputs/anchor_secondary/ANCHOR_SECONDARY_REPORT.md")
