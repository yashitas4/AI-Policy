# 00_run_all.R
# Runs the whole pipeline in order, from the repository root:
#   Rscript R/00_run_all.R        or, in R:  source("R/00_run_all.R")
#
#   01_build_panel.R          -> data/derived/panel_units.csv, unit_members.csv
#   02_analysis_and_checks.R  -> output/group_trends_*.csv, gap_estimates.csv, checks_summary.txt
#   03_annual_gap.R           -> output/annual_gap.csv, annual_gap*.png
#   04_within_group_gap.R     -> output/hef_hem_occupations.csv, within_group_gap.txt
#   05_numbers.R              -> output/numbers.csv (every number cited in the paper)
#   06_triple_difference.R    -> output/triple_difference.csv, adds ddd_* rows to numbers.csv
#
# 03-06 read files written by earlier scripts (06 must follow 05), so the order matters. Each script runs in its
# own environment, so objects from one script cannot leak into the next.

if (!file.exists("R/01_build_panel.R")) stop("Run from the repository root (the folder that contains R/).")

scripts <- c("R/01_build_panel.R", "R/02_analysis_and_checks.R", "R/03_annual_gap.R",
             "R/04_within_group_gap.R", "R/05_numbers.R", "R/06_triple_difference.R")

for (f in scripts) {
  cat("\n==================== ", f, " ====================\n", sep = "")
  t0 <- Sys.time()
  source(f, local = new.env(), echo = FALSE)
  cat(sprintf("-- %s done in %.1f s\n", f, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("\nAll scripts finished.\n")
