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
#   07_codechange_robustness.R-> output/codechange_*.csv, adds check4_* rows to numbers.csv
#   08_tech_confound.R        -> output/tech_confound.csv, hef_hem_by_major_group.csv, adds check5_*
#                                and majgrp_* rows to numbers.csv
#   09_education_split.R      -> output/education_shares.csv, education_split.csv, adds edu_* rows
#                                (needs BLS EP Table 5.3 xlsx in data/raw/bls/)
#   10_annual_gap_ci.R        -> output/annual_gap_ci.csv, annual_gap_ci.png (bootstrap CIs on 03), adds annual_* rows
#   11_ipums_panel.R          -> data/derived/ipums_unit_age_year.csv, output/ipums_validation.txt (~2 min).
#                                Needs the git-ignored IPUMS extract in data/raw/ipums/; skipped if it is
#                                absent, and 12 then uses the committed ipums_unit_age_year.csv.
#   12_age_gap.R              -> output/age_gap.csv, adds age* rows to numbers.csv
#   13_ipums_build.R          -> data/derived/ipums_unit_year_sex_deg_age.csv (~2 min; needs the raw IPUMS
#                                extract, skipped like 11 if absent, and 14 uses the committed table)
#   14_ipums_education_gap.R  -> output/ipums_education_gap.csv, ipums_education_cells.csv, adds deg_* rows
#   15_pew_reconcile.R        -> output/pew_reconcile.txt, data/derived/pew_exposure_units.csv, adds pew* rows
#                                (needs data/raw/onet/Work Activities.txt, O*NET 30.3)
#   16_figures.R              -> output/figures/fig1-5 *.png (slide/paper figures from existing outputs)
#
# 03-16 read files written by earlier scripts (06-16 must follow 05; 14 and 15 follow 12), so the order matters. Each script runs in its
# own environment, so objects from one script cannot leak into the next.

if (!file.exists("R/01_build_panel.R")) stop("Run from the repository root (the folder that contains R/).")

scripts <- c("R/01_build_panel.R", "R/02_analysis_and_checks.R", "R/03_annual_gap.R",
             "R/04_within_group_gap.R", "R/05_numbers.R", "R/06_triple_difference.R",
             "R/07_codechange_robustness.R", "R/08_tech_confound.R", "R/09_education_split.R",
             "R/10_annual_gap_ci.R", "R/11_ipums_panel.R", "R/12_age_gap.R",
             "R/13_ipums_build.R", "R/14_ipums_education_gap.R", "R/15_pew_reconcile.R",
             "R/16_figures.R")
ipums_tables <- c("R/11_ipums_panel.R" = "data/derived/ipums_unit_age_year.csv",
                  "R/13_ipums_build.R" = "data/derived/ipums_unit_year_sex_deg_age.csv")
ipums_raw <- file.exists("data/raw/ipums/cps_00002.dat.gz") && file.exists("data/raw/ipums/cps_00002.xml")

for (f in scripts) {
  cat("\n==================== ", f, " ====================\n", sep = "")
  if (f %in% names(ipums_tables) && !ipums_raw) {
    if (!file.exists(ipums_tables[[f]])) stop("No IPUMS extract and no ", ipums_tables[[f]], ".")
    cat("-- skipped: IPUMS extract not in data/raw/ipums/; using committed", ipums_tables[[f]], "\n")
    next
  }
  t0 <- Sys.time()
  source(f, local = new.env(), echo = FALSE)
  cat(sprintf("-- %s done in %.1f s\n", f, as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cat("\nAll scripts finished.\n")
