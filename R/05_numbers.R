# 05_numbers.R
# Collects every number the paper cites into one file, output/numbers.csv, so each figure in the
# text can be traced to the script that produced it. Run after 01-04 (R/00_run_all.R does this).
# Does not change any earlier output.
#
# Columns:
#   name        short identifier to cite in the text
#   value       full-precision value
#   display     value as it would be printed in the paper
#   unit        k (thousands of jobs), pct_per_yr, pp_per_yr, pct, count, beta, p, flag (1 = PASS)
#   script      script whose definitions produce the number
#   source      file the value was read from, or "recomputed" (from data/derived/panel_units.csv,
#               using the same definitions as `script`, and checked against that script's output)
#   description what the number is
#
# Definitions are those of 02_analysis_and_checks.R: gender groups by 2022 % women (>70 F, <30 M),
# high exposure = top 25% GPT-4 beta, CAGR growth 2015-19 (pre) and 2022-25 (post), gap = F - M
# change in growth among high-exposure units, weighted by 2022 employment, HC1 SEs.

rows <- list()
add <- function(name, value, unit, script, source, description, digits = 2, sign = FALSE) {
  value <- unname(as.numeric(value))
  display <- if (unit == "flag") ifelse(value == 1, "PASS", "FAIL")
             else if (unit %in% c("count", "k")) format(round(value), big.mark = ",")
             else formatC(value, format = "f", digits = digits, flag = if (sign) "+" else "")
  rows[[length(rows) + 1]] <<- data.frame(name = name, value = signif(value, 8), display = display,
                                          unit = unit, script = script, source = source,
                                          description = description)
}
S1 <- "R/01_build_panel.R"; S2 <- "R/02_analysis_and_checks.R"
S3 <- "R/03_annual_gap.R";  S4 <- "R/04_within_group_gap.R"
RC <- "recomputed"

# ---- Inputs ---------------------------------------------------------------------
P  <- read.csv("data/derived/panel_units.csv")
gt <- read.csv("output/group_trends_baseline.csv")
gt3 <- read.csv("output/group_trends_post2024.csv")
ge <- read.csv("output/gap_estimates.csv", row.names = 1)
ag <- read.csv("output/annual_gap.csv")
occ <- read.csv("output/hef_hem_occupations.csv")
wg <- readLines("output/within_group_gap.txt")

U <- P[P$in_sample, ]
U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))
cut_gpt4  <- quantile(U$beta_gpt4,  0.75, na.rm = TRUE)
cut_human <- quantile(U$beta_human, 0.75, na.rm = TRUE)
U$high <- U$beta_gpt4 >= cut_gpt4
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
grp <- function(g, h = TRUE) U[U$high == h & U$gender == g, ]
gt_row <- function(d, g, h = "high") d[d$exposure == h & d$gender == g, ]

# ---- 1. Sample and definitions (01, 02) ------------------------------------------------
add("n_units_complete", nrow(P), "count", S1, "data/derived/panel_units.csv",
    "Harmonized units complete on both sides of the 2010->2018 code change")
add("n_units_sample", nrow(U), "count", S1, "data/derived/panel_units.csv",
    "Units in the analysis sample (% women published, exposure score, positive employment)")
add("cutoff_beta_gpt4", cut_gpt4, "beta", S2, RC, "High-exposure cutoff: 75th percentile of GPT-4 beta", 3)
add("cutoff_beta_human", cut_human, "beta", S2, RC, "Check 1 cutoff: 75th percentile of human-rated beta", 3)
add("n_high_exposure", sum(U$high), "count", S2, RC, "High-exposure units (all gender groups)")

# ---- 2. Group counts, jobs and growth (02) -------------------------------------------
lab <- c(F = "HEF", M = "HEM", N = "HEN")
for (h in c(TRUE, FALSE)) for (g in c("F", "M", "N")) {
  d <- grp(g, h); r <- gt_row(gt, g, ifelse(h, "high", "lower"))
  pre <- cagr(sum(d$e2019), sum(d$e2015), 4); post <- cagr(sum(d$e2025), sum(d$e2022), 3)
  # recomputed values must agree with 02's (rounded) output
  stopifnot(nrow(d) == r$n_occ, sum(d$e2015) == r$emp2015, sum(d$e2019) == r$emp2019,
            sum(d$e2022) == r$emp2022, sum(d$e2025) == r$emp_post_end,
            round(pre, 2) == r$growth_pre, round(post, 2) == r$growth_post)
  p <- if (h) lab[[g]] else paste0("low", g)
  who <- sprintf("%s-exposure %s units", ifelse(h, "High", "Lower"),
                 c(F = "female-dominated", M = "male-dominated", N = "non-dominated")[[g]])
  add(paste0(p, "_n"), nrow(d), "count", S2, "output/group_trends_baseline.csv", paste(who, "(count)"))
  for (y in c(2015, 2019, 2022, 2025))
    add(sprintf("%s_jobs_%d", p, y), sum(d[[paste0("e", y)]]), "k", S2, "output/group_trends_baseline.csv",
        sprintf("%s: total employment %d (thousands)", who, y))
  add(paste0(p, "_growth_pre"), pre, "pct_per_yr", S2, RC,
      paste(who, ": employment CAGR 2015-19"), sign = TRUE)
  add(paste0(p, "_growth_post"), post, "pct_per_yr", S2, RC,
      paste(who, ": employment CAGR 2022-25"), sign = TRUE)
}

# ---- 3. Pre-trend gap (02, 03) --------------------------------------------------------
pre_F <- cagr(sum(grp("F")$e2019), sum(grp("F")$e2015), 4)
pre_M <- cagr(sum(grp("M")$e2019), sum(grp("M")$e2015), 4)
add("pretrend_gap_cagr", pre_F - pre_M, "pp_per_yr", S2, RC,
    "Pre-period growth gap, HEF minus HEM, group CAGR 2015-19", sign = TRUE)
tr <- ag[1, c("trend_HEF", "trend_HEM")]
add("trend_HEF_2015_19", tr$trend_HEF, "pct_per_yr", S3, "output/annual_gap.csv",
    "HEF 2015-19 log-linear trend growth", sign = TRUE)
add("trend_HEM_2015_19", tr$trend_HEM, "pct_per_yr", S3, "output/annual_gap.csv",
    "HEM 2015-19 log-linear trend growth", sign = TRUE)
add("pretrend_gap_loglinear", tr$trend_HEF - tr$trend_HEM, "pp_per_yr", S3, "output/annual_gap.csv",
    "Pre-period growth gap, HEF minus HEM, from 2015-19 log-linear trends", sign = TRUE)

# ---- 4. Main gap and checks 1 and 3 (02) --------------------------------------------
gap_rows <- function(prefix, r, what) {
  src <- "output/gap_estimates.csv"
  add(paste0(prefix, "_gap"), ge[r, "gap"], "pp_per_yr", S2, src, paste(what, ": F - M gap"), sign = TRUE)
  add(paste0(prefix, "_se"),  ge[r, "se"],  "pp_per_yr", S2, src, paste(what, ": HC1 standard error"))
  add(paste0(prefix, "_p"),   ge[r, "p"],   "p",         S2, src, paste(what, ": p-value"), 3)
  add(paste0(prefix, "_ci_lo"), ge[r, "lo"], "pp_per_yr", S2, src, paste(what, ": 95% CI lower bound"), sign = TRUE)
  add(paste0(prefix, "_ci_hi"), ge[r, "hi"], "pp_per_yr", S2, src, paste(what, ": 95% CI upper bound"), sign = TRUE)
  add(paste0(prefix, "_nF"), ge[r, "nF"], "count", S2, src, paste(what, ": female-dominated units in regression"))
  add(paste0(prefix, "_nM"), ge[r, "nM"], "count", S2, src, paste(what, ": male-dominated units in regression"))
}
gap_rows("main", "baseline", "Baseline (GPT-4 beta, post 2022-25)")

gap_rows("check1", "check1_human", "Check 1 (human-rated exposure)")
add("check1_pass", as.numeric(ge["check1_human", "gap"] > 0 && ge["check1_human", "hi"] > 0), "flag", S2, RC,
    "Check 1 pass standard: gap > 0 and 95% CI not entirely below zero")

# ---- 5. Check 2: 2019 baseline (02) ----------------------------------------------------
hf <- grp("F")
c2 <- 100 * (sum(hf$e2025) / sum(hf$e2019) - 1)
for (y in c(2019, 2022, 2025))
  add(sprintf("check2_HEF_jobs_%d", y), sum(hf[[paste0("e", y)]]), "k", S2, RC,
      sprintf("Check 2: HEF total employment %d (thousands)", y))
add("check2_pct_change_2019_25", c2, "pct", S2, RC, "Check 2: HEF employment change 2019 -> 2025", 1, TRUE)
add("check2_pass", as.numeric(sum(hf$e2025) >= sum(hf$e2019)), "flag", S2, RC,
    "Check 2 pass standard: HEF 2025 employment at or above 2019")
sec <- grepl("^Executive secretaries", U$title)   # same selector as 02
stopifnot(sum(sec) == 1, any(sec & U$high & U$gender == "F"))
hx <- hf[!grepl("^Executive secretaries", hf$title), ]
add("check2_secretaries_change_2019_25", U$e2025[sec] - U$e2019[sec], "k", S2, RC,
    "Check 2: change in secretaries unit employment 2019 -> 2025 (largest contributor)")
add("check2_HEF_ex_secretaries_pct_change", 100 * (sum(hx$e2025) / sum(hx$e2019) - 1), "pct", S2, RC,
    "Check 2 diagnostic: HEF change 2019 -> 2025 without the secretaries unit", 1, TRUE)

# ---- 6. Check 3: post-period ends 2024 (02) ------------------------------------------
gap_rows("check3", "check3_post2024", "Check 3 (post period 2022-24)")
add("check3_pass", as.numeric(ge["check3_post2024", "gap"] > 0), "flag", S2, RC,
    "Check 3 pass standard: gap stays positive without 2025")
for (g in c("F", "M")) add(paste0(lab[[g]], "_growth_post_2022_24"), gt_row(gt3, g)$growth_post, "pct_per_yr", S2,
                           "output/group_trends_post2024.csv",
                           paste(lab[[g]], "employment CAGR 2022-24 (rounded to 2 dp in source)"), sign = TRUE)

# ---- 7. Secretaries series (01 panel, used in 02) ------------------------------------
for (y in c(2015, 2019, 2022, 2024, 2025))
  add(sprintf("secretaries_jobs_%d", y), U[[paste0("e", y)]][sec], "k", S2, RC,
      sprintf("Secretaries and administrative assistants unit (%d combined 2018 codes), employment %d (thousands)",
              U$n_codes_2018[sec], y))

# ---- 8. Annual gap series (03) ----------------------------------------------------------
for (i in seq_len(nrow(ag))) {
  y <- ag$year[i]; note <- if (ag$crosses_code_change[i]) " [spans code change; not plotted]" else ""
  add(sprintf("annual_gap_%d", y), ag$gap[i], "pp", S3, "output/annual_gap.csv",
      sprintf("One-year growth gap HEF - HEM, %d-%d%s", y - 1, y, note), 1, TRUE)
  add(sprintf("annual_gap_detrended_%d", y), ag$gap_detrended[i], "pp", S3, "output/annual_gap.csv",
      sprintf("Annual gap net of each group's 2015-19 trend, %d-%d%s", y - 1, y, note), 1, TRUE)
}

# ---- 9. Occupation mix and within-major-group check (04) ----------------------------
add("HEF_n_major_groups", length(unique(occ$soc_major[occ$group == "HEF"])), "count", S4,
    "output/hef_hem_occupations.csv", "SOC major groups that contain HEF units")
add("HEM_n_major_groups", length(unique(occ$soc_major[occ$group == "HEM"])), "count", S4,
    "output/hef_hem_occupations.csv", "SOC major groups that contain HEM units")
add("HEM_n_computer_engineering", sum(occ$group == "HEM" & occ$soc_major %in% c(15, 17)), "count", S4,
    "output/hef_hem_occupations.csv", "HEM units in Computer/math (15) or Architecture/engineering (17)")
add("HEF_n_office_admin", sum(occ$group == "HEF" & occ$soc_major == 43), "count", S4,
    "output/hef_hem_occupations.csv", "HEF units in Office and administrative support (43)")
add("n_shared_major_groups", length(intersect(occ$soc_major[occ$group == "HEF"], occ$soc_major[occ$group == "HEM"])),
    "count", S4, "output/hef_hem_occupations.csv", "SOC major groups containing both HEF and HEM units")
w <- regmatches(wg, regexpr("^Occupations left: [0-9]+", wg)); stopifnot(length(w) == 1)
add("within_group_n_occ", as.numeric(sub("\\D+", "", w)), "count", S4, "output/within_group_gap.txt",
    "Units left for the within-major-group comparison")
w <- wg[grepl("^Within-group F - M gap", wg)]; stopifnot(length(w) == 1)
num <- as.numeric(regmatches(w, gregexpr("[-+]?[0-9]*\\.?[0-9]+", w))[[1]])
add("within_group_gap", num[1], "pp_per_yr", S4, "output/within_group_gap.txt",
    "Within-major-group F - M gap (NOT informative: 4 units, 2 residual df; cite only as a limitation)", sign = TRUE)

# ---- Write --------------------------------------------------------------------------------
N <- do.call(rbind, rows)
stopifnot(!anyDuplicated(N$name))
dir.create("output", showWarnings = FALSE)
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Wrote output/numbers.csv (%d numbers)\n", nrow(N)))
