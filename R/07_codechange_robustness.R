# 07_codechange_robustness.R
# Check 4: code-change robustness. Pass standard written in README.md (commit 1cb0c95) before
# this script was written or run.
#
# Pre-period growth uses 2010 Census codes, post-period growth uses 2018 codes. A unit whose
# employment jumps across the switch (e.g. "Computer occupations, all other": 72k in 2019,
# 969k in 2022) is mostly measuring reclassification, and 2022 weights then inflate it.
#
#   Flag:  in-sample unit with |E2022 / E2019 - 1| > 0.5.
#   (1)    main gap with flagged units dropped. Gender groups and the high-exposure cutoff are
#          kept from the baseline (cutoff computed on the full sample, as in 02).
#   (2)    main gap on the full sample, weighted by 2019 instead of 2022 employment.
#   Pass:  F - M gap > 0 in both (1) and (2).
#
# Gap definition as in 02: among high-exposure units, dg = (2022-25 CAGR) - (2015-19 CAGR),
# regression of dg on gender, F - M contrast, HC1 SEs.
# Run after 05 (and 06). Writes output/codechange_flagged_units.csv,
# output/codechange_robustness.csv, and adds its rows to output/numbers.csv.

suppressMessages({ library(sandwich); library(lmtest) })
U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")),
                   levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$dg <- cagr(U$e2025, U$e2022, 3) - cagr(U$e2019, U$e2015, 4)
U$chg_19_22 <- U$e2022 / U$e2019 - 1
U$flag <- abs(U$chg_19_22) > 0.5

gap <- function(d, w) {
  d$w <- d[[w]]
  m <- lm(dg ~ gender, data = d, weights = w)
  V <- vcovHC(m, type = "HC1"); L <- c(0, 1, -1)
  est <- sum(L * coef(m)); se <- sqrt(drop(t(L) %*% V %*% L)); df <- m$df.residual
  c(gap = est, se = se, p = 2 * pt(-abs(est / se), df),
    lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se,
    nF = sum(d$gender == "F"), nM = sum(d$gender == "M"), nN = sum(d$gender == "N"))
}
H <- U[U$high, ]
base <- gap(H, "e2022")
ge <- read.csv("output/gap_estimates.csv", row.names = 1)
stopifnot(abs(base["gap"] - ge["baseline", "gap"]) < 1e-8)   # reproduces 02's baseline

r1 <- gap(H[!H$flag, ], "e2022")
r2 <- gap(H, "e2019")
pass <- r1["gap"] > 0 && r2["gap"] > 0

# ---- Report ---------------------------------------------------------------------------
fl <- U[U$flag, ]
fl <- fl[order(-abs(fl$chg_19_22)), ]
grp <- ifelse(fl$high, paste0("HE", as.character(fl$gender)), paste0("low-", as.character(fl$gender)))
flagged <- data.frame(group = grp, unit_title = fl$title, emp2019_k = fl$e2019, emp2022_k = fl$e2022,
                      pct_change_19_22 = round(100 * fl$chg_19_22, 1), emp2025_k = fl$e2025)
cat(sprintf("Flagged units (|2019->2022 change| > 50%%): %d of %d in sample; %d high-exposure (HEF %d, HEM %d, HEN %d)\n",
            nrow(fl), nrow(U), sum(fl$high), sum(fl$high & fl$gender == "F"),
            sum(fl$high & fl$gender == "M"), sum(fl$high & fl$gender == "N")))
print(transform(flagged, unit_title = substr(unit_title, 1, 55)), row.names = FALSE)

fmt <- function(lab, r) sprintf("%-34s %+.2f pp/yr (SE %.2f, p = %.3f, 95%% CI %+.2f to %+.2f; n F=%d, M=%d)",
                                lab, r["gap"], r["se"], r["p"], r["lo"], r["hi"], r["nF"], r["nM"])
cat("\n", fmt("Baseline (2022 weights):", base), "\n", sep = "")
cat(fmt("(1) Flagged units dropped:", r1), "\n")
cat(fmt("(2) Weighted by 2019 employment:", r2), "\n")
cat(sprintf("Pass standard: gap > 0 in both (1) and (2) -> %s\n", ifelse(pass, "PASS", "FAIL")))

write.csv(flagged, "output/codechange_flagged_units.csv", row.names = FALSE)
write.csv(rbind(baseline = base, drop_flagged = r1, weight_2019 = r2), "output/codechange_robustness.csv")

# ---- Add to numbers.csv -------------------------------------------------------------------
S <- "R/07_codechange_robustness.R"; src <- "output/codechange_robustness.csv"
f2 <- function(v, d = 2, sign = TRUE) formatC(v, format = "f", digits = d, flag = if (sign) "+" else "")
row <- function(name, value, display, unit, source, description)
  data.frame(name = name, value = signif(unname(value), 8), display = display, unit = unit,
             script = S, source = source, description = description)
est_rows <- function(p, r, w) rbind(
  row(paste0(p, c("_gap", "_se", "_p", "_ci_lo", "_ci_hi")), r[c("gap", "se", "p", "lo", "hi")],
      c(f2(r["gap"]), f2(r["se"], sign = FALSE), f2(r["p"], 3, FALSE), f2(r["lo"]), f2(r["hi"])),
      c("pp_per_yr", "pp_per_yr", "p", "pp_per_yr", "pp_per_yr"), src,
      paste(w, c(": F - M gap", ": HC1 standard error", ": p-value", ": 95% CI lower bound", ": 95% CI upper bound"))),
  row(paste0(p, c("_nF", "_nM")), r[c("nF", "nM")], as.character(unname(r[c("nF", "nM")])), "count", src,
      paste(w, c(": female-dominated units", ": male-dominated units"))))
fsrc <- "output/codechange_flagged_units.csv"
new <- rbind(
  row("check4_n_flagged", nrow(fl), as.character(nrow(fl)), "count", fsrc,
      "Check 4: in-sample units with 2019->2022 employment change > 50% in either direction"),
  row("check4_n_flagged_HEF", sum(fl$high & fl$gender == "F"), as.character(sum(fl$high & fl$gender == "F")),
      "count", fsrc, "Check 4: flagged units that are HEF"),
  row("check4_n_flagged_HEM", sum(fl$high & fl$gender == "M"), as.character(sum(fl$high & fl$gender == "M")),
      "count", fsrc, "Check 4: flagged units that are HEM"),
  est_rows("check4_drop", r1, "Check 4 (1): main gap, flagged units dropped"),
  est_rows("check4_w2019", r2, "Check 4 (2): main gap, weighted by 2019 employment"),
  row("check4_pass", as.numeric(pass), ifelse(pass, "PASS", "FAIL"), "flag", src,
      "Check 4 pass standard: gap > 0 in both (1) and (2)"))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
