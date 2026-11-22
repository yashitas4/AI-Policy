# 06_triple_difference.R
# The milestone says low-exposure occupations are the baseline, but gap_reg in
# 02_analysis_and_checks.R compares F with M only among high-exposure units. This script adds
# the lower-exposure comparison and the triple difference.
#
#   dg = (2022-25 CAGR) - (2015-19 CAGR), per unit, as in 02.
#   (a) F - M gap among high-exposure units   (same regression as 02's baseline)
#   (b) F - M gap among lower-exposure units  (same regression, lower-exposure units)
#   (c) triple difference (a) - (b), from one pooled regression of dg on gender x exposure
#       (fully interacted, so the point estimate equals (a) - (b) exactly), weighted by 2022
#       employment, HC1 SEs. (a) and (b) come from separate regressions, so their SEs match 02.
#
# Run after 05_numbers.R. Writes output/triple_difference.csv and adds its rows to
# output/numbers.csv (replacing earlier rows of the same name, so reruns are safe).

suppressMessages({ library(sandwich); library(lmtest) })
U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")),
                   levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$dg <- cagr(U$e2025, U$e2022, 3) - cagr(U$e2019, U$e2015, 4)

contrast <- function(m, L) {
  V <- vcovHC(m, type = "HC1"); est <- sum(L * coef(m)); se <- sqrt(drop(t(L) %*% V %*% L))
  df <- m$df.residual
  c(gap = est, se = se, p = 2 * pt(-abs(est / se), df),
    lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se)
}
gap_in <- function(d) {                       # identical to gap_reg in 02
  m <- lm(dg ~ gender, data = d, weights = e2022)
  c(contrast(m, c(0, 1, -1)), nF = sum(d$gender == "F"), nM = sum(d$gender == "M"))
}
a <- gap_in(U[U$high, ])
b <- gap_in(U[!U$high, ])

m3 <- lm(dg ~ gender * high, data = U, weights = e2022)
L <- setNames(numeric(length(coef(m3))), names(coef(m3)))
L[c("genderF:highTRUE", "genderM:highTRUE")] <- c(1, -1)
c3 <- c(contrast(m3, L), nF = NA, nM = NA)
stopifnot(abs(c3["gap"] - (a["gap"] - b["gap"])) < 1e-8)

# (a) must reproduce 02's baseline
ge <- read.csv("output/gap_estimates.csv", row.names = 1)
stopifnot(abs(a["gap"] - ge["baseline", "gap"]) < 1e-8, abs(a["se"] - ge["baseline", "se"]) < 1e-8)

R <- rbind(high_exposure = a, lower_exposure = b, triple_difference = c3)
fmt <- function(lab, r) sprintf("%-38s %+.2f pp/yr (SE %.2f, p = %.3f, 95%% CI %+.2f to %+.2f)",
                                lab, r["gap"], r["se"], r["p"], r["lo"], r["hi"])
cat(fmt("(a) F - M, high exposure:", a), "\n")
cat(fmt("(b) F - M, lower exposure:", b), "\n")
cat(fmt("(c) Triple difference (a) - (b):", c3), "\n")
cat(sprintf("Lower-exposure n: F = %d, M = %d. Pooled regression residual df = %d.\n",
            b["nF"], b["nM"], m3$df.residual))
write.csv(R, "output/triple_difference.csv")

# ---- Add to numbers.csv -------------------------------------------------------------------
S <- "R/06_triple_difference.R"; src <- "output/triple_difference.csv"
f2 <- function(v, d = 2, sign = TRUE) formatC(v, format = "f", digits = d, flag = if (sign) "+" else "")
new <- do.call(rbind, lapply(list(list("ddd_high", a, "(a) F - M gap, high-exposure units"),
                                  list("ddd_low", b, "(b) F - M gap, lower-exposure units"),
                                  list("ddd_triple", c3, "(c) Triple difference (a) - (b), pooled interacted regression")),
  function(x) {
    p <- x[[1]]; r <- x[[2]]; w <- x[[3]]
    d <- data.frame(
      name  = paste0(p, c("_gap", "_se", "_p", "_ci_lo", "_ci_hi")),
      value = signif(unname(r[c("gap", "se", "p", "lo", "hi")]), 8),
      display = c(f2(r["gap"]), f2(r["se"], sign = FALSE), f2(r["p"], 3, FALSE), f2(r["lo"]), f2(r["hi"])),
      unit = c("pp_per_yr", "pp_per_yr", "p", "pp_per_yr", "pp_per_yr"),
      script = S, source = src,
      description = paste(w, c(": estimate", ": HC1 standard error", ": p-value",
                               ": 95% CI lower bound", ": 95% CI upper bound")))
    if (!is.na(r["nF"])) d <- rbind(d, data.frame(
      name = paste0(p, c("_nF", "_nM")), value = unname(r[c("nF", "nM")]),
      display = as.character(unname(r[c("nF", "nM")])), unit = "count", script = S, source = src,
      description = paste(w, c(": female-dominated units", ": male-dominated units"))))
    d
  }))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
