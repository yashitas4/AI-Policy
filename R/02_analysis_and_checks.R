# 02_analysis_and_checks.R
# Baseline result plus checks 1-3, run on data/derived/panel_units.csv (from 01_build_panel.R).
#
# Definitions (fixed before running the checks):
#   Female-dominated: >70% women in 2022; male-dominated: <30% women in 2022; else non-dominated.
#   High exposure: top 25% of GPT-4 beta (Eloundou et al. 2024) across units in the sample.
#   Growth: compound annual growth of employment, % per year.
#     Pre  = 2015 -> 2019 (4 yrs). Post = 2022 -> 2025 (3 yrs).
#   Gap: among high-exposure units, (post - pre growth) for female-dominated minus male-dominated,
#     from an occupation-level regression weighted by 2022 employment, HC1 robust SEs.
#
# Pass standards (from the project's check plan):
#   Check 1 (human-rated exposure): gap stays positive; 95% CI not entirely below zero.
#   Check 2 (2019 baseline / COVID rebound): high-exposure female-dominated employment in 2025
#            is at or above its 2019 level.
#   Check 3 (post-period ends 2024 / 2025 policy shocks): gap stays positive without 2025.

suppressMessages({ library(sandwich); library(lmtest) })
U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]

U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))
U$gender <- factor(U$gender, levels = c("N", "F", "M"))
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$g_pre    <- cagr(U$e2019, U$e2015, 4)
U$g_post   <- cagr(U$e2025, U$e2022, 3)
U$g_post24 <- cagr(U$e2024, U$e2022, 2)

cut_gpt4  <- quantile(U$beta_gpt4,  0.75, na.rm = TRUE)
cut_human <- quantile(U$beta_human, 0.75, na.rm = TRUE)
U$high       <- U$beta_gpt4  >= cut_gpt4
U$high_human <- !is.na(U$beta_human) & U$beta_human >= cut_human

out <- character()
say <- function(fmt, ...) { s <- if (...length()) sprintf(fmt, ...) else fmt; cat(s, "\n"); out <<- c(out, s) }

group_table <- function(high_var, post_var = "g_post", post_end = "e2025", post_yrs = 3) {
  do.call(rbind, lapply(c(TRUE, FALSE), function(h) do.call(rbind, lapply(c("F", "M", "N"), function(g) {
    d <- U[U[[high_var]] == h & U$gender == g, ]
    data.frame(exposure = ifelse(h, "high", "lower"), gender = g, n_occ = nrow(d),
               emp2015 = sum(d$e2015), emp2019 = sum(d$e2019), emp2022 = sum(d$e2022),
               emp_post_end = sum(d[[post_end]]),
               growth_pre  = round(cagr(sum(d$e2019), sum(d$e2015), 4), 2),
               growth_post = round(cagr(sum(d[[post_end]]), sum(d$e2022), post_yrs), 2))
  }))))
}

gap_reg <- function(high_var, post_var) {
  d <- U[U[[high_var]], ]
  d$dg <- d[[post_var]] - d$g_pre
  m <- lm(dg ~ gender, data = d, weights = e2022)
  V <- vcovHC(m, type = "HC1")
  L <- c(0, 1, -1)                                      # genderF - genderM
  est <- sum(L * coef(m)); se <- sqrt(drop(t(L) %*% V %*% L))
  df <- m$df.residual
  c(gap = est, se = se, p = 2 * pt(-abs(est / se), df),
    lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se,
    nF = sum(d$gender == "F"), nM = sum(d$gender == "M"), nN = sum(d$gender == "N"))
}
fmt_gap <- function(r) sprintf("F - M gap = %+.2f pp/yr (SE %.2f, p = %.3f, 95%% CI %+.2f to %+.2f; high-exposure n: F=%d, M=%d, N=%d)",
                               r["gap"], r["se"], r["p"], r["lo"], r["hi"], r["nF"], r["nM"], r["nN"])

say("Sample: %d harmonized occupations. High-exposure cutoff: GPT-4 beta >= %.3f (human beta >= %.3f).",
    nrow(U), cut_gpt4, cut_human)

# ---- Baseline ----------------------------------------------------------------
say("\n=== BASELINE (GPT-4 beta, post = 2022-25) ===")
gt <- group_table("high"); print(gt)
base <- gap_reg("high", "g_post"); say(fmt_gap(base))
ex <- U[grepl("^Executive secretaries", U$title), ]
say("Example - secretaries unit: %s k (2015), %s k (2019), %s k (2022), %s k (2025)",
    ex$e2015, ex$e2019, ex$e2022, ex$e2025)

# ---- Check 1: human-rated exposure ------------------------------------------
say("\n=== CHECK 1: human-rated exposure (rebuilt) ===")
c1 <- gap_reg("high_human", "g_post"); say(fmt_gap(c1))
say("Standard: gap > 0 and 95%% CI not entirely below zero -> %s",
    ifelse(c1["gap"] > 0 && c1["hi"] > 0, "PASS", "FAIL"))

# ---- Check 2: 2019 baseline ----------------------------------------------------
say("\n=== CHECK 2: 2019 baseline (COVID rebound) ===")
hf <- U[U$high & U$gender == "F", ]
say("High-exposure female-dominated (%d occupations): 2019 = %d k, 2022 = %d k, 2025 = %d k (2025 vs 2019: %+.1f%%)",
    nrow(hf), sum(hf$e2019), sum(hf$e2022), sum(hf$e2025), 100 * (sum(hf$e2025) / sum(hf$e2019) - 1))
say("Standard: 2025 level at or above 2019 -> %s", ifelse(sum(hf$e2025) >= sum(hf$e2019), "PASS", "FAIL"))
hx <- hf[!grepl("^Executive secretaries", hf$title), ]
say("Diagnostic only (not the pass standard): same group without the secretaries unit: 2019 = %d k, 2025 = %d k (%+.1f%%)",
    sum(hx$e2019), sum(hx$e2025), 100 * (sum(hx$e2025) / sum(hx$e2019) - 1))
# No male-dominated comparison here: two high-exposure male-dominated units (computer occupations,
# all other; web developers) are not comparable across the 2019 -> 2022 code change, so their
# 2019 -> 2025 levels would mislead. Growth rates elsewhere are measured within one code system.
hf$chg <- hf$e2025 - hf$e2019
say("Largest contributors to the high-exposure female-dominated change, 2019-2025 (k):")
o <- hf[order(hf$chg), c("title", "e2019", "e2025", "chg")]; o$title <- substr(o$title, 1, 60)
print(rbind(head(o, 4), tail(o, 3)), row.names = FALSE)
out <- c(out, capture.output(print(rbind(head(o, 4), tail(o, 3)), row.names = FALSE)))

# ---- Check 3: post-period ending 2024 ----------------------------------------
say("\n=== CHECK 3: post-period ends 2024 (drop 2025) ===")
gt3 <- group_table("high", "g_post24", "e2024", 2); print(gt3)
c3 <- gap_reg("high", "g_post24"); say(fmt_gap(c3))
say("Standard: gap stays positive -> %s", ifelse(c3["gap"] > 0, "PASS", "FAIL"))

dir.create("output", showWarnings = FALSE)
write.csv(gt,  "output/group_trends_baseline.csv", row.names = FALSE)
write.csv(gt3, "output/group_trends_post2024.csv", row.names = FALSE)
write.csv(rbind(baseline = base, check1_human = c1, check3_post2024 = c3), "output/gap_estimates.csv")
writeLines(out, "output/checks_summary.txt")
