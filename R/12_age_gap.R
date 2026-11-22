# 12_age_gap.R
# The main F - M gap by age band, from the IPUMS unit x age x year panel (11_ipums_panel.R).
#
# Groups are fixed exactly as in 02 (Table 11 2022 % women; top 25% GPT-4 beta over the 309 units),
# so a unit is HEF or HEM in every age band. Within each band b:
#   E_b(u, t)  = unit u's employment of workers aged b in year t (IPUMS, WTFINL annual average)
#   dg_b(u)    = CAGR 2022-25 - CAGR 2015-19 of E_b(u, .)
#   gap_b      = F - M contrast of a regression of dg_b on gender among high-exposure units,
#                weighted by E_b(u, 2022), HC1 SEs (as 02)
#   Units need E_b > 0 in 2015, 2019, 2022 and 2025; others are dropped from that band (reported).
#   Bootstrap: 1,000 draws resampling units with replacement within HEF and within HEM; the F - M
#   contrast equals the weighted mean dg of F minus that of M, recomputed each draw. 95% percentile
#   intervals. Seed fixed.
# "All 16+" sums the bands and should land close to Table 11's +2.45 (a consistency check).
# The bootstrap reflects which occupations are in each group, not CPS sampling error within a
#   unit-age cell, which is large for young workers: respondent counts are reported, and a band
#   is flagged THIN when the median HEF or HEM unit has fewer than 30 respondent-months in an
#   endpoint year.
# Run after 11 (and 05). Writes output/age_gap.csv and adds rows to output/numbers.csv.

suppressMessages({ library(sandwich); library(lmtest) })
B <- 1000; set.seed(20261122); THIN <- 30

D <- read.csv("data/derived/ipums_unit_age_year.csv")
U <- read.csv("data/derived/panel_units.csv"); U <- U[U$in_sample, ]
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")), levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
H <- U[U$high, c("unit", "title", "gender")]
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)

wide <- function(d, v) { w <- tapply(d[[v]], list(d$unit, d$year), sum); w[is.na(w)] <- 0; w }
bands <- list("22-25" = "22-25", "26-34" = "26-34", "35+" = "35+", "All 16+" = c("16-21", "22-25", "26-34", "35+"))

one_band <- function(lab) {
  d <- D[D$band %in% bands[[lab]], ]
  E <- wide(d, "emp_k"); N <- wide(d, "n_resp")
  h <- H; h$e15 <- E[h$unit, "2015"]; h$e19 <- E[h$unit, "2019"]; h$e22 <- E[h$unit, "2022"]; h$e25 <- E[h$unit, "2025"]
  ok <- h$e15 > 0 & h$e19 > 0 & h$e22 > 0 & h$e25 > 0
  dropped <- h[!ok & h$gender %in% c("F", "M"), ]
  h <- h[ok, ]
  h$dg <- cagr(h$e25, h$e22, 3) - cagr(h$e19, h$e15, 4)
  mod <- lm(dg ~ gender, data = h, weights = e22)
  V <- vcovHC(mod, type = "HC1"); L <- c(0, 1, -1)
  est <- sum(L * coef(mod)); se <- sqrt(drop(t(L) %*% V %*% L)); df <- mod$df.residual
  f <- h[h$gender == "F", ]; m <- h[h$gender == "M", ]
  wm <- function(x) sum(x$dg * x$e22) / sum(x$e22)
  stopifnot(abs(est - (wm(f) - wm(m))) < 1e-8)
  bs <- replicate(B, wm(f[sample(nrow(f), replace = TRUE), ]) - wm(m[sample(nrow(m), replace = TRUE), ]))
  grp <- function(x) c(pre = cagr(sum(x$e19), sum(x$e15), 4), post = cagr(sum(x$e25), sum(x$e22), 3))
  resp <- function(x) min(sapply(c("2015", "2019", "2022", "2025"), function(y) median(N[x$unit, y])))
  rF <- resp(f); rM <- resp(m)
  list(row = data.frame(band = lab, nF = nrow(f), nM = nrow(m), nF_dropped = sum(dropped$gender == "F"),
                        nM_dropped = sum(dropped$gender == "M"),
                        empF_2022_k = sum(f$e22), empM_2022_k = sum(m$e22),
                        HEF_growth_pre = grp(f)[["pre"]], HEF_growth_post = grp(f)[["post"]],
                        HEM_growth_pre = grp(m)[["pre"]], HEM_growth_post = grp(m)[["post"]],
                        gap = est, se_hc1 = se, p_hc1 = 2 * pt(-abs(est / se), df),
                        ci_hc1_lo = est - qt(0.975, df) * se, ci_hc1_hi = est + qt(0.975, df) * se,
                        boot_lo = unname(quantile(bs, 0.025)), boot_hi = unname(quantile(bs, 0.975)),
                        boot_se = sd(bs), median_resp_F = rF, median_resp_M = rM,
                        thin = rF < THIN || rM < THIN),
       dropped = dropped$title)
}
res <- lapply(names(bands), one_band)
R <- do.call(rbind, lapply(res, `[[`, "row"))

cat(sprintf("Gap by age (pp/yr; 95%% intervals: HC1 and %d-draw occupation bootstrap)\n", B))
for (i in seq_len(nrow(R))) with(R[i, ], cat(sprintf(
  "%-8s n F=%2d M=%2d | HEF %+5.2f -> %+5.2f, HEM %+5.2f -> %+5.2f | gap %+5.2f  HC1 [%+5.2f, %+5.2f]  boot [%+5.2f, %+5.2f] | median resp/unit-yr F %.0f, M %.0f%s\n",
  band, nF, nM, HEF_growth_pre, HEF_growth_post, HEM_growth_pre, HEM_growth_post, gap, ci_hc1_lo, ci_hc1_hi,
  boot_lo, boot_hi, median_resp_F, median_resp_M, if (thin) "  [THIN]" else "")))
for (i in seq_along(res)) if (length(res[[i]]$dropped))
  cat(sprintf("  %s: dropped (zero employment at an endpoint): %s\n", R$band[i], paste(res[[i]]$dropped, collapse = "; ")))
write.csv(R, "output/age_gap.csv", row.names = FALSE)

# ---- numbers.csv ----------------------------------------------------------------------------------
S <- "R/12_age_gap.R"; src <- "output/age_gap.csv"
f2 <- function(v, d = 2, sign = TRUE) formatC(v, format = "f", digits = d, flag = if (sign) "+" else "")
tag <- c("22-25" = "age2225", "26-34" = "age2634", "35+" = "age35p", "All 16+" = "ageall")
spec <- list(c("nF", "count", "HEF units"), c("nM", "count", "HEM units"),
             c("HEF_growth_pre", "pct_per_yr", "HEF employment CAGR 2015-19"),
             c("HEF_growth_post", "pct_per_yr", "HEF employment CAGR 2022-25"),
             c("HEM_growth_pre", "pct_per_yr", "HEM employment CAGR 2015-19"),
             c("HEM_growth_post", "pct_per_yr", "HEM employment CAGR 2022-25"),
             c("gap", "pp_per_yr", "F - M gap"), c("se_hc1", "pp_per_yr", "HC1 standard error"),
             c("p_hc1", "p", "HC1 p-value"), c("boot_lo", "pp_per_yr", "95% bootstrap interval, lower"),
             c("boot_hi", "pp_per_yr", "95% bootstrap interval, upper"), c("thin", "flag", "THIN flag (1 = few respondents)"))
new <- do.call(rbind, lapply(seq_len(nrow(R)), function(i) do.call(rbind, lapply(spec, function(s) {
  v <- as.numeric(R[i, s[1]])
  disp <- switch(s[2], count = as.character(v), p = f2(v, 3, FALSE), flag = ifelse(v == 1, "THIN", "ok"),
                 f2(v, sign = s[1] != "se_hc1"))
  data.frame(name = paste0(tag[[R$band[i]]], "_", s[1]), value = signif(v, 8), display = disp, unit = s[2],
             script = S, source = src, description = sprintf("IPUMS CPS, ages %s: %s", R$band[i], s[3]))
}))))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
