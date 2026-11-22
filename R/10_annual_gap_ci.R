# 10_annual_gap_ci.R
# Bootstrap 95% intervals for 03_annual_gap.R's year-by-year HEF - HEM growth gap, raw and net
# of each group's 2015-19 trend, plus a figure for the paper and slides.
#
# Same data, units, sample, groups and definitions as 03 (which is not modified):
#   growth_G(t) = 100 * (E_G(t) / E_G(t-1) - 1), E_G = total employment of group G's units
#   gap(t) = growth_HEF(t) - growth_HEM(t)
#   trend_G = 100 * (exp(b) - 1), b = slope of log E_G on year, 2015-19
#   gap_detrended(t) = (growth_HEF(t) - trend_HEF) - (growth_HEM(t) - trend_HEM)
# Bootstrap: 1,000 draws. Each draw resamples occupations with replacement within HEF and within
#   HEM (keeping each group's count), recomputes group totals for every year, and recomputes all
#   statistics, including the trends. Intervals are 2.5th-97.5th percentiles. Seed fixed.
# Summaries: pre-trend slope = trend_HEF - trend_HEM (pp/yr); 2023-25 average = mean of the
#   2023, 2024 and 2025 annual gaps (the three full post-ChatGPT years; ChatGPT: Nov 30, 2022).
# 2020 (2019 -> 2020) spans the 2010 -> 2018 code change: kept in the CSV, marked but not drawn
#   on the figure, and excluded from every summary.
# Run after 01 and 03 (and 05 for numbers.csv). Writes output/annual_gap_ci.csv,
# output/annual_gap_ci.png and adds rows to output/numbers.csv.

suppressMessages(library(readxl))
B <- 1000
set.seed(20261122)

# ---- 1. Unit x year employment (same reading as 03) ------------------------------------------
rd <- function(f, nm) read.delim(f, header = FALSE, quote = "", col.names = nm, na.strings = "")
a <- cbind(rd("data/raw/bls/t11_2010codes_2015_2019.tsv", c("title", "e2015", "e2019", "w2019")),
           rd("data/raw/bls/t11_2010codes_2016_2018.tsv", c("title2", "e2016", "e2017", "e2018")))
stopifnot(identical(a$title, a$title2))
x25 <- suppressMessages(read_excel("data/raw/bls/cpsaat11_2025.xlsx", col_names = FALSE))
t25 <- trimws(gsub("[[:space:]]+", " ", x25[[1]]))
k <- seq(which(t25 == "Total, 16 years and over"), nrow(x25)); k <- k[!is.na(t25[k]) & !is.na(x25[[2]][k])]
b <- cbind(rd("data/raw/bls/t11_2018codes_2022_2024.tsv", c("e2022", "w2022", "e2024")),
           rd("data/raw/bls/t11_2018codes_2020_2021_2023.tsv", c("e2020", "e2021", "e2023")))
stopifnot(nrow(b) == length(k))
b$title <- t25[k]; b$e2025 <- as.numeric(x25[[2]][k])

m <- read.csv("data/derived/unit_members.csv")
sum_by_unit <- function(rows, mem, yrs) {
  rows <- rows[match(mem$title, rows$title), ]
  stopifnot(!anyNA(rows$title))
  sapply(paste0("e", yrs), function(v) tapply(rows[[v]], mem$unit, sum))
}
yrs <- 2015:2025
E <- cbind(sum_by_unit(a, m[m$scheme == 2010, ], 2015:2019),
           sum_by_unit(b, m[m$scheme == 2018, ], 2020:2025)[, paste0("e", 2020:2025)])

U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
E <- E[U$unit, paste0("e", yrs)]
for (v in c("e2015", "e2019", "e2022", "e2024", "e2025")) stopifnot(all(E[, v] == U[[v]]))
U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
iF <- which(U$high & U$gender == "F"); iM <- which(U$high & U$gender == "M")

# ---- 2. Statistics for one draw ------------------------------------------------------------------
post <- as.character(2023:2025)
stats <- function(sF, sM) {
  tot <- cbind(F = colSums(E[sF, , drop = FALSE]), M = colSums(E[sM, , drop = FALSE]))
  gr <- 100 * (tot[-1, ] / tot[-nrow(tot), ] - 1)
  tr <- apply(log(tot[1:5, ]), 2, function(y) 100 * (exp(coef(lm(y ~ I(2015:2019)))[[2]]) - 1))
  gap <- gr[, "F"] - gr[, "M"]; gd <- gap - (tr["F"] - tr["M"])
  names(gap) <- names(gd) <- yrs[-1]
  c(setNames(gap, paste0("gap_", yrs[-1])), setNames(gd, paste0("gapdt_", yrs[-1])),
    trend_HEF = tr[["F"]], trend_HEM = tr[["M"]], pretrend_slope = tr[["F"]] - tr[["M"]],
    avg_2023_25 = mean(gap[post]), avg_2023_25_dt = mean(gd[post]))
}
est <- stats(iF, iM)

# Point estimates must reproduce 03's output
ag <- read.csv("output/annual_gap.csv")
stopifnot(max(abs(est[paste0("gap_", ag$year)] - ag$gap)) < 1e-8,
          max(abs(est[paste0("gapdt_", ag$year)] - ag$gap_detrended)) < 1e-8,
          abs(est[["trend_HEF"]] - ag$trend_HEF[1]) < 1e-8)

boot <- t(replicate(B, stats(sample(iF, replace = TRUE), sample(iM, replace = TRUE))))
ci <- apply(boot, 2, quantile, c(0.025, 0.975))
R <- data.frame(stat = names(est), estimate = unname(est), lo = ci[1, names(est)], hi = ci[2, names(est)],
                boot_se = apply(boot, 2, sd)[names(est)], row.names = NULL)

res <- data.frame(year = yrs[-1],
                  gap = est[paste0("gap_", yrs[-1])], gap_lo = ci[1, paste0("gap_", yrs[-1])],
                  gap_hi = ci[2, paste0("gap_", yrs[-1])],
                  gap_detrended = est[paste0("gapdt_", yrs[-1])], gap_detrended_lo = ci[1, paste0("gapdt_", yrs[-1])],
                  gap_detrended_hi = ci[2, paste0("gapdt_", yrs[-1])],
                  crosses_code_change = yrs[-1] == 2020, post_chatgpt = yrs[-1] >= 2023, row.names = NULL)
cat(sprintf("Bootstrap: %d draws, HEF n = %d, HEM n = %d (resampled within group)\n", B, length(iF), length(iM)))
print(res, digits = 2, row.names = FALSE)
s <- R[R$stat %in% c("trend_HEF", "trend_HEM", "pretrend_slope", "avg_2023_25", "avg_2023_25_dt"), ]
cat("\nSummaries (pp/yr; 95% bootstrap interval):\n")
for (i in seq_len(nrow(s))) cat(sprintf("  %-15s %+6.2f  [%+6.2f, %+6.2f]\n", s$stat[i], s$estimate[i], s$lo[i], s$hi[i]))

dir.create("output", showWarnings = FALSE)
write.csv(res, "output/annual_gap_ci.csv", row.names = FALSE)

# ---- 3. Figure -------------------------------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; muted <- "#8a8984"; grid <- "#e4e3df"; surface <- "#fcfcfb"
series <- "#2a78d6"; band <- adjustcolor(series, alpha.f = 0.18); postfill <- "#f1efe9"

draw_panel <- function(y, lo, hi, title, lim) {
  x <- res$year; keep <- x != 2020
  plot(NA, xlim = c(2015.6, 2025.4), ylim = lim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  rect(2022.5, lim[1], 2025.5, lim[2], col = postfill, border = NA)
  text(2024, lim[2], "After ChatGPT", col = ink2, adj = c(0.5, 1.5), cex = 0.78)
  abline(h = pretty(lim), col = grid, lwd = 0.8)
  abline(h = 0, col = ink2, lwd = 1.1)
  segments(2020, lim[1] + 0.13 * diff(lim), 2020, lim[2] - 0.12 * diff(lim), col = muted, lty = 2, lwd = 1)
  text(2020, lim[1], "2020: code change,\nnot shown", col = muted, adj = c(0.5, -0.2), cex = 0.66)
  for (seg in list(x[keep & x < 2020], x[keep & x > 2020])) {
    j <- match(seg, x)
    polygon(c(seg, rev(seg)), c(lo[j], rev(hi[j])), col = band, border = NA)
    lines(seg, y[j], col = series, lwd = 2)
  }
  points(x[keep], y[keep], pch = 21, bg = series, col = surface, cex = 1.15, lwd = 1.5)
  axis(1, at = c(2015, x), labels = c("", x), lwd = 0, lwd.ticks = 0, cex.axis = 0.72, col.axis = ink2,
       padj = -0.6, gap.axis = 0)
  yt <- pretty(lim)
  axis(2, at = yt, labels = ifelse(yt == 0, "0", sprintf("%+g", yt)), las = 1, lwd = 0, cex.axis = 0.8,
       col.axis = ink2, hadj = 0.8)
  mtext(title, side = 3, line = 0.4, adj = 0, cex = 0.92, font = 2, col = ink)
}

nb <- res$year != 2020
lim <- range(unlist(res[nb, c("gap_lo", "gap_hi", "gap_detrended_lo", "gap_detrended_hi")]), 0)
lim <- lim + c(-0.06, 0.1) * diff(lim)

png("output/annual_gap_ci.png", width = 3000, height = 1500, res = 300, bg = surface)
par(mfrow = c(1, 2), mar = c(2.2, 3.4, 1.6, 0.8), oma = c(3.2, 0.6, 2.0, 0.2),
    bg = surface, fg = ink2, family = "sans")
draw_panel(res$gap, res$gap_lo, res$gap_hi, "Raw gap", lim)
mtext("pp per year", side = 2, line = 2.4, cex = 0.72, col = ink2)
draw_panel(res$gap_detrended, res$gap_detrended_lo, res$gap_detrended_hi, "Net of each group's 2015-19 trend", lim)
mtext("Employment growth, high-exposure female- minus male-dominated occupations",
      outer = TRUE, side = 3, line = 0.6, adj = 0.01, cex = 1.05, font = 2, col = ink)
mtext(sprintf(paste0("Each point: one-year change from the previous year, HEF minus HEM. Band: 95%% bootstrap interval ",
                     "(%s draws, occupations resampled within each group; HEF n = %d, HEM n = %d).\n2020 spans the ",
                     "2010 -> 2018 occupation-code change and is not drawn. Shaded: 2023-25, the first full years after ",
                     "ChatGPT (Nov 2022). Source: BLS CPS Table 11; exposure: Eloundou et al. (2024), top 25%% GPT-4 beta."),
              format(B, big.mark = ","), length(iF), length(iM)),
      outer = TRUE, side = 1, line = 2.2, adj = 0.01, cex = 0.58, col = ink2)
invisible(dev.off())
cat("Wrote output/annual_gap_ci.csv and output/annual_gap_ci.png\n")

# ---- 4. Add to numbers.csv --------------------------------------------------------------------------
S <- "R/10_annual_gap_ci.R"; src <- "output/annual_gap_ci.csv"
f2 <- function(v) formatC(v, format = "f", digits = 2, flag = "+")
desc <- c(pretrend_slope = "Pre-trend slope: HEF minus HEM 2015-19 log-linear trend growth",
          trend_HEF = "HEF 2015-19 log-linear trend growth", trend_HEM = "HEM 2015-19 log-linear trend growth",
          avg_2023_25 = "Average annual HEF - HEM growth gap, 2023-25 (raw)",
          avg_2023_25_dt = "Average annual HEF - HEM growth gap, 2023-25, net of 2015-19 trends")
unit <- c(pretrend_slope = "pp_per_yr", trend_HEF = "pct_per_yr", trend_HEM = "pct_per_yr",
          avg_2023_25 = "pp_per_yr", avg_2023_25_dt = "pp_per_yr")
new <- do.call(rbind, lapply(names(desc), function(st) {
  r <- R[R$stat == st, ]; p <- paste0("annual_", st)
  data.frame(name = paste0(p, c("", "_ci_lo", "_ci_hi")), value = signif(c(r$estimate, r$lo, r$hi), 8),
             display = f2(c(r$estimate, r$lo, r$hi)), unit = unit[[st]], script = S, source = src,
             description = paste0(desc[[st]], c("", ": 95% bootstrap interval, lower", ": 95% bootstrap interval, upper"),
                                  sprintf(" (%d draws, within-group resampling)", B)))
}))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
