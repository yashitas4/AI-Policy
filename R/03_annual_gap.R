# 03_annual_gap.R
# Year-by-year employment-growth gap, high-exposure female-dominated (HEF) minus
# high-exposure male-dominated (HEM), 2016-2025, plus a version net of each group's
# 2015-19 linear trend. Run after 01_build_panel.R; does not modify earlier outputs.
#
# Uses the same units, sample, gender groups (2022 % women) and high-exposure cutoff
# (top 25% GPT-4 beta) as 02_analysis_and_checks.R.
#
# Each point (year t) compares one-year employment growth from t-1 to t:
#   growth_G(t) = 100 * (E_G(t) / E_G(t-1) - 1),  E_G = total employment of group G's units
#   gap(t)      = growth_HEF(t) - growth_HEM(t)                     [pp]
# Detrended: each group's trend growth is the slope of a linear fit of log E_G on year,
#   2015-2019 (five annual points), expressed as % per year. Then
#   gap_detrended(t) = (growth_HEF(t) - trend_HEF) - (growth_HEM(t) - trend_HEM).
#
# The 2020 point (2019 -> 2020) crosses the 2010 -> 2018 Census code change and is left off the
# plots: "Computer occupations, all other" jumps 72k -> 792k by reclassification. It stays in the CSV.
#
# Extra inputs (data/raw/bls), read from bls.gov Table 11 pages in a browser, Nov 2026:
#   t11_2010codes_2016_2018.tsv      title, emp2016, emp2017, emp2018 (rows = 2015/2019 file)
#   t11_2018codes_2020_2021_2023.tsv emp2020, emp2021, emp2023 (rows = 2025 file / 2022-24 file)
# Outputs: output/annual_gap.csv, output/annual_gap.png, output/annual_gap_detrended.png

suppressMessages(library(readxl))

# ---- 1. Row-level employment for every year ----------------------------------
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

# ---- 2. Collapse to harmonized units (membership from 01_build_panel.R) ----------
m <- read.csv("data/derived/unit_members.csv")
yrs10 <- 2015:2019; yrs18 <- 2020:2025
sum_by_unit <- function(rows, mem, yrs) {
  rows <- rows[match(mem$title, rows$title), ]
  stopifnot(!anyNA(rows$title))
  sapply(paste0("e", yrs), function(v) tapply(rows[[v]], mem$unit, sum))
}
E <- cbind(sum_by_unit(a, m[m$scheme == 2010, ], yrs10),
           sum_by_unit(b, m[m$scheme == 2018, ], yrs18)[, paste0("e", yrs18)])

U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
E <- E[U$unit, ]
# Consistency: re-aggregated years already in the panel must match it exactly
for (v in c("e2015", "e2019", "e2022", "e2024", "e2025")) stopifnot(all(E[, v] == U[[v]]))

U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)

# ---- 3. Group totals, annual growth, gap -----------------------------------------
yrs <- 2015:2025
tot <- sapply(c(HEF = "F", HEM = "M"), function(g) colSums(E[U$high & U$gender == g, paste0("e", yrs)]))
rownames(tot) <- yrs
gr <- 100 * (tot[-1, ] / tot[-nrow(tot), ] - 1)

trend <- apply(tot[as.character(2015:2019), ], 2, function(e) {
  b <- coef(lm(log(e) ~ I(2015:2019)))[2]; 100 * (exp(b) - 1)
})

res <- data.frame(year = yrs[-1],
                  emp_HEF = tot[-1, "HEF"], emp_HEM = tot[-1, "HEM"],
                  growth_HEF = gr[, "HEF"], growth_HEM = gr[, "HEM"],
                  gap = gr[, "HEF"] - gr[, "HEM"],
                  gap_detrended = (gr[, "HEF"] - trend["HEF"]) - (gr[, "HEM"] - trend["HEM"]),
                  crosses_code_change = yrs[-1] == 2020)
cat(sprintf("High-exposure units: HEF = %d, HEM = %d\n", sum(U$high & U$gender == "F"), sum(U$high & U$gender == "M")))
cat(sprintf("2015-19 trend growth (%%/yr): HEF %+.2f, HEM %+.2f\n", trend["HEF"], trend["HEM"]))
print(res, digits = 3, row.names = FALSE)

dir.create("output", showWarnings = FALSE)
write.csv(transform(res, trend_HEF = trend["HEF"], trend_HEM = trend["HEM"]),
          "output/annual_gap.csv", row.names = FALSE)

# ---- 4. Plots ----------------------------------------------------------------------
ink <- "#0b0b0b"; ink2 <- "#52514e"; series <- "#2a78d6"; covid <- "#e9e8e4"; grid <- "#e4e3df"

gap_plot <- function(file, y, title, subtitle, ylab) {
  png(file, width = 1800, height = 1100, res = 200)
  par(mar = c(5.2, 4.6, 4.2, 1.2), bg = "#fcfcfb", fg = ink2, col.axis = ink2, family = "sans")
  # 2020 is not plotted: it spans the 2010 -> 2018 code change, and one reclassified unit
  # ("Computer occupations, all other": 72k in 2019, 792k in 2020) dominates it.
  x <- res$year; y[x == 2020] <- NA
  lim <- range(c(0, y), na.rm = TRUE); lim <- lim + c(-1, 1) * 0.15 * diff(lim)
  plot(NA, xlim = c(2015.5, 2025.5), ylim = lim, xaxt = "n", yaxt = "n", xlab = "", ylab = "", bty = "n")
  rect(2019.5, lim[1], 2021.5, lim[2], col = covid, border = NA)
  text(2020.5, lim[2], "COVID", col = ink2, adj = c(0.5, 1.4), cex = 0.8)
  abline(h = pretty(lim), col = grid, lwd = 1)
  abline(h = 0, col = ink2, lwd = 1.2)
  axis(1, at = x, labels = x, lwd = 0, lwd.ticks = 0, cex.axis = 0.85)
  axis(2, at = pretty(lim), las = 1, lwd = 0, cex.axis = 0.85)
  mtext(ylab, side = 2, line = 3.2, cex = 0.8, col = ink2)
  lines(x, y, col = series, lwd = 2)                # NA at 2020 breaks the line
  text(2020, 0, "2020 not shown:\ncode change\n(see note)", col = ink2, cex = 0.62, pos = 1, offset = 0.8)
  points(x, y, pch = 21, bg = series, col = "#fcfcfb", cex = 1.4, lwd = 1.5)
  i <- which(x == 2022)
  points(x[i], y[i], pch = 21, bg = series, col = ink, cex = 2.1, lwd = 2)
  # label above a local peak, below a local trough, so labels sit off the line
  nb <- function(j) mean(c(y[max(j - 1, 1)], y[min(j + 1, length(y))]), na.rm = TRUE)
  up <- sapply(seq_along(y), function(j) is.na(nb(j)) || y[j] >= nb(j))
  ok <- !is.na(y) & x != 2022
  text(x[ok], y[ok], sprintf("%+.1f", y[ok]), pos = ifelse(up[ok], 3, 1), offset = 0.7,
       col = ink2, cex = 0.7)
  text(x[i], y[i], sprintf("2022: %+.1f", y[i]), pos = if (up[i]) 3 else 1, offset = 0.9,
       col = ink, cex = 0.8, font = 2)
  mtext(title, side = 3, line = 2.4, adj = 0, cex = 1.05, font = 2, col = ink)
  mtext(subtitle, side = 3, line = 1.1, adj = 0, cex = 0.75, col = ink2)
  mtext(paste("Each point: change from the previous year. 2020 (2019->2020) spans the 2010->2018 occupation-code change;",
              "\"Computer occupations, all other\"\ngoes from 72k to 792k by reclassification, so that year is omitted",
              "(value kept in output/annual_gap.csv). Source: BLS CPS Table 11; exposure: Eloundou et al. (2024)."),
        side = 1, line = 4.1, adj = 0, cex = 0.58, col = ink2)
  dev.off()
}

gap_plot("output/annual_gap.png", res$gap,
         "Employment-growth gap: high-exposure female- minus male-dominated occupations",
         "One-year growth in group employment, HEF minus HEM, 2016-2025",
         "Gap (percentage points)")
gap_plot("output/annual_gap_detrended.png", res$gap_detrended,
         "Employment-growth gap net of each group's 2015-19 trend",
         sprintf("HEF minus HEM, each group's growth minus its 2015-19 log-linear trend (HEF %+.2f%%/yr, HEM %+.2f%%/yr)",
                 trend["HEF"], trend["HEM"]),
         "Detrended gap (percentage points)")
cat("Wrote output/annual_gap.csv, output/annual_gap.png, output/annual_gap_detrended.png\n")
