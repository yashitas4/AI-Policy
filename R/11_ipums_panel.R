# 11_ipums_panel.R
# Employment by harmonized unit x age band x year (2015-2025) from IPUMS CPS basic monthly data.
#
# Input (git-ignored, see .gitignore): data/raw/ipums/cps_00002.dat.gz + cps_00002.xml (DDI).
#   Flood et al., IPUMS CPS: Version 13.0 (2025), doi:10.18128/D030.V13.0. Extract cps_00002:
#   basic monthly Jan 2015 - Sep 2026 (Oct 2025 not collected) plus ASEC 2009-2026.
# Column positions are read from the DDI codebook, not hard-coded. The 2.7 GB fixed-width file is
#   streamed through awk, which aggregates in one pass; R only sees the aggregated table.
#
# Definitions:
#   Records: basic monthly only (ASECFLAG != 1, so ASEC supplement records are excluded),
#     2015-2025, employed (EMPSTAT 10 at work, 12 has job not at work), age 16+ (Table 11 universe).
#   Age bands: 16-21, 22-25, 26-34, 35+. (16-21 is kept only so all-ages totals match Table 11.)
#   Employment: sum of WTFINL over a year's monthly files / number of months in that year
#     = annual average, in thousands. 2025 averages 11 months (no October 2025 survey).
#   Units: OCC is the Census code of the survey year: 2010 codes through 2019, 2018 codes from 2020,
#     the same codes as data/derived/unit_members.csv. Only the 309-unit analysis sample is kept.
# Output: data/derived/ipums_unit_age_year.csv (aggregates only; safe to commit), and
#   output/ipums_validation.txt (comparison with BLS Table 11). Stops if the match is poor.

dat <- "data/raw/ipums/cps_00002.dat.gz"; ddi <- "data/raw/ipums/cps_00002.xml"
if (!file.exists(dat) || !file.exists(ddi))
  stop("IPUMS extract not found: put cps_00002.dat.gz and cps_00002.xml in data/raw/ipums/ (git-ignored).")

# ---- 1. Column positions from the DDI ---------------------------------------------------------
x <- paste(readLines(ddi, warn = FALSE), collapse = "\n")
pos <- function(v) {
  m <- regmatches(x, regexec(sprintf('name="%s">\\s*<location EndPos="(\\d+)" StartPos="(\\d+)" width="(\\d+)"', v), x))[[1]]
  if (length(m) != 4) stop("Variable ", v, " not found in ", ddi)
  c(start = as.integer(m[3]), width = as.integer(m[4]))
}
P <- sapply(c("YEAR", "MONTH", "ASECFLAG", "WTFINL", "AGE", "SEX", "EMPSTAT", "OCC"), pos)
stopifnot(grepl('name="WTFINL"[^>]*dcml="4"|dcml="4"[^>]*name="WTFINL"', x))   # 4 implied decimals
cat("Column positions from DDI:\n"); print(P)

# ---- 2. Stream and aggregate with awk ---------------------------------------------------------------
s <- function(v) sprintf("substr($0,%d,%d)", P["start", v], P["width", v])
awk <- paste0(
  "{ y = ", s("YEAR"), " + 0; if (y < 2015 || y > 2025) next; ",
  "if (", s("ASECFLAG"), " == \"1\") next; ",
  "e = ", s("EMPSTAT"), " + 0; if (e != 10 && e != 12) next; ",
  "a = ", s("AGE"), " + 0; if (a < 16) next; ",
  "b = (a <= 21) ? \"16-21\" : (a <= 25) ? \"22-25\" : (a <= 34) ? \"26-34\" : \"35+\"; ",
  "k = y \",\" (", s("OCC"), " + 0) \",\" b \",\" (", s("SEX"), " + 0); ",
  "w[k] += ", s("WTFINL"), " / 10000; n[k]++; mo[y \",\" (", s("MONTH"), " + 0)] = 1 } ",
  "END { for (k in w) printf \"R,%s,%.4f,%d\\n\", k, w[k], n[k]; for (m in mo) print \"M,\" m \",0,0,0,0\" }")
t0 <- Sys.time()
cat("Streaming", dat, "...\n")
A <- read.csv(pipe(sprintf("gzip -dc %s | awk '%s'", shQuote(dat), gsub("'", "'\\\\''", awk))),
              header = FALSE, col.names = c("type", "year", "occ", "band", "sex", "wsum", "n"))
cat(sprintf("  done in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

months <- A[A$type == "M", c("year", "occ")]; names(months) <- c("year", "month")
nmon <- table(months$year)
cat("Months per year:", paste(names(nmon), nmon, sep = ":", collapse = " "), "\n")
stopifnot(all(nmon[as.character(2015:2024)] == 12), nmon[["2025"]] == 11)
A <- A[A$type == "R", -1]
A$emp_k <- A$wsum / as.numeric(nmon[as.character(A$year)]) / 1000
stopifnot(all(A$sex %in% 1:2))

# ---- 3. Map Census codes to units --------------------------------------------------------------------
m <- read.csv("data/derived/unit_members.csv")
U <- read.csv("data/derived/panel_units.csv"); U <- U[U$in_sample, ]
m <- m[m$unit %in% U$unit, ]
A$scheme <- ifelse(A$year <= 2019, 2010, 2018)
A$unit <- m$unit[match(paste(A$scheme, A$occ), paste(m$scheme, as.integer(m$code)))]
cov <- tapply(A$emp_k * !is.na(A$unit), A$year, sum) / tapply(A$emp_k, A$year, sum)
cat("Share of employment (16+) in the 309 sample units, by year:", paste(names(cov), round(cov, 3), collapse = " "), "\n")
A <- A[!is.na(A$unit), ]

key <- interaction(A$unit, A$year, A$band, drop = TRUE, sep = "|")
D <- data.frame(do.call(rbind, strsplit(levels(key), "|", fixed = TRUE)))
names(D) <- c("unit", "year", "band"); D$year <- as.integer(D$year)
D$emp_k <- as.numeric(tapply(A$emp_k, key, sum))
D$emp_women_k <- as.numeric(tapply(A$emp_k * (A$sex == 2), key, sum))
D$n_resp <- as.integer(tapply(A$n, key, sum))
# complete grid: unit x year x band, zeros where nobody was observed
G <- expand.grid(unit = U$unit, year = 2015:2025, band = c("16-21", "22-25", "26-34", "35+"), stringsAsFactors = FALSE)
D <- merge(G, D, all.x = TRUE)
D[is.na(D$emp_k), c("emp_k", "emp_women_k", "n_resp")] <- 0
D <- D[order(D$unit, D$band, D$year), ]
write.csv(D, "data/derived/ipums_unit_age_year.csv", row.names = FALSE)
cat(sprintf("Wrote data/derived/ipums_unit_age_year.csv: %d rows (%d units x 11 years x 4 bands)\n", nrow(D), nrow(U)))

# ---- 4. Validate against BLS Table 11 (all ages 16+) --------------------------------------------------
tot <- aggregate(cbind(emp_k, emp_women_k) ~ unit + year, data = D, FUN = sum)
out <- character(); say <- function(...) { s <- sprintf(...); cat(s, "\n"); out <<- c(out, s) }
say("IPUMS CPS (cps_00002, basic monthly, WTFINL annual average) vs BLS CPS Table 11, 309 sample units")
for (y in c(2015, 2019, 2022, 2024, 2025)) {
  t <- tot[tot$year == y, ]; t <- t[match(U$unit, t$unit), ]; b <- U[[paste0("e", y)]]
  say("  %d: total %6.0fk IPUMS vs %6.0fk Table 11 (ratio %.4f); unit-level correlation r = %.4f; median |diff| %.1f%%",
      y, sum(t$emp_k), sum(b), sum(t$emp_k) / sum(b), cor(t$emp_k, b), 100 * median(abs(t$emp_k / b - 1)))
  if (y == 2022) {
    r22 <- cor(t$emp_k, b)
    sw <- ifelse(t$emp_k > 0, 100 * t$emp_women_k / t$emp_k, NA)
    rw <- cor(sw, U$pct_women_2022, use = "complete.obs")
    say("  2022 %% women: unit-level correlation r = %.4f; median |diff| %.2f pp (%d units with no IPUMS employment in 2022: %s)",
        rw, median(abs(sw - U$pct_women_2022), na.rm = TRUE), sum(is.na(sw)), paste(U$title[is.na(sw)], collapse = "; "))
    wt <- cor(sw, U$pct_women_2022, use = "complete.obs")
    gF <- sum(!is.na(sw) & (sw > 70) != (U$pct_women_2022 > 70)); gM <- sum(!is.na(sw) & (sw < 30) != (U$pct_women_2022 < 30))
    say("  Units that would switch gender group on IPUMS 2022 %% women: %d across the 70%% line, %d across the 30%% line", gF, gM)
  }
}
writeLines(out, "output/ipums_validation.txt")
if (r22 < 0.99 || rw < 0.99) stop("IPUMS aggregates do not track Table 11 (r < 0.99); check the extract before using it.")
