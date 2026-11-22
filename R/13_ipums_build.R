# 13_ipums_build.R
# Weighted employment by harmonized unit x year x sex x degree x age band, 2015-2025, from IPUMS CPS
# basic monthly data. Extends 11_ipums_panel.R (unchanged) with sex and degree as table dimensions.
#
# Input (git-ignored, see .gitignore): data/raw/ipums/cps_00002.dat.gz + cps_00002.xml (DDI).
#   Flood et al., IPUMS CPS: Version 13.0 (2025), doi:10.18128/D030.V13.0. Basic monthly
#   Jan 2015 - Sep 2026 (Oct 2025 not collected) plus ASEC 2009-2026.
# Column positions are read from the DDI codebook. The 2.7 GB file is streamed through awk, which
#   aggregates in one pass; only the aggregate table is written.
#
# Definitions (as 11):
#   Records: basic monthly only (ASECFLAG != 1), 2015-2025, employed (EMPSTAT 10 or 12), age 16+.
#   Age bands: 16-21, 22-25, 26-34, 35+ (16-21 kept so all-ages totals match the Table 11 universe).
#   Degree: "BA+" = EDUC 111 bachelor's, 123 master's, 124 professional, 125 doctorate;
#     "noBA" = any other valid EDUC; "unknown" = EDUC 999, 000 or 001 (reported; expected ~0).
#   Employment: sum of WTFINL over a year's monthly files / months in that year, thousands.
#   n_resp: person-month survey responses behind the cell.
#   Units: OCC mapped to the 309 sample units with the same crosswalk as 01_build_panel.R
#     (data/derived/unit_members.csv): 2010 Census codes through 2019, 2018 codes from 2020.
# Output: data/derived/ipums_unit_year_sex_deg_age.csv (aggregates only; safe to commit).
#   Stops if its totals do not equal 11's data/derived/ipums_unit_age_year.csv.

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
P <- sapply(c("YEAR", "MONTH", "ASECFLAG", "WTFINL", "AGE", "SEX", "EMPSTAT", "OCC", "EDUC"), pos)
stopifnot(grepl('dcml="4"[^>]*name="WTFINL"', x))
for (code in c("111", "123", "124", "125"))                      # BA+ codes as labelled in this DDI
  stopifnot(grepl(sprintf("<catValu>%s</catValu>\\s*<labl>(Bachelor|Master|Professional|Doctorate)", code), x))
cat("Column positions from DDI:\n"); print(P)

# ---- 2. Stream and aggregate with awk --------------------------------------------------------------
s <- function(v) sprintf("substr($0,%d,%d)", P["start", v], P["width", v])
awk <- paste0(
  "{ y = ", s("YEAR"), " + 0; if (y < 2015 || y > 2025) next; ",
  "if (", s("ASECFLAG"), " == \"1\") next; ",
  "e = ", s("EMPSTAT"), " + 0; if (e != 10 && e != 12) next; ",
  "a = ", s("AGE"), " + 0; if (a < 16) next; ",
  "b = (a <= 21) ? \"16-21\" : (a <= 25) ? \"22-25\" : (a <= 34) ? \"26-34\" : \"35+\"; ",
  "ed = ", s("EDUC"), " + 0; ",
  "d = (ed == 111 || ed == 123 || ed == 124 || ed == 125) ? \"BA+\" : (ed == 999 || ed <= 1) ? \"unknown\" : \"noBA\"; ",
  "k = y \",\" (", s("OCC"), " + 0) \",\" b \",\" (", s("SEX"), " + 0) \",\" d; ",
  "w[k] += ", s("WTFINL"), " / 10000; n[k]++; mo[y \",\" (", s("MONTH"), " + 0)] = 1 } ",
  "END { for (k in w) printf \"R,%s,%.4f,%d\\n\", k, w[k], n[k]; for (m in mo) print \"M,\" m \",0,0,0,0,0\" }")
t0 <- Sys.time()
cat("Streaming", dat, "...\n")
A <- read.csv(pipe(sprintf("gzip -dc %s | awk '%s'", shQuote(dat), gsub("'", "'\\\\''", awk))),
              header = FALSE, col.names = c("type", "year", "occ", "band", "sex", "degree", "wsum", "n"))
cat(sprintf("  done in %.0f s\n", as.numeric(difftime(Sys.time(), t0, units = "secs"))))

months <- A[A$type == "M", c("year", "occ")]
nmon <- table(months$year)
stopifnot(all(nmon[as.character(2015:2024)] == 12), nmon[["2025"]] == 11)
A <- A[A$type == "R", -1]
A$emp_k <- A$wsum / as.numeric(nmon[as.character(A$year)]) / 1000
stopifnot(all(A$sex %in% 1:2))
unk <- tapply(A$emp_k, A$degree == "unknown", sum)
cat(sprintf("Employment with unknown degree: %.4f%% of all employed 16+ person-weight\n",
            100 * (if ("TRUE" %in% names(unk)) unk[["TRUE"]] else 0) / sum(A$emp_k)))

# ---- 3. Map Census codes to units (same crosswalk as 01) -------------------------------------------
m <- read.csv("data/derived/unit_members.csv")
U <- read.csv("data/derived/panel_units.csv"); U <- U[U$in_sample, ]
m <- m[m$unit %in% U$unit, ]
A$scheme <- ifelse(A$year <= 2019, 2010, 2018)
A$unit <- m$unit[match(paste(A$scheme, A$occ), paste(m$scheme, as.integer(m$code)))]
A <- A[!is.na(A$unit), ]
A$sex <- ifelse(A$sex == 2, "F", "M")

D <- aggregate(cbind(emp_k, n_resp = n) ~ unit + year + sex + degree + band, data = A, FUN = sum)
D <- D[order(D$unit, D$year, D$sex, D$degree, D$band), ]
write.csv(D, "data/derived/ipums_unit_year_sex_deg_age.csv", row.names = FALSE)
cat(sprintf("Wrote data/derived/ipums_unit_year_sex_deg_age.csv: %d non-empty cells\n", nrow(D)))

# ---- 4. Consistency with 11's table -----------------------------------------------------------------
if (file.exists("data/derived/ipums_unit_age_year.csv")) {
  O <- read.csv("data/derived/ipums_unit_age_year.csv")
  a <- aggregate(cbind(emp_k, n_resp) ~ unit + year + band, data = D, FUN = sum)
  j <- merge(O, a, by = c("unit", "year", "band"), all = TRUE, suffixes = c("_11", "_13"))
  j[is.na(j)] <- 0
  stopifnot(max(abs(j$emp_k_11 - j$emp_k_13)) < 1e-6, all(j$n_resp_11 == j$n_resp_13))
  w <- aggregate(emp_k ~ unit + year + band, data = D[D$sex == "F", ], FUN = sum)
  j <- merge(O, w, by = c("unit", "year", "band"), all.x = TRUE); j$emp_k.y[is.na(j$emp_k.y)] <- 0
  stopifnot(max(abs(j$emp_women_k - j$emp_k.y)) < 1e-6)
  cat("Totals by unit x year x band (all, and women) equal 11_ipums_panel.R's table exactly.\n")
}
