# 09_education_split.R
# Education split: is the F - M gap different for college and non-college occupations?
#
# Input: BLS Employment Projections Table 5.3, "Educational attainment for workers 25 years and
#   older by detailed occupation, 2023-24" (data/raw/bls/education.xlsx, sheet "Table 5.3", from
#   bls.gov/emp/ind-occ-matrix/education.xlsx; any xlsx whose name contains
#   "5.3", "5_3", "53" or "education"). Each row is a National Employment Matrix (SOC) occupation
#   with the % of workers at each attainment level.
#
# Method:
#   1. BA+ share of an EP occupation = bachelor's + master's + doctoral/professional %.
#   2. Each 2018 Census code in a unit is matched to EP occupations by SOC code, as
#      01_build_panel.R matches exposure scores: exact code, else the prefix of an aggregated
#      code (15-12XX -> 15-12*, 11-2030 -> 11-203*), else the NEM broad line item that contains a
#      detailed code (13-1022 -> 13-1020). Matched EP occupations are averaged
#      (unweighted: Table 5.3 has no employment column).
#   3. Unit BA+ share = Census codes' shares weighted by their 2022 Table 11 employment.
#   4. Units are split at the median BA+ share across the 309-unit analysis sample:
#      college = above the median, non-college = at or below it.
#   5. Within each half: group growth pre (2015-19) and post (2022-25) for HEF and HEM, and the
#      F - M gap from 02's regression (high-exposure units of that half, dg on gender,
#      2022 weights, HC1 SEs). Groups, cutoff (computed on the full sample) as in 02.
#   6. A cell with fewer than 5 occupations is flagged too small to interpret; a gap is flagged
#      if either its HEF or HEM cell is.
# Run after 05. Writes output/education_shares.csv, output/education_split.csv and adds rows to
# output/numbers.csv.

suppressMessages({ library(readxl); library(sandwich); library(lmtest) })
MIN_N <- 5

# ---- 1. Read Table 5.3 ------------------------------------------------------------------------
f <- list.files("data/raw/bls", pattern = "(5[._]?3|education).*\\.xlsx?$", ignore.case = TRUE, full.names = TRUE)
if (length(f) != 1) stop("Expected one Table 5.3 xlsx in data/raw/bls/ (name containing '5.3' or 'education'); found ",
                         length(f), ": ", paste(f, collapse = ", "))
sh <- excel_sheets(f); sh <- if (length(sh) > 1 && any(grepl("5\\.3", sh))) sh[grepl("5\\.3", sh)][1] else sh[1]
X <- suppressMessages(read_excel(f, sheet = sh, col_names = FALSE, col_types = "text"))
is_soc <- function(v) grepl("^[0-9]{2}-[0-9]{4}$", trimws(v))
code_col <- which.max(sapply(X, function(v) sum(is_soc(v), na.rm = TRUE)))
first <- which(is_soc(X[[code_col]]))[1]
if (is.na(first)) stop("No SOC codes (NN-NNNN) found in ", f)
hdr <- sapply(X[seq_len(first - 1), ], function(v) paste(na.omit(v), collapse = " "))
ba_cols <- which(grepl("bachelor|master|doctoral|professional degree", hdr, ignore.case = TRUE))
if (length(ba_cols) != 3) stop("Expected 3 BA+ columns (bachelor's, master's, doctoral/professional); found: ",
                               paste(sprintf("[%d] %s", ba_cols, hdr[ba_cols]), collapse = "; "))
cat("Table 5.3:", basename(f), "| sheet:", sh, "| BA+ columns:", paste(hdr[ba_cols], collapse = " / "), "\n")

E <- X[is_soc(X[[code_col]]), ]
ep <- data.frame(soc = trimws(E[[code_col]]),
                 ba = rowSums(sapply(E[, ba_cols], function(v) as.numeric(gsub("[^0-9.]", "", v)))))
type_col <- which(grepl("occupation type", hdr, ignore.case = TRUE))
# Table 5.3 rows are NEM line items except the 00-0000 total. Some line items are broad SOC codes
# ending in 0 (e.g. 13-1020 buyers, 31-1120 home health aides), so they are kept.
keep <- if (length(type_col) == 1) tolower(trimws(E[[type_col]])) == "line item" else ep$soc != "00-0000"
ep <- ep[keep, ]
ep <- ep[!is.na(ep$ba) & !duplicated(ep$soc), ]
stopifnot(nrow(ep) > 300, all(ep$ba >= 0 & ep$ba <= 100.5))

# ---- 2. Census code -> BA+ share ----------------------------------------------------------------
s18 <- suppressMessages(read_excel("data/raw/2018-occupation-code-list-and-crosswalk.xlsx",
                                   sheet = "2018 Census Occ Code List", col_names = FALSE, skip = 8))
names(s18) <- c("x", "t18", "c18", "soc18")
s18 <- s18[!is.na(s18$c18) & grepl("^[0-9]{4}$", s18$c18), ]
soc_match <- function(code) {                       # 01_build_panel.R's rule, plus a broad fallback
  code <- trimws(code)
  if (code %in% ep$soc) return(which(ep$soc == code))
  pat <- sub("X+$", "", code)
  if (pat == code) pat <- sub("0+$", "", code)
  k <- which(startsWith(ep$soc, pat))
  if (!length(k)) k <- which(ep$soc == paste0(substr(code, 1, 6), "0"))   # detailed -> NEM broad line item
  k
}
s18$ba <- sapply(s18$soc18, function(s) { k <- soc_match(s); if (length(k)) mean(ep$ba[k]) else NA })

# 2022 employment of each 2018-code Table 11 row (same reading as 01 / 03)
x25 <- suppressMessages(read_excel("data/raw/bls/cpsaat11_2025.xlsx", col_names = FALSE))
t25 <- trimws(gsub("[[:space:]]+", " ", x25[[1]]))
k <- seq(which(t25 == "Total, 16 years and over"), nrow(x25)); k <- k[!is.na(t25[k]) & !is.na(x25[[2]][k])]
b <- read.delim("data/raw/bls/t11_2018codes_2022_2024.tsv", header = FALSE, quote = "",
                col.names = c("e2022", "w2022", "e2024"), na.strings = "")
stopifnot(nrow(b) == length(k)); b$title <- t25[k]

m <- read.csv("data/derived/unit_members.csv"); m <- m[m$scheme == 2018, ]
m$c18 <- sprintf("%04d", as.integer(m$code))
m$ba <- s18$ba[match(m$c18, s18$c18)]
m$e2022 <- b$e2022[match(m$title, b$title)]
stopifnot(!anyNA(m$e2022))

U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
U$ba_share <- sapply(U$unit, function(u) {
  d <- m[m$unit == u & !is.na(m$ba), ]
  if (!nrow(d)) NA else if (sum(d$e2022) > 0) sum(d$ba * d$e2022) / sum(d$e2022) else mean(d$ba)
})
U$ba_cov <- sapply(U$unit, function(u) { d <- m[m$unit == u, ]; if (sum(d$e2022) == 0) 0 else sum(d$e2022[!is.na(d$ba)]) / sum(d$e2022) })
n_unmatched <- sum(is.na(U$ba_share))
if (n_unmatched) cat("Units with no Table 5.3 match (dropped from the split):", n_unmatched, "\n",
                     paste(" -", head(U$title[is.na(U$ba_share)], 20), collapse = "\n"), "\n")

# ---- 3. Groups and split ------------------------------------------------------------------------
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")),
                   levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)   # full-sample cutoff, as in 02
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$dg <- cagr(U$e2025, U$e2022, 3) - cagr(U$e2019, U$e2015, 4)
med <- median(U$ba_share, na.rm = TRUE)
U$edu <- ifelse(is.na(U$ba_share), NA, ifelse(U$ba_share > med, "college", "noncollege"))
cat(sprintf("Median BA+ share across %d matched units: %.1f%%\n", sum(!is.na(U$ba_share)), med))

# ---- 4. Cells and gaps --------------------------------------------------------------------------
gap <- function(d) {
  if (sum(d$gender == "F") == 0 || sum(d$gender == "M") == 0) return(c(gap = NA, se = NA, p = NA, lo = NA, hi = NA))
  # F as the base level: the F - M gap is minus the M coefficient. Identical to 02's contrast
  # (same fitted model, reparameterized), and still works if a half has no N units.
  d$gender <- relevel(droplevels(d$gender), ref = "F")
  mod <- lm(dg ~ gender, data = d, weights = e2022)
  V <- vcovHC(mod, type = "HC1")
  est <- -coef(mod)[["genderM"]]; se <- sqrt(V["genderM", "genderM"]); df <- mod$df.residual
  c(gap = est, se = se, p = 2 * pt(-abs(est / se), df),
    lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se)
}
ge <- read.csv("output/gap_estimates.csv", row.names = 1)    # gap() must reproduce 02's baseline
stopifnot(abs(gap(U[U$high, ])[["gap"]] - ge["baseline", "gap"]) < 1e-8,
          abs(gap(U[U$high, ])[["se"]]  - ge["baseline", "se"])  < 1e-8)
cells <- do.call(rbind, lapply(c("noncollege", "college"), function(e) do.call(rbind, lapply(c("F", "M"), function(g) {
  d <- U[!is.na(U$edu) & U$edu == e & U$high & U$gender == g, ]
  data.frame(education = e, group = ifelse(g == "F", "HEF", "HEM"), n_occ = nrow(d),
             emp2022_k = sum(d$e2022), mean_ba_share = if (nrow(d)) round(weighted.mean(d$ba_share, d$e2022), 1) else NA,
             growth_pre  = if (nrow(d)) cagr(sum(d$e2019), sum(d$e2015), 4) else NA,
             growth_post = if (nrow(d)) cagr(sum(d$e2025), sum(d$e2022), 3) else NA,
             too_small = nrow(d) < MIN_N)
}))))
gaps <- do.call(rbind, lapply(c("noncollege", "college"), function(e) {
  d <- U[!is.na(U$edu) & U$edu == e & U$high, ]
  r <- gap(d); nF <- sum(d$gender == "F"); nM <- sum(d$gender == "M")
  data.frame(education = e, t(r), nF = nF, nM = nM, too_small = nF < MIN_N || nM < MIN_N)
}))

cat("\nHigh-exposure cells (growth = group CAGR, % per year):\n")
print(transform(cells, growth_pre = round(growth_pre, 2), growth_post = round(growth_post, 2)), row.names = FALSE)
cat("\nF - M gap by education (pp/yr, 2022 weights, HC1):\n")
for (i in seq_len(nrow(gaps))) with(gaps[i, ], cat(sprintf(
  "%-11s n F=%d, M=%d | gap %+.2f (SE %.2f, p = %.3f, 95%% CI %+.2f to %+.2f)%s\n",
  education, nF, nM, gap, se, p, lo, hi, if (too_small) "  [TOO SMALL TO INTERPRET: a cell has < 5 occupations]" else "")))

write.csv(U[, c("unit", "title", "gender", "high", "ba_share", "ba_cov", "edu")], "output/education_shares.csv", row.names = FALSE)
write.csv(merge(cells, gaps[, c("education", "gap", "se", "p", "lo", "hi")], by = "education"),
          "output/education_split.csv", row.names = FALSE)

# ---- 5. Add to numbers.csv ----------------------------------------------------------------------
S <- "R/09_education_split.R"; src <- "output/education_split.csv"
f2 <- function(v, d = 2, sign = TRUE) ifelse(is.na(v), "NA", formatC(v, format = "f", digits = d, flag = if (sign) "+" else ""))
row <- function(name, value, display, unit, description, source = src)
  data.frame(name = name, value = signif(unname(as.numeric(value)), 8), display = display, unit = unit,
             script = S, source = source, description = description)
ab <- c(noncollege = "nc", college = "col")
new <- rbind(
  row("edu_median_ba_share", med, sprintf("%.1f", med), "pct",
      "Median share with a bachelor's degree or more across matched sample units (split point)", "output/education_shares.csv"),
  row("edu_n_unmatched", n_unmatched, as.character(n_unmatched), "count",
      "Sample units with no Table 5.3 match (excluded from the split)", "output/education_shares.csv"),
  do.call(rbind, lapply(seq_len(nrow(cells)), function(i) with(cells[i, ], {
    pre <- sprintf("edu_%s_%s", ab[[education]], group); w <- sprintf("%s units, %s half", group, education)
    rbind(row(paste0(pre, "_n"), n_occ, as.character(n_occ), "count", paste(w, ": occupations")),
          row(paste0(pre, "_emp2022"), emp2022_k, format(emp2022_k, big.mark = ","), "k", paste(w, ": 2022 employment (thousands)")),
          row(paste0(pre, "_growth_pre"), growth_pre, f2(growth_pre), "pct_per_yr", paste(w, ": employment CAGR 2015-19")),
          row(paste0(pre, "_growth_post"), growth_post, f2(growth_post), "pct_per_yr", paste(w, ": employment CAGR 2022-25")),
          row(paste0(pre, "_too_small"), as.numeric(too_small), ifelse(too_small, "TOO SMALL", "ok"), "flag",
              paste(w, ": fewer than 5 occupations (1 = too small to interpret)")))
  }))),
  do.call(rbind, lapply(seq_len(nrow(gaps)), function(i) with(gaps[i, ], {
    pre <- sprintf("edu_%s", ab[[education]]); w <- sprintf("F - M gap, %s half", education)
    rbind(row(paste0(pre, c("_gap", "_se", "_p", "_ci_lo", "_ci_hi")), c(gap, se, p, lo, hi),
              c(f2(gap), f2(se, sign = FALSE), f2(p, 3, FALSE), f2(lo), f2(hi)),
              c("pp_per_yr", "pp_per_yr", "p", "pp_per_yr", "pp_per_yr"),
              paste(w, c(": estimate", ": HC1 standard error", ": p-value", ": 95% CI lower bound", ": 95% CI upper bound"))),
          row(paste0(pre, "_gap_too_small"), as.numeric(too_small), ifelse(too_small, "TOO SMALL", "ok"), "flag",
              paste(w, ": HEF or HEM cell has fewer than 5 occupations (1 = too small to interpret)")))
  }))))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
