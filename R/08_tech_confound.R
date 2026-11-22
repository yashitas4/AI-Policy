# 08_tech_confound.R
# Check 5: tech-slowdown confound. Pass standard written in README.md (commit 75ae211) before
# this script was written or run.
#
# Most HEM units are computer or engineering jobs, so the F - M gap may track the tech cycle.
#   (1) Main gap with HEM excluding SOC 15 (Computer and mathematical) units and Computer and
#       information systems managers (SOC 11-3021).
#   (2) Main gap with HEF compared only with HEM units in SOC 17 (Architecture and engineering)
#       and SOC 41 (Sales and related).
#   HEF and non-dominated units are unchanged; gender groups, high-exposure cutoff (computed on the
#   full sample), 2022 weights and HC1 SEs are as in 02. Only the F - M contrast is reported.
#   Pass: gap's sign does not flip from the baseline (+2.45), i.e. gap > 0 in both.
# Also: HEF and HEM units by SOC major group with shares of 2022 employment.
# SOC major group from each unit's 2018 SOC codes, as in 04.
# Run after 05. Writes output/tech_confound.csv, output/hef_hem_by_major_group.csv and adds
# its rows to output/numbers.csv.

suppressMessages({ library(readxl); library(sandwich); library(lmtest) })

# ---- Units, groups, SOC codes ------------------------------------------------------------
s18 <- suppressMessages(read_excel("data/raw/2018-occupation-code-list-and-crosswalk.xlsx",
                                   sheet = "2018 Census Occ Code List", col_names = FALSE, skip = 8))
names(s18) <- c("x", "t18", "c18", "soc18")
s18 <- s18[!is.na(s18$c18) & grepl("^[0-9]{4}$", s18$c18), ]
m <- read.csv("data/derived/unit_members.csv"); m <- m[m$scheme == 2018, ]
m$soc <- trimws(s18$soc18[match(sprintf("%04d", as.integer(m$code)), s18$c18)])
stopifnot(!anyNA(m$soc))
socs <- tapply(m$soc, m$unit, function(v) paste(sort(unique(v)), collapse = ";"))
mg   <- tapply(substr(m$soc, 1, 2), m$unit, function(v) paste(sort(unique(v)), collapse = "/"))

U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")),
                   levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
U$major <- mg[U$unit]; U$socs <- socs[U$unit]
stopifnot(!grepl("/", U$major))
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$dg <- cagr(U$e2025, U$e2022, 3) - cagr(U$e2019, U$e2015, 4)
H <- U[U$high, ]

# Major groups must agree with 04's listing
occ <- read.csv("output/hef_hem_occupations.csv")
L <- H[H$gender %in% c("F", "M"), ]
stopifnot(nrow(L) == nrow(occ),
          all(sort(paste(L$title, L$major)) == sort(paste(occ$occupation, sprintf("%02d", occ$soc_major)))))

is_cis_mgr <- grepl("(^|;)11-3021", H$socs)
stopifnot(sum(is_cis_mgr & H$gender == "M") == 1)

# ---- Estimates ----------------------------------------------------------------------------
gap <- function(d) {
  mod <- lm(dg ~ gender, data = d, weights = e2022)
  V <- vcovHC(mod, type = "HC1"); Lc <- c(0, 1, -1)
  est <- sum(Lc * coef(mod)); se <- sqrt(drop(t(Lc) %*% V %*% Lc)); df <- mod$df.residual
  c(gap = est, se = se, p = 2 * pt(-abs(est / se), df),
    lo = est - qt(0.975, df) * se, hi = est + qt(0.975, df) * se,
    nF = sum(d$gender == "F"), nM = sum(d$gender == "M"),
    empF = sum(d$e2022[d$gender == "F"]), empM = sum(d$e2022[d$gender == "M"]))
}
isM <- H$gender == "M"
base <- gap(H)
ge <- read.csv("output/gap_estimates.csv", row.names = 1)
stopifnot(abs(base["gap"] - ge["baseline", "gap"]) < 1e-8)

r1 <- gap(H[!(isM & (H$major == "15" | is_cis_mgr)), ])
r2 <- gap(H[!isM | H$major %in% c("17", "41"), ])
pass <- r1["gap"] > 0 && r2["gap"] > 0

fmt <- function(lab, r) sprintf("%-44s n F=%d, M=%d | gap %+.2f pp/yr (SE %.2f, p = %.3f, 95%% CI %+.2f to %+.2f)",
                                lab, r["nF"], r["nM"], r["gap"], r["se"], r["p"], r["lo"], r["hi"])
cat(fmt("Baseline:", base), "\n")
cat(fmt("(1) HEM without SOC 15 and CIS managers:", r1), "\n")
cat(fmt("(2) HEM = engineering (17) + sales (41) only:", r2), "\n")
cat(sprintf("Pass standard: sign does not flip (gap > 0 in both) -> %s\n\n", ifelse(pass, "PASS", "FAIL")))
cat("HEM units kept in (1):\n")
print(data.frame(major = H$major, unit = substr(H$title, 1, 60), emp2022_k = H$e2022)[
  isM & !(H$major == "15" | is_cis_mgr), ], row.names = FALSE)

R <- rbind(baseline = base, excl_soc15_cismgr = r1, eng_sales_only = r2)
write.csv(R, "output/tech_confound.csv")

# ---- Major-group table -------------------------------------------------------------------
major_names <- c("11" = "Management", "13" = "Business and financial operations",
  "15" = "Computer and mathematical", "17" = "Architecture and engineering",
  "19" = "Life, physical, and social science", "21" = "Community and social service",
  "23" = "Legal", "25" = "Educational instruction and library",
  "27" = "Arts, design, entertainment, sports, and media",
  "29" = "Healthcare practitioners and technical", "31" = "Healthcare support",
  "33" = "Protective service", "35" = "Food preparation and serving related",
  "37" = "Building and grounds cleaning and maintenance", "39" = "Personal care and service",
  "41" = "Sales and related", "43" = "Office and administrative support",
  "45" = "Farming, fishing, and forestry", "47" = "Construction and extraction",
  "49" = "Installation, maintenance, and repair", "51" = "Production",
  "53" = "Transportation and material moving")
T <- do.call(rbind, lapply(c("F", "M"), function(g) {
  d <- L[L$gender == g, ]
  a <- aggregate(cbind(n_units = 1, emp2022_k = e2022) ~ major, data = d, FUN = sum)
  a$share_2022_pct <- round(100 * a$emp2022_k / sum(d$e2022), 1)
  data.frame(group = ifelse(g == "F", "HEF", "HEM"), soc_major = a$major,
             major_group = major_names[a$major], a[, -1], row.names = NULL)[order(-a$emp2022_k), ]
}))
rownames(T) <- NULL
cat("\nHEF and HEM by SOC major group (2022 employment):\n"); print(T, row.names = FALSE)
write.csv(T, "output/hef_hem_by_major_group.csv", row.names = FALSE)

# ---- Add to numbers.csv -------------------------------------------------------------------
S <- "R/08_tech_confound.R"; src <- "output/tech_confound.csv"; tsrc <- "output/hef_hem_by_major_group.csv"
f2 <- function(v, d = 2, sign = TRUE) formatC(v, format = "f", digits = d, flag = if (sign) "+" else "")
row <- function(name, value, display, unit, source, description)
  data.frame(name = name, value = signif(unname(value), 8), display = display, unit = unit,
             script = S, source = source, description = description)
est_rows <- function(p, r, w) rbind(
  row(paste0(p, c("_gap", "_se", "_p", "_ci_lo", "_ci_hi")), r[c("gap", "se", "p", "lo", "hi")],
      c(f2(r["gap"]), f2(r["se"], sign = FALSE), f2(r["p"], 3, FALSE), f2(r["lo"]), f2(r["hi"])),
      c("pp_per_yr", "pp_per_yr", "p", "pp_per_yr", "pp_per_yr"), src,
      paste(w, c(": F - M gap", ": HC1 standard error", ": p-value", ": 95% CI lower bound", ": 95% CI upper bound"))),
  row(paste0(p, c("_nF", "_nM", "_empM_2022")), r[c("nF", "nM", "empM")],
      c(as.character(unname(r[c("nF", "nM")])), format(unname(r["empM"]), big.mark = ",")),
      c("count", "count", "k"), src,
      paste(w, c(": female-dominated units", ": male-dominated units", ": HEM 2022 employment (thousands)"))))
tab_rows <- do.call(rbind, lapply(seq_len(nrow(T)), function(i) {
  p <- sprintf("majgrp_%s_%s", T$group[i], T$soc_major[i]); w <- sprintf("%s, SOC %s %s", T$group[i], T$soc_major[i], T$major_group[i])
  row(paste0(p, c("_n", "_emp2022", "_share2022")), c(T$n_units[i], T$emp2022_k[i], T$share_2022_pct[i]),
      c(as.character(T$n_units[i]), format(T$emp2022_k[i], big.mark = ","), sprintf("%.1f", T$share_2022_pct[i])),
      c("count", "k", "pct"), tsrc,
      paste(w, c(": units", ": 2022 employment (thousands)", ": share of group's 2022 employment")))
}))
new <- rbind(est_rows("check5_excl15", r1, "Check 5 (1): HEM without SOC 15 and computer and information systems managers"),
             est_rows("check5_engsales", r2, "Check 5 (2): HEM limited to engineering (17) and sales (41)"),
             row("check5_pass", as.numeric(pass), ifelse(pass, "PASS", "FAIL"), "flag", src,
                 "Check 5 pass standard: gap sign does not flip (> 0) in both re-estimates"),
             tab_rows)
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
