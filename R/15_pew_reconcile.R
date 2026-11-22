# 15_pew_reconcile.R
# Reconciles our result (female-dominated high-exposure occupations did no worse than male-dominated
# ones) with Pew Research Center (Smith, Oct 22 2026), which finds women's employment grew less than
# men's in more AI-exposed occupations.
#
# Pew's method (article + technical appendix):
#   - CPS basic monthly, 2017-19 and Jan 2023 - Jun 2026; 2020-22 omitted.
#   - Exposure: own index from O*NET 30.3 work activities = mean importance (IM) of 16 "high-exposure"
#     activities minus mean importance of 9 "low-exposure" activities; continuous; min set to 0.
#   - Model: log(E) by occupation x sex x year on Exposure x Post, Female x Exposure, Female x Post,
#     Female x Exposure x Post, Female, occupation FE, year FE; weights = pre-2020 occupation
#     employment; SEs clustered by occupation. beta3 (Female x Exposure x Post) is the gender gap.
#   - Headline: -6% per point, -5% per SD (women minus men).
#
# Our data: IPUMS CPS tables from 11_ipums_panel.R (unit x year x age band, with women) and
#   13_ipums_build.R (adds sex x degree), all ages 16+, 2015-2025. Our post period is 2022-25
#   (Pew: 2023 - mid-2026; our tables end in 2025).
#
# Sign convention throughout: women's side minus men's side. Positive = women / female-dominated
#   occupations did better. Pew's headline is negative.
#
# Parts:
#   A. Pew-style slopes: 2022-25 employment growth on exposure, separately for men and women, and
#      the women - men gap; overall and by degree (BA+ / no BA); Eloundou and Pew-rebuilt exposure.
#   B. The three choices, each switched on and off (2 x 2 x 2 = 8 specifications):
#        unit:     occupation groups (HEF vs HEM, top-25% exposure)  vs  workers (women vs men within
#                  each occupation, slope on continuous exposure)
#        exposure: Eloundou GPT-4 beta  vs  Pew-rebuilt O*NET index
#        pre:      none (2022-25 growth)  vs  minus 2015-19 growth
#   C. Pew's exact model on our data (2017-19 vs 2023-25), both exposure measures.
# Writes data/derived/pew_exposure_units.csv, output/pew_reconcile.txt, adds pew_* rows to numbers.csv.

suppressMessages({ library(readxl); library(sandwich); library(lmtest) })
out <- character(); say <- function(fmt = "", ...) { s <- sprintf(fmt, ...); cat(s, "\n"); out <<- c(out, s) }

U <- read.csv("data/derived/panel_units.csv"); U <- U[U$in_sample, ]
U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))

# ---- 1. Rebuild Pew's exposure index ------------------------------------------------------------
hi_ids <- c("4.A.1.a.1", "4.A.2.a.4", "4.A.4.b.3", "4.A.4.a.3", "4.A.4.b.6", "4.A.2.a.3", "4.A.2.b.2", "4.A.3.b.1",
            "4.A.2.b.1", "4.A.3.b.6", "4.A.2.b.4", "4.A.4.a.2", "4.A.4.a.1", "4.A.2.b.3", "4.A.2.b.6", "4.A.2.a.2")
lo_ids <- c("4.A.3.b.4", "4.A.1.b.2", "4.A.3.a.2", "4.A.4.a.5", "4.A.3.a.4", "4.A.3.a.1", "4.A.3.b.5", "4.A.4.a.8", "4.A.3.a.3")
wa <- read.delim("data/raw/onet/Work Activities.txt", quote = "", check.names = FALSE)
wa <- wa[wa[["Scale ID"]] == "IM", ]
stopifnot(all(c(hi_ids, lo_ids) %in% wa[["Element ID"]]))
sc <- function(ids) tapply(wa[["Data Value"]][wa[["Element ID"]] %in% ids], wa[["O*NET-SOC Code"]][wa[["Element ID"]] %in% ids], mean)
h <- sc(hi_ids); l <- sc(lo_ids); k <- intersect(names(h), names(l))
on <- data.frame(onet = k, pew = as.numeric(h[k] - l[k]))
on$soc <- substr(on$onet, 1, 7)
on6 <- aggregate(pew ~ soc, data = on, FUN = mean)        # O*NET-SOC detail -> 6-digit SOC (unweighted)

s18 <- suppressMessages(read_excel("data/raw/2018-occupation-code-list-and-crosswalk.xlsx",
                                   sheet = "2018 Census Occ Code List", col_names = FALSE, skip = 8))
names(s18) <- c("x", "t18", "c18", "soc18")
s18 <- s18[!is.na(s18$c18) & grepl("^[0-9]{4}$", s18$c18), ]
soc_match <- function(code) {                               # 01_build_panel.R's rule + broad fallback (as 09)
  code <- trimws(code)
  if (code %in% on6$soc) return(which(on6$soc == code))
  pat <- sub("X+$", "", code); if (pat == code) pat <- sub("0+$", "", code)
  k <- which(startsWith(on6$soc, pat))
  if (!length(k)) k <- which(startsWith(on6$soc, substr(code, 1, 6)))
  k
}
s18$pew <- sapply(s18$soc18, function(s) { k <- soc_match(s); if (length(k)) mean(on6$pew[k]) else NA })

x25 <- suppressMessages(read_excel("data/raw/bls/cpsaat11_2025.xlsx", col_names = FALSE))
t25 <- trimws(gsub("[[:space:]]+", " ", x25[[1]]))
kk <- seq(which(t25 == "Total, 16 years and over"), nrow(x25)); kk <- kk[!is.na(t25[kk]) & !is.na(x25[[2]][kk])]
b <- read.delim("data/raw/bls/t11_2018codes_2022_2024.tsv", header = FALSE, quote = "", col.names = c("e2022", "w2022", "e2024"))
b$title <- t25[kk]
m <- read.csv("data/derived/unit_members.csv"); m <- m[m$scheme == 2018 & m$unit %in% U$unit, ]
m$pew <- s18$pew[match(sprintf("%04d", as.integer(m$code)), s18$c18)]
m$e2022 <- b$e2022[match(m$title, b$title)]
U$pew <- sapply(U$unit, function(u) { d <- m[m$unit == u & !is.na(m$pew), ]
  if (!nrow(d)) NA else if (sum(d$e2022) > 0) sum(d$pew * d$e2022) / sum(d$e2022) else mean(d$pew) })
n_pew_na <- sum(is.na(U$pew))
U$pew[is.na(U$pew)] <- median(U$pew, na.rm = TRUE)        # Pew: occupations with no score get the median
U$pew <- U$pew - min(U$pew)                                # Pew: least exposed = 0
U$eloundou <- U$beta_gpt4
write.csv(U[, c("unit", "title", "gender", "eloundou", "pew")], "data/derived/pew_exposure_units.csv", row.names = FALSE)

# ---- 2. Employment by unit x sex x year (IPUMS, ages 16+) -------------------------------------------
D11 <- read.csv("data/derived/ipums_unit_age_year.csv")
tot <- aggregate(cbind(emp_k, emp_women_k) ~ unit + year, data = D11, FUN = sum)
Wm <- tapply(tot$emp_women_k, list(tot$unit, tot$year), sum)[U$unit, ]
Tm <- tapply(tot$emp_k, list(tot$unit, tot$year), sum)[U$unit, ]
Mm <- Tm - Wm
D13 <- read.csv("data/derived/ipums_unit_year_sex_deg_age.csv")
by_deg <- function(deg, sx) {
  d <- D13[D13$degree %in% deg & D13$sex == sx, ]
  w <- tapply(d$emp_k, list(d$unit, d$year), sum); out <- matrix(0, nrow(U), 11, dimnames = list(U$unit, 2015:2025))
  r <- intersect(U$unit, rownames(w)); out[r, colnames(w)] <- w[r, ]; out[is.na(out)] <- 0; out
}
stopifnot(max(abs(by_deg(c("BA+", "noBA", "unknown"), "F") - Wm)) < 1e-6)   # 13's table agrees with 11's
U$w_pre <- rowMeans(Tm[, c("2017", "2018", "2019")])                    # Pew weight: pre-2020 employment

z <- function(v) (v - mean(v)) / sd(v)
U$elo_z <- z(U$eloundou); U$pew_z <- z(U$pew)
say("Pew exposure index rebuilt from O*NET 30.3 (%d occupations); %d of %d units had no match and got the median.",
    nrow(on), n_pew_na, nrow(U))
say("Correlation across the 309 units: Eloundou GPT-4 beta vs Pew-rebuilt index r = %.2f (employment-weighted r = %.2f).",
    cor(U$eloundou, U$pew), cov.wt(cbind(U$eloundou, U$pew), wt = U$w_pre, cor = TRUE)$cor[1, 2])

# ---- A. Pew-style slopes: 2022-25 growth on exposure, men and women --------------------------------------
# g = 100 * annualized log growth 2022-25; slope per 1 SD of exposure, weighted by pre-2020 employment, HC1.
lg <- function(e1, e0, yrs) 100 * log(e1 / e0) / yrs
slope <- function(y, x, w) {
  ok <- is.finite(y) & w > 0
  f <- lm(y[ok] ~ x[ok], weights = w[ok]); ct <- coeftest(f, vcov. = vcovHC(f, type = "HC1"))
  c(b = ct[2, 1], se = ct[2, 2], p = ct[2, 4], n = sum(ok))
}
partA <- function(Wt, Mt, lab) {
  gw <- lg(Wt[, "2025"], Wt[, "2022"], 3); gm <- lg(Mt[, "2025"], Mt[, "2022"], 3)
  rows <- list()
  for (ex in c("elo_z", "pew_z")) {
    x <- U[[ex]]
    rows[[length(rows) + 1]] <- data.frame(sample = lab, exposure = ifelse(ex == "elo_z", "Eloundou", "Pew-rebuilt"),
      men = slope(gm, x, U$w_pre)[["b"]], women = slope(gw, x, U$w_pre)[["b"]],
      t(slope(gw - gm, x, U$w_pre)), row.names = NULL)
  }
  do.call(rbind, rows)
}
A <- rbind(partA(Wm, Mm, "All 16+"),
           partA(by_deg("BA+", "F"), by_deg("BA+", "M"), "BA+"),
           partA(by_deg("noBA", "F"), by_deg("noBA", "M"), "No BA"))
say("")
say("A. PEW-STYLE SLOPES: annualized employment growth 2022-25 (pp/yr) per 1 SD of exposure")
say("   weighted by pre-2020 (2017-19) employment, HC1 SEs. gap = women - men (Pew's sign).")
say("   %-8s %-12s %8s %8s %8s %6s %7s %4s", "sample", "exposure", "men", "women", "gap", "SE", "p", "n")
for (i in seq_len(nrow(A))) with(A[i, ], say("   %-8s %-12s %+8.2f %+8.2f %+8.2f %6.2f %7.3f %4d", sample, exposure, men, women, b, se, p, n))

# ---- B. Eight specifications ---------------------------------------------------------------------------
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
Uc <- U; Uc$g_post <- cagr(Tm[, "2025"], Tm[, "2022"], 3); Uc$g_pre <- cagr(Tm[, "2019"], Tm[, "2015"], 4)
Uc$e2022 <- Tm[, "2022"]; Uc$gf <- factor(Uc$gender, levels = c("N", "F", "M"))
groups_spec <- function(ex, pre) {          # ours: HEF vs HEM among top-25% units on exposure `ex`
  d <- Uc[Uc[[ex]] >= quantile(Uc[[ex]], 0.75), ]
  d$y <- d$g_post - if (pre) d$g_pre else 0
  f <- lm(y ~ gf, data = d, weights = e2022); V <- vcovHC(f, type = "HC1"); L <- c(0, 1, -1)
  est <- sum(L * coef(f)); se <- sqrt(drop(t(L) %*% V %*% L))
  c(est = est, se = se, p = 2 * pt(-abs(est / se), f$df.residual), nF = sum(d$gender == "F"), nM = sum(d$gender == "M"))
}
workers_spec <- function(exz, pre) {        # Pew-style: (women - men growth) within occupation, slope per SD
  y <- lg(Wm[, "2025"], Wm[, "2022"], 3) - lg(Mm[, "2025"], Mm[, "2022"], 3)
  if (pre) y <- y - (lg(Wm[, "2019"], Wm[, "2015"], 4) - lg(Mm[, "2019"], Mm[, "2015"], 4))
  s <- slope(y, U[[exz]], U$w_pre); c(est = s[["b"]], se = s[["se"]], p = s[["p"]], nF = s[["n"]], nM = NA)
}
G <- expand.grid(unit = c("Occupation groups", "Workers"), exposure = c("Eloundou", "Pew-rebuilt"),
                 pre = c(TRUE, FALSE), stringsAsFactors = FALSE)
G <- cbind(G, t(mapply(function(u, e, p) {
  if (u == "Occupation groups") groups_spec(ifelse(e == "Eloundou", "eloundou", "pew"), p)
  else workers_spec(ifelse(e == "Eloundou", "elo_z", "pew_z"), p)
}, G$unit, G$exposure, G$pre)))
base <- read.csv("output/numbers.csv"); ref <- base$value[base$name == "ageall_gap"]
stopifnot(abs(G$est[1] - ref) < 1e-5)       # spec 1 = our IPUMS all-ages headline (12_age_gap.R)
G$id <- seq_len(nrow(G))
say("")
say("B. THE THREE CHOICES, EACH SWITCHED (women's side - men's side; + = women did better)")
say("   Occupation groups: HEF - HEM gap among top-25%% units on that exposure, pp/yr, 2022 weights.")
say("   Workers: slope of (women's - men's annualized growth within each occupation) per 1 SD exposure, pp/yr,")
say("            pre-2020 weights. Pre-period: subtract the same quantity for 2015-19.")
say("   %2s  %-18s %-12s %-10s %8s %6s %7s  %s", "#", "unit", "exposure", "pre-period", "estimate", "SE", "p", "n")
for (i in seq_len(nrow(G))) with(G[i, ], say("   %2d  %-18s %-12s %-10s %+8.2f %6.2f %7.3f  %s%s", id, unit, exposure,
  ifelse(pre, "2015-19", "none"), est, se, p, if (is.na(nM)) sprintf("%d occupations", nF) else sprintf("F=%d M=%d", nF, nM),
  if (id == 1) "   <- our headline (IPUMS)" else if (unit == "Workers" && exposure == "Pew-rebuilt" && !pre) "   <- Pew-style growth slope (Pew's own model: Part C)" else ""))

# One-at-a-time switches from our headline, and from Pew-style
sw <- function(from, what) {
  r <- G[from, ]; t <- G
  t <- t[t$unit == (if (what == "unit") setdiff(unique(G$unit), r$unit) else r$unit) &
         t$exposure == (if (what == "exposure") setdiff(unique(G$exposure), r$exposure) else r$exposure) &
         t$pre == (if (what == "pre") !r$pre else r$pre), ]
  t
}
say("")
say("   Switching ONE choice at a time, starting from our headline (#1, %+.2f):", G$est[1])
for (w in c("unit", "exposure", "pre")) { t <- sw(1, w)
  say("     %-9s -> #%d: %+.2f  %s", w, t$id, t$est, ifelse(sign(t$est) != sign(G$est[1]), "SIGN FLIPS", "same sign")) }
pw <- which(G$unit == "Workers" & G$exposure == "Pew-rebuilt" & !G$pre)
say("   Switching ONE choice at a time, starting from Pew-style (#%d, %+.2f):", pw, G$est[pw])
for (w in c("unit", "exposure", "pre")) { t <- sw(pw, w)
  say("     %-9s -> #%d: %+.2f  %s", w, t$id, t$est, ifelse(sign(t$est) != sign(G$est[pw]), "SIGN FLIPS", "same sign")) }
say("   Signs across all 8: %s", paste(sprintf("#%d %s", G$id, ifelse(G$est > 0, "+", "-")), collapse = ", "))

# ---- C. Pew's exact model on our data ----------------------------------------------------------------------
mkP <- function(pre_yrs, post_yrs, units = U$unit) {
  P <- do.call(rbind, lapply(c(pre_yrs, post_yrs), function(y)
    rbind(data.frame(unit = U$unit, year = y, female = 0, E = Mm[, y]), data.frame(unit = U$unit, year = y, female = 1, E = Wm[, y]))))
  P <- merge(P, U[, c("unit", "elo_z", "pew_z", "eloundou", "pew", "w_pre")], by = "unit")
  P$post <- as.numeric(P$year %in% post_yrs)
  P[P$E > 0 & P$unit %in% units, ]
}
P <- mkP(c("2017", "2018", "2019"), c("2023", "2024", "2025"))
pew_model <- function(ex, P) {
  P$x <- P[[ex]]
  f <- lm(log(E) ~ x:post + female:x + female:post + female:x:post + female + factor(unit) + factor(year), data = P, weights = w_pre)
  term <- function(parts) { n <- names(coef(f)); k <- n[sapply(strsplit(n, ":"), function(p) setequal(p, parts))]
                            stopifnot(length(k) == 1); k }
  t3 <- term(c("x", "female", "post")); t0 <- term(c("x", "post"))
  V <- vcovCL(f, cluster = ~unit, type = "HC1"); b <- coef(f)[[t3]]; se <- sqrt(V[t3, t3])
  bm <- coef(f)[[t0]]
  c(gap_pct = 100 * (exp(b) - 1), lo = 100 * (exp(b - 1.96 * se) - 1), hi = 100 * (exp(b + 1.96 * se) - 1),
    p = 2 * pnorm(-abs(b / se)), men_pct = 100 * (exp(bm) - 1), women_pct = 100 * (exp(bm + b) - 1))
}
C <- rbind(cbind(exposure = "Eloundou", per = "1 SD", as.data.frame(t(pew_model("elo_z", P)))),
           cbind(exposure = "Pew-rebuilt", per = "1 SD", as.data.frame(t(pew_model("pew_z", P)))),
           cbind(exposure = "Pew-rebuilt", per = "1 point", as.data.frame(t(pew_model("pew", P)))))
say("")
say("C. PEW'S MODEL ON OUR DATA: log employment by occupation x sex x year, 2017-19 vs 2023-25,")
say("   occupation and year FE, pre-2020 weights, SEs clustered by occupation. %% change, women - men (beta3).")
say("   Pew reports -5%% per SD (-6%% per point) with 2023 - mid-2026 data.")
for (i in seq_len(nrow(C))) with(C[i, ], say("   %-12s per %-7s gap %+6.2f%% [%+6.2f, %+6.2f] p = %.3f | men %+6.2f%%, women %+6.2f%%",
  exposure, per, gap_pct, lo, hi, p, men_pct, women_pct))
say("   Dropped occupation x sex x year cells with zero employment: %d", 6 * 2 * nrow(U) - nrow(P))

# ---- D. Where Pew's gap comes from --------------------------------------------------------------------------
# Pew's 2017-19 is a baseline LEVEL, so its gap includes everything between 2019 and 2023 (COVID and the
# 2010 -> 2018 occupation-code change). Split the comparison at 2022, and drop units whose employment
# changes by > 50% across the code change (Check 4 flag, 07_codechange_robustness.R).
flag <- abs(U$e2022 / U$e2019 - 1) > 0.5                  # Table 11 levels, exactly as 07 flags units
keep <- U$unit[!flag]
Dspecs <- list(
  list("2017-19 -> 2023-25 (Pew's window)", c("2017", "2018", "2019"), c("2023", "2024", "2025"), U$unit),
  list("2017-19 -> 2022 (spans COVID + code change)", c("2017", "2018", "2019"), "2022", U$unit),
  list("2022 -> 2023-25 (post-ChatGPT only)", "2022", c("2023", "2024", "2025"), U$unit),
  list("2017-19 -> 2023-25, code-change units dropped", c("2017", "2018", "2019"), c("2023", "2024", "2025"), keep))
Dres <- do.call(rbind, lapply(Dspecs, function(s) do.call(rbind, lapply(c("elo_z", "pew_z"), function(ex)
  cbind(window = s[[1]], exposure = ifelse(ex == "elo_z", "Eloundou", "Pew-rebuilt"),
        as.data.frame(t(pew_model(ex, mkP(s[[2]], s[[3]], s[[4]])))))))))
say("")
say("D. WHERE PEW'S NEGATIVE GAP COMES FROM: Pew's model, different windows (%% per 1 SD, women - men)")
say("   Pew's 2017-19 is a baseline level, so its gap spans 2020-22 even though those years are omitted.")
for (i in seq_len(nrow(Dres))) with(Dres[i, ], say("   %-46s %-12s %+6.2f%% [%+6.2f, %+6.2f] p = %.3f", window, exposure, gap_pct, lo, hi, p))
say("   Code-change units dropped: %d of %d (Check 4 rule, |2022/2019 - 1| > 0.5).", sum(flag), nrow(U))

dir.create("output", showWarnings = FALSE)
writeLines(c("Reconciling with Pew Research Center (Smith, Oct 22 2026) -- written by R/15_pew_reconcile.R", "", out),
           "output/pew_reconcile.txt")

# ---- numbers.csv --------------------------------------------------------------------------------------------
S <- "R/15_pew_reconcile.R"; src <- "output/pew_reconcile.txt"
f2 <- function(v, d = 2) formatC(v, format = "f", digits = d, flag = "+")
row <- function(name, v, unit, desc, d = 2) data.frame(name = name, value = signif(v, 8), display = f2(v, d), unit = unit,
                                                     script = S, source = src, description = desc)
ab <- c("All 16+" = "all", "BA+" = "ba", "No BA" = "noba"); ex <- c(Eloundou = "elo", "Pew-rebuilt" = "pew")
new <- rbind(
  row("pew_corr_exposure", cor(U$eloundou, U$pew), "r", "Correlation of Eloundou beta and Pew-rebuilt exposure across 309 units"),
  do.call(rbind, lapply(seq_len(nrow(A)), function(i) with(A[i, ], rbind(
    row(sprintf("pewA_%s_%s_men", ab[[sample]], ex[[exposure]]), men, "pp_per_yr_per_sd", sprintf("Men's 2022-25 growth slope per SD %s exposure, %s", exposure, sample)),
    row(sprintf("pewA_%s_%s_women", ab[[sample]], ex[[exposure]]), women, "pp_per_yr_per_sd", sprintf("Women's 2022-25 growth slope per SD %s exposure, %s", exposure, sample)),
    row(sprintf("pewA_%s_%s_gap", ab[[sample]], ex[[exposure]]), b, "pp_per_yr_per_sd", sprintf("Women - men growth slope per SD %s exposure, %s", exposure, sample)),
    row(sprintf("pewA_%s_%s_gap_se", ab[[sample]], ex[[exposure]]), se, "pp_per_yr_per_sd", sprintf("HC1 SE of women - men slope, %s exposure, %s", exposure, sample)))))),
  do.call(rbind, lapply(seq_len(nrow(G)), function(i) with(G[i, ], rbind(
    row(sprintf("pewB_spec%d", id), est, "pp_per_yr", sprintf("Spec %d: %s, %s exposure, pre-period %s (women's side - men's side)", id, unit, exposure, ifelse(pre, "2015-19", "none"))),
    row(sprintf("pewB_spec%d_se", id), se, "pp_per_yr", sprintf("Spec %d: HC1 SE", id)))))),
  do.call(rbind, lapply(seq_len(nrow(C)), function(i) with(C[i, ], {
    tg <- sprintf("pewC_%s_%s", ex[[exposure]], ifelse(per == "1 SD", "sd", "pt"))
    rbind(row(paste0(tg, "_gap"), gap_pct, "pct", sprintf("Pew model on our data: women - men %% per %s %s exposure, 2017-19 vs 2023-25", per, exposure)),
          row(paste0(tg, "_lo"), lo, "pct", "95% CI lower (clustered by occupation)"),
          row(paste0(tg, "_hi"), hi, "pct", "95% CI upper (clustered by occupation)"))
  }))),
  do.call(rbind, lapply(seq_len(nrow(Dres)), function(i) with(Dres[i, ], {
    w <- c("pew", "to2022", "after2022", "nocodechange")[ceiling(i / 2)]
    tg <- sprintf("pewD_%s_%s", w, ex[[exposure]])
    rbind(row(paste0(tg, "_gap"), gap_pct, "pct", sprintf("Pew model, %s, %s exposure: women - men %% per SD", window, exposure)),
          row(paste0(tg, "_lo"), lo, "pct", "95% CI lower (clustered by occupation)"),
          row(paste0(tg, "_hi"), hi, "pct", "95% CI upper (clustered by occupation)"))
  }))),
  row("pewD_n_codechange_dropped", sum(flag), "count", "Units dropped as code-change units (Check 4 rule)", 0))
N <- read.csv("output/numbers.csv")
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Wrote output/pew_reconcile.txt; added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
