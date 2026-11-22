# 14_ipums_education_gap.R
# The main F - M gap, separately for workers with a bachelor's degree or more ("BA+") and without
# ("noBA"), from 13_ipums_build.R's unit x year x sex x degree x age table (all ages 16+).
#
# Same groups, cutoff and weights as 02: HEF/HEM from Table 11 2022 % women (>70 / <30), high
# exposure = top 25% GPT-4 beta over the 309 units (beta >= 0.511). For degree level d:
#   E_d(u, t) = unit u's employment of workers with degree level d in year t (IPUMS, WTFINL)
#   dg_d(u)   = CAGR 2022-25 - CAGR 2015-19 of E_d(u, .)
#   gap_d     = F - M contrast from a regression of dg_d on gender among high-exposure units,
#               weighted by E_d(u, 2022), HC1 SEs (as 02). Units need E_d > 0 at 2015, 2019, 2022, 2025.
#   Interval: 1,000 draws resampling occupations with replacement within HEF and within HEM;
#             the contrast is the weighted mean dg of F minus that of M. 2.5-97.5 percentiles. Seed fixed.
# Survey responses: person-month responses behind each HEF/HEM x degree x endpoint-year cell.
#   A cell under 1,000 responses is flagged; a degree level's gap is flagged if any of its cells is.
# Check: the all-degrees gap from this table must equal ageall_gap in numbers.csv (12_age_gap.R,
#   IPUMS all ages) and is reported next to main_gap (Table 11).
# Run after 13 and 12. Writes output/ipums_education_gap.csv, output/ipums_education_cells.csv and
# adds deg_* rows to output/numbers.csv.

suppressMessages({ library(sandwich); library(lmtest) })
B <- 1000; set.seed(20261122); MIN_RESP <- 1000

D <- read.csv("data/derived/ipums_unit_year_sex_deg_age.csv")
U <- read.csv("data/derived/panel_units.csv"); U <- U[U$in_sample, ]
U$gender <- factor(ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N")), levels = c("N", "F", "M"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
H <- U[U$high, c("unit", "title", "gender")]
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
yrs <- c("2015", "2019", "2022", "2025")

wide <- function(d, v) {
  w <- tapply(d[[v]], list(d$unit, d$year), sum); w[is.na(w)] <- 0
  out <- matrix(0, nrow(H), length(yrs), dimnames = list(H$unit, yrs))
  r <- intersect(H$unit, rownames(w)); out[r, ] <- w[r, yrs]; out
}

one <- function(lab, degs) {
  d <- D[D$degree %in% degs & D$year %in% as.integer(yrs), ]
  E <- wide(d, "emp_k"); R <- wide(d, "n_resp")
  h <- H; h[paste0("e", yrs)] <- E[h$unit, ]
  ok <- rowSums(E[h$unit, ] > 0) == 4
  dropped <- h$title[!ok & h$gender %in% c("F", "M")]
  h <- h[ok, ]
  h$dg <- cagr(h$e2025, h$e2022, 3) - cagr(h$e2019, h$e2015, 4)
  mod <- lm(dg ~ gender, data = h, weights = e2022)
  V <- vcovHC(mod, type = "HC1"); L <- c(0, 1, -1)
  est <- sum(L * coef(mod)); se <- sqrt(drop(t(L) %*% V %*% L)); df <- mod$df.residual
  f <- h[h$gender == "F", ]; m <- h[h$gender == "M", ]
  wm <- function(x) sum(x$dg * x$e2022) / sum(x$e2022)
  stopifnot(abs(est - (wm(f) - wm(m))) < 1e-8)
  bs <- replicate(B, wm(f[sample(nrow(f), replace = TRUE), ]) - wm(m[sample(nrow(m), replace = TRUE), ]))
  grp <- function(x) c(pre = cagr(sum(x$e2019), sum(x$e2015), 4), post = cagr(sum(x$e2025), sum(x$e2022), 3))
  cells <- do.call(rbind, lapply(list(HEF = f, HEM = m), function(x) colSums(R[x$unit, , drop = FALSE])))
  cell_df <- data.frame(degree = lab, group = rownames(cells), cells, check.names = FALSE, row.names = NULL)
  names(cell_df)[3:6] <- paste0("resp_", yrs)
  cell_df$min_resp <- apply(cells, 1, min); cell_df$under_1000 <- cell_df$min_resp < MIN_RESP
  list(row = data.frame(degree = lab, nF = nrow(f), nM = nrow(m),
                        empF_2022_k = sum(f$e2022), empM_2022_k = sum(m$e2022),
                        HEF_growth_pre = grp(f)[["pre"]], HEF_growth_post = grp(f)[["post"]],
                        HEM_growth_pre = grp(m)[["pre"]], HEM_growth_post = grp(m)[["post"]],
                        gap = est, se_hc1 = se, p_hc1 = 2 * pt(-abs(est / se), df),
                        ci_hc1_lo = est - qt(0.975, df) * se, ci_hc1_hi = est + qt(0.975, df) * se,
                        boot_lo = unname(quantile(bs, 0.025)), boot_hi = unname(quantile(bs, 0.975)),
                        min_resp_HEF = cell_df$min_resp[1], min_resp_HEM = cell_df$min_resp[2],
                        flag_under_1000 = any(cell_df$under_1000)),
       cells = cell_df, dropped = dropped)
}
res <- list(one("BA+", "BA+"), one("noBA", "noBA"), one("All", c("BA+", "noBA", "unknown")))
R <- do.call(rbind, lapply(res, `[[`, "row"))
C <- do.call(rbind, lapply(res, `[[`, "cells"))

# ---- Check against numbers.csv --------------------------------------------------------------------
N <- read.csv("output/numbers.csv")
ref <- N$value[N$name == "ageall_gap"]; mainv <- N$value[N$name == "main_gap"]
stopifnot(length(ref) == 1, length(mainv) == 1)
all_gap <- R$gap[R$degree == "All"]
match_ok <- abs(all_gap - ref) < 1e-5
cat(sprintf("Check: all-degrees gap %+.4f vs ageall_gap %+.4f in numbers.csv (IPUMS all ages) -> %s; Table 11 main_gap %+.4f\n",
            all_gap, ref, ifelse(match_ok, "MATCH", "MISMATCH"), mainv))
if (!match_ok) stop("All-degrees gap does not reproduce ageall_gap in numbers.csv.")

cat(sprintf("\nGap by degree, all ages 16+ (pp/yr; 95%% intervals: HC1 and %d-draw occupation resampling)\n", B))
for (i in seq_len(nrow(R))) with(R[i, ], cat(sprintf(
  "%-5s n F=%2d M=%2d | HEF %+5.2f -> %+5.2f, HEM %+5.2f -> %+5.2f | gap %+5.2f (SE %.2f) HC1 [%+5.2f, %+5.2f] resampled [%+5.2f, %+5.2f]%s\n",
  degree, nF, nM, HEF_growth_pre, HEF_growth_post, HEM_growth_pre, HEM_growth_post, gap, se_hc1, ci_hc1_lo, ci_hc1_hi,
  boot_lo, boot_hi, if (flag_under_1000) "  [CELL < 1,000 RESPONSES]" else "")))
cat("\nSurvey responses (person-months) behind each cell:\n"); print(C, row.names = FALSE)
for (i in seq_along(res)) if (length(res[[i]]$dropped))
  cat(sprintf("  %s: dropped (zero employment at an endpoint): %s\n", R$degree[i], paste(res[[i]]$dropped, collapse = "; ")))
write.csv(R, "output/ipums_education_gap.csv", row.names = FALSE)
write.csv(C, "output/ipums_education_cells.csv", row.names = FALSE)

# ---- numbers.csv ----------------------------------------------------------------------------------
S <- "R/14_ipums_education_gap.R"; src <- "output/ipums_education_gap.csv"; csrc <- "output/ipums_education_cells.csv"
f2 <- function(v, d = 2, sign = TRUE) formatC(v, format = "f", digits = d, flag = if (sign) "+" else "")
tag <- c("BA+" = "deg_ba", "noBA" = "deg_noba", "All" = "deg_all")
lab <- c("BA+" = "bachelor's degree or more", "noBA" = "no bachelor's degree", "All" = "all degree levels")
spec <- list(c("nF", "count", "HEF units"), c("nM", "count", "HEM units"),
             c("HEF_growth_pre", "pct_per_yr", "HEF employment CAGR 2015-19"),
             c("HEF_growth_post", "pct_per_yr", "HEF employment CAGR 2022-25"),
             c("HEM_growth_pre", "pct_per_yr", "HEM employment CAGR 2015-19"),
             c("HEM_growth_post", "pct_per_yr", "HEM employment CAGR 2022-25"),
             c("gap", "pp_per_yr", "F - M gap"), c("se_hc1", "pp_per_yr", "HC1 standard error"),
             c("p_hc1", "p", "HC1 p-value"),
             c("ci_hc1_lo", "pp_per_yr", "95% HC1 interval, lower"), c("ci_hc1_hi", "pp_per_yr", "95% HC1 interval, upper"),
             c("boot_lo", "pp_per_yr", "95% occupation-resampling interval, lower"),
             c("boot_hi", "pp_per_yr", "95% occupation-resampling interval, upper"),
             c("flag_under_1000", "flag", "a HEF/HEM endpoint-year cell has < 1,000 survey responses (1 = yes)"))
row <- function(name, v, disp, unit, source, desc)
  data.frame(name = name, value = signif(v, 8), display = disp, unit = unit, script = S, source = source, description = desc)
new <- do.call(rbind, lapply(seq_len(nrow(R)), function(i) do.call(rbind, lapply(spec, function(s) {
  v <- as.numeric(R[i, s[1]])
  disp <- switch(s[2], count = as.character(v), p = f2(v, 3, FALSE), flag = ifelse(v == 1, "UNDER 1,000", "ok"),
                 f2(v, sign = s[1] != "se_hc1"))
  row(paste0(tag[[R$degree[i]]], "_", s[1]), v, disp, s[2], src, sprintf("IPUMS CPS, ages 16+, %s: %s", lab[[R$degree[i]]], s[3]))
}))))
cell_rows <- do.call(rbind, lapply(seq_len(nrow(C)), function(i) do.call(rbind, lapply(yrs, function(y) {
  v <- C[i, paste0("resp_", y)]
  row(sprintf("%s_resp_%s_%s", tag[[C$degree[i]]], C$group[i], y), v, format(v, big.mark = ","), "count", csrc,
      sprintf("IPUMS CPS, %s, %s units: survey responses (person-months) in %s%s", lab[[C$degree[i]]], C$group[i], y,
              if (v < MIN_RESP) " [under 1,000]" else ""))
}))))
new <- rbind(new, cell_rows,
             row("deg_all_matches_ageall", as.numeric(match_ok), ifelse(match_ok, "MATCH", "MISMATCH"), "flag", src,
                 "Check: all-degrees gap from 13's table equals ageall_gap (12_age_gap.R)"))
N <- rbind(N[!N$name %in% new$name, ], new)
stopifnot(!anyDuplicated(N$name))
write.csv(N, "output/numbers.csv", row.names = FALSE)
cat(sprintf("Added %d rows to output/numbers.csv (now %d)\n", nrow(new), nrow(N)))
