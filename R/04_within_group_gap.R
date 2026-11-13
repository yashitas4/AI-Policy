# 04_within_group_gap.R
# Lists the high-exposure female-dominated (HEF) and male-dominated (HEM) occupations with
# their SOC major group and 2022 employment, then re-estimates the baseline gap within SOC
# major groups, keeping only groups that contain both HEF and HEM occupations.
#
# Definitions, sample, cutoff and gap measure are those of 02_analysis_and_checks.R:
#   dg = (2022-25 CAGR) - (2015-19 CAGR); gap = F - M, weighted by 2022 employment, HC1 SEs.
# Within-group version: HEF and HEM units in shared major groups only, with major-group
# fixed effects, so F is compared with M in the same major group.
# SOC major group = first two digits of the 2018 SOC code of the unit's Census codes
# (every unit in the sample falls in a single major group; the script checks this).
# With this sample only Sales (41) qualifies: 4 occupations, 2 residual df. The output says so;
# the estimate is a documented limitation, not a result.
# Outputs: output/hef_hem_occupations.csv, output/within_group_gap.txt

suppressMessages({ library(readxl); library(sandwich); library(lmtest) })

s18 <- suppressMessages(read_excel("data/raw/2018-occupation-code-list-and-crosswalk.xlsx",
                                   sheet = "2018 Census Occ Code List", col_names = FALSE, skip = 8))
names(s18) <- c("x", "t18", "c18", "soc18")
s18 <- s18[!is.na(s18$c18) & grepl("^[0-9]{4}$", s18$c18), ]
m <- read.csv("data/derived/unit_members.csv"); m <- m[m$scheme == 2018, ]
m$soc <- s18$soc18[match(sprintf("%04d", as.integer(m$code)), s18$c18)]
stopifnot(!anyNA(m$soc))
mg <- tapply(substr(m$soc, 1, 2), m$unit, function(v) paste(sort(unique(v)), collapse = "/"))

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

U <- read.csv("data/derived/panel_units.csv")
U <- U[U$in_sample, ]
U$gender <- ifelse(U$pct_women_2022 > 70, "F", ifelse(U$pct_women_2022 < 30, "M", "N"))
U$high <- U$beta_gpt4 >= quantile(U$beta_gpt4, 0.75, na.rm = TRUE)
U$major <- mg[U$unit]
stopifnot(!grepl("/", U$major))
cagr <- function(e1, e0, yrs) 100 * ((e1 / e0)^(1 / yrs) - 1)
U$dg <- cagr(U$e2025, U$e2022, 3) - cagr(U$e2019, U$e2015, 4)

# ---- 1. Listing --------------------------------------------------------------------
L <- U[U$high & U$gender %in% c("F", "M"), ]
L <- L[order(L$gender, L$major, -L$e2022), ]
out <- data.frame(group = ifelse(L$gender == "F", "HEF", "HEM"), soc_major = L$major,
                  major_group = major_names[L$major], occupation = L$title,
                  emp2022_k = L$e2022, pct_women_2022 = L$pct_women_2022,
                  beta_gpt4 = round(L$beta_gpt4, 3), row.names = NULL)
dir.create("output", showWarnings = FALSE)
write.csv(out, "output/hef_hem_occupations.csv", row.names = FALSE)

# ---- 2. Within-major-group gap -------------------------------------------------------
shared <- intersect(L$major[L$gender == "F"], L$major[L$gender == "M"])
W <- L[L$major %in% shared, ]
W$F <- as.numeric(W$gender == "F")

est <- function(f, d) {
  fit <- lm(f, data = d, weights = e2022)
  ct <- coeftest(fit, vcov. = vcovHC(fit, type = "HC1"))["F", ]
  df <- fit$df.residual
  c(gap = ct[[1]], se = ct[[2]], p = ct[[4]], df = df,
    lo = ct[[1]] - qt(0.975, df) * ct[[2]], hi = ct[[1]] + qt(0.975, df) * ct[[2]])
}
fe <- if (length(shared) > 1) dg ~ F + factor(major) else dg ~ F
r <- est(fe, W)

txt <- c(
  sprintf("High-exposure occupations: HEF = %d, HEM = %d (SOC major groups: HEF %d, HEM %d)",
          sum(L$gender == "F"), sum(L$gender == "M"),
          length(unique(L$major[L$gender == "F"])), length(unique(L$major[L$gender == "M"]))),
  sprintf("Major groups with both HEF and HEM: %s",
          if (length(shared)) paste0(shared, " (", major_names[shared], ")", collapse = ", ") else "none"),
  sprintf("Occupations left: %d (HEF %d, HEM %d); 2022 employment %d k of %d k",
          nrow(W), sum(W$gender == "F"), sum(W$gender == "M"), sum(W$e2022), sum(L$e2022)),
  sprintf("Within-group F - M gap = %+.2f pp/yr (HC1 SE %.2f, p = %.3f, residual df = %d, 95%% CI %+.2f to %+.2f)",
          r["gap"], r["se"], r["p"], r["df"], r["lo"], r["hi"]),
  if (r["df"] < 10) c(
    sprintf("CAVEAT: this estimate is not informative. It rests on %d occupations (%d HEF) in %d major group(s),",
            nrow(W), sum(W$gender == "F"), length(shared)),
    sprintf("with %d residual degrees of freedom; HC1 standard errors are unreliable at this size (R warns the", r["df"]),
    "covariance is near-singular), so the SE, p-value and CI should not be read as evidence. The substantive",
    "finding is that HEF and HEM occupations almost never share a SOC major group, so the gap cannot be",
    "estimated within major groups in this sample."),
  "Occupations used:",
  capture.output(print(data.frame(group = ifelse(W$gender == "F", "HEF", "HEM"), major = W$major,
                                  occupation = substr(W$title, 1, 60), emp2022_k = W$e2022,
                                  dg = round(W$dg, 2)), row.names = FALSE)))
writeLines(txt); writeLines(txt, "output/within_group_gap.txt")
