# AI exposure and employment in female- vs. male-dominated occupations

POLISCI 158 final project (Reed Marthers, Yashita Singh). This is a descriptive comparison of U.S. employment change in high-exposure female-dominated occupations and high-exposure male-dominated occupations, 2015–2025.

## Run it

From the repository root, in R 4.x with the packages `readxl`, `sandwich` and `lmtest`:

```r
source("R/01_build_panel.R")          # -> data/derived/panel_units.csv
source("R/02_analysis_and_checks.R")  # -> output/*.csv, output/checks_summary.txt
source("R/03_annual_gap.R")           # -> output/annual_gap.csv, output/annual_gap*.png
source("R/04_within_group_gap.R")     # -> output/hef_hem_occupations.csv, output/within_group_gap.txt
source("R/05_numbers.R")              # -> output/numbers.csv (every number cited in the paper)
source("R/06_triple_difference.R")    # -> output/triple_difference.csv, adds rows to numbers.csv
source("R/07_codechange_robustness.R") # -> output/codechange_robustness.csv, adds rows to numbers.csv
```

Run them in this order, or run everything with `source("R/00_run_all.R")`. Later scripts read files written by earlier ones.

## Data (`data/raw`)

| File | Source |
|---|---|
| `bls/cpsaat11_2025.xlsx` | BLS CPS Table 11, 2025 annual averages (downloaded as-is) |
| `bls/t11_2010codes_2015_2019.tsv` | BLS Table 11, 2015 and 2019 (employment, % women 2019). Extracted from bls.gov/cps/aa2015 and aa2019 `cpsaat11.htm` |
| `bls/t11_2018codes_2022_2024.tsv` | BLS Table 11, 2022 (employment, % women) and 2024 (employment). Rows follow the same order as the 2025 file. Extracted from bls.gov `cpsaat11.htm` for 2022 and 2024 |
| `bls/t11_2010codes_2016_2018.tsv` | BLS Table 11, 2016, 2017 and 2018 (employment). Rows follow the same order as the 2015/2019 file. Used only by `03_annual_gap.R` |
| `bls/t11_2018codes_2020_2021_2023.tsv` | BLS Table 11, 2020, 2021 and 2023 (employment). Rows follow the same order as the 2025 file. Used only by `03_annual_gap.R` |
| `2018-occupation-code-list-and-crosswalk.xlsx` | U.S. Census Bureau, 2018 occupation code list and 2010→2018 crosswalk |
| `eloundou/occ_level.csv` | Eloundou et al. (2024), occupation-level exposure scores (github.com/openai/GPTs-are-GPTs) |

BLS blocks scripted downloads, so the 2015–2024 values were read from the BLS web tables in a browser. What has been verified so far: the 2022 employment and % women values match the official BLS 2022 Table 11 file in all 595 rows, and the 2024 values were spot-checked against the BLS page. The other years were transcribed from the BLS pages and should be hand-checked against them.

## Method in brief

- Table 11 titles are matched to Census codes. Codes linked by the 2010→2018 changes are grouped into harmonized units.
  - Many-to-many "bridge" links are dropped, so unrelated jobs don't chain together. For example, software developers would otherwise merge with "managers, all other" through project management specialists.
  - This gives 309 units with exposure scores and published % women.
- **Gender groups** use 2022 % women: female-dominated means more than 70% women, male-dominated means fewer than 30%, and everything else is non-dominated.
- **High exposure** means the top 25% of GPT-4 β scores (cutoff β ≥ 0.511).
- **Growth** is compound annual growth: 2015–19 for the pre period, 2022–25 for the post period.
- **Gap**: among high-exposure units, the change in growth for female-dominated units minus male-dominated units. This comes from a regression weighted by 2022 employment, with HC1 standard errors.

## Check 4: code-change robustness (pass standard written 2026-11-22, before running)

Pre-period growth uses 2010 Census codes and post-period growth uses 2018 codes. Some units change size across the switch by reclassification. For example, "Computer occupations, all other" goes from 72k in 2019 to 969k in 2022.

- **Flag:** a harmonized unit in the analysis sample is flagged if its employment changes by more than 50% between 2019 and 2022, in either direction (|E2022 / E2019 − 1| > 0.5).
- **Re-estimate 1:** the main gap without the flagged units. The gender groups and the high-exposure cutoff (β ≥ 0.511) stay as in the baseline.
- **Re-estimate 2:** the main gap on the full sample, weighted by 2019 employment instead of 2022.
- **Pass standard:** the F − M gap stays positive in both re-estimates. If either is ≤ 0, the check fails.

Script: `R/07_codechange_robustness.R`.

## Check 5: tech-slowdown confound (pass standard written 2026-11-22, before running)

Most HEM units are computer or engineering jobs, so the gap could reflect the tech-hiring cycle rather than gender. SOC major groups come from each unit's 2018 SOC codes, as in `R/04_within_group_gap.R`.

- **Re-estimate 1:** the main gap with these units removed from HEM: all Computer and mathematical occupations (SOC 15) and Computer and information systems managers (SOC 11-3021). HEF is unchanged.
- **Re-estimate 2:** the main gap with HEF compared only with HEM units in Architecture and engineering (SOC 17) and Sales and related (SOC 41). HEF is unchanged.
- **Everything else** stays as in the baseline: gender groups, the high-exposure cutoff (β ≥ 0.511), 2022 employment weights and HC1 standard errors.
- **Pass standard:** the gap's sign doesn't flip. The baseline is +2.45, so both re-estimates must be > 0. If either is ≤ 0, the check fails.
- **Also reported:** HEF and HEM units by SOC major group, with each group's share of 2022 employment.

Script: `R/08_tech_confound.R`.

## Note

This pipeline was rebuilt from public sources by Claude (Anthropic) in November 2026, because the original scripts were not available. Its numbers differ from earlier drafts. See `output/checks_summary.txt`.

## Referee review

A referee review of the November 2026 implementation milestone is in [`review/referee2_review.md`](review/referee2_review.md). It lists the three biggest threats to the main claim and checks every number in the milestone PDF against these scripts' output.
