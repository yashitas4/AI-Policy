# AI exposure and employment in female- vs. male-dominated occupations

POLISCI 158 final project (Reed Marthers, Yashita Singh). This is a descriptive comparison of U.S. employment change in high-exposure female-dominated occupations and high-exposure male-dominated occupations, 2015–2025.

## Run it

From the repository root, in R 4.x with the packages `readxl`, `sandwich` and `lmtest`:

```r
source("R/01_build_panel.R")          # -> data/derived/panel_units.csv
source("R/02_analysis_and_checks.R")  # -> output/*.csv, output/checks_summary.txt
source("R/03_annual_gap.R")           # -> output/annual_gap.csv, output/annual_gap*.png
source("R/04_within_group_gap.R")     # -> output/hef_hem_occupations.csv, output/within_group_gap.txt
```

Run them in this order: 03 and 04 read `data/derived/` files written by 01.

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

## Note

This pipeline was rebuilt from public sources by Claude (Anthropic) in November 2026, because the original scripts were not available. Its numbers differ from earlier drafts. See `output/checks_summary.txt`.

## Referee review

A referee review of the November 2026 implementation milestone is in [`review/referee2_review.md`](review/referee2_review.md). It lists the three biggest threats to the main claim and checks every number in the milestone PDF against these scripts' output.
