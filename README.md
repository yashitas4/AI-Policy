# AI exposure and employment in female- vs. male-dominated occupations

POLISCI 158 final project (Reed Marthers, Yashita Singh). This is a descriptive comparison of U.S. employment change in high-exposure female-dominated occupations and high-exposure male-dominated occupations, 2015–2025.

## Run it

From the repository root, in R 4.x with the packages `readxl`, `sandwich` and `lmtest`:

```r
source("R/01_build_panel.R")          # -> data/derived/panel_units.csv
source("R/02_analysis_and_checks.R")  # -> output/*.csv, output/checks_summary.txt
```

## Data (`data/raw`)

| File | Source |
|---|---|
| `bls/cpsaat11_2025.xlsx` | BLS CPS Table 11, 2025 annual averages (downloaded as-is) |
| `bls/t11_2010codes_2015_2019.tsv` | BLS Table 11, 2015 and 2019 (employment, % women 2019). Extracted from bls.gov/cps/aa2015 and aa2019 `cpsaat11.htm` |
| `bls/t11_2018codes_2022_2024.tsv` | BLS Table 11, 2022 (employment, % women) and 2024 (employment). Rows follow the same order as the 2025 file. Extracted from bls.gov `cpsaat11.htm` for 2022 and 2024 |
| `2018-occupation-code-list-and-crosswalk.xlsx` | U.S. Census Bureau, 2018 occupation code list and 2010→2018 crosswalk |
| `eloundou/occ_level.csv` | Eloundou et al. (2024), occupation-level exposure scores (github.com/openai/GPTs-are-GPTs) |

BLS blocks scripted downloads, so the 2015–2024 values were read from the BLS web tables in a browser. Each file was checked byte for byte (SHA-256) against the values on the BLS pages.

## Method in brief

- Table 11 titles are matched to Census codes. Codes linked by the 2010→2018 changes are grouped into harmonized units. This gives 291 units with exposure scores and published % women.
- **Gender groups** use 2022 % women: female-dominated means more than 70% women, male-dominated means fewer than 30%, and everything else is non-dominated.
- **High exposure** means the top 25% of GPT-4 β scores (cutoff β ≥ 0.50).
- **Growth** is compound annual growth: 2015–19 for the pre period, 2022–25 for the post period.
- **Gap**: among high-exposure units, the change in growth for female-dominated units minus male-dominated units. This comes from a regression weighted by 2022 employment, with HC1 standard errors.

## Note

This pipeline was rebuilt from public sources by Claude (Anthropic) in November 2026, because the original scripts were not available. Its numbers differ from earlier drafts. See `output/checks_summary.txt`.
