# AI exposure and employment in female- vs. male-dominated occupations

POLISCI 158 final project (Reed Marthers, Yashita Singh). This is a descriptive comparison of U.S. employment change in high-exposure female-dominated occupations and high-exposure male-dominated occupations, 2015–2025.

## Run it

From the repository root, run everything with one command:

```bash
Rscript R/00_run_all.R
```

**Requirements:** R 4.x (tested with R 4.6.2) and the packages `readxl`, `sandwich` and `lmtest`. On macOS, `R/16_figures.R` uses the built-in Quartz PNG device. Scripts 11 and 13 also need `gzip` and `awk`, which come with macOS and Linux.

**Without the IPUMS extract:** the raw IPUMS files are not in the repository (see Data). If they are missing, `00_run_all.R` skips scripts 11 and 13 and uses the committed tables they produce, so every other script still runs. A full run takes about 4 minutes with the IPUMS extract and under 20 seconds without it.

**Every number in the paper** is collected in `output/numbers.csv`, one row per number: its name, value, display form, unit, the script that produced it, and its source file. Script 05 creates the file, and scripts 06–10, 12, 14 and 15 each add their own rows. Several scripts stop with an error if their results don't reproduce an earlier script's.

### Scripts

Run in this order. Later scripts read files written by earlier ones.

| Script | What it does | Writes |
|---|---|---|
| `00_run_all.R` | Runs 01–16 in order | — |
| `01_build_panel.R` | Builds the 309 harmonized occupation units from Table 11, the Census crosswalk and Eloundou exposure | `data/derived/panel_units.csv`, `unit_members.csv` |
| `02_analysis_and_checks.R` | Main gap and Checks 1–3 | `output/group_trends_*.csv`, `gap_estimates.csv`, `checks_summary.txt` |
| `03_annual_gap.R` | Year-by-year gap, raw and net of the 2015–19 trend | `output/annual_gap.csv`, `annual_gap*.png` |
| `04_within_group_gap.R` | HEF/HEM occupation list; gap within SOC major groups | `output/hef_hem_occupations.csv`, `within_group_gap.txt` |
| `05_numbers.R` | Creates the table of every cited number | `output/numbers.csv` |
| `06_triple_difference.R` | Same gap among lower-exposure units, and the triple difference | `output/triple_difference.csv` |
| `07_codechange_robustness.R` | Check 4 (see below) | `output/codechange_*.csv` |
| `08_tech_confound.R` | Check 5 (see below); HEF/HEM by SOC major group | `output/tech_confound.csv`, `hef_hem_by_major_group.csv` |
| `09_education_split.R` | Gap for college vs. non-college occupations (median share with a bachelor's degree, BLS Table 5.3) | `output/education_shares.csv`, `education_split.csv` |
| `10_annual_gap_ci.R` | Bootstrap 95% intervals for the year-by-year gap | `output/annual_gap_ci.csv`, `annual_gap_ci.png` |
| `11_ipums_panel.R` | IPUMS employment by unit × age band × year; checks it against Table 11 | `data/derived/ipums_unit_age_year.csv`, `output/ipums_validation.txt` |
| `12_age_gap.R` | Gap by age band (22–25, 26–34, 35+) with occupation bootstrap | `output/age_gap.csv` |
| `13_ipums_build.R` | IPUMS employment by unit × year × sex × degree × age band | `data/derived/ipums_unit_year_sex_deg_age.csv` |
| `14_ipums_education_gap.R` | Gap for workers with and without a bachelor's degree; survey responses behind each cell | `output/ipums_education_gap.csv`, `ipums_education_cells.csv` |
| `15_pew_reconcile.R` | Compares our result with Pew Research Center (Oct 2026); rebuilds Pew's O*NET exposure index | `output/pew_reconcile.txt`, `data/derived/pew_exposure_units.csv` |
| `16_figures.R` | Five 16:9 slide/paper figures from the outputs above (growth before/after, annual gap, composition, Pew windows, summary of all estimates and checks) | `output/figures/fig1-5*.png` |

Scripts 05–10, 12, 14 and 15 also add rows to `output/numbers.csv`.

## Data (`data/raw`)

| File | Source |
|---|---|
| `bls/cpsaat11_2025.xlsx` | BLS CPS Table 11, 2025 annual averages (downloaded as-is) |
| `bls/t11_2010codes_2015_2019.tsv` | BLS Table 11, 2015 and 2019 (employment, % women 2019). Extracted from bls.gov/cps/aa2015 and aa2019 `cpsaat11.htm` |
| `bls/t11_2018codes_2022_2024.tsv` | BLS Table 11, 2022 (employment, % women) and 2024 (employment). Rows follow the same order as the 2025 file. Extracted from bls.gov `cpsaat11.htm` for 2022 and 2024 |
| `bls/t11_2010codes_2016_2018.tsv` | BLS Table 11, 2016, 2017 and 2018 (employment). Rows follow the same order as the 2015/2019 file. Used by `03` and `10` |
| `bls/t11_2018codes_2020_2021_2023.tsv` | BLS Table 11, 2020, 2021 and 2023 (employment). Rows follow the same order as the 2025 file. Used by `03` and `10` |
| `bls/education.xlsx` | BLS Employment Projections, education tables (downloaded as-is from bls.gov/emp/ind-occ-matrix/education.xlsx, Nov 22, 2026). Sheet "Table 5.3": educational attainment of workers 25+ by occupation, 2023–24. Matches the bls.gov web table in all 832 rows. Used only by `09_education_split.R` |
| `2018-occupation-code-list-and-crosswalk.xlsx` | U.S. Census Bureau, 2018 occupation code list and 2010→2018 crosswalk |
| `eloundou/occ_level.csv` | Eloundou et al. (2024), occupation-level exposure scores (github.com/openai/GPTs-are-GPTs) |
| `onet/Work Activities.txt` | O*NET 30.3 database, Work Activities (downloaded as-is from onetcenter.org, Nov 22, 2026). Used only by `15_pew_reconcile.R` to rebuild Pew's exposure index |
| `ipums/cps_00002.dat.gz`, `ipums/cps_00002.xml` | IPUMS CPS extract and its codebook. **Not in the repository**, see below |

BLS blocks scripted downloads, so the 2015–2024 values were read from the BLS web tables in a browser. What has been verified so far: the 2022 employment and % women values match the official BLS 2022 Table 11 file in all 595 rows, and the 2024 values were spot-checked against the BLS page. The other years were transcribed from the BLS pages and should be hand-checked against them.

### IPUMS CPS (scripts 11–15)

Table 11 has no age or education detail, so the age and degree splits and the Pew comparison use IPUMS CPS microdata: Flood, S., King, M., Rodgers, R., et al. *IPUMS CPS: Version 13.0* [dataset]. Minneapolis, MN: IPUMS, 2025. https://doi.org/10.18128/D030.V13.0.

**Not committed.** IPUMS terms of use don't allow redistributing the microdata, and the file is 425 MB. `.gitignore` keeps everything in `data/raw/ipums/` out of git. The aggregated tables built from it are committed in `data/derived/` (`ipums_unit_age_year.csv`, `ipums_unit_year_sex_deg_age.csv`), so scripts 12, 14 and 15 run without the raw data.

**To rebuild those tables yourself**, request an extract at cps.ipums.org:
- **Samples:** basic monthly, January 2015 – September 2026 (the scripts use 2015–2025). October 2025 does not exist; that survey wasn't collected. ASEC samples can be included; the scripts drop them.
- **Variables used:** `YEAR`, `MONTH`, `ASECFLAG`, `WTFINL`, `AGE`, `SEX`, `EMPSTAT`, `OCC`, `EDUC`. Other variables can be included; the scripts ignore them. Our extract (`cps_00002`) also has `SERIAL`, `HWTFINL`, `CPSID`, `HFLAG`, `ASECWTH`, `PERNUM`, `CPSIDP`, `CPSIDV`, `ASECWT`, `OCC2010`, `IND`, `UHRSWORKT`, `EARNWT`, `EARNWEEK`.
- **Format:** fixed-width `.dat` with the DDI codebook (`.xml`). Put both in `data/raw/ipums/`, named `cps_00002.dat.gz` and `cps_00002.xml`, renaming if your extract number differs. The scripts read column positions from the codebook, so the variable order doesn't matter.

**How it's used:** employed people (`EMPSTAT` 10 or 12) aged 16+ in the basic monthly files, weighted by `WTFINL` and averaged over each year's months, so 2025 averages 11 months. `OCC` is matched to the harmonized units with the same crosswalk as `01_build_panel.R`. A bachelor's degree or more means `EDUC` 111, 123, 124 or 125. Across the 309 units, IPUMS totals match Table 11 at r = 0.9999 (`output/ipums_validation.txt`). The public files never use the Census codes for statisticians (1230) or judges (2110), so those two units are zero in IPUMS; neither is HEF or HEM.

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
