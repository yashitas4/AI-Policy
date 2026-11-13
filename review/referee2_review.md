# Referee review: Project Implementation Milestone

POLISCI 158 · AI exposure and employment in female- vs. male-dominated occupations · November 13, 2026

This review checks the milestone PDF against the four R scripts in this repository. All four scripts were re-run on a clean copy of commit `8f91a6e`, and they reproduce every committed file in `output/` and `data/derived/` exactly. No analysis scripts or data were changed. A formatted version of this page is in [`referee2_review.html`](referee2_review.html) (download it and open it in a browser).

**Claim under review.** After 2022 there is no reliable evidence that high-exposure female-dominated (HEF) occupations did worse than high-exposure male-dominated (HEM) ones, and the post-2022 pattern continues the pre-AI trend. Rebuilt baseline gap: **+2.45 pp/yr** (95% CI −1.70 to +6.60).

**Number check at a glance:** 7 PDF figures match the scripts, 6 disagree, and 11 can't be traced to any script in the repo.

## Three biggest threats to the claim

### 1. The gender comparison is really a clerical-vs-tech comparison

`R/04_within_group_gap.R` shows HEF and HEM occupations share only one SOC major group (Sales, 4 occupations). 13 of the 19 HEM units are computer or engineering jobs. HEF is mostly office and administrative work.

- The gap therefore measures office work against the tech cycle, and gender can't be separated from occupation family.
- The second named rival, the tech slowdown, sits inside the comparison group itself. A positive gap could mean tech cooled, not that female-dominated jobs held up.
- None of the three pre-set checks addresses this rival.

*Suggested response:* say plainly that the design compares occupation families, and add a check that drops or separately reports the computer occupations in HEM.

### 2. Half the gap also appears among low-exposure jobs, and the stated baseline isn't in the code

Page 1 says "low-exposure groups serve as the baseline," and the memo calls the gap "relative to lower-exposure jobs." `gap_reg` in `R/02_analysis_and_checks.R` only compares F with M inside the high-exposure group.

- Among lower-exposure jobs, the F − M gap is **+1.22** (SE 0.93).
- Netting that out gives a triple difference of **+1.23** (SE 2.27), half the headline +2.45.
- With Check 1 (human-rated exposure gives −0.06), little of the gap is specific to GPT-4 exposure.

*Suggested response:* either add the low-exposure comparison to the method and report it, or remove the baseline sentence.

### 3. Pre and post periods measure different workers in some HEM units

Pre-period growth (2015–19) is computed on 2010 Census codes and post-period growth (2022–25) on 2018 codes.

- "Computer occupations, all other" goes from **72k** (2019) to **969k** (2022). "Web developers" goes from 193k to 79k. Both are reclassification.
- These units pass the "complete on both sides" filter because dropping bridge links leaves their 2018 codes absorbing workers from outside the unit.
- The regression weights units by 2022 employment, which inflates the first unit about 13×. Dropping it moves the gap from +2.45 to +3.36.
- Leave-one-out is reassuring on sign: the gap ranges from +1.43 to +4.17 and never turns negative.

*Suggested response:* flag or drop units whose size jumps across the code change, and weight by a pre-period size as a robustness check.

## Number check: PDF vs. script output

| PDF says | Scripts give | Status |
|---|---|---|
| 309 harmonized units; cutoff β ≥ 0.511 | 309; 0.511 | ✅ Match |
| Rebuild gap +2.45, 95% CI −1.70 to +6.60 | +2.45 (−1.70, +6.60), p = 0.244 | ✅ Match |
| Check 1: −0.06 (CI −4.48 to +4.37) | −0.06 (−4.48, +4.37) | ✅ Match |
| Check 2: −6.2%, 9.37M → 8.79M | 9,371k → 8,792k, −6.2% | ✅ Match |
| Check 2 driven by secretaries, −807k | −807k | ✅ Match |
| Check 3: +2.59 | +2.59 | ✅ Match |
| Secretaries 2.87M → 2.28M → 1.88M | 2,870k → 2,279k → 1,881k | ✅ Match |
| 22 HEF occupations, 9.4M jobs | 21 occupations; 8.77M (2022). 9.37M is the 2019 total | ❌ Mismatch |
| 17 HEM occupations, 9.2M jobs | 19 occupations; 9.26M (2022) | ❌ Mismatch |
| HEF growth −0.39% → +1.06%/yr | −0.62% → +0.07% | ❌ Mismatch |
| HEM growth +3.48% → +1.27%/yr | +2.76% → +1.25% | ❌ Mismatch |
| HEFs declined "~4% per year" relative to HEMs, 2015–19 | ≈3.4 pp/yr (CAGR −0.62 vs +2.76; log trend −0.84 vs +2.51) | ❌ Mismatch |
| Check 1 pass standard: "interval ≥ zero" | Script tests "CI not entirely below zero." Read literally, the PDF wording would fail the baseline too. | ❌ Wording |
| Gap +3.5 (SE 2.0, p = 0.08; CI ≈ −0.5 to +7.5) | Not produced by any script | ⚠️ Untraceable |
| Positive across all 14 alternative definitions | No alternative-definition script in the repo | ⚠️ Untraceable |
| "Nine R scripts" | The repo has four | ⚠️ Untraceable |
| Check 1 original: +1.2 (SE 2.4) | Not produced by any script | ⚠️ Untraceable |
| IPUMS: 26–34 HEM +5.7% → −0.2%/yr | No IPUMS data or code in the repo | ⚠️ Untraceable |
| IPUMS: ages 22–25 HEF +5.8% since 2022 | No IPUMS data or code in the repo | ⚠️ Untraceable |
| IPUMS: gender shares r = 0.9998; 7.35M records | No IPUMS data or code in the repo | ⚠️ Untraceable |
| Ages 22–25 intervals about −11 to +11 pp/yr | No IPUMS data or code in the repo | ⚠️ Untraceable |
| Ruled out a trend-adjusted decline larger than ~5% | Not produced by any script | ⚠️ Untraceable |
| Event study relative to 2022 | `R/03_annual_gap.R` gives annual and detrended gaps, but no event-study regression with intervals | ⚠️ Untraceable |
| Month-by-month 22–25 breakdown | Not in the repo | ⚠️ Untraceable |

The untraceable figures come from the earlier analysis. Pages 1–2 lead with them, and they mix with the rebuild's 309 units. The memo on page 11 of the same PDF recommends dropping them because nobody can reproduce them. The Indeed postings figure (233.8 → 142.5) is an outside source and was not checked.

## Other problems

- **Still open:** pages 4–14 of the PDF look like pasted chat transcripts, not milestone text. They include a passage saying no `.R` files existed on the machine, which sits awkwardly next to the AI-use note's "nine-script analysis." Check what was meant to be submitted. The PDF isn't in this repo, so its fixes are the authors' to make.
- **Fixed in this repo:** the README said the data were "checked byte for byte (SHA-256) against the values on the BLS pages," which a browser check can't do. It now states what was verified (2022 in all 595 rows, 2024 spot-checked) and that the other years still need hand-checks.
- **Fixed in this repo:** the README's run instructions listed only scripts 01 and 02, and its data table left out the 2016–18 and 2020–23 files. Both now cover all four scripts and every input.

## Method

Scripts 01–04 were re-run in R on a clean copy of commit `8f91a6e`, and outputs were compared file by file. The diagnostics (leave-one-out, low-exposure comparison, triple difference) used the same sample, cutoffs, CAGR definitions, 2022 employment weights and HC1 standard errors as `R/02_analysis_and_checks.R`. Prepared with Claude Code.
