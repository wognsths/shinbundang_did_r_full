# Shinbundang Line Gangnam–Sinsa Extension: DID Analysis

**Citation:** Nam, G., Shon, J., Park, S., and Kim, S. (2026). Traffic and Commercial Reallocation Effects of the Shinbundang Line Gangnam–Sinsa Extension. *STA4118 Causal Inference, Spring 2026, Yonsei University*.

## Overview

This project replicates the opening of the Shinbundang Line Gangnam–Sinsa extension (28 May 2022) as a natural experiment to study whether the new line **reallocated** existing transport flows and commercial consumption rather than expanding aggregate demand.

**Main Findings:** 
- Existing-line ridership at nearby stations fell ~11% (subway, main evidence), concentrated in commuting directions
- Commercial sales showed no clear gains, only a compositional shift toward younger consumers
- Bus and living-population data corroborate route reallocation + demographic composition change

## Quick Start: Reproduce All Results

### Requirements
- **R** ≥ 4.0
- **R packages:** `tidyverse`, `fixest`, `Synth`, `tidysynth`, `data.table`, `ggplot2`, `cowplot`, `scales`, `stringr`, `lubridate`, `sf`, `ggtext`

```r
pkgs <- c("tidyverse", "fixest", "Synth", "tidysynth", "data.table", 
          "ggplot2", "cowplot", "scales", "stringr", "lubridate", "sf", "ggtext")
install.packages(pkgs)
```

### Run the Pipeline
From the project root:
```bash
Rscript run_all.R
```

**Expected runtime:** ~5–10 minutes.

This generates:
- All tables (`.csv`) in `outputs/tables/`, `outputs/*/tables/`
- All figures (`.png`) in `outputs/figures/`, `outputs/*/figures/`
- Clean datasets in `outputs/processed/`
- Technical report summary in `outputs/REPORT_SUMMARY.md`

## Project Structure

```
.
├── README.md                   # This file
├── run_all.R                   # Master pipeline script
├── R/
│   ├── 02_prepare_subway.R
│   ├── 03_analyze_subway.R
│   ├── 04_prepare_commerce.R
│   ├── 05_analyze_commerce.R
│   ├── 06_make_summary.R
│   ├── 07_analyze_anchor_secondary.R
│   ├── 08_analyze_commute_composition.R
│   ├── 09_analyze_supplementary_flows.R
│   ├── 10_analyze_gyeonggi_activity.R
│   ├── 11_analyze_control_robustness.R
│   ├── 12_identification_diagnostics.R
│   └── 13_balance_descriptive.R
├── outputs/processed/          # Clean panel datasets
└── outputs/                    # All results (tables & figures)
    ├── figures/
    ├── tables/
    ├── anchor_secondary/
    ├── commute_composition/
    ├── control_robustness/
    ├── gyeonggi_activity/
    ├── supplementary_flows/
    ├── identification/
    ├── balance/
    ├── figs → figures/         # Symlink for LaTeX
    ├── FINAL_REPORT_DRAFT.pdf
    └── REPORT_SUMMARY.md
```

## Analysis Summary

**Treatment:** Shinbundang Line opening on **28 May 2022**  
**Treated units:** 4 station–lines (Line 2/3/7/9 at Gangnam/Sinnonhyeon/Nonhyeon/Sinsa)  
**Controls:** 21 station–lines in Gangnam/Seocho; 14 commerce dongs  
**Method:** Two-way fixed-effects DiD with event-study, synthetic control, and DAG robustness  
**Period:** Subway 2018–2024, Commerce 2019–2024

### Main Results

| Domain | Effect | p | 95% CI | Notes |
|--------|--------|---|--------|-------|
| **Subway (main)** | | | | |
| Existing-line ridership | −11.0% | 0.057 | [−21.1%, +0.4%] | Concentrated in commuting |
| Morning alighting | −11.3% | 0.007 | | Direction specificity |
| Evening boarding | −9.8% | 0.029 | | Route reallocation signal |
| **Commerce (exploratory)** | | | | Parallel trends violated |
| Log sales | −9.6% | 0.131 | [−20.9%, +3.4%] | Not significant |
| Age 20–30 share (secondary) | +3.1pp | 0.004 | | Composition signal |

## Reproducibility Checklist

- [ ] R ≥ 4.0 with packages installed
- [ ] `Rscript run_all.R` completes without error
- [ ] `outputs/tables/*main.csv` match report tables
- [ ] `outputs/figures/` contains all 8 required figures
- [ ] `outputs/REPORT_SUMMARY.md` generated
- [ ] LaTeX report: figures accessible as `figs/*.png`

## Robustness Analyses

All generated automatically:
- **Pretrend tests:** Wald joint & linear (outputs/tables/*pretrend*.csv)
- **Synthetic control:** Pre-RMSPE, post gaps, donor weights (outputs/tables/*scm*)
- **Control robustness:** DAG-admissible variables, alternative control sets (outputs/control_robustness/)
- **Heterogeneity:** By time, direction, weekday, demographic group, individual dong
- **Permutation inference:** 999 random re-assignment, p-values (outputs/identification/)

## Limitations (Section 7, Report)

1. **Interference (SUTVA):** Controls may be affected by opening; effect is relative reallocation
2. **No anticipation:** Pre-announced date; pretrend spike in 2022.1–2 visible
3. **Commerce parallel trends fail:** Pretrend p < 10⁻⁸ for all outcomes → exploratory only
4. **Few clusters:** 4 treated station–lines → clustered SE anti-conservative; permutation checks provided
5. **Commerce resolution:** Dong–quarter level dilutes station-area effects
6. **Gyeonggi data break:** 2023 level shift; cleanest 2021–2022 window only

## Citation

```bibtex
@article{nam2026shinbundang,
  title={Traffic and Commercial Reallocation Effects of the Shinbundang Line Gangnam--Sinsa Extension},
  author={Nam, Gyeongsu and Shon, Jaehun and Park, Sungha and Kim, Sungsu},
  year={2026},
  month={June}
}
```

## Files & Presentations

- **Technical Report:** `outputs/FINAL_REPORT_DRAFT.pdf` (Korean, 22pp, detailed methods & robustness)
- **Summary:** `outputs/REPORT_SUMMARY.md` (Korean, 1p key findings)
- **Presentation:** `outputs/presentation.html` (Interactive Reveal.js deck, 14 slides)
- **Presentation PDF:** `outputs/presentation.pdf` (Static version)

---

**Last updated:** June 2026  
**Authors:** Nam, Shon, Park, Kim | Yonsei University
