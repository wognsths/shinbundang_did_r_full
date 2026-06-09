# Output Files Manifest

This file documents all generated tables and figures by the `run_all.R` pipeline and their use in the report.

## Main Results Tables

### Subway Analysis (`outputs/tables/`)

| File | Report Table | Description | Rows |
|------|--------------|-------------|------|
| `subway_did_main.csv` | Table 2 | Main DiD estimate + synthetic control summary | 3 |
| `subway_group_means.csv` | Table 3 | Pre/post means (treated vs. control) + SC donors | 5 |
| `subway_timeband_did.csv` | Table 4 | DiD by time of day (peak, daytime, late-night) | 4 |
| `subway_timeband_direction_did.csv` | Table 4 | DiD by commuting direction (AM alight, PM board, etc.) | 4 |
| `subway_did_robustness.csv` | Table 5 | Robustness: DAG controls + alternative control sets | 7 |
| `subway_pretrend_tests.csv` | — | Pretrend Wald joint & linear tests | 2 |
| `subway_scm_summary.csv` | Table 2 | Synthetic control post gaps & pre-RMSPE | 1 |
| `subway_scm_weights.csv` | Table 3 | SC donor weights (top donors shown) | 21 |

### Commerce Analysis (`outputs/tables/`)

| File | Report Table | Description | Rows |
|------|--------------|-------------|------|
| `commerce_did_main_2019_2024.csv` | Table 6 | Commerce aggregate DiD (all outcomes + pretrend p) | 6 |
| `commerce_heterogeneity_2019_2024.csv` | Table 7 | Anchor vs. secondary split + individual dong effects | 8 |
| `commerce_scm_summary.csv` | — | Commerce SC gaps & pre-RMSPE | — |
| `commerce_pretrend_tests_2019_2024.csv` | — | Pretrend Wald tests (all outcomes rejected) | 6 |

### Supplementary Results (`outputs/*/tables/`)

| File | Report Table | Description |
|------|--------------|-------------|
| `anchor_secondary/commerce_anchor_vs_secondary_split_did.csv` | Table 7 | Detailed anchor/secondary split |
| `commute_composition/subway_commute_direction_did.csv` | Table 4 | Direction-specific estimates |
| `commute_composition/commerce_secondary_age_gender_did.csv` | Table 8 | Secondary corridor age/gender composition |
| `commute_composition/commerce_secondary_consumption_composition_did.csv` | — | Revenue shares by time of day |
| `supplementary_flows/bus_*.csv` | Table 8 | Corridor bus boarding/alighting effects |
| `supplementary_flows/living_pop_*.csv` | Table 8 | Living population composition effects |
| `gyeonggi_activity/gyeonggi_bundang_*.csv` | Table 8 | Bundang original-corridor activity effects |
| `control_robustness/control_robustness_summary_for_report.csv` | Table 5 | Robustness summary across all domains |
| `identification/tables/subway_permutation_pvalue.csv` | — | Permutation inference p-values (999 reps) |

## Figure Files

### Main Figures (Report Figures)

| File | Report Figure | Source Script | Description |
|------|---------------|---------------|-------------|
| `outputs/figures/subway_trend_treated_control_2018_2024.png` | Figure 1 (3) | 03_analyze_subway.R | Treated vs. control ridership trends |
| `outputs/figures/subway_scm_2018_2024.png` | Figure 1 (5) | 03_analyze_subway.R | Treated vs. synthetic control |
| `outputs/figures/subway_event_study_2018_2024.png` | Figure 4 | 03_analyze_subway.R | Event-study coefficients with pre-trend window |
| `outputs/commute_composition/figures/subway_direction_timeband_effects.png` | Figure 6 | 08_analyze_commute_composition.R | Direction & time-band DiD |
| `outputs/control_robustness/figures/dag_identification.png` | Figure 1 (1) | 11_analyze_control_robustness.R | DAG-based control logic |
| `outputs/anchor_secondary/figures/commerce_three_group_sales_trend.png` | Figure 7 | 07_analyze_anchor_secondary.R | Anchor vs. secondary vs. control trends |
| `outputs/commute_composition/figures/commerce_secondary_age_gender_effects.png` | Figure 8 | 08_analyze_commute_composition.R | Age/gender composition DiD |
| `outputs/gyeonggi_activity/figures/gyeonggi_bundang_did_effects.png` | Figure 13 | 10_analyze_gyeonggi_activity.R | Bundang activity population DiD |

### Supporting Figures (Analysis Details)

| File | Source Script | Description |
|------|---|---|
| `outputs/figures/subway_event_study_night_share_2018_2024.png` | 03_analyze_subway.R | Event study for evening boarding specifically |
| `outputs/figures/commerce_trend_sales_2019_2024.png` | 05_analyze_commerce.R | Raw commerce trends pre/post |
| `outputs/figures/commerce_event_study_log_sales_2019_2024.png` | 05_analyze_commerce.R | Commerce event study (pretrend fail) |
| `outputs/commute_composition/figures/commerce_three_group_sales_trend.png` | — | Alternative trending plot |
| `outputs/commute_composition/figures/commerce_secondary_service_group_sales_effects.png` | — | Commerce by service type |
| `outputs/control_robustness/figures/did_control_robustness.png` | 11_analyze_control_robustness.R | Robustness estimate ranges |
| `outputs/gyeonggi_activity/figures/gyeonggi_bundang_activity_trend.png` | 10_analyze_gyeonggi_activity.R | Bundang trend plot (pre/post) |
| `outputs/gyeonggi_activity/figures/gyeonggi_bundang_single_dong_effects.png` | 10_analyze_gyeonggi_activity.R | Individual dong heterogeneity |

## Processed Datasets (`outputs/processed/`)

| File | Description | Unit | Rows | Period |
|------|---|---|---|---|
| `subway_daily_panel_2018_2024.csv` | Subway ridership panels | station–line–day | ~100k | 2018–2024 |
| `subway_monthly_panel_2018_2024.csv` | Aggregated subway (monthly) | station–line–month | ~2.1k | 2018–2024 |
| `commerce_dong_quarter_panel_2019_2024.csv` | Commerce sales panels | dong–quarter | ~1.3k | 2019–2024 |
| `commerce_service_dong_quarter_panel_2019_2024.csv` | Commerce by service type | dong–service–quarter | ~8k | 2019–2024 |
| `bus_corridor_monthly_panel_2019_2024.csv` | Corridor bus boardings | stop–month | ~300 | 2019–2024 |
| `living_population_dong_monthly_panel_2021_2024.csv` | Seoul living population | dong–month | ~1.6k | 2021–2024 |
| `gyeonggi_bundang_day_dong_panel_2018_2025.csv` | Bundang activity population | dong–day | ~26k | 2018–2025 |

## Code Notebooks & Scripts

| File | Purpose |
|------|---------|
| `run_all.R` | Master pipeline; sources R/02 through R/13 in order |
| `R/02_prepare_subway.R` | Load & clean Seoul subway data; construct panel |
| `R/03_analyze_subway.R` | Main DiD, event study, synthetic control for subway |
| `R/04_prepare_commerce.R` | Load & clean Seoul commerce data; construct panel |
| `R/05_analyze_commerce.R` | Commerce DiD, event study, pretrend diagnostics |
| `R/06_make_summary.R` | Summary statistics, balance table construction |
| `R/07_analyze_anchor_secondary.R` | Split commerce by "anchor" vs. "secondary corridor" |
| `R/08_analyze_commute_composition.R` | Time-band & directional breakdowns; age/gender composition |
| `R/09_analyze_supplementary_flows.R` | Bus & living-population analysis |
| `R/10_analyze_gyeonggi_activity.R` | Bundang original-corridor Gyeonggi API analysis |
| `R/11_analyze_control_robustness.R` | DAG-based control sensitivity, alternative control sets |
| `R/12_identification_diagnostics.R` | Pretrend Wald/linear tests, permutation inference |
| `R/13_balance_descriptive.R` | Descriptive tables, balance diagnostics |

## Reports & Presentations

| File | Format | Language | Pages | Contents |
|------|--------|----------|-------|----------|
| `FINAL_REPORT_DRAFT.pdf` | PDF | Korean | 22 | Full technical report with methods, results, limitations |
| `REPORT_SUMMARY.md` | Markdown | Korean | 1 | Key findings summary |
| `presentation.html` | HTML (Reveal.js) | Korean | 14 slides | Interactive presentation deck |
| `presentation.pdf` | PDF | Korean | 14 pages | Static PDF export of presentation |

## Data Codebooks

| File | Description |
|------|---|
| `outputs/processed/commerce_dong_codebook.csv` | Administrative dong codes & names (treatment/control assignment) |
| `outputs/processed/commerce_service_codebook.csv` | Service category codes (food, retail, entertainment, etc.) |

## Metadata & Logs

| File | Description |
|------|---|
| `run_all.R` (completion message) | Lists all output directory paths upon successful run |
| `outputs/REPORT_SUMMARY.md` | Auto-generated summary of key results (Korean) |

## Directory Tree (Post-run)

```
outputs/
├── tables/                          # Main results tables (CSV)
│   ├── subway_did_main.csv
│   ├── subway_*_did.csv
│   ├── commerce_*_2019_2024.csv
│   ├── *_scm*.csv
│   └── *_pretrend*.csv
├── figures/                         # Main & supporting figures (PNG)
│   ├── subway_*.png
│   ├── commerce_*.png
│   └── figs → ./figures (symlink for LaTeX)
├── anchor_secondary/                # Anchor/secondary split results
│   ├── tables/
│   └── figures/
├── commute_composition/             # Time/direction/composition breakdowns
│   ├── tables/
│   └── figures/
├── control_robustness/              # DAG robustness & control sensitivity
│   ├── tables/
│   └── figures/
├── gyeonggi_activity/               # Bundang original corridor
│   ├── tables/
│   └── figures/
├── supplementary_flows/             # Bus, living population
│   ├── tables/
│   └── figures/
├── identification/                  # Pretrend tests, permutation
│   └── tables/
├── balance/                         # Descriptive & balance stats
│   └── tables/
├── processed/                       # Clean panel datasets
│   ├── *.csv (panels)
│   └── *_codebook.csv
├── FINAL_REPORT_DRAFT.pdf          # Technical report
├── REPORT_SUMMARY.md               # Summary (Korean)
└── FINAL_REPORT_DRAFT.md           # Report source markdown
```

---

**Note:** All CSV files are machine-readable; tables in the report are manually typeset in LaTeX from these values. Figures are directly embedded in the report.

**Last updated:** June 2026
