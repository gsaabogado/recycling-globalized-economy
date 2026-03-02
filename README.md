# Replication Package: Recycling in a Globalised Economy

**Authors:** Eugenie Joltreau and Luis Sarmiento

This repository contains the data and code to replicate all empirical results in "Recycling in a Globalised Economy."

## Software Requirements

- **R** (>= 4.3.0)
- **RStudio** (recommended, not required)

### R Packages

The main packages used are:

| Package | Purpose |
|---------|---------|
| `tidyverse` | Data wrangling and visualization |
| `fixest` | Fixed-effects regressions (OLS, Poisson) |
| `glmmTMB` | Zero-inflated Poisson models |
| `vroom` | Fast CSV reading |
| `readxl` | Excel file reading |
| `countrycode` | Country code conversion |
| `conflicted` | Package conflict resolution |
| `NatParksPalettes` | Color palettes for figures |
| `cowplot` | Multi-panel figures |
| `ggpubr` | Publication-ready plots |
| `kableExtra` | Formatted tables |
| `broom` / `broom.mixed` | Tidy model output |
| `parallel` | Parallel computation for bootstrap |
| `pscl` | Count data models |
| `scales` | Axis formatting |

Install all packages with:

```r
install.packages(c("tidyverse", "fixest", "glmmTMB", "vroom", "readxl",
                   "countrycode", "conflicted", "NatParksPalettes", "cowplot",
                   "ggpubr", "kableExtra", "broom", "broom.mixed", "parallel",
                   "pscl", "scales"))
```

## Data

Raw data files that are too large for GitHub must be downloaded separately. See [`01_data/README_data.md`](01_data/README_data.md) for detailed download instructions and file placement.

**Included in the repository** (small files):

- `01_data/01_trade/codebook.xlsx` -- Waste product and country classification
- `01_data/01_trade/baci_hs_codes.csv` -- HS code descriptions
- `01_data/01_trade/country_codes_V202401b.csv` -- BACI country codes
- `01_data/01_trade/product_codes_HS96_V202401b.csv` -- BACI product codes
- `01_data/02_material_imbalance/miso_062024.xlsx` -- MISO2 data (June 2024)
- `01_data/02_material_imbalance/miso_072024.xlsx` -- MISO2 data (July 2024)
- `01_data/03_recycling_rates/env_waspac.csv` -- EUROSTAT packaging waste
- `01_data/06_chinese_recycling_rates/China recycling 2015-2023.xlsx` -- Chinese recycling rates
- `01_data/rPET_prices.xlsx` -- rPET market prices

**Must be downloaded** (see `01_data/README_data.md`):

- BACI HS96 trade data (~10 GB) from CEPII
- CEPII Gravity database (~300 MB CSV, exported from PostgreSQL)
- MISO2 full material flows (~27 MB) from Zenodo
- ESCAP/World Bank trade costs (~178 MB)

## How to Run

1. Download and place all required data files (see `01_data/README_data.md`).
2. Open `r_project.Rproj` in RStudio (or set your working directory to the project root).
3. Run the master script:

```r
source("run_all.R")
```

This sources all scripts in the correct dependency order. Output figures are saved to `04_output/figures/` and intermediate datasets to `02_gen/`.

**Note:** The zero-inflated Poisson models (scripts 06-08, 10) use bootstrapped standard errors and are computationally intensive. They were originally run on an HPC cluster. On a standard machine, these scripts may take several hours.

## Repository Structure

```
.
├── README.md
├── LICENSE
├── run_all.R                         # Master orchestration script
├── r_project.Rproj
├── 01_data/                          # Raw data (large files gitignored)
│   ├── README_data.md
│   ├── 01_trade/
│   ├── 02_material_imbalance/
│   ├── 03_recycling_rates/
│   ├── 05_trade_costs/
│   └── 06_chinese_recycling_rates/
├── 02_gen/                           # Intermediate datasets (gitignored)
├── 03_scripts/                       # All analysis scripts
│   ├── 00_setup.R
│   ├── 01_load_data.R
│   ├── 02_main_plots.R
│   ├── ...
│   └── _export_gravity_db.R
└── 04_output/                        # Figures and tables (gitignored)
    ├── figures/
    └── tables/
```

## Exhibit Map

### Main Text

| Exhibit | Type | Script | Output File |
|---------|------|--------|-------------|
| Figure 1 | Recyclable Waste Volume and Values | `02_main_plots.R` | `waste_volume_balance.png` |
| Figure 2 | Material Imbalance (Production vs. Consumption) | `02_main_plots.R` | `miso_f78_f89.png` |
| Figure 3 | Material Imbalance (Products' Life-Span) | `04_appendix_plots.R` | `miso_f78_f1011.png` |
| Table 1 | EU Waste Generation and Recycling Shares | `02_main_plots.R` | (inline in paper) |
| Figure 4 | EU Recycling Rates and Waste Exports | `02_main_plots.R` | `eu_waste.png` |
| Table 2 | Relative Material Imbalance and Waste Trade (PPMLE) | `13_tables_results.R` | (LaTeX output) |
| Table 3 | Heterogeneous Effects by Materials | `13_tables_results.R` | (LaTeX output) |

### Appendix

| Exhibit | Type | Script | Output File |
|---------|------|--------|-------------|
| Figure A.1 | rPET Market Prices | `02_main_plots.R` | `pet_prices.png` |
| Figure A.2 | Chinese Recycling Rates | `05_desc_miso.R` | `chinese recycling rates .png` |
| Figure A.3 | Robustness: Imbalance Definitions | `13_tables_results.R` | `rob_imbalance_pooled.png` |
| Figure A.4 | Robustness: Different Estimators | `13_tables_results.R` | `rob_estimators_pooled.png` |
| Figure A.5 | Robustness: Estimators by Product | `13_tables_results.R` | `rob_estimators_split.png` |
| Table A.1 | EU Recycling Shares (Balanced Sample) | `02_main_plots.R` | (inline in paper) |
| Table A.2 | Restricted Sample PPMLE | `13_tables_results.R` | (LaTeX output) |

### Scripts Not Producing Paper Exhibits

| Script | Purpose |
|--------|---------|
| `00_setup.R` | Path configuration and data file checks |
| `01_load_data.R` | Build all intermediate datasets from raw data |
| `03_regressions.R` | Recycling rate elasticity, gravity, and DID regressions |
| `05_desc_miso.R` | MISO descriptive statistics and raw material imbalance |
| `05b_reg_gravity.R` | MISO-gravity regressions |
| `06_gravity_log_diff_pooled.R` | ZIP models, pooled log-differences |
| `07_gravity_log_diff_split.R` | ZIP models, split by product |
| `08_gravity_log_diff_stacked.R` | ZIP models, stacked log-differences |
| `09_poisson_log_diff.R` | Poisson models, log-differences |
| `10_gravity_zip.R` | ZIP models, absolute differences |
| `11_ols_log_diff.R` | OLS models, log-differences |
| `12_log_ols_log_diff.R` | Log-OLS models, log-differences |
| `14_agg_estimates.R` | Aggregate estimates across specifications |
| `_export_gravity_db.R` | Documents PostgreSQL gravity export (one-time, not part of pipeline) |

## License

MIT License. See [LICENSE](LICENSE).
