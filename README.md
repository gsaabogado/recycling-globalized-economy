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
| `scales` | Axis formatting |

For exact package versions used by the authors, restore the environment with:

```r
install.packages("renv")
renv::restore()
```

Or install manually:

```r
install.packages(c("tidyverse", "fixest", "glmmTMB", "vroom", "readxl",
                   "countrycode", "conflicted", "NatParksPalettes", "cowplot",
                   "ggpubr", "kableExtra", "broom", "broom.mixed", "parallel",
                   "scales"))
```

## Data

All data are provided via Zenodo at **[10.5281/zenodo.18935716](https://zenodo.org/records/18935716)**. The Zenodo archive contains two folders:

- **`in.zip`** — all raw and auxiliary input files (unzip into project root so `in/` appears alongside `scripts/`)
- **`out.zip`** — all pre-processed intermediate datasets (unzip into project root so `out/` appears alongside `scripts/`)

See [`README_data.md`](README_data.md) for descriptions of the original data sources.

### Raw data files (`in.zip`)

| File | Source | Description |
|------|--------|-------------|
| `trade/codebook.xlsx` | Authors | Waste product and country classification |
| `trade/baci_hs_codes.csv` | CEPII | HS code descriptions |
| `trade/country_codes_V202401b.csv` | CEPII | BACI country codes |
| `trade/product_codes_HS96_V202401b.csv` | CEPII | BACI product codes (HS96) |
| `trade/BACI_HS96/` | CEPII | Annual trade data 1996–2022 (~10 GB) |
| `trade/gravity_cepii.csv` | CEPII | Gravity database CSV export (~300 MB) |
| `material_imbalance/miso_062024.xlsx` | Wiedenhofer et al. | MISO2 data extract (June 2024) |
| `material_imbalance/miso_072024.xlsx` | Wiedenhofer et al. | MISO2 data extract (July 2024) |
| `material_imbalance/MISO2_allflows_noendofuse.xlsx` | Wiedenhofer et al. | MISO2 full material flows (~27 MB) |
| `recycling_rates/env_waspac.csv` | EUROSTAT | Packaging waste statistics |
| `trade_costs/` | ESCAP/World Bank | Trade cost database (~178 MB) |
| `chinese_recycling_rates/China recycling 2015-2023.xlsx` | Authors | Chinese recycling rates |
| `rPET_prices.xlsx` | Authors | rPET market prices (2016–2022) |

### Intermediate datasets (`out.zip`)

The archive contains pre-processed files organized by which pipeline stage produces them:

| File | Produced by | Used by |
|------|-------------|---------|
| `out/trade/baci_raw.rds` | `load_data.R` | `gravity_log_diff_pooled.R` through `log_ols_log_diff.R` |
| `out/trade/gravity_data.rds` | `load_data.R` | `main_plots.R` |
| `out/trade/balance_data.rds` | `load_data.R` | `regressions.R` |
| `out/miso/material_imbalance.rds` | `load_data.R` | `main_plots.R` |
| `out/miso/raw_material_imbalance.rds` | `desc_miso.R` | `gravity_log_diff_pooled.R` through `log_ols_log_diff.R` |
| `out/macro/gravity_controls.rds` | `load_data.R` | `gravity_log_diff_pooled.R` through `log_ols_log_diff.R` |
| `out/eu_packaging_waste.rds` | `load_data.R` | `regressions.R` |
| `out/wb_tradecost.rds` | `load_data.R` | `regressions.R` |
| `out/results/data_gravity_pooled_log_differences.rds` | `gravity_log_diff_pooled.R` | `poisson_log_diff.R`, `ols_log_diff.R`, `log_ols_log_diff.R` |
| `out/results/data_gravity_stacked_log_differences.rds` | `gravity_log_diff_stacked.R` | `poisson_log_diff.R`, `ols_log_diff.R`, `log_ols_log_diff.R` |
| `out/results/poisson_pooled.rds` | `poisson_log_diff.R` | `tables_results.R`, `agg_estimates.R` |
| `out/results/poisson_split.rds` | `poisson_log_diff.R` | `tables_results.R`, `agg_estimates.R` |
| `out/results/poisson_stacked.rds` | `poisson_log_diff.R` | `agg_estimates.R` |
| `out/results/poisson_pooled_post_2006_only_eu.rds` | `poisson_log_diff.R` | `tables_results.R` |
| `out/results/ols_pooled.rds` | `ols_log_diff.R` | `agg_estimates.R` |
| `out/results/ols_split.rds` | `ols_log_diff.R` | `agg_estimates.R` |
| `out/results/ols_stacked.rds` | `ols_log_diff.R` | `agg_estimates.R` |
| `out/results/log_ols_pooled.rds` | `log_ols_log_diff.R` | `agg_estimates.R` |
| `out/results/log_ols_split.rds` | `log_ols_log_diff.R` | `agg_estimates.R` |
| `out/results/log_ols_stacked.rds` | `log_ols_log_diff.R` | `agg_estimates.R` |
| `out/results/all_est_log_diff.rds` | `agg_estimates.R` | `robustness_plots.R` |

## How to Run

### Option A: Full replication from raw data

For complete from-scratch replication, download all raw data (see `README_data.md`) and run:

```r
source("run_all.R")
```

**Note on computational intensity:** The zero-inflated Poisson models (`gravity_log_diff_pooled.R`, `gravity_log_diff_split.R`, `gravity_log_diff_stacked.R`, `gravity_zip.R`) use bootstrapped standard errors and were originally run on an HPC cluster. On a standard machine, these scripts may take many hours. The PPMLE models (`poisson_log_diff.R`) produce slightly different numerical results on systems using Intel MKL versus Apple Accelerate BLAS. Pre-computed PPMLE outputs from the authors' Mac are provided on Zenodo.

### Option B: Reproduce paper exhibits only (recommended)

If you only want to verify the tables and figures in the paper, download `out.zip` from Zenodo and skip the heavy computation:

```r
# After unzipping out.zip into the project root:

source("scripts/setup.R")           # Creates output directories

source("scripts/main_plots.R")      # Figures 1-4, Table 1, Figure A.1
source("scripts/desc_miso.R")       # Figure A.2
source("scripts/agg_estimates.R")   # Aggregates model results
source("scripts/robustness_plots.R") # Figures A.3-A.5
source("scripts/tables_results.R")  # Tables 4, 5, A.4
```

All output goes to `images/figures/` and `images/tables/`.

## Repository Structure

```
.
├── README.md
├── README_data.md                    # Data source documentation
├── LICENSE
├── run_all.R                         # Master orchestration script
├── renv.lock                         # Pinned package versions
├── r_project.Rproj
├── scripts/                          # All analysis scripts
│   ├── setup.R
│   ├── load_data.R
│   ├── main_plots.R
│   ├── ...
│   └── export_gravity_db.R
├── in/                               # Raw data (download in.zip from Zenodo)
├── out/                              # Intermediate datasets (download out.zip from Zenodo)
└── images/                           # Figures and tables (regenerated by scripts)
```

## Exhibit Map

### Main Text

| Exhibit | Description | Script | Output File |
|---------|-------------|--------|-------------|
| Figure 1 | Recyclable Waste Volume and Values | `main_plots.R` | `waste_volume_balance.png` |
| Figure 2 | Material Imbalance (Production vs. Consumption) | `main_plots.R` | `miso_f78_f89.png` |
| Figure 3 | Material Imbalance (Products' Life-Span) | `main_plots.R` | `miso_f78_f1011.png` |
| Table 1 | EU Waste Generation and Recycling Shares | `main_plots.R` | (inline in paper) |
| Figure 4 | EU Recycling Rates and Waste Exports | `main_plots.R` | `eu_waste.png` |
| Table 4 | Relative Material Imbalance and Waste Trade (PPMLE) | `tables_results.R` | `table4_ppmle_pooled.tex` |
| Table 5 | Heterogeneous Effects by Materials | `tables_results.R` | `table5_ppmle_split.tex` |

### Appendix

| Exhibit | Description | Script | Output File |
|---------|-------------|--------|-------------|
| Figure A.1 | rPET Market Prices | `main_plots.R` | `pet_prices.png` |
| Figure A.2 | Chinese Recycling Rates | `desc_miso.R` | `chinese recycling rates .png` |
| Figure A.3 | Robustness: Imbalance Definitions | `robustness_plots.R` | `rob_imbalance_pooled.png` |
| Figure A.4 | Robustness: Different Estimators | `robustness_plots.R` | `rob_estimators_pooled.png` |
| Figure A.5 | Robustness: Estimators by Product | `robustness_plots.R` | `rob_estimators_split.png` |
| Table A.2 | EU Recycling Shares (Balanced Sample) | `main_plots.R` | (inline in paper) |
| Table A.4 | EU-Restricted Sample PPMLE | `tables_results.R` | `tableA4_eu_restricted.tex` |

### Scripts Not Producing Paper Exhibits

| Script | Purpose |
|--------|---------|
| `setup.R` | Path configuration and data file checks |
| `load_data.R` | Build all intermediate datasets from raw data |
| `regressions.R` | Recycling rate elasticity, gravity, and DID regressions |
| `appendix_plots.R` | Exploratory figures from earlier draft; not in current pipeline |
| `reg_gravity.R` | MISO-gravity regressions |
| `gravity_log_diff_pooled.R` | ZIP models, pooled log-differences |
| `gravity_log_diff_split.R` | ZIP models, split by product |
| `gravity_log_diff_stacked.R` | ZIP models, stacked log-differences |
| `poisson_log_diff.R` | PPMLE models, log-differences (produces Tables 4, 5, A.4 inputs) |
| `gravity_zip.R` | ZIP models, absolute differences |
| `ols_log_diff.R` | OLS models, log-differences |
| `log_ols_log_diff.R` | Log-OLS models, log-differences |
| `agg_estimates.R` | Aggregate estimates across specifications (input for Fig A.3-A.5) |
| `export_gravity_db.R` | Documents PostgreSQL gravity export (one-time, not part of pipeline) |

## License

MIT License. See [LICENSE](LICENSE).
