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

All data are provided via Zenodo at **[DOI TBD]**. The Zenodo archive contains two folders:

- **`01_data.zip`** — all raw and auxiliary input files (unzip into project root so `01_data/` appears alongside `03_scripts/`)
- **`02_gen.zip`** — all pre-processed intermediate datasets (unzip into project root so `02_gen/` appears alongside `03_scripts/`)

See [`01_data/README_data.md`](01_data/README_data.md) for descriptions of the original data sources.

### Raw data files (`01_data.zip`)

| File | Source | Description |
|------|--------|-------------|
| `01_trade/codebook.xlsx` | Authors | Waste product and country classification |
| `01_trade/baci_hs_codes.csv` | CEPII | HS code descriptions |
| `01_trade/country_codes_V202401b.csv` | CEPII | BACI country codes |
| `01_trade/product_codes_HS96_V202401b.csv` | CEPII | BACI product codes (HS96) |
| `01_trade/BACI_HS96/` | CEPII | Annual trade data 1996–2022 (~10 GB) |
| `01_trade/gravity_cepii.csv` | CEPII | Gravity database CSV export (~300 MB) |
| `02_material_imbalance/miso_062024.xlsx` | Wiedenhofer et al. | MISO2 data extract (June 2024) |
| `02_material_imbalance/miso_072024.xlsx` | Wiedenhofer et al. | MISO2 data extract (July 2024) |
| `02_material_imbalance/MISO2_allflows_noendofuse.xlsx` | Wiedenhofer et al. | MISO2 full material flows (~27 MB) |
| `03_recycling_rates/env_waspac.csv` | EUROSTAT | Packaging waste statistics |
| `05_trade_costs/` | ESCAP/World Bank | Trade cost database (~178 MB) |
| `06_chinese_recycling_rates/China recycling 2015-2023.xlsx` | Authors | Chinese recycling rates |
| `rPET_prices.xlsx` | Authors | rPET market prices (2016–2022) |

### Intermediate datasets (`02_gen.zip`)

The archive contains pre-processed files organized by which pipeline stage produces them:

| File | Produced by | Used by |
|------|-------------|---------|
| `02_gen/01_trade/baci_raw.rds` | `01_load_data.R` | scripts 06-12 |
| `02_gen/01_trade/gravity_data.rds` | `01_load_data.R` | `02_main_plots.R` |
| `02_gen/01_trade/balance_data.rds` | `01_load_data.R` | `03_regressions.R` |
| `02_gen/02_miso/material_imbalance.rds` | `01_load_data.R` | `02_main_plots.R` |
| `02_gen/02_miso/raw_material_imbalance.rds` | `05_desc_miso.R` | scripts 06-12 |
| `02_gen/03_macro/gravity_controls.rds` | `01_load_data.R` | scripts 06-12 |
| `02_gen/eu_packaging_waste.rds` | `01_load_data.R` | `03_regressions.R` |
| `02_gen/wb_tradecost.rds` | `01_load_data.R` | `03_regressions.R` |
| `02_gen/04_results/data_gravity_pooled_log_differences.rds` | `06_gravity_log_diff_pooled.R` | scripts 09, 11, 12 |
| `02_gen/04_results/data_gravity_stacked_log_differences.rds` | `08_gravity_log_diff_stacked.R` | scripts 09, 11, 12 |
| `02_gen/04_results/poisson_pooled.rds` | `09_poisson_log_diff.R` | `13_tables_results.R`, `14_agg_estimates.R` |
| `02_gen/04_results/poisson_split.rds` | `09_poisson_log_diff.R` | `13_tables_results.R`, `14_agg_estimates.R` |
| `02_gen/04_results/poisson_stacked.rds` | `09_poisson_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/poisson_pooled_post_2006_only_eu.rds` | `09_poisson_log_diff.R` | `13_tables_results.R` |
| `02_gen/04_results/ols_pooled.rds` | `11_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/ols_split.rds` | `11_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/ols_stacked.rds` | `11_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/log_ols_pooled.rds` | `12_log_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/log_ols_split.rds` | `12_log_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/log_ols_stacked.rds` | `12_log_ols_log_diff.R` | `14_agg_estimates.R` |
| `02_gen/04_results/all_est_log_diff.rds` | `14_agg_estimates.R` | `15_robustness_plots.R` |

## How to Run

### Option A: Full replication from raw data

For complete from-scratch replication, download all raw data (see `01_data/README_data.md`) and run:

```r
source("run_all.R")
```

**Note on computational intensity:** The zero-inflated Poisson models (scripts 06-08, 10) use bootstrapped standard errors and were originally run on an HPC cluster. On a standard machine, these scripts may take many hours. The PPMLE models (script 09) produce slightly different numerical results on systems using Intel MKL versus Apple Accelerate BLAS. Pre-computed PPMLE outputs from the authors' Mac are provided on Zenodo.

### Option B: Reproduce paper exhibits only (recommended)

If you only want to verify the tables and figures in the paper, download `02_gen.zip` from Zenodo and skip the heavy computation:

```r
# After unzipping 02_gen.zip into the project root:

source("03_scripts/02_main_plots.R")      # Figures 1-4, Table 1, Figure A.1
source("03_scripts/05_desc_miso.R")       # Figure A.2
source("03_scripts/14_agg_estimates.R")   # Aggregates model results
source("03_scripts/15_robustness_plots.R") # Figures A.3-A.5
source("03_scripts/13_tables_results.R")  # Tables 4, 5, A.4
```

All output goes to `04_output/figures/` and `04_output/tables/`.

## Repository Structure

```
.
├── README.md
├── LICENSE
├── run_all.R                         # Master orchestration script
├── renv.lock                         # Pinned package versions
├── r_project.Rproj
├── 01_data/                          # Raw data (all files on Zenodo; gitignored)
│   ├── README_data.md
│   ├── 01_trade/
│   ├── 02_material_imbalance/
│   ├── 03_recycling_rates/
│   ├── 05_trade_costs/
│   └── 06_chinese_recycling_rates/
├── 02_gen/                           # Intermediate datasets (gitignored; download from Zenodo)
│   ├── 01_trade/
│   ├── 02_miso/
│   ├── 03_macro/
│   └── 04_results/
├── 03_scripts/                       # All analysis scripts
│   ├── 00_setup.R
│   ├── 01_load_data.R
│   ├── 02_main_plots.R
│   ├── ...
│   └── _export_gravity_db.R
└── 04_output/                        # Figures and tables (gitignored; regenerated by scripts)
    ├── figures/
    └── tables/
```

## Exhibit Map

### Main Text

| Exhibit | Description | Script | Output File |
|---------|-------------|--------|-------------|
| Figure 1 | Recyclable Waste Volume and Values | `02_main_plots.R` | `waste_volume_balance.png` |
| Figure 2 | Material Imbalance (Production vs. Consumption) | `02_main_plots.R` | `miso_f78_f89.png` |
| Figure 3 | Material Imbalance (Products' Life-Span) | `02_main_plots.R` | `miso_f78_f1011.png` |
| Table 1 | EU Waste Generation and Recycling Shares | `02_main_plots.R` | (inline in paper) |
| Figure 4 | EU Recycling Rates and Waste Exports | `02_main_plots.R` | `eu_waste.png` |
| Table 4 | Relative Material Imbalance and Waste Trade (PPMLE) | `13_tables_results.R` | `table4_ppmle_pooled.tex` |
| Table 5 | Heterogeneous Effects by Materials | `13_tables_results.R` | `table5_ppmle_split.tex` |

### Appendix

| Exhibit | Description | Script | Output File |
|---------|-------------|--------|-------------|
| Figure A.1 | rPET Market Prices | `02_main_plots.R` | `pet_prices.png` |
| Figure A.2 | Chinese Recycling Rates | `05_desc_miso.R` | `chinese recycling rates .png` |
| Figure A.3 | Robustness: Imbalance Definitions | `15_robustness_plots.R` | `rob_imbalance_pooled.png` |
| Figure A.4 | Robustness: Different Estimators | `15_robustness_plots.R` | `rob_estimators_pooled.png` |
| Figure A.5 | Robustness: Estimators by Product | `15_robustness_plots.R` | `rob_estimators_split.png` |
| Table A.2 | EU Recycling Shares (Balanced Sample) | `02_main_plots.R` | (inline in paper) |
| Table A.4 | EU-Restricted Sample PPMLE | `13_tables_results.R` | `tableA4_eu_restricted.tex` |

### Scripts Not Producing Paper Exhibits

| Script | Purpose |
|--------|---------|
| `00_setup.R` | Path configuration and data file checks |
| `01_load_data.R` | Build all intermediate datasets from raw data |
| `03_regressions.R` | Recycling rate elasticity, gravity, and DID regressions |
| `04_appendix_plots.R` | Exploratory figures from earlier draft; not in current pipeline |
| `05b_reg_gravity.R` | MISO-gravity regressions |
| `06_gravity_log_diff_pooled.R` | ZIP models, pooled log-differences |
| `07_gravity_log_diff_split.R` | ZIP models, split by product |
| `08_gravity_log_diff_stacked.R` | ZIP models, stacked log-differences |
| `09_poisson_log_diff.R` | PPMLE models, log-differences (produces Tables 4, 5, A.4 inputs) |
| `10_gravity_zip.R` | ZIP models, absolute differences |
| `11_ols_log_diff.R` | OLS models, log-differences |
| `12_log_ols_log_diff.R` | Log-OLS models, log-differences |
| `14_agg_estimates.R` | Aggregate estimates across specifications (input for Fig A.3-A.5) |
| `_export_gravity_db.R` | Documents PostgreSQL gravity export (one-time, not part of pipeline) |

## License

MIT License. See [LICENSE](LICENSE).
