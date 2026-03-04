# Data Download Instructions

This file describes how to obtain the raw data files required to replicate the results in "Recycling in a Globalised Economy." Small data files are included in the repository. Large files must be downloaded separately and placed in the directories described below.

## 1. BACI Trade Data (HS96)

- **Source:** CEPII (Centre d'Etudes Prospectives et d'Informations Internationales)
- **URL:** http://www.cepii.fr/CEPII/en/bdd_modele/bdd_modele_item.asp?id=37
- **Version:** V202401b, HS96 classification
- **Size:** ~10 GB (27 annual CSV files, 1996-2022)
- **Placement:** Download all `BACI_HS96_Y*_V202401b.csv` files into:
  ```
  01_data/01_trade/BACI_HS96/
  ```
- **Reference:** Gaulier, G. and Zignago, S. (2010). "BACI: International Trade Database at the Product-Level. The 1994-2007 Version." CEPII Working Paper, N.2010-23.

## 2. CEPII Gravity Database

- **Source:** CEPII Gravity Database, compiled by Pacha (2024)
- **URL:** https://github.com/pachadotdev/gravitydatasets
- **Size:** ~300 MB (CSV export)
- **How to obtain:** This file is a CSV export of the `cepii_gravity` table from the PostgreSQL gravity database. To produce it:
  1. Download the SQL dump from the URL above
  2. Load it into a PostgreSQL database
  3. Run `03_scripts/_export_gravity_db.R` to export the table to CSV
- **Placement:** Place the exported file at:
  ```
  01_data/01_trade/gravity_cepii.csv
  ```
- **Note:** This is a one-time prerequisite. The SQL dump (`gravitydatasets.sql`) is ~900 MB and is not included in the repository.

## 3. MISO2 Full Material Flows

- **Source:** Wiedenhofer et al. (2024)
- **URL:** https://zenodo.org/records/12772223
- **Files needed:** `MISO2_allflows_noendofuse.xlsx` (~27 MB)
- **Placement:**
  ```
  01_data/02_material_imbalance/MISO2_allflows_noendofuse.xlsx
  ```
- **Reference:** Wiedenhofer, D., Streeck, J., Wieland, H., et al. (2024). "From Extraction to End-uses and Waste Management: Modelling Economy-wide Material Cycles and Stock Dynamics Around the World." Available at SSRN 4794611.

## 4. ESCAP/World Bank Trade Cost Database

- **Source:** ESCAP-World Bank Trade Cost Database
- **URL:** https://www.unescap.org/resources/escap-world-bank-trade-cost-database
- **Size:** ~178 MB (two Excel files)
- **Files needed:**
  - `escap_wb_tradecost_19952010.xlsx`
  - `escap_wb_tradecost_20112022.xlsx`
- **Placement:**
  ```
  01_data/05_trade_costs/escap_wb_tradecost_19952010.xlsx
  01_data/05_trade_costs/escap_wb_tradecost_20112022.xlsx
  ```

## Files Included in the Repository

The following files are small enough to be committed and do not require separate download:

| File | Size | Description |
|------|------|-------------|
| `01_trade/codebook.xlsx` | 32 KB | Waste product codes and country classification |
| `01_trade/baci_hs_codes.csv` | 1.1 MB | HS code descriptions |
| `01_trade/country_codes_V202401b.csv` | 5 KB | BACI country codes |
| `01_trade/product_codes_HS96_V202401b.csv` | 530 KB | BACI product codes (HS96) |
| `02_material_imbalance/miso_062024.xlsx` | 2.5 MB | MISO2 data extract (June 2024) |
| `02_material_imbalance/miso_072024.xlsx` | 5 MB | MISO2 data extract (July 2024) |
| `03_recycling_rates/env_waspac.csv` | 8.5 MB | EUROSTAT packaging waste statistics |
| `06_chinese_recycling_rates/China recycling 2015-2023.xlsx` | 37 KB | Chinese recycling rates |
| `rPET_prices.xlsx` | 11 KB | rPET market prices (2016-2022) |

## Verification

After downloading all files, run `source("03_scripts/00_setup.R")` from the project root to verify that all required files are in place.
