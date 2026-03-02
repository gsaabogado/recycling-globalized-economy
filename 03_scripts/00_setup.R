#### ..................................................................... ####
####      Setup: paths, output directories, and data file checks           ####
#### ..................................................................... ####

#### ── Output directories ── ####
dir.create("02_gen/01_trade",   recursive = TRUE, showWarnings = FALSE)
dir.create("02_gen/02_miso",    recursive = TRUE, showWarnings = FALSE)
dir.create("02_gen/03_macro",   recursive = TRUE, showWarnings = FALSE)
dir.create("02_gen/04_results", recursive = TRUE, showWarnings = FALSE)
dir.create("04_output/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("04_output/tables",  recursive = TRUE, showWarnings = FALSE)

#### ── Define figure and table output paths ── ####
fig_dir   <- "04_output/figures/"
table_dir <- "04_output/tables/"

#### ── Required data files ── ####
required_files <- c(
  # BACI trade data (at least one year file must exist)
  "01_data/01_trade/BACI_HS96/BACI_HS96_Y2000_V202401b.csv",
  # Codebook
  "01_data/01_trade/codebook.xlsx",
  # HS codes
  "01_data/01_trade/baci_hs_codes.csv",
  # Country and product codes
  "01_data/01_trade/country_codes_V202401b.csv",
  "01_data/01_trade/product_codes_HS96_V202401b.csv",
  # Gravity data (exported from PostgreSQL; see _export_gravity_db.R)
  "01_data/01_trade/gravity_cepii.csv",
  # MISO material imbalance data
  "01_data/02_material_imbalance/miso_062024.xlsx",
  "01_data/02_material_imbalance/miso_072024.xlsx",
  # MISO2 full flows (for appendix plots)
  "01_data/02_material_imbalance/MISO2_allflows_noendofuse.xlsx",
  # EU packaging waste / recycling rates
  "01_data/03_recycling_rates/env_waspac.csv",
  # Trade costs
  "01_data/05_trade_costs/escap_wb_tradecost_19952010.xlsx",
  "01_data/05_trade_costs/escap_wb_tradecost_20112022.xlsx",
  # Chinese recycling rates
  "01_data/06_chinese_recycling_rates/China recycling 2015-2023.xlsx",
  # rPET prices
  "01_data/rPET_prices.xlsx"
)

#### ── Check that all required files exist ── ####
missing <- required_files[!file.exists(required_files)]

if (length(missing) > 0) {
  cat("WARNING: The following required data files are missing:\n")
  cat(paste0("  - ", missing, "\n"), sep = "")
  cat("\nSee 01_data/README_data.md for download instructions.\n")
} else {
  cat("All required data files found.\n")
}

cat("00_setup.R completed successfully.\n")
