#### ..................................................................... ####
####      Setup: paths, output directories, and data file checks           ####
#### ..................................................................... ####

#### ── Output directories ── ####
dir.create("out/trade",   recursive = TRUE, showWarnings = FALSE)
dir.create("out/miso",    recursive = TRUE, showWarnings = FALSE)
dir.create("out/macro",   recursive = TRUE, showWarnings = FALSE)
dir.create("out/results", recursive = TRUE, showWarnings = FALSE)
dir.create("images/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("images/tables",  recursive = TRUE, showWarnings = FALSE)

#### ── Define figure and table output paths ── ####
fig_dir   <- "images/figures/"
table_dir <- "images/tables/"

#### ── Required data files ── ####
required_files <- c(
  # BACI trade data (at least one year file must exist)
  "in/trade/BACI_HS96/BACI_HS96_Y2000_V202401b.csv",
  # Codebook
  "in/trade/codebook.xlsx",
  # HS codes
  "in/trade/baci_hs_codes.csv",
  # Country and product codes
  "in/trade/country_codes_V202401b.csv",
  "in/trade/product_codes_HS96_V202401b.csv",
  # Gravity data (exported from PostgreSQL; see _export_gravity_db.R)
  "in/trade/gravity_cepii.csv",
  # MISO material imbalance data
  "in/material_imbalance/miso_062024.xlsx",
  "in/material_imbalance/miso_072024.xlsx",
  # MISO2 full flows (for appendix plots)
  "in/material_imbalance/MISO2_allflows_noendofuse.xlsx",
  # EU packaging waste / recycling rates
  "in/recycling_rates/env_waspac.csv",
  # Trade costs
  "in/trade_costs/escap_wb_tradecost_19952010.xlsx",
  "in/trade_costs/escap_wb_tradecost_20112022.xlsx",
  # Chinese recycling rates
  "in/chinese_recycling_rates/China recycling 2015-2023.xlsx",
  # rPET prices
  "in/rPET_prices.xlsx"
)

#### ── Check that all required files exist ── ####
missing <- required_files[!file.exists(required_files)]

if (length(missing) > 0) {
  cat("WARNING: The following required data files are missing:\n")
  cat(paste0("  - ", missing, "\n"), sep = "")
  cat("\nSee README_data.md for download instructions.\n")
} else {
  cat("All required data files found.\n")
}

cat("setup.R completed successfully.\n")
