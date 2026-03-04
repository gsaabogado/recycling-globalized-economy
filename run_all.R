#### ..................................................................... ####
####    Master script: Recycling in a Globalized Economy                   ####
####    Joltreau & Sarmiento                                               ####
#### ..................................................................... ####
####                                                                       ####
####  This script sources all analysis scripts in dependency order.        ####
####  Run from the project root (where r_project.Rproj lives).            ####
####                                                                       ####
####  Prerequisites:                                                       ####
####   - All raw data files downloaded (see 01_data/README_data.md)        ####
####   - gravity_cepii.csv exported (see 03_scripts/_export_gravity_db.R)  ####
####   - R packages installed (see renv.lock or README.md)                 ####
####                                                                       ####
#### ..................................................................... ####

set.seed(42)
t0 <- Sys.time()

run_script <- function(script, label = script) {
  cat(sprintf("\n===== [%s] %s =====\n", format(Sys.time(), "%H:%M:%S"), label))
  source(file.path("03_scripts", script), local = FALSE)
  cat(sprintf("  -> Finished in %.1f min\n", difftime(Sys.time(), t0, units = "mins")))
}

#### ── Step 0: Setup paths and check data files ── ####
run_script("00_setup.R", "Setup and data checks")

#### ── Step 1: Load and build all intermediate datasets ── ####
run_script("01_load_data.R", "Load raw data and build intermediate datasets")

#### ── Step 2: MISO descriptive statistics ── ####
run_script("05_desc_miso.R", "MISO material imbalance descriptives")

#### ── Step 3: Main regressions (recycling rate / gravity / DID) ── ####
run_script("03_regressions.R", "Recycling rate and gravity regressions")

#### ── Step 4: Main paper figures ── ####
run_script("02_main_plots.R", "Main figures")

#### ── Step 5: MISO-gravity regressions ── ####
run_script("05b_reg_gravity.R", "MISO gravity regressions")

#### ── Step 6: ZIP models (pooled log-differences) ── ####
run_script("06_gravity_log_diff_pooled.R", "ZIP pooled log-differences")

#### ── Step 7: ZIP models (stacked log-differences) ── ####
run_script("08_gravity_log_diff_stacked.R", "ZIP stacked log-differences")

#### ── Step 8: ZIP models (split by product) ── ####
run_script("07_gravity_log_diff_split.R", "ZIP split log-differences")

#### ── Step 9: Poisson models ── ####
run_script("09_poisson_log_diff.R", "Poisson log-differences")

#### ── Step 10: ZIP absolute differences ── ####
run_script("10_gravity_zip.R", "ZIP absolute differences")

#### ── Step 11: OLS models ── ####
run_script("11_ols_log_diff.R", "OLS log-differences")

#### ── Step 12: Log-OLS models ── ####
run_script("12_log_ols_log_diff.R", "Log-OLS log-differences")

#### ── Step 13: Aggregate estimates ── ####
run_script("14_agg_estimates.R", "Aggregate estimates")

#### ── Step 14: Robustness plots ── ####
run_script("15_robustness_plots.R", "Robustness plots (Figures A3-A5)")

#### ── Step 15: Result tables ── ####
run_script("13_tables_results.R", "Format result tables")

#### ── Done ── ####
cat(sprintf("\n===== All scripts completed in %.1f min =====\n",
            difftime(Sys.time(), t0, units = "mins")))
