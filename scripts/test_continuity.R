#!/usr/bin/env Rscript
# Test 1: Adapted from test_output_continuity.R
# Runs run_trisk_model() with pure test data to verify model works

library(trisk.model)
library(dplyr)

cat("============================================\n")
cat("Test 1: run_trisk_model continuity test\n")
cat("============================================\n\n")

# Load test datasets
assets <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
carbon <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))

cat("Data loaded:\n")
cat("  Assets:", nrow(assets), "rows, years:", min(assets$production_year), "-", max(assets$production_year), "\n")
cat("  Scenarios:", nrow(scenarios), "rows, years:", min(scenarios$scenario_year), "-", max(scenarios$scenario_year), "\n")
cat("  Financial:", nrow(financial), "rows\n")
cat("  Carbon:", nrow(carbon), "rows\n")

cat("\nRunning run_trisk_model()...\n")
result <- tryCatch({
  run_trisk_model(
    assets_data = assets,
    scenarios_data = scenarios,
    financial_data = financial,
    carbon_data = carbon,
    baseline_scenario = "NGFS2023GCAM_CP",
    target_scenario = "NGFS2023GCAM_NZ2050",
    scenario_geography = "Global"
  )
}, error = function(e) {
  cat("FAILED:", e$message, "\n")
  quit(status = 1)
})

cat("\nSUCCESS!\n")
cat("Result components:", paste(names(result), collapse = ", "), "\n")
cat("NPV results:", nrow(result$npv_results), "rows\n")
cat("NPV columns:", paste(names(result$npv_results), collapse = ", "), "\n")
cat("PD results:", nrow(result$pd_results), "rows\n")
cat("PD columns:", paste(names(result$pd_results), collapse = ", "), "\n")
cat("Trajectories:", nrow(result$company_trajectories), "rows\n")

# Basic sanity checks
stopifnot(nrow(result$npv_results) > 0)
stopifnot(nrow(result$pd_results) > 0)
stopifnot("crispy_perc_value_change" %in% names(result$npv_results))
stopifnot("pd_baseline" %in% names(result$pd_results))
stopifnot("pd_shock" %in% names(result$pd_results))

cat("\nAll sanity checks passed!\n")
cat("============================================\n\n")
