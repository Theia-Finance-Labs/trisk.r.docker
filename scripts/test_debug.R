#!/usr/bin/env Rscript
# Debug script to diagnose the production_year assertion error

library(trisk.model)
library(trisk.analysis)
library(dplyr)

cat("=== Loading test data ===\n")
assets <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
carbon <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))

cat("\n=== Assets production_year range ===\n")
cat("Min:", min(assets$production_year), "Max:", max(assets$production_year), "\n")
cat("Unique years:", paste(sort(unique(assets$production_year)), collapse = ", "), "\n")
cat("Sectors:", paste(unique(assets$sector), collapse = ", "), "\n")
cat("Technologies:", paste(unique(assets$technology), collapse = ", "), "\n")

cat("\n=== Test scenarios year range ===\n")
cat("Min:", min(scenarios$scenario_year), "Max:", max(scenarios$scenario_year), "\n")
cat("Scenarios:", paste(unique(scenarios$scenario), collapse = ", "), "\n")

# Check if pre-downloaded GCS scenarios exist
gcs_path <- "/opt/trisk/data/scenarios/scenarios.csv"
if (file.exists(gcs_path)) {
  cat("\n=== GCS scenarios found ===\n")
  gcs_scenarios <- read.csv(gcs_path)
  cat("Min year:", min(gcs_scenarios$scenario_year), "Max year:", max(gcs_scenarios$scenario_year), "\n")
  cat("All scenarios:", paste(unique(gcs_scenarios$scenario), collapse = ", "), "\n")
  cat("Geographies:", paste(unique(gcs_scenarios$scenario_geography), collapse = ", "), "\n")

  if ("scenario_type" %in% names(gcs_scenarios)) {
    cat("Scenario types:", paste(unique(gcs_scenarios$scenario_type), collapse = ", "), "\n")
  }

  # Check each baseline scenario's year range
  cat("\n=== Per-scenario year ranges (GCS) ===\n")
  for (s in unique(gcs_scenarios$scenario)) {
    sub <- gcs_scenarios[gcs_scenarios$scenario == s,]
    cat(sprintf("  %s: %d-%d\n", s, min(sub$scenario_year), max(sub$scenario_year)))
  }

  # Test with GCS scenarios + test assets
  cat("\n=== Testing with GCS scenarios + test assets ===\n")

  # Find baseline scenarios
  if ("scenario_type" %in% names(gcs_scenarios)) {
    baselines <- unique(gcs_scenarios$scenario[tolower(gcs_scenarios$scenario_type) == "baseline"])
    targets <- unique(gcs_scenarios$scenario[tolower(gcs_scenarios$scenario_type) == "target"])
    cat("Baseline scenarios:", paste(baselines, collapse = ", "), "\n")
    cat("Target scenarios:", paste(targets, collapse = ", "), "\n")
  }

  # Try running with first available baseline and target
  cat("\n=== Attempting run_trisk_model with GCS scenarios ===\n")
  tryCatch({
    result <- run_trisk_model(
      assets_data = assets,
      scenarios_data = gcs_scenarios,
      financial_data = financial,
      carbon_data = carbon,
      baseline_scenario = baselines[1],
      target_scenario = targets[1],
      scenario_geography = "Global"
    )
    cat("SUCCESS! Result names:", paste(names(result), collapse = ", "), "\n")
    cat("NPV rows:", nrow(result$npv_results), "\n")
    cat("PD rows:", nrow(result$pd_results), "\n")
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
  })
} else {
  cat("\n=== No GCS scenarios found, using test scenarios ===\n")
}

# Test 1: run_trisk_model with pure test data (should work)
cat("\n=== Test 1: run_trisk_model with pure test data ===\n")
tryCatch({
  result1 <- run_trisk_model(
    assets_data = assets,
    scenarios_data = scenarios,
    financial_data = financial,
    carbon_data = carbon,
    baseline_scenario = "NGFS2023GCAM_CP",
    target_scenario = "NGFS2023GCAM_NZ2050",
    scenario_geography = "Global"
  )
  cat("SUCCESS! NPV rows:", nrow(result1$npv_results), "PD rows:", nrow(result1$pd_results), "\n")
  cat("NPV columns:", paste(names(result1$npv_results), collapse = ", "), "\n")
  cat("PD columns:", paste(names(result1$pd_results), collapse = ", "), "\n")
}, error = function(e) {
  cat("FAILED:", e$message, "\n")
})

# Test 2: run_trisk_on_portfolio with test data
cat("\n=== Test 2: run_trisk_on_portfolio with test data ===\n")

# Load portfolio test data
portfolio <- NULL
for (f in c("portfolio_ids_testdata.csv", "portfolio_names_testdata.csv", "simple_portfolio.csv")) {
  path <- system.file("testdata", f, package = "trisk.analysis", mustWork = FALSE)
  if (nzchar(path) && file.exists(path)) {
    portfolio <- read.csv(path)
    cat("Loaded portfolio from:", f, "\n")
    cat("Portfolio columns:", paste(names(portfolio), collapse = ", "), "\n")
    cat("Portfolio rows:", nrow(portfolio), "\n")
    if ("company_id" %in% names(portfolio)) {
      cat("Portfolio company_ids:", paste(unique(portfolio$company_id), collapse = ", "), "\n")
    }
    break
  }
}

if (!is.null(portfolio)) {
  # Check overlap between portfolio companies and assets companies
  if ("company_id" %in% names(portfolio) && "company_id" %in% names(assets)) {
    common <- intersect(as.character(portfolio$company_id), as.character(assets$company_id))
    cat("Common company_ids between portfolio and assets:", length(common), "\n")
    cat("  Portfolio IDs:", paste(unique(as.character(portfolio$company_id)), collapse = ", "), "\n")
    cat("  Assets IDs:", paste(unique(as.character(assets$company_id)), collapse = ", "), "\n")
  }

  tryCatch({
    result2 <- run_trisk_on_portfolio(
      assets_data = assets,
      scenarios_data = scenarios,
      financial_data = financial,
      carbon_data = carbon,
      portfolio_data = portfolio,
      baseline_scenario = "NGFS2023GCAM_CP",
      target_scenario = "NGFS2023GCAM_NZ2050",
      scenario_geography = "Global"
    )
    cat("SUCCESS! Result rows:", nrow(result2), "\n")
    cat("Result columns:", paste(names(result2), collapse = ", "), "\n")
  }, error = function(e) {
    cat("FAILED:", e$message, "\n")
    cat("Full error:\n")
    traceback()
  })
} else {
  cat("No portfolio test data found\n")
}

cat("\n=== Debug complete ===\n")
