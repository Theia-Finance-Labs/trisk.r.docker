#!/usr/bin/env Rscript
# Test 2: Portfolio PD output test
# Runs run_trisk_on_portfolio() with test data and validates output format
# matches what trisk.analysis expects for its plotting pipeline

library(trisk.model)
library(trisk.analysis)
library(dplyr)

cat("============================================\n")
cat("Test 2: Portfolio PD output format test\n")
cat("============================================\n\n")

# Load test datasets
assets <- read.csv(system.file("testdata", "assets_testdata.csv", package = "trisk.model"))
scenarios <- read.csv(system.file("testdata", "scenarios_testdata.csv", package = "trisk.model"))
financial <- read.csv(system.file("testdata", "financial_features_testdata.csv", package = "trisk.model"))
carbon <- read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv", package = "trisk.model"))

# Load portfolio test data
portfolio <- NULL
for (f in c("portfolio_ids_testdata.csv", "portfolio_names_testdata.csv")) {
  path <- system.file("testdata", f, package = "trisk.analysis", mustWork = FALSE)
  if (nzchar(path) && file.exists(path)) {
    portfolio <- read.csv(path)
    cat("Loaded portfolio from:", f, "\n")
    break
  }
}

if (is.null(portfolio)) {
  stop("No portfolio test data found in trisk.analysis", call. = FALSE)
}

cat("Data loaded:\n")
cat("  Assets:", nrow(assets), "rows\n")
cat("  Scenarios:", nrow(scenarios), "rows\n")
cat("  Financial:", nrow(financial), "rows\n")
cat("  Carbon:", nrow(carbon), "rows\n")
cat("  Portfolio:", nrow(portfolio), "rows\n")
cat("  Portfolio columns:", paste(names(portfolio), collapse = ", "), "\n")

# Check company overlap
common_ids <- intersect(as.character(portfolio$company_id), as.character(assets$company_id))
cat("  Company overlap:", length(common_ids), "IDs\n")

cat("\nRunning run_trisk_on_portfolio()...\n")
result <- tryCatch({
  run_trisk_on_portfolio(
    assets_data = assets,
    scenarios_data = scenarios,
    financial_data = financial,
    carbon_data = carbon,
    portfolio_data = portfolio,
    baseline_scenario = "NGFS2023GCAM_CP",
    target_scenario = "NGFS2023GCAM_NZ2050",
    scenario_geography = "Global"
  )
}, error = function(e) {
  stop("run_trisk_on_portfolio() failed: ", conditionMessage(e), call. = FALSE)
})

cat("\nSUCCESS!\n")
cat("Result:", nrow(result), "rows,", ncol(result), "columns\n")
cat("Columns:", paste(names(result), collapse = ", "), "\n\n")

# Validate expected columns for trisk.analysis plot functions
# pipeline_crispy_npv_change_plot expects: crispy_perc_value_change (derived from the
#   NPV columns by compute_analysis_metrics inside the pipeline), sector, technology
# pipeline_crispy_exposure_change_plot expects: exposure_at_default or similar
# pipeline_crispy_expected_loss_plot expects: expected_loss_baseline, expected_loss_shock
# pipeline_crispy_pd_term_plot expects: pd_baseline, pd_shock, term

expected_columns <- c(
  "company_id", "sector", "technology",
  "net_present_value_baseline", "net_present_value_shock",
  "pd_baseline", "pd_shock",
  "exposure_value_usd", "term", "loss_given_default"
)

cat("--- Column validation ---\n")
for (col in expected_columns) {
  present <- col %in% names(result)
  cat(sprintf("  %-35s %s\n", col, if (present) "OK" else "MISSING"))
}

missing <- setdiff(expected_columns, names(result))
if (length(missing) > 0) {
  stop("Missing expected columns: ", paste(missing, collapse = ", "), call. = FALSE)
} else {
  cat("\nAll expected columns present!\n")
}

# Check for expected_loss columns (may be computed separately)
if ("expected_loss_baseline" %in% names(result)) {
  cat("  expected_loss_baseline: present\n")
} else {
  cat("  expected_loss_baseline: not present (can be computed as exposure * pd * lgd)\n")
}
if ("expected_loss_shock" %in% names(result)) {
  cat("  expected_loss_shock: present\n")
} else {
  cat("  expected_loss_shock: not present (can be computed as exposure * pd * lgd)\n")
}

# Try running the plot pipeline functions
cat("\n--- Plot function tests ---\n")
plot_failures <- character()

tryCatch({
  p <- pipeline_crispy_npv_change_plot(result)
  cat("  pipeline_crispy_npv_change_plot(): OK\n")
}, error = function(e) {
  cat("  pipeline_crispy_npv_change_plot(): FAILED -", e$message, "\n")
  plot_failures <<- c(plot_failures, "pipeline_crispy_npv_change_plot")
})

tryCatch({
  p <- pipeline_crispy_exposure_change_plot(result)
  cat("  pipeline_crispy_exposure_change_plot(): OK\n")
}, error = function(e) {
  cat("  pipeline_crispy_exposure_change_plot(): FAILED -", e$message, "\n")
  plot_failures <<- c(plot_failures, "pipeline_crispy_exposure_change_plot")
})

tryCatch({
  p <- pipeline_crispy_expected_loss_plot(result)
  cat("  pipeline_crispy_expected_loss_plot(): OK\n")
}, error = function(e) {
  cat("  pipeline_crispy_expected_loss_plot(): FAILED -", e$message, "\n")
  plot_failures <<- c(plot_failures, "pipeline_crispy_expected_loss_plot")
})

tryCatch({
  p <- pipeline_crispy_pd_term_plot(result)
  cat("  pipeline_crispy_pd_term_plot(): OK\n")
}, error = function(e) {
  cat("  pipeline_crispy_pd_term_plot(): FAILED -", e$message, "\n")
  plot_failures <<- c(plot_failures, "pipeline_crispy_pd_term_plot")
})

cat("\n--- Sample output ---\n")
print(head(result, 5))

cat("\n============================================\n")
cat("Test 2 complete!\n")
cat("============================================\n")

if (length(plot_failures) > 0) {
  stop("Plot functions failed: ", paste(plot_failures, collapse = ", "), call. = FALSE)
}
