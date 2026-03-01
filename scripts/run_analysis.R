#!/usr/bin/env Rscript
# run_analysis.R
# Command-line interface for running TRISK analysis
# Usage: Rscript run_analysis.R --portfolio=portfolio.csv --assets=assets.csv ...

suppressPackageStartupMessages({
  library(trisk.model)
  library(trisk.analysis)
  library(optparse)
  library(readr)
  library(jsonlite)
})

# Define command line options
option_list <- list(
  make_option(c("-p", "--portfolio"), type = "character", default = NULL,
              help = "Path to portfolio CSV file [required]"),
  make_option(c("-a", "--assets"), type = "character", default = NULL,
              help = "Path to assets CSV file [required]"),
  make_option(c("-f", "--financial"), type = "character", default = NULL,
              help = "Path to financial features CSV file [required]"),
  make_option(c("-s", "--scenarios"), type = "character", 
              default = "/opt/trisk/data/scenarios/scenarios.csv",
              help = "Path to scenarios CSV [default: pre-loaded]"),
  make_option(c("-c", "--carbon"), type = "character", default = NULL,
              help = "Path to carbon price CSV [optional, uses internal data if not provided]"),
  make_option(c("-o", "--output"), type = "character", default = "/data/output",
              help = "Output directory [default: /data/output]"),
  make_option(c("--baseline"), type = "character", default = "NGFS2023GCAM_CP",
              help = "Baseline scenario name [default: NGFS2023GCAM_CP]"),
  make_option(c("--target"), type = "character", default = "NGFS2023GCAM_NZ2050",
              help = "Target (shock) scenario name [default: NGFS2023GCAM_NZ2050]"),
  make_option(c("--geography"), type = "character", default = "Global",
              help = "Scenario geography [default: Global]"),
  make_option(c("--shock-year"), type = "integer", default = 2030,
              help = "Shock year [default: 2030]"),
  make_option(c("--risk-free-rate"), type = "double", default = 0.02,
              help = "Risk-free rate [default: 0.02]"),
  make_option(c("--discount-rate"), type = "double", default = 0.07,
              help = "Discount rate [default: 0.07]"),
  make_option(c("--growth-rate"), type = "double", default = 0.03,
              help = "Terminal growth rate [default: 0.03]"),
  make_option(c("--market-passthrough"), type = "double", default = 0,
              help = "Market passthrough [default: 0]"),
  make_option(c("--format"), type = "character", default = "csv",
              help = "Output format: csv or json [default: csv]"),
  make_option(c("-v", "--verbose"), action = "store_true", default = FALSE,
              help = "Print verbose output")
)

parser <- OptionParser(
  usage = "usage: %prog [options]",
  description = "\nTRISK Climate Transition Risk Stress Testing Tool\nCommand-line interface for batch processing\n",
  option_list = option_list
)

args <- parse_args(parser)

# Print help if no arguments
if (is.null(args$portfolio) || is.null(args$assets) || is.null(args$financial)) {
  print_help(parser)
  cat("\nExample usage:\n")
  cat("  Rscript run_analysis.R \\\n")
  cat("    --portfolio=/data/input/portfolio.csv \\\n")
  cat("    --assets=/data/input/assets.csv \\\n")
  cat("    --financial=/data/input/financial.csv \\\n")
  cat("    --output=/data/output \\\n")
  cat("    --baseline=NGFS2023GCAM_CP \\\n")
  cat("    --target=NGFS2023GCAM_NZ2050\n\n")
  quit(status = 0)
}

# Logging function
log_msg <- function(msg) {
  if (args$verbose) {
    message(paste("[", Sys.time(), "]", msg))
  }
}

# Main execution
tryCatch({
  
  log_msg("Starting TRISK analysis...")
  
  # Load data
  log_msg(paste("Loading portfolio from:", args$portfolio))
  portfolio_data <- read_csv(args$portfolio, show_col_types = FALSE)
  
  log_msg(paste("Loading assets from:", args$assets))
  assets_data <- read_csv(args$assets, show_col_types = FALSE)
  
  log_msg(paste("Loading financial data from:", args$financial))
  financial_data <- read_csv(args$financial, show_col_types = FALSE)
  
  log_msg(paste("Loading scenarios from:", args$scenarios))
  scenarios_data <- read_csv(args$scenarios, show_col_types = FALSE)
  
  # Load carbon price data
  if (!is.null(args$carbon) && file.exists(args$carbon)) {
    log_msg(paste("Loading carbon prices from:", args$carbon))
    carbon_data <- read_csv(args$carbon, show_col_types = FALSE)
  } else {
    log_msg("Using internal carbon price test data")
    carbon_data <- read.csv(
      system.file("testdata", "ngfs_carbon_price_testdata.csv", 
                  package = "trisk.model", mustWork = TRUE)
    )
  }
  
  log_msg("Running TRISK model...")
  
  # Run analysis
  results <- run_trisk_on_portfolio(
    assets_data = assets_data,
    scenarios_data = scenarios_data,
    financial_data = financial_data,
    carbon_data = carbon_data,
    portfolio_data = portfolio_data,
    baseline_scenario = args$baseline,
    target_scenario = args$target,
    scenario_geography = args$geography,
    shock_year = args$`shock-year`,
    risk_free_rate = args$`risk-free-rate`,
    discount_rate = args$`discount-rate`,
    growth_rate = args$`growth-rate`,
    market_passthrough = args$`market-passthrough`
  )
  
  # Create output directory if needed
  if (!dir.exists(args$output)) {
    dir.create(args$output, recursive = TRUE)
  }
  
  # Generate timestamp for filenames
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  
  # Save results
  if (args$format == "json") {
    output_file <- file.path(args$output, paste0("trisk_results_", timestamp, ".json"))
    write_json(results, output_file, pretty = TRUE)
  } else {
    output_file <- file.path(args$output, paste0("trisk_results_", timestamp, ".csv"))
    write_csv(results, output_file)
  }
  
  log_msg(paste("Results saved to:", output_file))
  
  # Print summary
  cat("\n========================================\n")
  cat("TRISK Analysis Complete\n")
  cat("========================================\n")
  cat(paste("Companies analyzed:", length(unique(results$company_id)), "\n"))
  cat(paste("Output file:", output_file, "\n"))
  cat("========================================\n\n")
  
}, error = function(e) {
  cat("\n========================================\n")
  cat("ERROR: TRISK Analysis Failed\n")
  cat("========================================\n")
  cat(paste("Error message:", e$message, "\n"))
  cat("========================================\n\n")
  quit(status = 1)
})
