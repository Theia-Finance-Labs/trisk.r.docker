# install_packages.R
# Installs all TRISK dependencies in a reproducible way
# This script is run during Docker build

message("========================================")
message("Installing TRISK packages and dependencies")
message("========================================")

# Set CRAN mirror explicitly to avoid PPM issues
options(repos = c(CRAN = "https://cloud.r-project.org"))

# Use pak for fast, reliable installation with dependency resolution
message("\n[1/3] Installing core TRISK packages from GitHub...")

pak::pak(c(
  "Theia-Finance-Labs/trisk.model",
  "Theia-Finance-Labs/trisk.analysis"
), 
ask = FALSE,
upgrade = TRUE
)

message("\n[2/3] Installing Shiny UI dependencies...")

# Install packages one group at a time to better handle failures
# Use install.packages as fallback for problematic packages
tryCatch({
  pak::pak(c(
    "httpuv",
    "htmltools",
    "shiny",
    "shinydashboard",
    "shinyWidgets",
    "shinyjs",
    "DT",
    "ggplot2",
    "plotly",
    "readr",
    "writexl",
    "jsonlite",
    "dplyr",
    "tidyr"
  ), ask = FALSE)
}, error = function(e) {
  message("pak failed, falling back to install.packages...")
  install.packages(c(
    "httpuv",
    "htmltools",
    "shiny",
    "shinydashboard",
    "shinyWidgets",
    "shinyjs",
    "DT",
    "ggplot2",
    "plotly",
    "readr",
    "writexl",
    "jsonlite",
    "dplyr",
    "tidyr"
  ), repos = "https://cloud.r-project.org")
})

message("\n[3/3] Verifying installation...")

# Verify core packages load correctly
suppressPackageStartupMessages({
  library(trisk.model)
  library(trisk.analysis)
  library(shiny)
  library(shinydashboard)
})

message("\n========================================")
message("Installation completed successfully!")
message("========================================")
message(paste("trisk.model version:   ", packageVersion("trisk.model")))
message(paste("trisk.analysis version:", packageVersion("trisk.analysis")))
message(paste("R version:             ", R.version$version.string))
message("========================================\n")
