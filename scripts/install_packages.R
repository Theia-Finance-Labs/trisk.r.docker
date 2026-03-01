# install_packages.R
# Installs all TRISK dependencies in a reproducible way.
#
# When run inside Docker, the CRAN_REPO env var and commit SHAs are set by
# the Dockerfile ARGs.  When run standalone (e.g. local dev), the defaults
# below are used.
#
# IMPORTANT: keep the default commit SHAs here in sync with the Dockerfile
# ARGs so that local and containerised builds produce the same result.

message("========================================")
message("Installing TRISK packages and dependencies")
message("========================================")

# ── Repo configuration ──
# Prefer CRAN_REPO env var (set by Dockerfile to a Posit PPM date snapshot).
# Fallback: Posit PPM snapshot pinned to a known-good date.
cran_repo <- Sys.getenv("CRAN_REPO", unset = "https://packagemanager.posit.co/cran/2025-02-01")
options(repos = c(CRAN = cran_repo))
message(paste("CRAN repo:", cran_repo))

# ── GitHub commit SHAs ──
# These are overridden by Dockerfile build-args; keep as fallback.
trisk_model_ref    <- Sys.getenv("TRISK_MODEL_SHA",    unset = "b716c9d3573bee2a730973995b9da6fc9b9703c5")
trisk_analysis_ref <- Sys.getenv("TRISK_ANALYSIS_SHA", unset = "307e52aa035b3ebe8bb0ddb967268e38600a1763")

message("\n[1/3] Installing core TRISK packages from GitHub (pinned commits)...")

pak::pak(c(
  paste0("Theia-Finance-Labs/trisk.model@", trisk_model_ref),
  paste0("Theia-Finance-Labs/trisk.analysis@", trisk_analysis_ref)
),
ask = FALSE,
upgrade = FALSE
)

message("\n[2/3] Installing Shiny UI dependencies (frozen CRAN snapshot)...")

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
), ask = FALSE, upgrade = FALSE)

message("\n[3/3] Verifying installation...")

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
message(paste("CRAN snapshot:         ", cran_repo))
message("========================================\n")
