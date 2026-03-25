# Run all testthat tests for TRISK Shiny app
library(testthat)

# Source global.R functions (without starting the app)
# This loads validation, sanitization, and helper functions
source(file.path("..", "app", "global.R"), local = TRUE)

test_dir("testthat")
