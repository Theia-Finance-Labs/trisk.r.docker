#!/usr/bin/env Rscript
# Master test runner for TRISK Docker tool
# Runs all validation tests and reports results

cat("\n########################################\n")
cat("# TRISK Docker Tool - Test Suite       #\n")
cat("########################################\n\n")

test_results <- list()

# Test 1: Continuity test
cat("Running Test 1: Model continuity...\n")
t1 <- tryCatch({
  source("/opt/trisk/scripts/test_continuity.R", local = TRUE)
  "PASS"
}, error = function(e) {
  paste("FAIL:", e$message)
})
test_results[["Model Continuity"]] <- t1

# Test 2: Portfolio PD output
cat("\nRunning Test 2: Portfolio PD output...\n")
t2 <- tryCatch({
  source("/opt/trisk/scripts/test_portfolio_pd.R", local = TRUE)
  "PASS"
}, error = function(e) {
  paste("FAIL:", e$message)
})
test_results[["Portfolio PD Output"]] <- t2

# Test 3: Year range diagnostic
cat("\nRunning Test 3: Year range compatibility...\n")
t3 <- tryCatch({
  source("/opt/trisk/scripts/test_debug.R", local = TRUE)
  "PASS"
}, error = function(e) {
  paste("FAIL:", e$message)
})
test_results[["Year Range Compatibility"]] <- t3

# Summary
cat("\n\n########################################\n")
cat("# TEST SUMMARY                         #\n")
cat("########################################\n\n")

all_pass <- TRUE
for (name in names(test_results)) {
  status <- test_results[[name]]
  marker <- if (startsWith(status, "PASS")) "PASS" else "FAIL"
  if (marker == "FAIL") all_pass <- FALSE
  cat(sprintf("  [%s] %s", marker, name))
  if (marker == "FAIL") cat(paste(" -", status))
  cat("\n")
}

cat("\n")
if (all_pass) {
  cat("All tests passed!\n")
} else {
  cat("Some tests failed. Review output above for details.\n")
}
cat("########################################\n\n")

if (!all_pass) quit(status = 1)
