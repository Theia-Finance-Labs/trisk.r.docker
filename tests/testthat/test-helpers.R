# Tests for helper functions in global.R

test_that("smart_round uses 2 decimals for large values", {
  expect_equal(smart_round(1234.5678), 1234.57)
  expect_equal(smart_round(-100.999), -101)
})

test_that("smart_round uses 5 decimals for small values", {
  expect_equal(smart_round(0.123456789), 0.12346)
  expect_equal(smart_round(5.123456789), 5.12346)
})

test_that("display_round always returns 2 decimals", {
  expect_equal(display_round(3.47265), 3.47)
  expect_equal(display_round(0.001), 0)
})

test_that("format_number formats millions correctly", {
  expect_equal(format_number(1500000), "1.5M")
  expect_equal(format_number(-2000000), "-2M")
})

test_that("format_number formats thousands correctly", {
  expect_equal(format_number(5000), "5K")
  expect_equal(format_number(1234), "1.23K")
})

test_that("format_number handles small values", {
  expect_equal(format_number(42.567), "42.57")
})

test_that("generate_run_id produces expected format", {
  config <- list(rate = 0.02, year = 2030)
  id <- generate_run_id(config)
  # Format: YYYYMMDD_HHMMSS_XXXXXXXX (8-char hash)
  expect_match(id, "^\\d{8}_\\d{6}_[a-f0-9]+$")
})

test_that("generate_run_id produces different IDs for different configs", {
  id1 <- generate_run_id(list(rate = 0.02, year = 2030))
  Sys.sleep(1.1)  # ensure different timestamp
  id2 <- generate_run_id(list(rate = 0.05, year = 2035))
  expect_false(id1 == id2)
})

test_that("calculate_expected_loss returns correct values", {
  # EL = -(exposure * lgd) * pd
  result <- calculate_expected_loss(1000000, 0.02, 0.45)
  expect_equal(result, -(1000000 * 0.45) * 0.02)
})

test_that("calculate_expected_loss handles zero PD", {
  result <- calculate_expected_loss(1000000, 0, 0.45)
  expect_equal(result, 0)
})

test_that("compute_el_columns adds expected columns", {
  df <- data.frame(
    exposure_value_usd = 1000000,
    loss_given_default = 0.45,
    pd_baseline = 0.02,
    pd_shock = 0.05,
    net_present_value_baseline = 500000,
    net_present_value_shock = 400000
  )
  result <- compute_el_columns(df)
  expect_true("exposure_at_default" %in% names(result))
  expect_true("expected_loss_baseline" %in% names(result))
  expect_true("expected_loss_shock" %in% names(result))
  expect_true("expected_loss_difference" %in% names(result))
  expect_true("net_present_value_difference" %in% names(result))
  expect_true("crispy_perc_value_change" %in% names(result))
})

test_that("compute_el_columns handles zero NPV baseline", {
  df <- data.frame(
    exposure_value_usd = 1000000,
    loss_given_default = 0.45,
    pd_baseline = 0.02,
    pd_shock = 0.05,
    net_present_value_baseline = 0,
    net_present_value_shock = -100000
  )
  result <- compute_el_columns(df)
  expect_true(is.na(result$crispy_perc_value_change))
})

test_that("dataset_template returns correct columns for portfolio", {
  tmpl <- dataset_template("portfolio")
  expected <- c("company_id", "company_name", "country_iso2",
                "exposure_value_usd", "term", "loss_given_default")
  expect_true(all(expected %in% names(tmpl)))
  expect_equal(nrow(tmpl), 1)
})

test_that("dataset_template returns correct columns for assets", {
  tmpl <- dataset_template("assets")
  expect_true("sector" %in% names(tmpl))
  expect_true("technology" %in% names(tmpl))
})

test_that("audit_dataset detects missing values", {
  df <- data.frame(
    company_id = c("A", "B", "C"),
    pd = c(0.02, NA, 0.05),
    volatility = c(0.3, 0.2, 0.1),
    net_profit_margin = c(0.1, 0.2, 0.3),
    debt_equity_ratio = c(1.0, 1.5, 2.0)
  )
  result <- audit_dataset(df, "financial")
  expect_true(nrow(result$issues) > 0)
  expect_true(any(result$issues$issue_type == "missing_value"))
})

test_that("audit_dataset detects duplicate company_id", {
  df <- data.frame(
    company_id = c("A", "A", "B"),
    company_name = c("X", "X", "Y"),
    country_iso2 = c("US", "US", "DE"),
    exposure_value_usd = c(1e6, 2e6, 3e6),
    term = c(5, 5, 3),
    loss_given_default = c(0.45, 0.45, 0.6)
  )
  result <- audit_dataset(df, "portfolio")
  expect_true(any(result$issues$issue_type == "duplicate_id"))
})

test_that("audit_dataset detects invalid ISO-2 codes in portfolio", {
  df <- data.frame(
    company_id = "A", company_name = "X",
    country_iso2 = "INVALID",
    exposure_value_usd = 1e6, term = 5, loss_given_default = 0.45
  )
  result <- audit_dataset(df, "portfolio")
  expect_true(any(result$issues$issue_type == "invalid_iso2"))
})
