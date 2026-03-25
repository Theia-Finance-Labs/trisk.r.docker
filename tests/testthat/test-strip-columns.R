# Tests for column stripping / PII protection

test_that("strip_columns removes non-model columns from portfolio", {
  df <- data.frame(
    company_id = "C001", company_name = "Corp", country_iso2 = "US",
    exposure_value_usd = 1e6, term = 5, loss_given_default = 0.45,
    social_security_number = "123-45-6789",  # PII - must be stripped
    internal_rating = "A+",                   # Extra - must be stripped
    stringsAsFactors = FALSE
  )
  result <- strip_columns(df, "portfolio")
  expect_true("company_id" %in% names(result))
  expect_true("exposure_value_usd" %in% names(result))
  expect_false("social_security_number" %in% names(result))
  expect_false("internal_rating" %in% names(result))
})

test_that("strip_columns removes non-model columns from assets", {
  df <- data.frame(
    company_id = "C001", sector = "Power", technology = "CoalCap",
    production_year = 2025, capacity = 500, capacity_factor = 0.85,
    secret_field = "sensitive"
  )
  result <- strip_columns(df, "assets")
  expect_false("secret_field" %in% names(result))
  expect_true("sector" %in% names(result))
})

test_that("strip_columns removes non-model columns from financial", {
  df <- data.frame(
    company_id = "C001", pd = 0.02, net_profit_margin = 0.08,
    debt_equity_ratio = 1.5, volatility = 0.25,
    internal_credit_score = 750
  )
  result <- strip_columns(df, "financial")
  expect_false("internal_credit_score" %in% names(result))
  expect_true("pd" %in% names(result))
})

test_that("strip_columns keeps all columns for unknown type", {
  df <- data.frame(a = 1, b = 2, c = 3)
  result <- strip_columns(df, "unknown_type")
  expect_equal(ncol(result), 3)
})

test_that("sanitize_export filters to allowed export columns", {
  df <- data.frame(
    company_id = "C001", company_name = "Corp",
    pd_baseline = 0.02, pd_shock = 0.05,
    internal_intermediate_calc = 999  # Not in EXPORT_COLUMNS
  )
  result <- sanitize_export(df)
  expect_true("company_id" %in% names(result))
  expect_true("pd_baseline" %in% names(result))
  expect_false("internal_intermediate_calc" %in% names(result))
})
