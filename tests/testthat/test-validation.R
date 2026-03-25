# Tests for data validation functions in global.R

test_that("validate_portfolio accepts valid data", {
  df <- data.frame(
    company_id = "COMP001",
    company_name = "Example Corp",
    country_iso2 = "US",
    exposure_value_usd = 1000000,
    term = 5,
    loss_given_default = 0.45,
    stringsAsFactors = FALSE
  )
  expect_null(validate_portfolio(df))
})

test_that("validate_portfolio rejects missing columns", {
  df <- data.frame(company_id = "COMP001", company_name = "Example Corp")
  result <- validate_portfolio(df)
  expect_type(result, "character")
  expect_match(result, "Missing required columns")
  expect_match(result, "country_iso2")
})

test_that("validate_assets accepts valid data", {
  df <- data.frame(
    company_id = "COMP001", sector = "Power", technology = "CoalCap",
    production_year = 2025, capacity = 500, capacity_factor = 0.85
  )
  expect_null(validate_assets(df))
})

test_that("validate_assets rejects missing columns", {
  df <- data.frame(company_id = "COMP001", sector = "Power")
  result <- validate_assets(df)
  expect_type(result, "character")
  expect_match(result, "Missing required columns")
})

test_that("validate_financial accepts valid data", {
  df <- data.frame(
    company_id = "COMP001", pd = 0.02, net_profit_margin = 0.08,
    debt_equity_ratio = 1.5, volatility = 0.25
  )
  expect_null(validate_financial(df))
})

test_that("validate_internal_pd_csv accepts valid data", {
  df <- data.frame(company_id = "COMP001", internal_pd = 0.05)
  expect_null(validate_internal_pd_csv(df))
})

test_that("validate_internal_pd_csv rejects out-of-range values", {
  df <- data.frame(company_id = "COMP001", internal_pd = 1.5)
  result <- validate_internal_pd_csv(df)
  expect_match(result, "\\[0, 1\\]")
})

test_that("validate_internal_pd_csv rejects NA values", {
  df <- data.frame(company_id = c("A", "B"), internal_pd = c(0.05, NA))
  result <- validate_internal_pd_csv(df)
  expect_match(result, "NA values")
})

test_that("validate_internal_pd_csv rejects missing columns", {
  df <- data.frame(company_id = "COMP001", some_other = 0.05)
  result <- validate_internal_pd_csv(df)
  expect_match(result, "Missing required column: internal_pd")
})

test_that("validate_internal_el_csv accepts valid data", {
  df <- data.frame(company_id = "COMP001", internal_el = -50000)
  expect_null(validate_internal_el_csv(df))
})

test_that("validate_internal_el_csv rejects non-numeric", {
  df <- data.frame(company_id = "COMP001", internal_el = "not_a_number",
                   stringsAsFactors = FALSE)
  result <- validate_internal_el_csv(df)
  expect_match(result, "must be numeric")
})
