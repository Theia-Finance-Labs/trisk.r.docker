# Tests for scenario labeling, categorization, and baseline matching

test_that("scenario_label parses NGFS codes correctly", {
  expect_match(scenario_label("NGFS2023_GCAM_NZ2050"), "Net Zero 2050")
  expect_match(scenario_label("NGFS2023_GCAM_NZ2050"), "NGFS 2023")
  expect_match(scenario_label("NGFS2023_GCAM_NZ2050"), "GCAM")
})

test_that("scenario_label parses GECO codes correctly", {
  expect_match(scenario_label("GECO2023_CP"), "Current Policies")
  expect_match(scenario_label("GECO2023_CP"), "GECO 2023")
})

test_that("scenario_label parses IPR codes correctly", {
  expect_match(scenario_label("IPR2023_FPS"), "Forecast Policy Scenario")
  expect_match(scenario_label("IPR2023_FPS"), "IPR 2023")
})

test_that("scenario_label handles IPR Automotive variant", {
  expect_match(scenario_label("IPR2023Automotive_FPS"), "Automotive")
})

test_that("scenario_label parses mission_possible codes", {
  expect_match(scenario_label("mission_possible_Steel_NZ"), "Net Zero")
  expect_match(scenario_label("mission_possible_Steel_NZ"), "Mission Possible")
})

test_that("scenario_label returns original for unknown codes", {
  expect_equal(scenario_label("CustomScenario"), "CustomScenario")
})

test_that("scenario_category classifies correctly", {
  expect_match(scenario_category("NGFS2023_GCAM_NZ2050"), "Orderly")
  expect_match(scenario_category("NGFS2023_GCAM_DT"), "Disorderly")
  expect_match(scenario_category("NGFS2023_GCAM_CP"), "Hot House World")
  expect_match(scenario_category("NGFS2023_GCAM_FW"), "Too Little")
  expect_match(scenario_category("UnknownScenario"), "Other")
})

test_that("scenario_family groups related scenarios", {
  expect_equal(scenario_family("NGFS2024_GCAM_NZ2050"), "NGFS2024GCAM")
  expect_equal(scenario_family("NGFS2024_GCAM_CP"), "NGFS2024GCAM")
  expect_equal(scenario_family("GECO2023_CP"), "GECO2023")
  expect_equal(scenario_family("GECO2023_FPS"), "GECO2023")
})

test_that("baseline_for_scenario selects GECO baseline for GECO targets", {
  baselines <- c("NGFS2024_GCAM_CP", "GECO2023_CP", "mission_possible_Steel_baseline")
  result <- baseline_for_scenario("GECO2023_FPS", baselines, "NGFS2024_GCAM_CP")
  expect_equal(result, "GECO2023_CP")
})

test_that("baseline_for_scenario selects mission_possible baseline correctly", {
  baselines <- c("NGFS2024_GCAM_CP", "mission_possible_Steel_baseline")
  result <- baseline_for_scenario("mission_possible_Steel_NZ", baselines, "NGFS2024_GCAM_CP")
  expect_equal(result, "mission_possible_Steel_baseline")
})

test_that("baseline_for_scenario falls back to default for NGFS targets", {
  baselines <- c("NGFS2024_GCAM_CP", "GECO2023_CP")
  result <- baseline_for_scenario("NGFS2024_GCAM_NZ2050", baselines, "NGFS2024_GCAM_CP")
  expect_equal(result, "NGFS2024_GCAM_CP")
})

test_that("build_baseline_map returns named vector", {
  targets <- c("NGFS2024_GCAM_NZ2050", "GECO2023_FPS")
  baselines <- c("NGFS2024_GCAM_CP", "GECO2023_CP")
  result <- build_baseline_map(targets, baselines, "NGFS2024_GCAM_CP")
  expect_named(result, targets)
  expect_equal(result[["GECO2023_FPS"]], "GECO2023_CP")
})

test_that("build_scenario_choices returns grouped list", {
  codes <- c("NGFS2024_GCAM_NZ2050", "NGFS2024_GCAM_CP")
  result <- build_scenario_choices(codes)
  expect_type(result, "list")
  expect_true(length(result) > 0)
})
