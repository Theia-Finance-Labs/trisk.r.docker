# Scenario Sensitivity Analysis — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Add a new "Scenario Sensitivity" tab that automatically detects alternative scenarios from the same family/vintage as the user's chosen target, runs the model for each, and displays results as distributions with confidence intervals.

**Architecture:** One new sidebar tab, one new module file, helper functions in `global.R`, and new reactive values in `server.R`. The tab auto-detects sibling target scenarios (same NGFS family+vintage, all pathways x all IAMs), runs each against the same baseline, and renders violin/dot plots showing the distribution of key metrics. The user's primary scenario is highlighted in the distribution.

**Key Design Decisions:**
- **Scope**: All target scenarios in the same family+vintage as the user's core target. For NGFS 2024, that's 18 targets (6 pathways x 3 IAMs — NZ2050, B2DS, LD, DT, FW, NDC x GCAM, MESSAGE, REMIND).
- **Baseline**: Same baseline for all sensitivity runs (matching app's existing convention — `baseline_for_scenario()` determines the correct one).
- **Portfolio filtering**: Same sector-filtered approach as the main run — core pair sectors only (Oil&Gas, Coal, Power). Non-core pairs (Automotive, Steel) are excluded since they typically have only one target scenario variant.
- **Results storage**: Single flat dataframe with a `sensitivity_scenario` column, separate from `rv$results`.
- **UI**: Nearly zero-effort — one-click run after main analysis, auto-detected scenario list shown before run.

---

### Task 1: Add Scenario Parsing Helpers to `global.R`

**Files:**
- Modify: `app/global.R` (after `scenario_family()`, ~line 748)

**Step 1: Add `parse_scenario_code()` function**

This function decomposes a scenario code into its constituent parts (family, vintage, IAM model, pathway):

```r
#' Parse a scenario code into structured components
#' @return list(family, vintage, iam, pathway, family_prefix) or NULL if unparseable
parse_scenario_code <- function(code) {
  # NGFS pattern: NGFS{year}[_]{model}[_]{pathway}
  m <- regmatches(code, regexec("^NGFS(\\d{4})[_]?(GCAM|MESSAGE|REMIND)[_]?(.+)$", code))[[1]]
  if (length(m) == 4) {
    return(list(family = "NGFS", vintage = m[2], iam = m[3], pathway = m[4],
                family_prefix = paste0("NGFS", m[2])))
  }
  # GECO pattern: GECO{year}_{pathway}
  m <- regmatches(code, regexec("^GECO(\\d{4})[_](.+)$", code))[[1]]
  if (length(m) == 3) {
    return(list(family = "GECO", vintage = m[2], iam = NA, pathway = m[3],
                family_prefix = paste0("GECO", m[2])))
  }
  # IPR pattern: IPR{year}[Automotive]_{pathway}
  m <- regmatches(code, regexec("^IPR(\\d{4})(Automotive)?[_](.+)$", code))[[1]]
  if (length(m) == 4) {
    suffix <- if (nzchar(m[3])) m[3] else ""
    return(list(family = "IPR", vintage = m[2], iam = NA, pathway = m[4],
                family_prefix = paste0("IPR", m[2], suffix)))
  }
  # mission_possible pattern
  m <- regmatches(code, regexec("^mission_possible[_](.+)[_](baseline|NZ)$", code))[[1]]
  if (length(m) == 3) {
    return(list(family = "MissionPossible", vintage = NA, iam = NA, pathway = m[3],
                family_prefix = paste0("mission_possible_", m[2])))
  }
  NULL
}
```

**Step 2: Add `find_sensitivity_scenarios()` function**

Finds all sibling target scenarios in the same family+vintage:

```r
#' Find sibling target scenarios for sensitivity analysis
#' @param target_code The user's selected target scenario code
#' @param all_targets Character vector of all available target scenario codes
#' @return list(scenarios, parsed_info, labels) or NULL if no siblings found
find_sensitivity_scenarios <- function(target_code, all_targets) {
  parsed <- parse_scenario_code(target_code)
  if (is.null(parsed)) return(NULL)

  # Find all targets in the same family + vintage
  siblings <- character()
  for (code in all_targets) {
    p <- parse_scenario_code(code)
    if (is.null(p)) next
    if (p$family == parsed$family && identical(p$vintage, parsed$vintage)) {
      siblings <- c(siblings, code)
    }
  }

  # Must have at least 2 scenarios (including the primary) for sensitivity to be meaningful
  if (length(siblings) < 2) return(NULL)

  # Build labels for display
  labels <- sapply(siblings, scenario_label, USE.NAMES = TRUE)
  categories <- sapply(siblings, scenario_category, USE.NAMES = TRUE)

  list(
    scenarios = siblings,
    primary = target_code,
    family = parsed$family,
    vintage = parsed$vintage,
    labels = labels,
    categories = categories,
    n_total = length(siblings),
    n_additional = length(siblings) - 1L
  )
}
```

**Step 3: Commit**

---

### Task 2: Add Reactive Values and Sidebar Menu Item

**Files:**
- Modify: `app/server.R:10-44` (reactive values)
- Modify: `app/ui.R:28-42` (sidebar menu)

**Step 1: Add reactive values for sensitivity analysis**

In `app/server.R`, after `scenario_groups = "core"` (line 43), add:

```r
    # Scenario sensitivity analysis
    sensitivity_config = NULL,      # list from find_sensitivity_scenarios()
    sensitivity_results = NULL,     # flat dataframe with all scenario results
    sensitivity_summary = NULL,     # per-scenario aggregated KPIs
    sensitivity_running = FALSE
```

**Step 2: Add sidebar menu item**

In `app/ui.R`, after the "4. Portfolio Results" menuItem (line 36), add:

```r
      menuItem("5. Scenario Sensitivity", tabName = "sensitivity", icon = icon("chart-bar")),
```

Renumber the remaining items:
- "5. Integrate PD/EL" -> "6. Integrate PD/EL"
- "6. Download" -> "7. Download"

**Step 3: Commit**

---

### Task 3: Build the Sensitivity Tab UI

**Files:**
- Modify: `app/ui.R` (add new tabItem in the tabItems list, after "results" tabItem)

**Step 1: Add the sensitivity tabItem**

After the `tabItem(tabName = "results", ...)` closing parenthesis, add:

```r
      # ============================================
      # Scenario Sensitivity Tab
      # ============================================
      tabItem(tabName = "sensitivity",
        fluidRow(
          box(
            title = NULL, width = 12, status = "danger", solidHeader = FALSE,
            div(class = "section-header section-header--dark",
              h4(icon("chart-bar"), " Scenario Sensitivity Analysis", class = "section-title"),
              tags$small("Test how sensitive your results are to scenario choice",
                        class = "opacity-85")
            )
          )
        ),
        # Dynamic content: pre-run state or results
        uiOutput("sensitivity_content_ui")
      ),
```

**Step 2: Commit**

---

### Task 4: Build the Sensitivity Module (Server Logic)

**Files:**
- Create: `app/modules/mod_sensitivity.R`
- Modify: `app/server.R` (source the module, call setup function)

**Step 1: Create `mod_sensitivity.R`**

This module handles all sensitivity analysis logic: scenario detection, running, and results rendering. It follows the same pattern as `mod_results_horizon.R`.

The module should contain:

1. **`setup_sensitivity <- function(input, output, session, rv)`** — main setup function

2. **Scenario detection observer**: When `rv$results` changes AND `rv$target_scenarios_all` is available, call `find_sensitivity_scenarios()` to detect siblings. Store in `rv$sensitivity_config`.

3. **`output$sensitivity_content_ui` renderUI**: Renders different states:
   - **No main results**: "Run the main analysis first" empty state
   - **No sensitivity scenarios found**: "No alternative scenarios found" message
   - **Pre-run**: Show detected scenarios grouped by category, with "Run Sensitivity Analysis" button
   - **Running**: Progress indicator
   - **Results ready**: Summary cards + plots + table

4. **`observeEvent(input$run_sensitivity, ...)`**: The execution handler:
   - Gets the core scenario pair's baseline from the main run
   - Loops through all sensitivity scenarios x shock years
   - Calls `run_trisk_on_portfolio()` for each (reusing same data prep as main run)
   - Includes the primary scenario's results (from `rv$results_by_year`) in the combined dataframe
   - Stores flat results in `rv$sensitivity_results`
   - Computes per-scenario summary in `rv$sensitivity_summary`

5. **Results rendering outputs**:
   - `output$sensitivity_summary_cards`: KPI cards showing median, IQR, range
   - `output$plot_sensitivity_pd`: Violin/dot plot of PD Change (%) across scenarios
   - `output$plot_sensitivity_el`: Violin/dot plot of Expected Loss Change across scenarios
   - `output$plot_sensitivity_npv`: Violin/dot plot of NPV Change (%) across scenarios
   - `output$sensitivity_comparison_table`: DT table with one row per scenario, all KPIs

**Key implementation details for the run handler:**

```r
observeEvent(input$run_sensitivity, {
  req(rv$sensitivity_config, rv$results_by_year, rv$portfolio, rv$assets)

  config <- rv$sensitivity_config
  scenarios_to_run <- config$scenarios
  shock_years <- sort(as.integer(names(rv$results_by_year)))

  # Determine baseline (same as core pair used in main run)
  core_baseline <- rv$results$baseline_scenario[1]
  core_sectors <- SECTOR_SCENARIO_DEFAULTS[["core"]]$sectors

  withProgress(message = "Running sensitivity analysis...", value = 0, {
    all_results <- list()
    total_runs <- length(scenarios_to_run) * length(shock_years)
    run_count <- 0

    for (scen in scenarios_to_run) {
      for (yr in shock_years) {
        run_count <- run_count + 1
        incProgress(1 / total_runs,
                    detail = paste0(scenario_label(scen), " -- year ", yr,
                                   " (", run_count, "/", total_runs, ")"))

        # Check if this is the primary scenario -- reuse existing results
        if (scen == config$primary) {
          yr_results <- rv$results_by_year[[as.character(yr)]]
          if (!is.null(yr_results)) {
            yr_results$sensitivity_scenario <- scen
            all_results[[paste0(scen, "_", yr)]] <- yr_results
          }
          next
        }

        tryCatch({
          # Determine correct baseline for this target
          bl <- baseline_for_scenario(scen, rv$available_baselines, core_baseline)

          # Geography fallback
          geo_result <- get_geography_for_pair(bl, input$scenario_geography)

          # Filter data to core sectors
          portfolio_subset <- rv$portfolio %>%
            dplyr::filter(.data$sector %in% core_sectors)
          assets_subset <- rv$assets %>%
            dplyr::filter(.data$sector %in% core_sectors)

          # Year range filter
          pair_scenarios <- rv$scenarios %>%
            dplyr::filter(.data$scenario %in% c(bl, scen),
                          .data$scenario_geography %in% geo_result$geography)
          if (nrow(pair_scenarios) > 0) {
            scen_min <- min(pair_scenarios$scenario_year)
            scen_max <- max(pair_scenarios$scenario_year)
            assets_subset <- assets_subset %>%
              dplyr::filter(.data$production_year >= scen_min,
                            .data$production_year <= scen_max)
          }

          raw <- run_trisk_on_portfolio(
            assets_data = assets_subset,
            scenarios_data = rv$scenarios,
            financial_data = rv$financial,
            carbon_data = rv$carbon,
            portfolio_data = portfolio_subset,
            baseline_scenario = bl,
            target_scenario = scen,
            scenario_geography = geo_result$geography,
            shock_year = yr,
            risk_free_rate = input$risk_free_rate,
            discount_rate = input$discount_rate,
            growth_rate = input$growth_rate,
            market_passthrough = input$market_passthrough
          )

          result_df <- extract_result_df(raw)
          result_df <- compute_el_columns(result_df)
          result_df$shock_year <- yr
          result_df$sensitivity_scenario <- scen
          result_df$baseline_scenario <- bl
          all_results[[paste0(scen, "_", yr)]] <- result_df
        }, error = function(e) {
          log_message(paste0("Sensitivity: ", scen, " year ", yr, " failed: ",
                            conditionMessage(e)))
        })
      }
    }

    rv$sensitivity_results <- dplyr::bind_rows(all_results)
    # Compute per-scenario summary (see Task 5)
  })
})
```

**Step 2: Wire the module into server.R**

In `app/server.R`, after the existing module source calls, add:

```r
source("modules/mod_sensitivity.R", local = TRUE)
```

And after the existing `setup_*` calls, add:

```r
setup_sensitivity(input, output, session, rv)
```

Note: `extract_result_df` and `get_geography_for_pair` are defined locally in the `execute_trisk_analysis` closure. These need to be either: (a) moved to `global.R` as shared helpers, or (b) duplicated in the module. Option (a) is cleaner — move `extract_result_df` to `global.R`.

**Step 3: Move `extract_result_df` to `global.R`**

Cut the `extract_result_df` function from `server.R:1378-1387` and paste it into `global.R` (after `compute_el_columns`). This makes it available to both the main run handler and the sensitivity module.

**Step 4: Commit**

---

### Task 5: Build Sensitivity Results Visualizations

**Files:**
- Modify: `app/modules/mod_sensitivity.R` (add plot and table outputs)

**Step 1: Add per-scenario summary computation**

After the run loop completes, compute per-scenario aggregated KPIs:

```r
rv$sensitivity_summary <- rv$sensitivity_results %>%
  dplyr::mutate(
    pd_change_pct = ifelse(pd_baseline != 0,
                           (pd_shock - pd_baseline) / pd_baseline * 100, 0)
  ) %>%
  dplyr::group_by(sensitivity_scenario, shock_year) %>%
  dplyr::summarise(
    scenario_label = scenario_label(sensitivity_scenario[1]),
    category = scenario_category(sensitivity_scenario[1]),
    avg_pd_change = if ("exposure_value_usd" %in% names(.)) {
      weighted.mean(pd_change_pct, exposure_value_usd, na.rm = TRUE)
    } else mean(pd_change_pct, na.rm = TRUE),
    total_el_change = if ("expected_loss_difference" %in% names(.))
      sum(expected_loss_difference, na.rm = TRUE) else NA_real_,
    avg_npv_change = if ("crispy_perc_value_change" %in% names(.))
      mean(crispy_perc_value_change, na.rm = TRUE) * 100 else NA_real_,
    n_companies = dplyr::n_distinct(company_id),
    is_primary = sensitivity_scenario[1] == rv$sensitivity_config$primary,
    .groups = "drop"
  )
```

**Step 2: Build summary cards**

Show median and range of the per-scenario KPIs (across all scenarios x years):

```r
output$sensitivity_summary_cards <- renderUI({
  req(rv$sensitivity_summary)
  ss <- rv$sensitivity_summary

  median_pd <- median(ss$avg_pd_change, na.rm = TRUE)
  range_pd <- range(ss$avg_pd_change, na.rm = TRUE)
  median_el <- median(ss$total_el_change, na.rm = TRUE)
  median_npv <- median(ss$avg_npv_change, na.rm = TRUE)

  fluidRow(
    make_card("Scenarios Tested", nrow(dplyr::distinct(ss, sensitivity_scenario))),
    make_card("Median PD Change", paste0(round(median_pd, 1), "%")),
    make_card("PD Change Range",
      paste0(round(range_pd[1], 0), "% to ", round(range_pd[2], 0), "%")),
    make_card("Median EL Change", format_currency(median_el)),
    make_card("Median NPV Change", paste0(round(median_npv, 1), "%"))
  )
})
```

**Step 3: Build violin/dot plots**

Three plots, each using `geom_violin()` for the distribution and `geom_point()` for individual scenario values. The primary scenario is highlighted with a distinct marker.

Group scenarios by NGFS category on the x-axis. Use `geom_hline()` to show the primary scenario's value as a reference line.

Same pattern for EL and NPV plots. Use `trisk_plot_theme()` + `plotly_theme_colors()` for consistent styling. Each plot should be wrapped in `ggplotly()` with `PLOTLY_CONFIG` and `PLOTLY_HOVERLABEL`.

**Step 4: Build comparison table**

DT table with one row per scenario x shock year, showing all KPIs. Primary scenario row highlighted in bold.

**Step 5: Commit**

---

### Task 6: Full Verification and Integration

**Step 1: Rebuild Docker and test end-to-end**

Run: `docker compose -f docker-compose.dev.yml up --build -d`

Verification checklist:
1. Load demo data -> Configure -> Run main analysis (3 shock years)
2. Navigate to "5. Scenario Sensitivity" tab
3. Verify auto-detected scenarios are listed (should show 17 additional NGFS 2024 scenarios)
4. Click "Run Sensitivity Analysis" -> verify progress bar
5. After completion:
   - Summary cards show meaningful values
   - Violin plots render with dots grouped by NGFS category
   - Primary scenario (NGFS2024GCAM_NZ2050) highlighted in red
   - Comparison table shows all scenarios with KPIs
6. Check Docker logs for errors
7. Test empty state: navigate to sensitivity tab BEFORE running main analysis -> should show "Run main analysis first"

**Step 2: Commit any fixes**

---

## Layout Reference

```
+-----------------------------------------------------------+
| Section Header: Scenario Sensitivity Analysis             |
+-----------------------------------------------------------+

Before run:
+-----------------------------------------------------------+
| Your target: Net Zero 2050 (NGFS 2024, GCAM)              |
|                                                           |
| 17 additional NGFS 2024 target scenarios detected:        |
|                                                           |
| Orderly (8): NZ2050x2, B2DSx3, LDx3                      |
| Disorderly (3): DTx3                                      |
| Too Little, Too Late (6): FWx3, NDCx3                     |
|                                                           |
| [ Run Sensitivity Analysis ]                              |
+-----------------------------------------------------------+

After run:
+----------+-----------+----------+---------+----------+
| Scenarios| Median PD | PD Range | Med. EL | Med. NPV |
| Tested   | Change    |          | Change  | Change   |
+----------+-----------+----------+---------+----------+
+--------------------------+--------------------------+
| PD Change Distribution   | Expected Loss Distribn   |
| [violin + dots by cat]   | [violin + dots by cat]   |
| * = your scenario        | * = your scenario        |
+--------------------------+--------------------------+
+-----------------------------------------------------------+
| NPV Change Distribution (full width)                      |
| [violin + dots by NGFS category]                          |
| * = your scenario                                         |
+-----------------------------------------------------------+
+-----------------------------------------------------------+
| Comparison Table (DT)                                     |
| Scenario | Category | Year | PD% | EL | NPV% | Cos      |
+-----------------------------------------------------------+
```

## Files Modified

| File | Changes |
|------|---------|
| `app/global.R` | Add `parse_scenario_code()`, `find_sensitivity_scenarios()` (~50 lines); move `extract_result_df()` from server.R |
| `app/ui.R` | New sidebar menu item; new `tabItem("sensitivity")` with `uiOutput("sensitivity_content_ui")` |
| `app/server.R` | New reactive values (`sensitivity_*`); source and call `setup_sensitivity()`; remove inline `extract_result_df` |
| `app/modules/mod_sensitivity.R` | **New file** -- full sensitivity module: scenario detection observer, run handler, results rendering (summary cards, 3 violin plots, comparison table) |
