# mod_config.R
# Configuration: scenario selection, parameter sync, defaults, warnings

setup_config <- function(input, output, session, rv) {
  # ============================================
  # Update scenario choices (filtered by scenario_type)
  # ============================================

  observe({
    req(rv$scenarios)

    has_type_col <- "scenario_type" %in% names(rv$scenarios)
    all_scenarios <- sort(unique(rv$scenarios$scenario))

    if (has_type_col) {
      baseline_scenarios <- sort(unique(
        rv$scenarios$scenario[tolower(rv$scenarios$scenario_type) == "baseline"]
      ))
      target_scenarios <- sort(unique(
        rv$scenarios$scenario[tolower(rv$scenarios$scenario_type) == "target"]
      ))
      if (length(baseline_scenarios) == 0) baseline_scenarios <- all_scenarios
      if (length(target_scenarios) == 0) target_scenarios <- all_scenarios
    } else {
      baseline_scenarios <- all_scenarios
      target_scenarios <- all_scenarios
    }

    geographies <- sort(unique(rv$scenarios$scenario_geography))

    # Determine the primary default baseline (NGFS GCAM 2024 CP preferred)
    ngfs_gcam_cp_candidates <- grep("^NGFS\\d{4}[_]?GCAM[_]?CP$", baseline_scenarios, value = TRUE)
    baseline_default <- if (length(ngfs_gcam_cp_candidates) > 0) {
      # Pick the most recent year
      ngfs_gcam_cp_candidates[length(ngfs_gcam_cp_candidates)]
    } else if ("NGFS2023GCAM_CP" %in% baseline_scenarios) {
      "NGFS2023GCAM_CP"
    } else {
      baseline_scenarios[1]
    }

    ngfs_gcam_nz_candidates <- grep("^NGFS\\d{4}[_]?GCAM[_]?NZ2050$", target_scenarios, value = TRUE)
    target_default <- if (length(ngfs_gcam_nz_candidates) > 0) {
      ngfs_gcam_nz_candidates[length(ngfs_gcam_nz_candidates)]
    } else if ("NGFS2023GCAM_NZ2050" %in% target_scenarios) {
      "NGFS2023GCAM_NZ2050"
    } else {
      target_scenarios[1]
    }

    geo_default <- if ("Global" %in% geographies) "Global" else geographies[1]

    # Build flat labeled choices for baseline multi-select (selectize doesn't do optgroups well in multi mode)
    baseline_labels <- sapply(baseline_scenarios, function(s) {
      paste0(scenario_label(s), "  [", s, "]")
    }, USE.NAMES = FALSE)
    baseline_choices_flat <- setNames(baseline_scenarios, baseline_labels)

    # For multi-select target, we need a flat named vector
    target_labels <- sapply(target_scenarios, function(s) {
      paste0(scenario_label(s), "  [", s, "]")
    }, USE.NAMES = FALSE)
    target_choices_flat <- setNames(target_scenarios, target_labels)

    updateSelectizeInput(session, "baseline_scenario",
                         choices = baseline_choices_flat, selected = baseline_default,
                         server = FALSE)
    updateSelectizeInput(session, "target_scenarios",
                         choices = target_choices_flat, selected = target_default,
                         server = FALSE)
    updateSelectInput(session, "scenario_geography",
                      choices = geographies, selected = geo_default)

    # Store target scenarios by category for quick-select buttons
    rv$target_scenarios_all <- target_scenarios
    rv$target_scenario_categories <- sapply(target_scenarios, scenario_category, USE.NAMES = TRUE)
    # Store all available baseline scenario codes for baseline matching
    rv$available_baselines <- baseline_scenarios
  })

  # ---- Quick-select buttons for target scenarios ----
  observeEvent(input$sel_orderly, {
    req(rv$target_scenario_categories)
    cats <- rv$target_scenario_categories
    selected <- names(cats[grepl("Orderly", cats)])
    updateSelectizeInput(session, "target_scenarios", selected = selected)
  })
  observeEvent(input$sel_disorderly, {
    req(rv$target_scenario_categories)
    cats <- rv$target_scenario_categories
    selected <- names(cats[grepl("Disorderly", cats)])
    updateSelectizeInput(session, "target_scenarios", selected = selected)
  })
  observeEvent(input$sel_hotthouse, {
    req(rv$target_scenario_categories)
    cats <- rv$target_scenario_categories
    selected <- names(cats[grepl("Hot House", cats)])
    updateSelectizeInput(session, "target_scenarios", selected = selected)
  })
  observeEvent(input$sel_all_targets, {
    req(rv$target_scenarios_all)
    updateSelectizeInput(session, "target_scenarios", selected = rv$target_scenarios_all)
  })
  observeEvent(input$sel_clear_targets, {
    updateSelectizeInput(session, "target_scenarios", selected = character(0))
  })

  # ---- Slider <-> Numeric input sync (Advanced Assumptions) ----
  observeEvent(input$risk_free_rate, {
    if (!isTRUE(all.equal(input$risk_free_rate, input$risk_free_rate_num))) {
      updateNumericInput(session, "risk_free_rate_num", value = input$risk_free_rate)
    }
  })
  observeEvent(input$risk_free_rate_num, {
    val <- input$risk_free_rate_num
    if (!is.null(val) && !is.na(val) && val >= 0 && val <= 0.1) {
      if (!isTRUE(all.equal(val, input$risk_free_rate))) {
        updateSliderInput(session, "risk_free_rate", value = val)
      }
    }
  })

  observeEvent(input$discount_rate, {
    if (!isTRUE(all.equal(input$discount_rate, input$discount_rate_num))) {
      updateNumericInput(session, "discount_rate_num", value = input$discount_rate)
    }
  })
  observeEvent(input$discount_rate_num, {
    val <- input$discount_rate_num
    if (!is.null(val) && !is.na(val) && val >= 0 && val <= 0.2) {
      if (!isTRUE(all.equal(val, input$discount_rate))) {
        updateSliderInput(session, "discount_rate", value = val)
      }
    }
  })

  observeEvent(input$growth_rate, {
    if (!isTRUE(all.equal(input$growth_rate, input$growth_rate_num))) {
      updateNumericInput(session, "growth_rate_num", value = input$growth_rate)
    }
  })
  observeEvent(input$growth_rate_num, {
    val <- input$growth_rate_num
    if (!is.null(val) && !is.na(val) && val >= 0 && val <= 0.1) {
      if (!isTRUE(all.equal(val, input$growth_rate))) {
        updateSliderInput(session, "growth_rate", value = val)
      }
    }
  })

  observeEvent(input$market_passthrough, {
    if (!isTRUE(all.equal(input$market_passthrough, input$market_passthrough_num))) {
      updateNumericInput(session, "market_passthrough_num", value = input$market_passthrough)
    }
  })
  observeEvent(input$market_passthrough_num, {
    val <- input$market_passthrough_num
    if (!is.null(val) && !is.na(val) && val >= 0 && val <= 1) {
      if (!isTRUE(all.equal(val, input$market_passthrough))) {
        updateSliderInput(session, "market_passthrough", value = val)
      }
    }
  })

  # ---- Reset to defaults button ----
  observeEvent(input$reset_defaults, {
    defs <- config_defaults()
    updateSliderInput(session, "risk_free_rate", value = defs$risk_free_rate)
    updateNumericInput(session, "risk_free_rate_num", value = defs$risk_free_rate)
    updateSliderInput(session, "discount_rate", value = defs$discount_rate)
    updateNumericInput(session, "discount_rate_num", value = defs$discount_rate)
    updateSliderInput(session, "growth_rate", value = defs$growth_rate)
    updateNumericInput(session, "growth_rate_num", value = defs$growth_rate)
    updateSliderInput(session, "market_passthrough", value = defs$market_passthrough)
    updateNumericInput(session, "market_passthrough_num", value = defs$market_passthrough)
    updateRadioButtons(session, "carbon_price_model", selected = defs$carbon_price_model)
    updateSelectizeInput(session, "shock_years", selected = defs$shock_years)
    showNotification("All parameters reset to defaults.", type = "message", duration = 3)
  })

  # ---- Scenario safety warnings ----
  output$scenario_warnings <- renderUI({
    warnings <- list()

    # No baseline selected
    if (is.null(input$baseline_scenario) || length(input$baseline_scenario) == 0) {
      warnings <- c(warnings, list(
        div(class = "alert alert-danger alert-compact",
            icon("exclamation-triangle"), " No baseline scenario selected. Analysis requires at least one baseline.")
      ))
    }

    # No target selected
    if (is.null(input$target_scenarios) || length(input$target_scenarios) == 0) {
      warnings <- c(warnings, list(
        div(class = "alert alert-danger alert-compact",
            icon("exclamation-triangle"), " No target scenario selected. Select at least one target.")
      ))
    }

    # Baseline == one of the targets
    if (!is.null(input$baseline_scenario) && length(input$baseline_scenario) > 0 &&
        !is.null(input$target_scenarios) && any(input$baseline_scenario %in% input$target_scenarios)) {
      overlap <- intersect(input$baseline_scenario, input$target_scenarios)
      warnings <- c(warnings, list(
        div(class = "alert alert-warning alert-compact",
            icon("exclamation-circle"), paste0(" Baseline also selected as target: ",
                                              paste(overlap, collapse = ", "),
                                              ". Results will show zero change for those scenarios."))
      ))
    }

    # Show baseline mapping info when targets are selected
    if (!is.null(input$baseline_scenario) && length(input$baseline_scenario) > 0 &&
        !is.null(input$target_scenarios) && length(input$target_scenarios) > 0) {
      bmap <- build_baseline_map(input$target_scenarios, input$baseline_scenario, input$baseline_scenario[1])
      # Show mapping if multiple distinct baselines are used
      unique_baselines <- unique(unname(bmap))
      if (length(unique_baselines) > 1) {
        mapping_lines <- sapply(names(bmap), function(tgt) {
          tags$li(class = "fs-12",
                  tags$code(scenario_label(tgt)), " \u2192 baseline: ", tags$code(scenario_label(bmap[tgt])))
        }, USE.NAMES = FALSE)
        warnings <- c(warnings, list(
          div(class = "alert alert-info alert-compact",
              icon("info-circle"), " Multiple baselines active. Mapping:",
              tags$ul(class = "compact-list", mapping_lines))
        ))
      }
    }

    # Performance warning: >5 targets
    if (!is.null(input$target_scenarios) && length(input$target_scenarios) > 5) {
      warnings <- c(warnings, list(
        div(class = "alert alert-info alert-compact",
            icon("info-circle"), paste0(" ", length(input$target_scenarios), " target scenarios selected. Runs with >5 targets may be slow."))
      ))
    }

    if (length(warnings) > 0) do.call(tagList, warnings) else NULL
  })

  output$scenario_info <- renderUI({
    req(rv$scenarios)
    has_type_col <- "scenario_type" %in% names(rv$scenarios)

    if (has_type_col) {
      n_baseline <- length(unique(
        rv$scenarios$scenario[tolower(rv$scenarios$scenario_type) == "baseline"]
      ))
      n_target <- length(unique(
        rv$scenarios$scenario[tolower(rv$scenarios$scenario_type) == "target"]
      ))
      n_total <- length(unique(rv$scenarios$scenario))
      tagList(
        tags$p(tags$small(paste(n_baseline, "baseline scenario(s),", n_target, "target scenario(s),", n_total, "total"))),
        tags$p(tags$small(paste("Geographies:", paste(sort(unique(rv$scenarios$scenario_geography)), collapse = ", "))))
      )
    } else {
      tags$p(tags$small("Note: scenario data has no 'scenario_type' column. All scenarios shown in both dropdowns."))
    }
  })

  # ---- Empty state navigation links ----
  observeEvent(input$goto_config_horizon, {
    updateTabItems(session, "tabs", selected = "config")
  })
  observeEvent(input$goto_config_scenario, {
    updateTabItems(session, "tabs", selected = "config")
  })
  observeEvent(input$goto_run_attribution, {
    updateTabItems(session, "tabs", selected = "run")
  })
  observeEvent(input$goto_run_concentration, {
    updateTabItems(session, "tabs", selected = "run")
  })
}
