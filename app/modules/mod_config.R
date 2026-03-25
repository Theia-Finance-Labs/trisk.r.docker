# mod_config.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_config <- function(input, output, session, rv) {

  # ============================================
  # Update scenario choices (filtered by scenario_type)
  # ============================================

  observe({
    req(rv$scenarios)

    all_scenarios <- sort(unique(rv$scenarios$scenario))

    # Filter by scenario_type: baseline dropdown shows only baselines,
    # target dropdown shows only target/shock scenarios
    has_type_col <- "scenario_type" %in% names(rv$scenarios)
    if (has_type_col) {
      baseline_scenarios <- sort(get_scenarios_by_type(rv$scenarios, "baseline"))
      target_scenarios <- sort(get_scenarios_by_type(rv$scenarios, "target"))
    } else {
      baseline_scenarios <- all_scenarios
      target_scenarios <- all_scenarios
    }

    geographies <- sort(unique(rv$scenarios$scenario_geography))

    # Determine the primary default baseline (NGFS GCAM 2024 CP preferred)
    ngfs_gcam_cp_candidates <- grep("^NGFS\\d{4}[_]?GCAM[_]?CP$", baseline_scenarios, value = TRUE)
    bl_default <- if (length(ngfs_gcam_cp_candidates) > 0) {
      ngfs_gcam_cp_candidates[length(ngfs_gcam_cp_candidates)]
    } else if ("NGFS2023GCAM_CP" %in% baseline_scenarios) {
      "NGFS2023GCAM_CP"
    } else {
      baseline_scenarios[1]
    }

    ngfs_gcam_nz_candidates <- grep("^NGFS\\d{4}[_]?GCAM[_]?NZ2050$", target_scenarios, value = TRUE)
    tgt_default <- if (length(ngfs_gcam_nz_candidates) > 0) {
      ngfs_gcam_nz_candidates[length(ngfs_gcam_nz_candidates)]
    } else if ("NGFS2023GCAM_NZ2050" %in% target_scenarios) {
      "NGFS2023GCAM_NZ2050"
    } else {
      target_scenarios[1]
    }

    geo_default <- if ("Global" %in% geographies) "Global" else geographies[1]

    # Store choices for renderUI — no updateSelectizeInput needed
    rv$baseline_choices <- build_scenario_choices(baseline_scenarios)
    rv$target_choices <- build_scenario_choices(target_scenarios)

    rv$baseline_default <- bl_default
    rv$target_default <- tgt_default

    updateSelectInput(session, "scenario_geography",
                      choices = geographies, selected = geo_default)

    # Store filtered scenario lists
    rv$target_scenarios_all <- target_scenarios
    rv$available_baselines <- baseline_scenarios

    # --- Multi-pair auto-init disabled ---
    # if (length(rv$pair_ids) == 0) {
    #   add_pair_to_ui(baseline_selected = bl_default, target_selected = tgt_default)
    # }
  })

  # ============================================
  # Dynamic sector-aware scenario rows
  # ============================================

  output$scenario_rows_ui <- renderUI({
    req(rv$scenarios)

    groups <- rv$scenario_groups
    if (is.null(groups) || length(groups) == 0) groups <- "core"

    baseline_choices <- rv$baseline_choices
    target_choices   <- rv$target_choices

    # Can't render dropdowns until choices are built
    if (is.null(baseline_choices) || is.null(target_choices)) return(NULL)

    rows <- lapply(groups, function(key) {
      entry <- SECTOR_SCENARIO_DEFAULTS[[key]]

      # Check if the default scenarios exist in available choices
      all_bl_codes  <- unlist(baseline_choices, use.names = FALSE)
      all_tgt_codes <- unlist(target_choices, use.names = FALSE)
      bl_available  <- entry$baseline %in% all_bl_codes
      tgt_available <- entry$target %in% all_tgt_codes

      # Core row keeps the canonical IDs for backward compatibility
      bl_id  <- if (key == "core") "baseline_scenario" else paste0("baseline_scenario_", key)
      tgt_id <- if (key == "core") "target_scenario"   else paste0("target_scenario_", key)

      # Select the right default
      bl_selected  <- if (bl_available) entry$baseline
                      else if (key == "core") rv$baseline_default
                      else NULL
      tgt_selected <- if (tgt_available) entry$target
                      else if (key == "core") rv$target_default
                      else NULL

      div(
        class = "scenario-pair-row",
        style = if (key != "core") "margin-top: 12px; padding-top: 10px; border-top: 1px solid #444;" else "",

        # Row header with label + optional tooltip
        fluidRow(
          column(12,
            tags$label(
              class = "control-label",
              style = "font-weight: 600; margin-bottom: 4px; font-size: 13px;",
              entry$label,
              if (!is.null(entry$tooltip)) {
                tags$span(
                  class = "cursor-help",
                  title = entry$tooltip,
                  style = "margin-left: 6px; color: #999; font-weight: normal; font-size: 12px;",
                  icon("info-circle"),
                  tags$small(paste0(" ", entry$tooltip))
                )
              }
            )
          )
        ),

        # Baseline + Target dropdowns side-by-side
        fluidRow(
          column(6,
            selectizeInput(
              inputId  = bl_id,
              label    = "Baseline (Business-as-Usual)",
              choices  = baseline_choices,
              selected = bl_selected,
              multiple = FALSE,
              options  = list(placeholder = "Select baseline scenario...")
            )
          ),
          column(6,
            selectizeInput(
              inputId  = tgt_id,
              label    = "Target (Transition Goal)",
              choices  = target_choices,
              selected = tgt_selected,
              multiple = FALSE,
              options  = list(placeholder = "Select target scenario...")
            )
          )
        ),

        # Warning if default scenario not in loaded data
        if (!is.null(entry$tooltip) && (!bl_available || !tgt_available)) {
          div(
            class = "alert alert-info",
            style = "padding: 6px 10px; margin: 0 0 6px 0; font-size: 12px;",
            icon("info-circle"),
            " Default scenario not found in loaded data. Please select manually."
          )
        },

        # Help text explaining baseline vs target vs shock (core row only)
        if (key == "core") {
          tags$p(class = "fs-11 fg-secondary mt-4",
            icon("info-circle"),
            " The ", tags$b("baseline"), " represents the business-as-usual pathway (e.g., Current Policies). ",
            "The ", tags$b("target"), " is the climate goal (e.g., Net Zero 2050). ",
            "The ", tags$b("shock scenario"), " is computed by the model: at the shock year, companies are forced from baseline to target.")
        }
      )
    })

    do.call(tagList, rows)
  })

  # --- Per-pair geography fallback ---
  # When a scenario pair doesn't support the selected geography (e.g. IPR only has
  # "Global" while user selected "OecdAndEu"), fall back to "Global".
  get_geography_for_pair <- function(scenario_code, selected_geography) {
    available_geos <- unique(rv$scenarios$scenario_geography[rv$scenarios$scenario == scenario_code])
    if (selected_geography %in% available_geos) {
      return(list(geography = selected_geography, fallback = FALSE))
    }
    return(list(geography = "Global", fallback = TRUE))
  }
  #
  # observeEvent(input$add_pair, {
  #   req(rv$baseline_choices)
  #   add_pair_to_ui()
  # })
  #
  # observeEvent(input$sel_ngfs_nz, { ... })
  # observeEvent(input$sel_sector, { ... })
  # observeEvent(input$sel_ipr, { ... })
  # observeEvent(input$sel_clear_targets, { clear_all_pairs() })
  #
  # --- END MULTI-PAIR HELPERS & QUICK-SELECT ---

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

  # ---- Scenario safety warnings (all pairs) ----
  output$scenario_warnings <- renderUI({
    groups <- rv$scenario_groups
    if (is.null(groups)) groups <- "core"

    warnings <- list()

    for (key in groups) {
      entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
      if (key == "core") {
        bl <- input$baseline_scenario; tgt <- input$target_scenario
      } else {
        bl <- input[[paste0("baseline_scenario_", key)]]
        tgt <- input[[paste0("target_scenario_", key)]]
      }
      if (!is.null(bl) && !is.null(tgt) && nzchar(bl) && nzchar(tgt) && bl == tgt) {
        warnings <- c(warnings, list(
          div(class = "alert alert-warning", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
              icon("exclamation-circle"),
              paste0(" ", entry$label, ": Baseline and target are the same. Results will show zero change."))
        ))
      }
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
