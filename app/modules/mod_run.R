# mod_run.R
# Run Analysis: execution loop, run history, comparison, navigation

setup_run <- function(input, output, session, rv, log_message) {
  # ============================================
  # Run checklist
  # ============================================

  output$run_checklist <- renderUI({
    checks <- list(
      portfolio = !is.null(rv$portfolio) && is.null(validate_portfolio(rv$portfolio)),
      assets = !is.null(rv$assets) && is.null(validate_assets(rv$assets)),
      financial = !is.null(rv$financial) && is.null(validate_financial(rv$financial)),
      scenarios = !is.null(rv$scenarios),
      carbon = !is.null(rv$carbon),
      baseline = !is.null(input$baseline_scenario) && length(input$baseline_scenario) > 0,
      target = !is.null(input$target_scenarios) && length(input$target_scenarios) > 0
    )

    tagList(
      tags$p(
        if (checks$portfolio) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Portfolio data uploaded"
      ),
      tags$p(
        if (checks$assets) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Assets data uploaded"
      ),
      tags$p(
        if (checks$financial) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Financial data uploaded"
      ),
      tags$p(
        if (checks$scenarios) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Scenarios data available"
      ),
      tags$p(
        if (checks$carbon) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Carbon price data available"
      ),
      tags$p(
        if (checks$baseline) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Baseline scenario selected"
      ),
      tags$p(
        if (checks$target) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Target scenario selected"
      ),
      hr(),
      if (all(unlist(checks))) {
        tags$p(class = "status-ok", strong(icon("rocket"), " Ready to run analysis!"))
      } else {
        tags$p(class = "status-missing", strong("Please complete all items above"))
      }
    )
  })

  output$config_summary <- renderText({
    target_labels <- paste(sapply(input$target_scenarios, scenario_label), collapse = ", ")
    paste(
      "Baseline Scenario(s):", paste(sapply(input$baseline_scenario, scenario_label), collapse = ", "),
      "\nTarget Scenario(s):", target_labels,
      "\nGeography:", input$scenario_geography,
      "\nShock Year(s):", paste(input$shock_years, collapse = ", "),
      "\nRisk-Free Rate:", input$risk_free_rate,
      "\nDiscount Rate:", input$discount_rate,
      "\nGrowth Rate:", input$growth_rate,
      "\nMarket Passthrough:", input$market_passthrough,
      "\nCarbon Price Model:", input$carbon_price_model
    )
  })

  # ============================================
  # Run analysis
  # ============================================

  # Confirmation modal before re-running (only if results already exist)
  observeEvent(input$run_analysis, {
    req(rv$portfolio, rv$assets, rv$financial, rv$scenarios)

    # Validate at least one shock year selected
    if (is.null(input$shock_years) || length(input$shock_years) == 0) {
      showNotification("Please select at least one shock year on the Configure tab.", type = "error")
      return()
    }
    # Validate at least one target scenario selected
    if (is.null(input$target_scenarios) || length(input$target_scenarios) == 0) {
      showNotification("Please select at least one target scenario on the Configure tab.", type = "error")
      return()
    }

    if (!is.null(rv$results)) {
      # Show confirmation modal
      showModal(modalDialog(
        title = tags$span(icon("triangle-exclamation"), " Re-run Analysis?"),
        tags$p("You already have analysis results. Running again will:"),
        tags$ul(
          tags$li("Save the current run to ", tags$b("Run History"), " (up to 5 runs kept)"),
          tags$li("Replace current TRISK results with new ones"),
          tags$li("Reset PD/EL integration results (must re-calculate)"),
          if (!is.null(rv$internal_pd) || !is.null(rv$internal_el))
            tags$li(tags$span(class = "text-success",
                              icon("check-circle"),
                              " Your uploaded Internal PD/EL values will be ", tags$b("preserved")))
        ),
        tags$p(class = "mt-10",
               tags$em("Tip: You can compare previous runs from the Portfolio Results tab.")),
        footer = tagList(
          modalButton("Cancel"),
          actionButton("confirm_rerun", "Re-run Analysis",
                      class = "btn-success", icon = icon("play-circle"))
        ),
        easyClose = TRUE
      ))
    } else {
      # First run: proceed directly
      execute_trisk_analysis()
    }
  })

  # Handle confirmed re-run
  observeEvent(input$confirm_rerun, {
    removeModal()
    execute_trisk_analysis()
  })

  # The actual analysis logic (extracted to reusable function)
  execute_trisk_analysis <- function() {
    req(rv$portfolio, rv$assets, rv$financial, rv$scenarios)

    # --- SAVE CURRENT RUN TO HISTORY (before overwriting) ---
    if (!is.null(rv$results)) {
      current_config <- list(
        baseline_scenario = paste(input$baseline_scenario, collapse = ", "),
        baseline_scenarios = input$baseline_scenario,
        target_scenario = paste(input$target_scenarios, collapse = ", "),
        target_scenarios = input$target_scenarios,
        scenario_geography = input$scenario_geography,
        shock_year = paste(input$shock_years, collapse = ", "),
        shock_years = input$shock_years,
        risk_free_rate = input$risk_free_rate,
        discount_rate = input$discount_rate,
        growth_rate = input$growth_rate,
        carbon_price_model = input$carbon_price_model,
        market_passthrough = input$market_passthrough
      )

      history_entry <- list(
        results = rv$results,
        config = current_config,
        run_id = rv$run_id,
        timestamp = Sys.time(),
        n_companies = nrow(rv$results),
        pd_integration = rv$pd_integration_result,
        el_integration = rv$el_integration_result
      )

      # Prepend to history (most recent first), cap at 5
      rv$run_history <- c(list(history_entry), rv$run_history)
      if (length(rv$run_history) > 5) {
        rv$run_history <- rv$run_history[1:5]
        gc()
      }
      rv$compare_run_idx <- NULL

      log_message(paste("Saved previous run to history. History size:", length(rv$run_history)))
    }

    rv$analysis_log <- character()
    log_message("Starting TRISK analysis...")

    withProgress(message = "Running TRISK analysis...", value = 0, {

      tryCatch({

        incProgress(0.1, detail = "Validating data...")
        log_message("Validating input data...")

        incProgress(0.2, detail = "Preparing data...")
        log_message("--- Input Data ---")
        log_message(paste("  Portfolio rows:", nrow(rv$portfolio)))
        log_message(paste("  Portfolio columns:", paste(names(rv$portfolio), collapse = ", ")))
        log_message(paste("  Assets rows:", nrow(rv$assets)))
        log_message(paste("  Assets companies:", length(unique(rv$assets$company_id))))
        log_message(paste("  Financial rows:", nrow(rv$financial)))
        log_message(paste("  Scenarios rows:", nrow(rv$scenarios)))
        log_message(paste("  Scenarios available:", paste(unique(rv$scenarios$scenario), collapse = ", ")))
        log_message(paste("  Carbon price rows:", if (!is.null(rv$carbon)) nrow(rv$carbon) else "NULL"))
        target_scenarios_run <- input$target_scenarios
        n_scenarios <- length(target_scenarios_run)

        # ---- BUILD BASELINE MAP ----
        # The user may select multiple baselines; the first one is the default.
        # Each target scenario is mapped to its family-matched baseline.
        selected_baselines <- input$baseline_scenario
        default_baseline <- selected_baselines[1]
        baseline_map <- build_baseline_map(target_scenarios_run, selected_baselines, default_baseline)

        log_message("--- Parameters ---")
        log_message(paste("  Selected baseline(s):", paste(selected_baselines, collapse = ", ")))
        log_message(paste("  Default baseline:", default_baseline))
        log_message(paste("  Baseline mapping:"))
        for (tgt in names(baseline_map)) {
          log_message(paste0("    ", tgt, " -> ", baseline_map[tgt]))
        }
        log_message(paste("  Target scenario(s):", paste(target_scenarios_run, collapse = ", ")))
        log_message(paste("  Number of target scenarios:", n_scenarios))
        log_message(paste("  Geography:", input$scenario_geography))
        log_message(paste("  Shock year(s):", paste(input$shock_years, collapse = ", ")))
        log_message(paste("  Risk-free rate:", input$risk_free_rate))
        log_message(paste("  Discount rate:", input$discount_rate))
        log_message(paste("  Growth rate:", input$growth_rate))
        log_message(paste("  Market passthrough:", input$market_passthrough))
        log_message(paste("  Carbon price model:", input$carbon_price_model))

        incProgress(0.2, detail = "Preparing data...")

        # Validate year range compatibility between assets and scenarios
        assets_for_run <- rv$assets
        assets_min_year <- min(assets_for_run$production_year)
        assets_max_year <- max(assets_for_run$production_year)

        all_baselines_used <- unique(unname(baseline_map))
        selected_scenarios <- rv$scenarios %>%
          filter(
            .data$scenario %in% c(all_baselines_used, target_scenarios_run),
            .data$scenario_geography %in% input$scenario_geography
          )
        scen_min_year <- min(selected_scenarios$scenario_year)
        scen_max_year <- max(selected_scenarios$scenario_year)

        log_message("--- Year Range Check ---")
        log_message(paste("  Assets years:", assets_min_year, "-", assets_max_year))
        log_message(paste("  Scenario years (filtered):", scen_min_year, "-", scen_max_year))

        if (assets_min_year < scen_min_year) {
          log_message(paste("  WARNING: Assets start year", assets_min_year,
                           "< scenario start year", scen_min_year,
                           "- trimming assets"))
          assets_for_run <- assets_for_run %>% filter(.data$production_year >= scen_min_year)
          log_message(paste("  Assets after trim:", nrow(assets_for_run), "rows,",
                           "years:", min(assets_for_run$production_year), "-", max(assets_for_run$production_year)))
        }

        if (assets_max_year > scen_max_year) {
          log_message(paste("  WARNING: Assets end year", assets_max_year,
                           "> scenario end year", scen_max_year,
                           "- trimming assets"))
          assets_for_run <- assets_for_run %>% filter(.data$production_year <= scen_max_year)
          log_message(paste("  Assets after trim:", nrow(assets_for_run), "rows,",
                           "years:", min(assets_for_run$production_year), "-", max(assets_for_run$production_year)))
        }

        # ---- MULTI-SCENARIO × MULTI-HORIZON LOOP ----
        shock_years <- sort(as.integer(input$shock_years))
        n_years <- length(shock_years)
        total_runs <- n_scenarios * n_years
        run_count <- 0

        # Helper to extract result df from raw output
        extract_result_df <- function(raw) {
          if (is.data.frame(raw)) return(raw)
          if (is.list(raw)) {
            if ("portfolio_results_tech_detail" %in% names(raw)) return(raw$portfolio_results_tech_detail)
            if ("portfolio_results" %in% names(raw)) return(raw$portfolio_results)
            df_idx <- which(sapply(raw, is.data.frame))
            if (length(df_idx) > 0) return(raw[[df_idx[1]]])
          }
          stop("Could not extract a data.frame from analysis results")
        }

        # Store results: scenario -> year -> df
        all_scenario_results <- list()

        for (scen_i in seq_along(target_scenarios_run)) {
          target_scen <- target_scenarios_run[scen_i]
          scen_label <- scenario_label(target_scen)
          all_year_results <- list()

          # Resolve baseline for this target scenario
          matched_baseline <- baseline_map[target_scen]

          for (yr_i in seq_along(shock_years)) {
            yr <- shock_years[yr_i]
            run_count <- run_count + 1
            progress_val <- 0.2 + 0.6 * (run_count / total_runs)
            incProgress(progress_val - (0.2 + 0.6 * ((run_count - 1) / total_runs)),
                        detail = paste0("Scenario ", scen_i, "/", n_scenarios,
                                       ", year ", yr, " (", run_count, "/", total_runs, ")..."))
            log_message(paste0("--- Scenario: ", target_scen, " | Baseline: ", matched_baseline,
                              " | Year: ", yr, " (", run_count, "/", total_runs, ") ---"))

            raw_results <- run_trisk_on_portfolio(
              assets_data = assets_for_run,
              scenarios_data = rv$scenarios,
              financial_data = rv$financial,
              carbon_data = rv$carbon,
              portfolio_data = rv$portfolio,
              baseline_scenario = matched_baseline,
              target_scenario = target_scen,
              scenario_geography = input$scenario_geography,
              shock_year = yr,
              risk_free_rate = input$risk_free_rate,
              discount_rate = input$discount_rate,
              growth_rate = input$growth_rate,
              market_passthrough = input$market_passthrough
            )

            result_df <- extract_result_df(raw_results)
            result_df <- compute_el_columns(result_df)
            result_df$shock_year <- yr
            result_df$target_scenario <- target_scen
            result_df$baseline_scenario <- matched_baseline
            all_year_results[[as.character(yr)]] <- result_df
            log_message(paste0("  ", nrow(result_df), " rows"))
          }

          all_scenario_results[[target_scen]] <- all_year_results
          rm(all_year_results); gc()
        }

        # Primary results = first scenario, first year (backward compatibility)
        primary_scenario <- target_scenarios_run[1]
        primary_year <- as.character(shock_years[1])
        rv$results <- all_scenario_results[[primary_scenario]][[primary_year]]
        rv$results_by_year <- all_scenario_results[[primary_scenario]]
        rv$results_by_scenario <- all_scenario_results
        log_message("Computed EL and derived columns for all scenarios × shock years")

        log_message("--- Results (Primary) ---")
        log_message(paste("  Primary scenario:", primary_scenario))
        log_message(paste("  Primary shock year:", primary_year))
        log_message(paste("  Result rows:", nrow(rv$results)))
        log_message(paste("  Result columns:", paste(names(rv$results), collapse = ", ")))
        log_message(paste("  Total scenarios:", n_scenarios, "| Total years:", n_years,
                         "| Total runs:", total_runs))

        if ("crispy_perc_value_change" %in% names(rv$results)) {
          log_message(paste("  Avg NPV change:", smart_round(mean(rv$results$crispy_perc_value_change, na.rm = TRUE) * 100), "%"))
        }
        if ("pd_shock" %in% names(rv$results)) {
          log_message(paste("  Max PD shock:", smart_round(max(rv$results$pd_shock, na.rm = TRUE) * 100), "%"))
        }
        if ("expected_loss_shock" %in% names(rv$results)) {
          log_message(paste("  Total expected loss (shock):", smart_round(sum(rv$results$expected_loss_shock, na.rm = TRUE))))
        }
        if ("sector" %in% names(rv$results)) {
          log_message(paste("  Sectors:", paste(unique(rv$results$sector), collapse = ", ")))
        }
        if ("technology" %in% names(rv$results)) {
          log_message(paste("  Technologies:", paste(unique(rv$results$technology), collapse = ", ")))
        }

        # Reset computed integration results (need recalculation with new model outputs)
        # BUT preserve user-uploaded Internal PD/EL values across runs
        rv$pd_integration_result <- NULL
        rv$el_integration_result <- NULL
        # NOTE: rv$internal_pd and rv$internal_el are intentionally preserved

        rv$run_id <- generate_run_id(list(
          baseline = paste(input$baseline_scenario, collapse = ","),
          targets = paste(input$target_scenarios, collapse = ","),
          geography = input$scenario_geography,
          shock_years = paste(input$shock_years, collapse = ","),
          discount_rate = input$discount_rate,
          growth_rate = input$growth_rate,
          risk_free_rate = input$risk_free_rate,
          market_passthrough = input$market_passthrough,
          carbon_price_model = input$carbon_price_model
        ))

        incProgress(0.9, detail = "Finalizing...")
        log_message(paste("Analysis complete! Run ID:", rv$run_id))

        n_yr_label <- if (n_years > 1) paste0(" across ", n_years, " shock years") else ""
        showNotification(paste0("Analysis completed successfully!", n_yr_label), type = "message", duration = 5)

        # Reclaim memory after large analysis
        gc()

        # Switch to results tab
        updateTabsetPanel(session, "tabs", selected = "results")

      }, error = function(e) {
        log_message(paste("ERROR:", e$message))
        message("[ERROR] Analysis failed: ", e$message)
        showNotification(
          "Analysis failed. Check the log for details.",
          type = "error",
          duration = 10
        )
      })
    })
  }

  output$analysis_log <- renderText({
    paste(rv$analysis_log, collapse = "\n")
  })

  # ---- Run ID display (Step 3) ----
  output$run_id_display <- renderUI({
    if (is.null(rv$run_id)) return(NULL)
    # NOTE: needs CSS class .run-id-label { font-size: 13px; padding: 4px 10px; }
    div(class = "mt-10",
      tags$span(class = "label label-info run-id-label",
                icon("fingerprint"), paste(" Run ID:", rv$run_id))
    )
  })

  # ---- Run ID display (Step 6) ----
  output$run_id_step6 <- renderUI({
    if (is.null(rv$run_id)) return(NULL)
    div(class = "mb-10",
      tags$span(class = "label label-info run-id-label",
                icon("fingerprint"), paste(" Run ID:", rv$run_id))
    )
  })

  # ============================================
  # Run History Panel (on Results tab)
  # ============================================

  output$run_history_panel <- renderUI({
    if (length(rv$run_history) == 0) return(NULL)

    history_rows <- lapply(seq_along(rv$run_history), function(i) {
      run <- rv$run_history[[i]]
      cfg <- run$config
      ts <- format(run$timestamp, "%H:%M:%S %b %d")
      baseline_lbl <- scenario_label(cfg$baseline_scenario)
      # Handle multi-scenario config (target_scenario may be comma-separated)
      target_codes <- if (!is.null(cfg$target_scenarios)) cfg$target_scenarios
                      else strsplit(cfg$target_scenario, ",\\s*")[[1]]
      if (length(target_codes) <= 2) {
        target_lbl <- paste(sapply(target_codes, scenario_label), collapse = ", ")
      } else {
        target_lbl <- paste0(scenario_label(target_codes[1]), " + ", length(target_codes) - 1, " more")
      }

      # Compute key metrics for compact display
      avg_npv <- if ("crispy_perc_value_change" %in% names(run$results)) {
        paste0(round(mean(run$results$crispy_perc_value_change, na.rm = TRUE) * 100, 2), "%")
      } else "N/A"
      avg_pd <- if ("pd_shock" %in% names(run$results)) {
        paste0(round(mean(run$results$pd_shock, na.rm = TRUE) * 100, 4), "%")
      } else "N/A"

      is_selected <- !is.null(rv$compare_run_idx) && rv$compare_run_idx == i
      # NOTE: needs CSS classes:
      # .history-row { padding: 8px 12px; margin-bottom: 4px; border-radius: 4px; }
      # .history-row--default { background: #FBF5F2; border-left: 3px solid #DDD0D4; }
      # .history-row--selected { background: #FFFDE7; border-left: 3px solid #F53D3F; }
      row_class <- if (is_selected) "history-row history-row--selected" else "history-row history-row--default"

      tags$div(class = row_class,
        fluidRow(
          # NOTE: needs CSS class .history-index { font-weight: 700; color: #666; font-size: 16px; }
          column(1, tags$span(class = "history-index", paste0("#", i))),
          column(3,
            tags$div(class = "history-meta", ts),
            tags$div(class = "fs-12",
              tags$b(target_lbl), " vs ", baseline_lbl)
          ),
          column(2, tags$div(class = "fs-12",
            tags$span(class = "fg-secondary", "Geography: "), cfg$scenario_geography,
            tags$br(),
            tags$span(class = "fg-secondary", "Shock: "), cfg$shock_year
          )),
          column(2, tags$div(class = "fs-12",
            tags$span(class = "fg-secondary", "NPV: "), tags$b(avg_npv),
            tags$br(),
            tags$span(class = "fg-secondary", "PD: "), tags$b(avg_pd)
          )),
          column(2, tags$div(class = "fs-12",
            tags$span(class = "fg-secondary", "Companies: "), run$n_companies,
            tags$br(),
            tags$span(class = "fg-secondary", "Run: "), run$run_id
          )),
          column(2,
            # NOTE: needs CSS class .btn-history { width: 100%; margin-bottom: 4px; }
            actionButton(paste0("compare_run_", i), "Compare",
                        class = if (is_selected) "btn-warning btn-sm btn-history" else "btn-default btn-sm btn-history",
                        icon = icon(if (is_selected) "eye-slash" else "exchange-alt")),
            actionButton(paste0("dup_run_", i), "Duplicate",
                        class = "btn-info btn-sm w-100",
                        icon = icon("copy"))
          )
        )
      )
    })

    fluidRow(
      box(
        width = 12,
        collapsible = TRUE,
        collapsed = TRUE,
        title = tags$span(icon("clock-rotate-left"),
                          paste0(" Run History (", length(rv$run_history), " previous run",
                                 if (length(rv$run_history) != 1) "s" else "", ")")),
        status = "info",
        solidHeader = FALSE,
        tags$div(class = "history-meta mb-8",
          icon("info-circle"),
          " Click ", tags$b("Compare"), " to see a side-by-side comparison with the current run.
            Internal PD/EL values are preserved across re-runs."
        ),
        tagList(history_rows),
        # Comparison panel (shows when a run is selected)
        uiOutput("run_comparison_panel")
      )
    )
  })

  # Handle compare button clicks for each history entry
  observe({
    lapply(seq_along(rv$run_history), function(i) {
      btn_id <- paste0("compare_run_", i)
      observeEvent(input[[btn_id]], {
        if (!is.null(rv$compare_run_idx) && rv$compare_run_idx == i) {
          rv$compare_run_idx <- NULL  # Toggle off
        } else {
          rv$compare_run_idx <- i  # Select for comparison
        }
      }, ignoreInit = TRUE)
    })
  })

  # Handle duplicate button clicks for each history entry
  observe({
    lapply(seq_along(rv$run_history), function(i) {
      btn_id <- paste0("dup_run_", i)
      observeEvent(input[[btn_id]], {
        if (i > length(rv$run_history)) return()
        cfg <- rv$run_history[[i]]$config

        # Restore all config inputs from the history entry
        updateSelectInput(session, "baseline_scenario", selected = cfg$baseline_scenario)
        if (!is.null(cfg$target_scenarios)) {
          updateSelectizeInput(session, "target_scenarios", selected = cfg$target_scenarios)
        }
        updateSelectInput(session, "scenario_geography", selected = cfg$scenario_geography)
        if (!is.null(cfg$shock_years)) {
          updateSelectizeInput(session, "shock_years", selected = cfg$shock_years)
        }
        if (!is.null(cfg$risk_free_rate)) {
          updateSliderInput(session, "risk_free_rate", value = cfg$risk_free_rate)
          updateNumericInput(session, "risk_free_rate_num", value = cfg$risk_free_rate)
        }
        if (!is.null(cfg$discount_rate)) {
          updateSliderInput(session, "discount_rate", value = cfg$discount_rate)
          updateNumericInput(session, "discount_rate_num", value = cfg$discount_rate)
        }
        if (!is.null(cfg$growth_rate)) {
          updateSliderInput(session, "growth_rate", value = cfg$growth_rate)
          updateNumericInput(session, "growth_rate_num", value = cfg$growth_rate)
        }
        if (!is.null(cfg$market_passthrough)) {
          updateSliderInput(session, "market_passthrough", value = cfg$market_passthrough)
          updateNumericInput(session, "market_passthrough_num", value = cfg$market_passthrough)
        }
        if (!is.null(cfg$carbon_price_model)) {
          updateRadioButtons(session, "carbon_price_model", selected = cfg$carbon_price_model)
        }

        # Navigate to config tab
        updateTabItems(session, "tabs", selected = "config")
        showNotification(paste0("Config loaded from run #", i, ". Adjust and re-run."),
                         type = "message", duration = 3)
      }, ignoreInit = TRUE)
    })
  })

  # Render comparison panel when a history run is selected
  output$run_comparison_panel <- renderUI({
    req(rv$compare_run_idx)
    idx <- rv$compare_run_idx
    if (idx > length(rv$run_history)) return(NULL)

    prev <- rv$run_history[[idx]]
    curr <- rv$results
    if (is.null(curr)) return(NULL)

    prev_cfg <- prev$config
    curr_cfg <- list(
      baseline_scenario = paste(input$baseline_scenario, collapse = ", "),
      target_scenario = paste(input$target_scenarios, collapse = ", "),
      scenario_geography = input$scenario_geography,
      shock_year = paste(input$shock_years, collapse = ", "),
      discount_rate = input$discount_rate,
      growth_rate = input$growth_rate
    )

    # Compute deltas
    curr_npv <- if ("crispy_perc_value_change" %in% names(curr))
      mean(curr$crispy_perc_value_change, na.rm = TRUE) * 100 else NA
    prev_npv <- if ("crispy_perc_value_change" %in% names(prev$results))
      mean(prev$results$crispy_perc_value_change, na.rm = TRUE) * 100 else NA

    curr_pd <- if ("pd_shock" %in% names(curr))
      mean(curr$pd_shock, na.rm = TRUE) * 100 else NA
    prev_pd <- if ("pd_shock" %in% names(prev$results))
      mean(prev$results$pd_shock, na.rm = TRUE) * 100 else NA

    curr_el <- if ("expected_loss_shock" %in% names(curr))
      sum(curr$expected_loss_shock, na.rm = TRUE) else NA
    prev_el <- if ("expected_loss_shock" %in% names(prev$results))
      sum(prev$results$expected_loss_shock, na.rm = TRUE) else NA

    # Delta formatting helper
    delta_span <- function(curr_val, prev_val, fmt = "round2", invert = FALSE) {
      if (is.na(curr_val) || is.na(prev_val)) return(tags$span("N/A"))
      d <- curr_val - prev_val
      is_worse <- if (invert) d < 0 else d > 0
      color <- if (abs(d) < 0.001) "#666" else if (is_worse) "#C44245" else "#6B9F3B"
      arrow <- if (abs(d) < 0.001) "\u2194" else if (d > 0) "\u2191" else "\u2193"
      val_str <- switch(fmt,
        "round2" = paste0(round(d, 2), "%"),
        "round4" = paste0(round(d, 4), "%"),
        "number" = format_number(d)
      )
      tags$span(class = "fw-600", style = paste0("color:", color, ";"), paste0(arrow, " ", val_str))
    }

    # Config diff
    config_diffs <- list()
    if (prev_cfg$baseline_scenario != curr_cfg$baseline_scenario)
      config_diffs <- c(config_diffs, list(tags$li("Baseline: ",
        tags$s(scenario_label(prev_cfg$baseline_scenario)), " \u2192 ",
        tags$b(scenario_label(curr_cfg$baseline_scenario)))))
    if (prev_cfg$target_scenario != curr_cfg$target_scenario)
      config_diffs <- c(config_diffs, list(tags$li("Target: ",
        tags$s(scenario_label(prev_cfg$target_scenario)), " \u2192 ",
        tags$b(scenario_label(curr_cfg$target_scenario)))))
    if (prev_cfg$scenario_geography != curr_cfg$scenario_geography)
      config_diffs <- c(config_diffs, list(tags$li("Geography: ",
        tags$s(prev_cfg$scenario_geography), " \u2192 ",
        tags$b(curr_cfg$scenario_geography))))
    if (prev_cfg$shock_year != curr_cfg$shock_year)
      config_diffs <- c(config_diffs, list(tags$li("Shock Year: ",
        tags$s(prev_cfg$shock_year), " \u2192 ",
        tags$b(curr_cfg$shock_year))))
    if (prev_cfg$discount_rate != curr_cfg$discount_rate)
      config_diffs <- c(config_diffs, list(tags$li("Discount Rate: ",
        tags$s(prev_cfg$discount_rate), " \u2192 ",
        tags$b(curr_cfg$discount_rate))))

    tags$div(
      class = "compare-panel",
      # NOTE: needs CSS class .compare-heading { margin-top: 0; margin-bottom: 12px; font-weight: 600; }
      h4(icon("code-compare"), " Comparison: Current Run vs Run #", idx,
         class = "compare-heading"),

      # Config changes
      # NOTE: needs CSS class .compare-config-list { font-size: 13px; margin-top: 4px; }
      if (length(config_diffs) > 0) {
        tags$div(class = "mb-12",
          tags$span(class = "fw-600 fs-13", icon("sliders"), " Parameter Changes:"),
          tags$ul(class = "compare-config-list", config_diffs)
        )
      } else {
        tags$div(class = "mb-12 fs-13 fg-secondary",
                 icon("equals"), " Same parameters — re-run with identical configuration")
      },

      # NOTE: needs CSS classes:
      # .compare-table { font-size: 13px; margin-bottom: 0; background: white; }
      # .compare-table-header { background: #F0E6EA; }
      # .fg-info { color: #5A8EAE; }
      tags$table(
        class = "table table-bordered compare-table",
        tags$thead(
          tags$tr(class = "compare-table-header",
            tags$th("Metric"),
            tags$th(class = "th-right", paste0("Run #", idx, " (Previous)")),
            tags$th(class = "th-right", "Current Run"),
            tags$th(class = "th-center", "Delta")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Average NPV Change"),
            tags$td(class = "td-right", if (!is.na(prev_npv)) paste0(round(prev_npv, 2), "%") else "N/A"),
            tags$td(class = "td-right", if (!is.na(curr_npv)) paste0(round(curr_npv, 2), "%") else "N/A"),
            tags$td(class = "td-center", delta_span(curr_npv, prev_npv, "round2", invert = TRUE))
          ),
          tags$tr(
            tags$td("Average PD Shock"),
            tags$td(class = "td-right", if (!is.na(prev_pd)) paste0(round(prev_pd, 4), "%") else "N/A"),
            tags$td(class = "td-right", if (!is.na(curr_pd)) paste0(round(curr_pd, 4), "%") else "N/A"),
            tags$td(class = "td-center", delta_span(curr_pd, prev_pd, "round4"))
          ),
          tags$tr(
            tags$td("Total EL Shock"),
            tags$td(class = "td-right", if (!is.na(prev_el)) format_number(prev_el) else "N/A"),
            tags$td(class = "td-right", if (!is.na(curr_el)) format_number(curr_el) else "N/A"),
            tags$td(class = "td-center", delta_span(curr_el, prev_el, "number", invert = TRUE))
          ),
          tags$tr(
            tags$td("Companies"),
            tags$td(class = "td-right", prev$n_companies),
            tags$td(class = "td-right", nrow(curr)),
            tags$td(class = "td-center",
                    if (nrow(curr) == prev$n_companies) tags$span(class = "fg-secondary", "=")
                    else tags$span(class = "fg-info", paste0(nrow(curr) - prev$n_companies)))
          )
        )
      )
    )
  })

}
