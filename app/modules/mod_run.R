# mod_run.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_run <- function(input, output, session, rv, log_message) {

  # ============================================
  # Run checklist
  # ============================================

  output$run_checklist <- renderUI({
    bl <- input$baseline_scenario
    tgt <- input$target_scenario
    has_core_selected <- !is.null(bl) && nzchar(bl) && !is.null(tgt) && nzchar(tgt)

    checks <- list(
      portfolio = !is.null(rv$portfolio) && is.null(validate_portfolio(rv$portfolio)),
      assets = !is.null(rv$assets) && is.null(validate_assets(rv$assets)),
      financial = !is.null(rv$financial) && is.null(validate_financial(rv$financial)),
      scenarios = !is.null(rv$scenarios),
      carbon = !is.null(rv$carbon),
      pair = has_core_selected
    )

    # Build sector coverage info
    sector_items <- list()
    if (!is.null(rv$detected_sectors) && length(rv$detected_sectors) > 0) {
      sector_items <- list(
        tags$p(
          icon("check-circle", class = "status-ok"),
          paste(" Portfolio sectors:", paste(rv$detected_sectors, collapse = ", "))
        ),
        tags$p(
          icon("check-circle", class = "status-ok"),
          paste(" Scenario pairs:", length(rv$scenario_groups))
        )
      )
    }

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
        if (checks$pair) icon("check-circle", class = "status-ok") else icon("times-circle", class = "status-missing"),
        " Baseline and target scenarios selected"
      ),
      do.call(tagList, sector_items),
      hr(),
      if (all(unlist(checks))) {
        tags$p(class = "status-ok", strong(icon("rocket"), " Ready to run analysis!"))
      } else {
        tags$p(class = "status-missing", strong("Please complete all items above"))
      }
    )
  })

  output$config_summary <- renderText({
    groups <- rv$scenario_groups
    if (is.null(groups)) groups <- "core"

    pair_lines <- sapply(groups, function(key) {
      entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
      if (key == "core") {
        bl <- input$baseline_scenario; tgt <- input$target_scenario
      } else {
        bl <- input[[paste0("baseline_scenario_", key)]]
        tgt <- input[[paste0("target_scenario_", key)]]
      }
      bl_label <- if (!is.null(bl) && nzchar(bl)) scenario_label(bl) else "(none)"
      tgt_label <- if (!is.null(tgt) && nzchar(tgt)) scenario_label(tgt) else "(none)"
      paste0(entry$label, ": ", tgt_label, " vs ", bl_label)
    })

    paste(
      paste(pair_lines, collapse = "\n"),
      paste("\nGeography:", input$scenario_geography),
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
    # Validate at least the core scenarios are selected
    if (is.null(input$baseline_scenario) || input$baseline_scenario == "" ||
        is.null(input$target_scenario) || input$target_scenario == "") {
      showNotification("Please select both a baseline and a target scenario on the Configure tab.", type = "error")
      return()
    }
    # Validate additional sector-specific pairs (if present)
    for (key in setdiff(rv$scenario_groups, "core")) {
      bl_key <- input[[paste0("baseline_scenario_", key)]]
      tgt_key <- input[[paste0("target_scenario_", key)]]
      if (is.null(bl_key) || bl_key == "" || is.null(tgt_key) || tgt_key == "") {
        showNotification(
          paste0("Please select scenarios for the ", SECTOR_SCENARIO_DEFAULTS[[key]]$label, " pair, or remove those sectors from your portfolio."),
          type = "warning")
      }
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
            tags$li(tags$span(style = "color: #6B9F3B;",
                              icon("check-circle"),
                              " Your uploaded Internal PD/EL values will be ", tags$b("preserved")))
        ),
        tags$p(style = "margin-top: 10px;",
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
      # Collect all active scenario pairs for history
      hist_pairs <- lapply(rv$scenario_groups, function(key) {
        entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
        if (key == "core") {
          bl <- input$baseline_scenario; tgt <- input$target_scenario
        } else {
          bl <- input[[paste0("baseline_scenario_", key)]]
          tgt <- input[[paste0("target_scenario_", key)]]
        }
        list(group = key, label = entry$label, baseline = bl, target = tgt)
      })
      current_config <- list(
        baseline_scenario = input$baseline_scenario,
        target_scenario = input$target_scenario,
        scenario_pairs = hist_pairs,
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
        log_message(paste("  Portfolio columns:", ncol(rv$portfolio)))
        log_message(paste("  Assets rows:", nrow(rv$assets)))
        log_message(paste("  Assets companies:", length(unique(rv$assets$company_id))))
        log_message(paste("  Financial rows:", nrow(rv$financial)))
        log_message(paste("  Scenarios rows:", nrow(rv$scenarios)))
        log_message(paste("  Scenarios count:", length(unique(rv$scenarios$scenario))))
        log_message(paste("  Carbon price rows:", if (!is.null(rv$carbon)) nrow(rv$carbon) else "NULL"))
        if (isTRUE(as.logical(Sys.getenv("TRISK_DEBUG_LOG", "false")))) {
          log_message(paste("  Debug - Portfolio columns:", paste(names(rv$portfolio), collapse = ", ")))
          log_message(paste("  Debug - Scenarios available:", paste(unique(rv$scenarios$scenario), collapse = ", ")))
        }
        # ---- COLLECT SCENARIO PAIRS FROM SECTOR-AWARE UI ----
        groups <- rv$scenario_groups
        if (is.null(groups) || length(groups) == 0) groups <- "core"

        scenario_pairs <- list()
        for (key in groups) {
          entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
          # Core row uses canonical IDs; extra rows use suffixed IDs
          if (key == "core") {
            bl  <- input$baseline_scenario
            tgt <- input$target_scenario
          } else {
            bl  <- input[[paste0("baseline_scenario_", key)]]
            tgt <- input[[paste0("target_scenario_", key)]]
          }
          # Skip pairs where user hasn't selected anything
          if (!is.null(bl) && nzchar(bl) && !is.null(tgt) && nzchar(tgt)) {
            scenario_pairs[[key]] <- list(
              key      = key,
              label    = entry$label,
              baseline = bl,
              target   = tgt,
              sectors  = entry$sectors
            )
          }
        }
        req(length(scenario_pairs) > 0)
        n_scenario_pairs <- length(scenario_pairs)

        log_message("--- Parameters ---")
        log_message(paste("  Scenario pairs:", n_scenario_pairs))
        for (key in names(scenario_pairs)) {
          p <- scenario_pairs[[key]]
          log_message(paste0("    ", p$label, ": ", p$baseline, " -> ", p$target,
                            " [sectors: ", paste(p$sectors, collapse = ", "), "]"))
        }
        log_message(paste("  Geography:", input$scenario_geography))
        log_message(paste("  Shock year(s):", paste(input$shock_years, collapse = ", ")))
        log_message(paste("  Risk-free rate:", input$risk_free_rate))
        log_message(paste("  Discount rate:", input$discount_rate))
        log_message(paste("  Growth rate:", input$growth_rate))
        log_message(paste("  Market passthrough:", input$market_passthrough))
        log_message(paste("  Carbon price model:", input$carbon_price_model))

        incProgress(0.2, detail = "Preparing data...")

        # ---- ENRICH PORTFOLIO WITH TECHNOLOGY (local copy, never mutate rv$portfolio) ----
        # trisk.analysis::join_trisk_outputs_to_portfolio() joins results back
        # to the portfolio by (company_id, country_iso2, sector, technology, term).
        # User-uploaded portfolios are often company-level (no technology column).
        # Expand each company to one row per technology from the assets data.
        portfolio_for_run <- rv$portfolio
        if (!"technology" %in% names(portfolio_for_run)) {
          company_techs <- rv$assets %>%
            dplyr::distinct(.data$company_id, .data$sector, .data$technology)
          portfolio_for_run <- portfolio_for_run %>%
            dplyr::select(-any_of("sector")) %>%
            dplyr::inner_join(company_techs, by = "company_id")
          log_message(paste("  Portfolio enriched: added technology column from assets data.",
                           nrow(rv$portfolio), "->", nrow(portfolio_for_run), "rows"))
        }

        # ---- ENRICH PORTFOLIO COMPANY NAMES FROM ASSETS ----
        if ("company_name" %in% names(portfolio_for_run) && all(is.na(portfolio_for_run$company_name)) &&
            "company_name" %in% names(rv$assets)) {
          asset_names <- rv$assets %>%
            dplyr::distinct(.data$company_id, asset_company_name = .data$company_name)
          portfolio_for_run <- portfolio_for_run %>%
            dplyr::left_join(asset_names, by = "company_id") %>%
            dplyr::mutate(company_name = dplyr::coalesce(.data$asset_company_name, .data$company_name)) %>%
            dplyr::select(-"asset_company_name")
          log_message("  Enriched portfolio company_name from assets data")
        }

        # ---- PREPARE ASSETS FOR RUN ----
        assets_for_run <- rv$assets

        # ---- IDENTIFY PAIRS WITH MATCHING PORTFOLIO COMPANIES ----
        pairs_with_data <- list()
        for (key in names(scenario_pairs)) {
          pair <- scenario_pairs[[key]]
          portfolio_subset <- portfolio_for_run %>%
            dplyr::filter(.data$sector %in% pair$sectors)
          if (nrow(portfolio_subset) > 0) {
            pairs_with_data[[key]] <- pair
            pairs_with_data[[key]]$n_companies <- nrow(portfolio_subset)
            log_message(paste0("  ", pair$label, ": ", nrow(portfolio_subset),
                              " portfolio rows in sectors [",
                              paste(pair$sectors, collapse = ", "), "]"))
          } else {
            log_message(paste0("  Skipping ", pair$label,
                              ": no portfolio companies in sectors [",
                              paste(pair$sectors, collapse = ", "), "]"))
          }
        }

        if (length(pairs_with_data) == 0) {
          showNotification("No scenario pairs match any companies in your portfolio.", type = "error")
          return()
        }

        # ---- MULTI-PAIR × MULTI-HORIZON LOOP ----
        shock_years <- sort(as.integer(input$shock_years))
        n_years <- length(shock_years)
        total_runs <- length(pairs_with_data) * n_years
        run_count <- 0

        all_combined_results <- list()  # year -> combined df across all pairs

        for (yr_i in seq_along(shock_years)) {
          yr <- shock_years[yr_i]
          year_results <- list()

          for (key in names(pairs_with_data)) {
            pair <- pairs_with_data[[key]]
            run_count <- run_count + 1
            progress_val <- 0.2 + 0.6 * (run_count / total_runs)
            incProgress(progress_val - (0.2 + 0.6 * ((run_count - 1) / total_runs)),
                        detail = paste0(pair$label, ", year ", yr,
                                       " (", run_count, "/", total_runs, ")..."))

            tryCatch({
              # Per-pair geography fallback: use selected geography if available,
              # otherwise fall back to "Global" (e.g. IPR/MissionPossible don't have OecdAndEu)
              geo_result <- get_geography_for_pair(pair$baseline, input$scenario_geography, rv$scenarios)
              pair_geography <- geo_result$geography
              if (geo_result$fallback) {
                log_message(paste0("  Geography fallback for ", pair$label,
                                  ": ", input$scenario_geography, " -> ", pair_geography))
              }

              log_message(paste0("--- ", pair$label, ": ", pair$target,
                                " <- ", pair$baseline,
                                " | Geography: ", pair_geography,
                                " | Year: ", yr,
                                " | Sectors: ", paste(pair$sectors, collapse = ", "),
                                " (", run_count, "/", total_runs, ") ---"))

              # Filter portfolio and assets to this pair's sectors
              portfolio_subset <- portfolio_for_run %>%
                dplyr::filter(.data$sector %in% pair$sectors)
              assets_subset <- assets_for_run %>%
                dplyr::filter(.data$sector %in% pair$sectors)

              # Per-pair year range validation (using pair-specific geography)
              pair_scenarios <- rv$scenarios %>%
                dplyr::filter(
                  .data$scenario %in% c(pair$baseline, pair$target),
                  .data$scenario_geography %in% pair_geography
                )
              if (nrow(pair_scenarios) > 0) {
                scen_min <- min(pair_scenarios$scenario_year)
                scen_max <- max(pair_scenarios$scenario_year)
                assets_subset <- assets_subset %>%
                  dplyr::filter(.data$production_year >= scen_min,
                                .data$production_year <= scen_max)
              }

              raw_results <- run_trisk_on_portfolio(
                assets_data = assets_subset,
                scenarios_data = rv$scenarios,
                financial_data = rv$financial,
                carbon_data = rv$carbon,
                portfolio_data = portfolio_subset,
                baseline_scenario = pair$baseline,
                target_scenario = pair$target,
                scenario_geography = pair_geography,
                shock_year = yr,
                risk_free_rate = input$risk_free_rate,
                discount_rate = input$discount_rate,
                growth_rate = input$growth_rate,
                market_passthrough = input$market_passthrough
              )

              result_df <- extract_result_df(raw_results)
              result_df <- compute_el_columns(result_df)
              result_df$shock_year <- yr
              result_df$target_scenario <- pair$target
              result_df$baseline_scenario <- pair$baseline
              result_df$scenario_group <- key
              year_results[[key]] <- result_df
              log_message(paste0("  ", nrow(result_df), " rows from ", pair$label))
            }, error = function(e) {
              message(paste0("ANALYSIS ERROR: ", pair$label, " year=", yr,
                            " bl=", pair$baseline, " tgt=", pair$target,
                            " geo=", pair_geography, " err=", conditionMessage(e)))
              log_message(paste0("  WARNING: ", pair$label, " failed for year ", yr,
                                ": ", conditionMessage(e)))
              showNotification(
                paste0(pair$label, " (year ", yr, "): ", conditionMessage(e)),
                type = "warning", duration = 10
              )
            })
          }

          # Combine all sector results for this year
          all_combined_results[[as.character(yr)]] <- dplyr::bind_rows(year_results)
        }

        # ---- STORE COMBINED RESULTS ----
        primary_year <- as.character(shock_years[1])
        rv$results <- all_combined_results[[primary_year]]
        rv$results_by_year <- all_combined_results

        # For backward compatibility, store under primary target scenario
        primary_target <- if ("core" %in% names(pairs_with_data)) {
          pairs_with_data[["core"]]$target
        } else {
          pairs_with_data[[1]]$target
        }
        rv$results_by_scenario <- list()
        rv$results_by_scenario[[primary_target]] <- all_combined_results

        log_message("--- Combined Results ---")
        log_message(paste("  Scenario pairs executed:", length(pairs_with_data)))
        log_message(paste("  Primary shock year:", primary_year))
        log_message(paste("  Result rows (combined):", nrow(rv$results)))
        log_message(paste("  Result columns:", ncol(rv$results)))
        log_message(paste("  Total pairs:", length(pairs_with_data),
                         "| Total years:", n_years,
                         "| Total runs:", total_runs))

        if ("sector" %in% names(rv$results)) {
          log_message(paste("  Distinct sectors:", length(unique(rv$results$sector))))
        }
        if ("technology" %in% names(rv$results)) {
          log_message(paste("  Distinct technologies:", length(unique(rv$results$technology))))
        }
        if (isTRUE(as.logical(Sys.getenv("TRISK_DEBUG_LOG", "false")))) {
          log_message(paste("  Debug - Result columns:", paste(names(rv$results), collapse = ", ")))
          if ("crispy_perc_value_change" %in% names(rv$results)) {
            log_message(paste("  Debug - Avg PD shock:", smart_round(mean(rv$results$pd_shock, na.rm = TRUE) * 100), "%"))
          }
          if ("pd_shock" %in% names(rv$results)) {
            log_message(paste("  Debug - Max PD shock:", smart_round(max(rv$results$pd_shock, na.rm = TRUE) * 100), "%"))
          }
          if ("expected_loss_shock" %in% names(rv$results)) {
            log_message(paste("  Debug - Total expected loss (shock):", smart_round(sum(rv$results$expected_loss_shock, na.rm = TRUE))))
          }
          if ("sector" %in% names(rv$results)) {
            log_message(paste("  Debug - Sectors:", paste(unique(rv$results$sector), collapse = ", ")))
          }
          if ("technology" %in% names(rv$results)) {
            log_message(paste("  Debug - Technologies:", paste(unique(rv$results$technology), collapse = ", ")))
          }
        }

        # Reset computed integration results (need recalculation with new model outputs)
        # BUT preserve user-uploaded Internal PD/EL values across runs
        rv$pd_integration_result <- NULL
        rv$el_integration_result <- NULL
        # NOTE: rv$internal_pd and rv$internal_el are intentionally preserved

        rv$run_id <- generate_run_id()

        incProgress(0.9, detail = "Finalizing...")
        log_message(paste("Analysis complete! Run ID:", rv$run_id))
        audit_log("analysis_run", list(run_id = rv$run_id,
                                       n_scenario_pairs = length(pairs_with_data),
                                       n_years = n_years, result_rows = nrow(rv$results)))

        n_yr_label <- if (n_years > 1) paste0(" across ", n_years, " shock years") else ""
        showNotification(paste0("Analysis completed successfully!", n_yr_label), type = "message", duration = 5)

        # Switch to results tab
        updateTabsetPanel(session, "tabs", selected = "results")

      }, error = function(e) {
        message(paste("ERROR (analysis run):", conditionMessage(e)))
        log_message("ERROR: Analysis failed (see server log for details)")
        showNotification(
          "Analysis failed. Please check your data and parameters.",
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
    div(style = "margin-top: 10px;",
      tags$span(class = "label label-info", style = "font-size: 13px; padding: 4px 10px;",
                icon("fingerprint"), paste(" Run ID:", rv$run_id))
    )
  })

  # ---- Run ID display (Step 6) ----
  output$run_id_step6 <- renderUI({
    if (is.null(rv$run_id)) return(NULL)
    div(style = "margin-bottom: 10px;",
      tags$span(class = "label label-info", style = "font-size: 13px; padding: 4px 10px;",
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
      # Handle both new (scenario_pairs) and old (baseline_scenario/target_scenario) config formats
      if (!is.null(cfg$scenario_pairs) && length(cfg$scenario_pairs) > 0) {
        n_pairs <- length(cfg$scenario_pairs)
        first_pair <- cfg$scenario_pairs[[1]]
        baseline_lbl <- scenario_label(first_pair$baseline)
        target_lbl <- if (n_pairs <= 2) {
          paste(sapply(cfg$scenario_pairs, function(p) scenario_label(p$target)), collapse = ", ")
        } else {
          paste0(scenario_label(first_pair$target), " + ", n_pairs - 1, " more")
        }
      } else {
        baseline_lbl <- scenario_label(cfg$baseline_scenario)
        target_codes <- if (!is.null(cfg$target_scenarios)) cfg$target_scenarios
                        else strsplit(cfg$target_scenario, ",\\s*")[[1]]
        if (length(target_codes) <= 2) {
          target_lbl <- paste(sapply(target_codes, scenario_label), collapse = ", ")
        } else {
          target_lbl <- paste0(scenario_label(target_codes[1]), " + ", length(target_codes) - 1, " more")
        }
      }

      # Compute key metrics for compact display
      avg_pd <- if ("pd_shock" %in% names(run$results)) {
        paste0(round(mean(run$results$pd_shock, na.rm = TRUE) * 100, 4), "%")
      } else "N/A"
      total_el <- if (all(c("expected_loss_baseline", "expected_loss_shock") %in% names(run$results))) {
        format_number(sum(abs(run$results$expected_loss_shock) - abs(run$results$expected_loss_baseline), na.rm = TRUE))
      } else "N/A"

      is_selected <- !is.null(rv$compare_run_idx) && rv$compare_run_idx == i
      row_style <- if (is_selected) {
        "background: #FFFDE7; border-left: 3px solid #F53D3F; padding: 8px 12px; margin-bottom: 4px; border-radius: 4px;"
      } else {
        "background: #FBF5F2; padding: 8px 12px; margin-bottom: 4px; border-radius: 4px; border-left: 3px solid #DDD0D4;"
      }

      tags$div(style = row_style,
        fluidRow(
          column(1, tags$span(style = "font-weight: 700; color: #666; font-size: 16px;", paste0("#", i))),
          column(3,
            tags$div(style = "font-size: 12px; color: #666;", ts),
            tags$div(style = "font-size: 12px;",
              tags$b(target_lbl), " vs ", baseline_lbl)
          ),
          column(2, tags$div(style = "font-size: 12px;",
            tags$span(style = "color: #666;", "Geography: "), cfg$scenario_geography,
            tags$br(),
            tags$span(style = "color: #666;", "Shock: "), cfg$shock_year
          )),
          column(2, tags$div(style = "font-size: 12px;",
            tags$span(style = "color: #666;", "PD: "), tags$b(avg_pd),
            tags$br(),
            tags$span(style = "color: #666;", "EL: "), tags$b(total_el)
          )),
          column(2, tags$div(style = "font-size: 12px;",
            tags$span(style = "color: #666;", "Companies: "), run$n_companies,
            tags$br(),
            tags$span(style = "color: #666;", "Run: "), run$run_id
          )),
          column(2,
            actionButton(paste0("compare_run_", i), "Compare",
                        class = if (is_selected) "btn-warning btn-sm" else "btn-default btn-sm",
                        icon = icon(if (is_selected) "eye-slash" else "exchange-alt"),
                        style = "width: 100%; margin-bottom: 4px;"),
            actionButton(paste0("dup_run_", i), "Duplicate",
                        class = "btn-info btn-sm",
                        icon = icon("copy"),
                        style = "width: 100%;")
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
        tags$div(style = "margin-bottom: 8px; font-size: 12px; color: #666;",
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

  # Handle compare button clicks for each history entry (with destroy pattern to prevent accumulation)
  compare_observers <- reactiveVal(list())
  observe({
    # Destroy previous observers before creating new ones
    for (obs in compare_observers()) obs$destroy()
    new_obs <- lapply(seq_along(rv$run_history), function(i) {
      btn_id <- paste0("compare_run_", i)
      observeEvent(input[[btn_id]], {
        if (!is.null(rv$compare_run_idx) && rv$compare_run_idx == i) {
          rv$compare_run_idx <- NULL
        } else {
          rv$compare_run_idx <- i
        }
      }, ignoreInit = TRUE)
    })
    compare_observers(new_obs)
  })

  # Handle duplicate button clicks for each history entry (with destroy pattern)
  dup_observers <- reactiveVal(list())
  observe({
    for (obs in dup_observers()) obs$destroy()
    new_obs <- lapply(seq_along(rv$run_history), function(i) {
      btn_id <- paste0("dup_run_", i)
      observeEvent(input[[btn_id]], {
        if (i > length(rv$run_history)) return()
        cfg <- rv$run_history[[i]]$config

        if (!is.null(cfg$baseline_scenario)) {
          updateSelectizeInput(session, "baseline_scenario", selected = cfg$baseline_scenario)
        }
        if (!is.null(cfg$target_scenario)) {
          updateSelectizeInput(session, "target_scenario", selected = cfg$target_scenario)
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

        updateTabItems(session, "tabs", selected = "config")
        showNotification(paste0("Config loaded from run #", i, ". Adjust and re-run."),
                         type = "message", duration = 3)
      }, ignoreInit = TRUE)
    })
    dup_observers(new_obs)
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
    # Build label from all active scenario pairs
    pair_labels <- sapply(rv$scenario_groups, function(key) {
      if (key == "core") {
        bl <- input$baseline_scenario; tgt <- input$target_scenario
      } else {
        bl <- input[[paste0("baseline_scenario_", key)]]
        tgt <- input[[paste0("target_scenario_", key)]]
      }
      paste0(bl %||% "", " -> ", tgt %||% "")
    })
    curr_pairs_label <- paste(pair_labels, collapse = "; ")
    curr_cfg <- list(
      baseline_scenario = input$baseline_scenario %||% "",
      target_scenario = input$target_scenario %||% "",
      scenario_geography = input$scenario_geography,
      shock_year = paste(input$shock_years, collapse = ", "),
      discount_rate = input$discount_rate,
      growth_rate = input$growth_rate
    )

    # Compute deltas
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
      tags$span(style = paste0("color:", color, "; font-weight: 600;"), paste0(arrow, " ", val_str))
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
      style = "margin-top: 15px; padding: 15px; background: #F8F4F6; border: 1px solid #DDD0D4; border-radius: 6px;",
      h4(icon("code-compare"), " Comparison: Current Run vs Run #", idx,
         style = "margin-top: 0; margin-bottom: 12px; font-weight: 600;"),

      # Config changes
      if (length(config_diffs) > 0) {
        tags$div(style = "margin-bottom: 12px;",
          tags$span(style = "font-weight: 600; font-size: 13px;", icon("sliders"), " Parameter Changes:"),
          tags$ul(style = "font-size: 13px; margin-top: 4px;", config_diffs)
        )
      } else {
        tags$div(style = "margin-bottom: 12px; font-size: 13px; color: #666;",
                 icon("equals"), " Same parameters — re-run with identical configuration")
      },

      # Metrics comparison table
      tags$table(
        class = "table table-bordered",
        style = "font-size: 13px; margin-bottom: 0; background: white;",
        tags$thead(
          tags$tr(style = "background: #F0E6EA;",
            tags$th("Metric"),
            tags$th(style = "text-align: right;", paste0("Run #", idx, " (Previous)")),
            tags$th(style = "text-align: right;", "Current Run"),
            tags$th(style = "text-align: center;", "Delta")
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td("Average PD Shock"),
            tags$td(style = "text-align: right;", if (!is.na(prev_pd)) paste0(round(prev_pd, 4), "%") else "N/A"),
            tags$td(style = "text-align: right;", if (!is.na(curr_pd)) paste0(round(curr_pd, 4), "%") else "N/A"),
            tags$td(style = "text-align: center;", delta_span(curr_pd, prev_pd, "round4"))
          ),
          tags$tr(
            tags$td("Total EL Shock"),
            tags$td(style = "text-align: right;", if (!is.na(prev_el)) format_number(prev_el) else "N/A"),
            tags$td(style = "text-align: right;", if (!is.na(curr_el)) format_number(curr_el) else "N/A"),
            tags$td(style = "text-align: center;", delta_span(curr_el, prev_el, "number", invert = TRUE))
          ),
          tags$tr(
            tags$td("Companies"),
            tags$td(style = "text-align: right;", prev$n_companies),
            tags$td(style = "text-align: right;", nrow(curr)),
            tags$td(style = "text-align: center;",
                    if (nrow(curr) == prev$n_companies) tags$span(style = "color: #666;", "=")
                    else tags$span(style = "color: #5A8EAE;", paste0(nrow(curr) - prev$n_companies)))
          )
        )
      )
    )
  })


}
