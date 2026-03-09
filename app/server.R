# server.R
# Server logic for TRISK Shiny application
# Uses trisk.analysis plotting pipeline and correct run_trisk_model parameter names

server <- function(input, output, session) {

  # ============================================
  # Reactive values to store data
  # ============================================
  rv <- reactiveValues(
    portfolio = NULL,
    assets = NULL,
    financial = NULL,
    scenarios = scenarios_data_preloaded,
    carbon = carbon_data_internal,
    results = NULL,
    run_id = NULL,
    analysis_log = character(),
    # Integration: internal PD/EL values entered by user
    internal_pd = NULL,
    internal_el = NULL,
    pd_integration_result = NULL,
    el_integration_result = NULL,
    # Multi-horizon results: named list keyed by shock_year (e.g. list("2030" = df, "2035" = df))
    results_by_year = NULL,
    # Multi-scenario results: named list keyed by scenario code
    # Each entry is itself a named list of year results (like results_by_year)
    results_by_scenario = NULL,
    # Run history: list of up to 5 previous runs
    # Each entry: list(results, config, run_id, timestamp, n_companies)
    run_history = list(),
    # Currently selected comparison run (index into run_history)
    compare_run_idx = NULL
  )

  # ============================================
  # Logging helper
  # ============================================
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    rv$analysis_log <- c(rv$analysis_log, paste(timestamp, msg))
  }

  # ============================================
  # Load mock/demo data from trisk packages
  # ============================================

  observeEvent(input$load_mock_data, {
    tryCatch({
      log_message("Loading mock data from trisk packages...")

      rv$assets <- load_mock_data("assets")
      if (!is.null(rv$assets)) log_message(paste("  Assets:", nrow(rv$assets), "rows"))

      rv$financial <- load_mock_data("financial")
      if (!is.null(rv$financial)) log_message(paste("  Financial:", nrow(rv$financial), "rows"))

      # Only load test scenarios if no scenarios are already loaded (e.g. from GCS download)
      if (is.null(rv$scenarios)) {
        rv$scenarios <- load_mock_data("scenarios")
        if (!is.null(rv$scenarios)) log_message(paste("  Scenarios: loaded testdata,", nrow(rv$scenarios), "rows"))
      } else {
        log_message(paste("  Scenarios: keeping existing data (", length(unique(rv$scenarios$scenario)), "scenarios)"))
      }

      # Same for carbon - keep pre-loaded if available
      if (is.null(rv$carbon)) {
        rv$carbon <- load_mock_data("carbon")
        if (!is.null(rv$carbon)) log_message(paste("  Carbon: loaded testdata,", nrow(rv$carbon), "rows"))
      } else {
        log_message(paste("  Carbon: keeping existing data (", nrow(rv$carbon), "rows)"))
      }

      rv$portfolio <- load_mock_data("portfolio")
      if (!is.null(rv$portfolio)) log_message(paste("  Portfolio:", nrow(rv$portfolio), "rows"))

      loaded <- sum(!sapply(list(rv$assets, rv$financial, rv$scenarios, rv$carbon, rv$portfolio), is.null))

      if (loaded == 5) {
        showNotification("All demo data loaded successfully! Go to Configure.", type = "message", duration = 5)
        log_message("All mock data loaded successfully.")
      } else {
        showNotification(paste("Loaded", loaded, "of 5 datasets. Some may be missing."), type = "warning", duration = 5)
        log_message(paste("Warning: only", loaded, "of 5 datasets loaded."))
      }
    }, error = function(e) {
      showNotification(paste("Error loading mock data:", e$message), type = "error")
      log_message(paste("ERROR loading mock data:", e$message))
    })
  })

  output$mock_data_status <- renderUI({
    if (is.null(rv$portfolio) && is.null(rv$assets)) return(NULL)

    items <- list(
      portfolio = rv$portfolio,
      assets = rv$assets,
      financial = rv$financial,
      scenarios = rv$scenarios,
      carbon = rv$carbon
    )

    tagList(
      hr(),
      lapply(names(items), function(nm) {
        if (!is.null(items[[nm]])) {
          tags$p(class = "status-ok", icon("check-circle"),
                 paste0(" ", tools::toTitleCase(nm), ": ", nrow(items[[nm]]), " rows"))
        } else {
          tags$p(class = "status-missing", icon("times-circle"),
                 paste0(" ", tools::toTitleCase(nm), ": not loaded"))
        }
      })
    )
  })

  # ============================================
  # File upload handlers
  # ============================================

  observeEvent(input$portfolio_file, {
    req(input$portfolio_file)
    tryCatch({
      rv$portfolio <- tibble::as_tibble(data.table::fread(input$portfolio_file$datapath))
      showNotification(paste("Portfolio loaded:", nrow(rv$portfolio), "rows"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading portfolio:", e$message), type = "error")
    })
  })

  observeEvent(input$assets_file, {
    req(input$assets_file)
    tryCatch({
      rv$assets <- tibble::as_tibble(data.table::fread(input$assets_file$datapath))
      showNotification(paste("Assets loaded:", nrow(rv$assets), "rows"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading assets:", e$message), type = "error")
    })
  })

  observeEvent(input$financial_file, {
    req(input$financial_file)
    tryCatch({
      rv$financial <- tibble::as_tibble(data.table::fread(input$financial_file$datapath))
      showNotification(paste("Financial data loaded:", nrow(rv$financial), "rows"), type = "message")
    }, error = function(e) {
      showNotification(paste("Error loading financial data:", e$message), type = "error")
    })
  })

  observeEvent(input$scenarios_file, {
    req(input$scenarios_file)
    tryCatch({
      rv$scenarios <- tibble::as_tibble(data.table::fread(input$scenarios_file$datapath))
      showNotification(
        paste("Scenarios loaded:", length(unique(rv$scenarios$scenario)), "scenarios"),
        type = "message"
      )
    }, error = function(e) {
      showNotification(paste("Error loading scenarios:", e$message), type = "error")
    })
  })

  # ============================================
  # Data preview outputs
  # ============================================

  output$portfolio_preview <- renderDT({
    req(rv$portfolio)
    df <- head(rv$portfolio, 10)
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) df[num_cols] <- lapply(df[num_cols], smart_round)
    datatable(df, options = list(scrollX = TRUE, dom = "t", pageLength = 5,
                    columnDefs = list(list(width = '80px', targets = '_all'))), rownames = FALSE)
  })

  output$assets_preview <- renderDT({
    req(rv$assets)
    df <- head(rv$assets, 10)
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) df[num_cols] <- lapply(df[num_cols], smart_round)
    datatable(df, options = list(scrollX = TRUE, dom = "t", pageLength = 5,
                    columnDefs = list(list(width = '80px', targets = '_all'))), rownames = FALSE)
  })

  output$financial_preview <- renderDT({
    req(rv$financial)
    df <- head(rv$financial, 10)
    num_cols <- names(df)[sapply(df, is.numeric)]
    if (length(num_cols) > 0) df[num_cols] <- lapply(df[num_cols], smart_round)
    datatable(df, options = list(scrollX = TRUE, dom = "t", pageLength = 5,
                    columnDefs = list(list(width = '80px', targets = '_all'))), rownames = FALSE)
  })

  # ============================================
  # Status indicators
  # ============================================

  output$portfolio_status <- renderUI({
    if (is.null(rv$portfolio)) {
      tags$p(class = "status-missing", icon("times-circle"), " No data loaded")
    } else {
      validation <- validate_portfolio(rv$portfolio)
      if (!is.null(validation)) {
        tags$p(class = "status-missing", icon("exclamation-triangle"), validation)
      } else {
        tags$p(class = "status-ok", icon("check-circle"),
               paste(" Loaded:", nrow(rv$portfolio), "rows,", ncol(rv$portfolio), "columns"))
      }
    }
  })

  output$assets_status <- renderUI({
    if (is.null(rv$assets)) {
      tags$p(class = "status-missing", icon("times-circle"), " No data loaded")
    } else {
      validation <- validate_assets(rv$assets)
      if (!is.null(validation)) {
        tags$p(class = "status-missing", icon("exclamation-triangle"), validation)
      } else {
        tags$p(class = "status-ok", icon("check-circle"),
               paste(" Loaded:", nrow(rv$assets), "rows,",
                    length(unique(rv$assets$company_id)), "companies"))
      }
    }
  })

  output$financial_status <- renderUI({
    if (is.null(rv$financial)) {
      tags$p(class = "status-missing", icon("times-circle"), " No data loaded")
    } else {
      validation <- validate_financial(rv$financial)
      if (!is.null(validation)) {
        tags$p(class = "status-missing", icon("exclamation-triangle"), validation)
      } else {
        tags$p(class = "status-ok", icon("check-circle"),
               paste(" Loaded:", nrow(rv$financial), "companies"))
      }
    }
  })

  output$upload_summary <- renderUI({
    portfolio_ok <- !is.null(rv$portfolio) && is.null(validate_portfolio(rv$portfolio))
    assets_ok <- !is.null(rv$assets) && is.null(validate_assets(rv$assets))
    financial_ok <- !is.null(rv$financial) && is.null(validate_financial(rv$financial))
    scenarios_ok <- !is.null(rv$scenarios)

    all_ok <- portfolio_ok && assets_ok && financial_ok && scenarios_ok

    tagList(
      tags$p(
        if (portfolio_ok) icon("check", class = "status-ok") else icon("times", class = "status-missing"),
        " Portfolio Data"
      ),
      tags$p(
        if (assets_ok) icon("check", class = "status-ok") else icon("times", class = "status-missing"),
        " Assets Data"
      ),
      tags$p(
        if (financial_ok) icon("check", class = "status-ok") else icon("times", class = "status-missing"),
        " Financial Data"
      ),
      tags$p(
        if (scenarios_ok) icon("check", class = "status-ok") else icon("times", class = "status-missing"),
        " Scenarios Data"
      ),
      hr(),
      if (all_ok) {
        tags$p(class = "status-ok", strong(icon("thumbs-up"), " Ready to proceed to configuration!"))
      } else {
        tags$p(class = "status-missing", strong(icon("exclamation-triangle"), " Please upload all required data"))
      }
    )
  })

  # ============================================
  # Data quality audit outputs
  # ============================================

  # Helper to render an audit panel
  render_audit_panel <- function(df, type) {
    if (is.null(df)) return(NULL)
    audit <- audit_dataset(df, type)
    if (is.null(audit)) return(NULL)

    # Schema badge
    schema_badge <- if (audit$schema_valid) {
      tags$span(class = "label label-success", icon("check"), " Schema Valid")
    } else {
      tags$span(class = "label label-danger", icon("times"), " Schema Invalid")
    }

    # Stats table rows
    stats_rows <- tagList(
      tags$tr(tags$td("Rows"), tags$td(tags$b(format(audit$n_rows, big.mark = ",")))),
      tags$tr(tags$td("Columns"), tags$td(tags$b(audit$n_cols)))
    )
    if (!is.na(audit$n_distinct_id)) {
      stats_rows <- tagList(stats_rows,
        tags$tr(tags$td("Distinct company_id"), tags$td(tags$b(format(audit$n_distinct_id, big.mark = ",")))),
        tags$tr(tags$td("Duplicate company_id"), tags$td(tags$b(
          if (audit$n_duplicate_id > 0) tags$span(style = "color: #C44245;", audit$n_duplicate_id) else "0"
        )))
      )
    }

    # Null rates for columns with issues
    null_info <- NULL
    if (length(audit$high_null_cols) > 0) {
      null_items <- lapply(audit$high_null_cols, function(col) {
        tags$li(paste0(col, ": ", audit$null_rates[col], "% missing"))
      })
      null_info <- tagList(
        tags$small(tags$b("Columns with missing values:"), style = "color: #C44245;"),
        tags$ul(style = "font-size: 12px; margin: 4px 0;", null_items)
      )
    }

    div(style = "background: #f9f9f9; border: 1px solid #eee; border-radius: 4px; padding: 10px; margin: 8px 0;",
      div(style = "margin-bottom: 6px;", schema_badge),
      tags$table(class = "table table-condensed", style = "font-size: 12px; margin-bottom: 4px;",
        stats_rows
      ),
      null_info
    )
  }

  output$portfolio_audit <- renderUI({
    render_audit_panel(rv$portfolio, "portfolio")
  })

  output$assets_audit <- renderUI({
    render_audit_panel(rv$assets, "assets")
  })

  output$financial_audit <- renderUI({
    render_audit_panel(rv$financial, "financial")
  })

  # ============================================
  # Template CSV downloads
  # ============================================

  output$download_portfolio_template <- downloadHandler(
    filename = function() "template_portfolio.csv",
    content = function(file) write.csv(dataset_template("portfolio"), file, row.names = FALSE)
  )

  output$download_assets_template <- downloadHandler(
    filename = function() "template_assets.csv",
    content = function(file) write.csv(dataset_template("assets"), file, row.names = FALSE)
  )

  output$download_financial_template <- downloadHandler(
    filename = function() "template_financial.csv",
    content = function(file) write.csv(dataset_template("financial"), file, row.names = FALSE)
  )

  # ============================================
  # Error report (combined issues from all datasets)
  # ============================================

  # Reactive flag for conditionalPanel
  output$has_data_issues <- reactive({
    issues_count <- 0
    for (type in c("portfolio", "assets", "financial")) {
      df <- switch(type,
        "portfolio" = rv$portfolio,
        "assets"    = rv$assets,
        "financial" = rv$financial
      )
      if (!is.null(df)) {
        audit <- audit_dataset(df, type)
        if (!is.null(audit)) issues_count <- issues_count + nrow(audit$issues)
      }
    }
    issues_count > 0
  })
  outputOptions(output, "has_data_issues", suspendWhenHidden = FALSE)

  output$download_error_report <- downloadHandler(
    filename = function() paste0("data_quality_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"),
    content = function(file) {
      all_issues <- data.frame(dataset = character(), row = integer(),
                               column = character(), issue_type = character(),
                               detail = character(), stringsAsFactors = FALSE)
      for (type in c("portfolio", "assets", "financial")) {
        df <- switch(type,
          "portfolio" = rv$portfolio,
          "assets"    = rv$assets,
          "financial" = rv$financial
        )
        if (!is.null(df)) {
          audit <- audit_dataset(df, type)
          if (!is.null(audit) && nrow(audit$issues) > 0) {
            issues_with_ds <- audit$issues
            issues_with_ds$dataset <- type
            all_issues <- rbind(all_issues, issues_with_ds[, c("dataset", "row", "column", "issue_type", "detail")])
          }
        }
      }
      write.csv(all_issues, file, row.names = FALSE)
    }
  )

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
        div(class = "alert alert-danger", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
            icon("exclamation-triangle"), " No baseline scenario selected. Analysis requires at least one baseline.")
      ))
    }

    # No target selected
    if (is.null(input$target_scenarios) || length(input$target_scenarios) == 0) {
      warnings <- c(warnings, list(
        div(class = "alert alert-danger", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
            icon("exclamation-triangle"), " No target scenario selected. Select at least one target.")
      ))
    }

    # Baseline == one of the targets
    if (!is.null(input$baseline_scenario) && length(input$baseline_scenario) > 0 &&
        !is.null(input$target_scenarios) && any(input$baseline_scenario %in% input$target_scenarios)) {
      overlap <- intersect(input$baseline_scenario, input$target_scenarios)
      warnings <- c(warnings, list(
        div(class = "alert alert-warning", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
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
          tags$li(style = "font-size: 12px;",
                  tags$code(scenario_label(tgt)), " \u2192 baseline: ", tags$code(scenario_label(bmap[tgt])))
        }, USE.NAMES = FALSE)
        warnings <- c(warnings, list(
          div(class = "alert alert-info", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
              icon("info-circle"), " Multiple baselines active. Mapping:",
              tags$ul(style = "margin: 4px 0 0 0; padding-left: 20px;", mapping_lines))
        ))
      }
    }

    # Performance warning: >5 targets
    if (!is.null(input$target_scenarios) && length(input$target_scenarios) > 5) {
      warnings <- c(warnings, list(
        div(class = "alert alert-info", style = "padding: 8px 12px; margin-bottom: 6px; font-size: 13px;",
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

        # ---- ENRICH PORTFOLIO WITH TECHNOLOGY ----
        # trisk.analysis::join_trisk_outputs_to_portfolio() joins results back
        # to the portfolio by (company_id, country_iso2, sector, technology, term).
        # User-uploaded portfolios are often company-level (no technology column).
        # Expand each company to one row per technology from the assets data.
        if (!"technology" %in% names(rv$portfolio)) {
          company_techs <- rv$assets %>%
            dplyr::distinct(.data$company_id, .data$sector, .data$technology)
          portfolio_enriched <- rv$portfolio %>%
            dplyr::select(-any_of("sector")) %>%        # drop portfolio-level sector to avoid conflict
            dplyr::inner_join(company_techs, by = "company_id")
          log_message(paste("  Portfolio enriched: added technology column from assets data.",
                           nrow(rv$portfolio), "->", nrow(portfolio_enriched), "rows"))
          rv$portfolio <- portfolio_enriched
        }

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

        # Switch to results tab
        updateTabsetPanel(session, "tabs", selected = "results")

      }, error = function(e) {
        log_message(paste("ERROR:", e$message))
        showNotification(
          paste("Analysis failed:", e$message),
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
            tags$span(style = "color: #666;", "NPV: "), tags$b(avg_npv),
            tags$br(),
            tags$span(style = "color: #666;", "PD: "), tags$b(avg_pd)
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
            tags$td("Average NPV Change"),
            tags$td(style = "text-align: right;", if (!is.na(prev_npv)) paste0(round(prev_npv, 2), "%") else "N/A"),
            tags$td(style = "text-align: right;", if (!is.na(curr_npv)) paste0(round(curr_npv, 2), "%") else "N/A"),
            tags$td(style = "text-align: center;", delta_span(curr_npv, prev_npv, "round2", invert = TRUE))
          ),
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

  # ============================================
  # Results outputs - Value boxes (6 boxes)
  # ============================================

  safe_col <- function(df, ...) {
    candidates <- c(...)
    for (col in candidates) {
      if (col %in% names(df)) return(df[[col]])
    }
    return(NULL)
  }

  output$vb_companies <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "Companies Analyzed", icon = icon("building"), color = "blue"))
    }
    n <- {
      col <- safe_col(rv$results, "company_id", "company_name")
      if (!is.null(col)) length(unique(col)) else nrow(rv$results)
    }
    valueBox(as.character(n), "Companies Analyzed", icon = icon("building"), color = "blue")
  })

  output$vb_total_exposure <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "Total Exposure (USD)", icon = icon("dollar-sign"), color = "green"))
    }
    val <- {
      col <- safe_col(rv$results, "exposure_value_usd", "exposure")
      if (!is.null(col)) sum(col, na.rm = TRUE) else 0
    }
    valueBox(format_number(val), "Total Exposure (USD)", icon = icon("dollar-sign"), color = "green")
  })

  output$vb_avg_npv_change <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "Average NPV Change (%)", icon = icon("chart-line"), color = "blue"))
    }
    val <- {
      col <- safe_col(rv$results, "crispy_perc_value_change", "net_present_value_change", "npv_change")
      if (!is.null(col)) mean(col, na.rm = TRUE) * 100 else 0
    }
    valueBox(tags$span(paste0(display_round(val), "%"),
                       title = paste0("Full precision: ", smart_round(val), "%"),
                       style = "cursor: help;"),
             "Average NPV Change (%)",
             icon = if (val < 0) icon("arrow-down") else icon("arrow-up"),
             color = if (val < 0) "red" else "blue")
  })

  output$vb_max_pd_shock <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "Maximum PD Shock (%)", icon = icon("chart-bar"), color = "blue"))
    }
    val <- {
      col <- safe_col(rv$results, "pd_shock", "crispy_pd_shock")
      if (!is.null(col)) max(col, na.rm = TRUE) * 100 else 0
    }
    valueBox(tags$span(paste0(display_round(val), "%"),
                       title = paste0("Full precision: ", smart_round(val), "%"),
                       style = "cursor: help;"),
             "Maximum PD Shock (%)",
             icon = if (val > 5) icon("exclamation-triangle") else icon("arrow-up"),
             color = if (val > 5) "red" else "blue")
  })

  # Portfolio-level PD Change value box
  output$vb_pd_change <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "PD Change, Exposure-Weighted (percentage points)", icon = icon("balance-scale"), color = "blue"))
    }
    val <- 0
    df <- rv$results
    if (all(c("pd_baseline", "pd_shock") %in% names(df))) {
      exp_col <- if ("exposure_value_usd" %in% names(df)) df$exposure_value_usd else rep(1, nrow(df))
      total_exp <- sum(exp_col, na.rm = TRUE)
      if (total_exp > 0) {
        val <- sum((df$pd_shock - df$pd_baseline) * exp_col, na.rm = TRUE) / total_exp * 100
      }
    }
    valueBox(tags$span(paste0(display_round(val), " pp"),
                       title = paste0("Full precision: ", smart_round(val), " pp"),
                       style = "cursor: help;"),
             "PD Change, Exposure-Weighted (percentage points)",
             icon = if (val > 0) icon("arrow-up") else icon("arrow-down"),
             color = if (val > 0.01) "red" else if (val < -0.01) "green" else "blue")
  })

  # Portfolio-level EL Change value box
  output$vb_el_change <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "EL Change (USD)", icon = icon("exclamation-circle"), color = "blue"))
    }
    val <- 0
    df <- rv$results
    if (all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))) {
      val <- sum(df$expected_loss_shock - df$expected_loss_baseline, na.rm = TRUE)
    }
    valueBox(tags$span(format_number(val),
                       title = paste0("Full precision: ", round(val, 4)),
                       style = "cursor: help;"),
             "EL Change (USD)",
             icon = if (val < 0) icon("arrow-down") else icon("arrow-up"),
             color = if (val > 0) "green" else if (val < 0) "red" else "blue")
  })

  # ============================================
  # Results - Summary table
  # ============================================

  output$results_summary_table <- renderDT({
    req(rv$results)
    df <- rv$results

    tryCatch({
      if ("sector" %in% names(df) && "technology" %in% names(df)) {
        summary_df <- df %>%
          group_by(sector, technology) %>%
          summarise(
            n_rows = n(),
            .groups = "drop"
          )

        npv_col <- intersect(names(df), c("crispy_perc_value_change", "net_present_value_change"))
        if (length(npv_col) > 0) {
          avg_npv <- df %>%
            group_by(sector, technology) %>%
            summarise(avg_npv_change = smart_round(mean(.data[[npv_col[1]]], na.rm = TRUE) * 100), .groups = "drop")
          summary_df <- left_join(summary_df, avg_npv, by = c("sector", "technology"))
        }

        pd_shock_col <- intersect(names(df), c("pd_shock", "crispy_pd_shock"))
        if (length(pd_shock_col) > 0) {
          avg_pd <- df %>%
            group_by(sector, technology) %>%
            summarise(avg_pd_shock = smart_round(mean(.data[[pd_shock_col[1]]], na.rm = TRUE) * 100), .groups = "drop")
          summary_df <- left_join(summary_df, avg_pd, by = c("sector", "technology"))
        }

        num_cols <- names(summary_df)[sapply(summary_df, is.numeric)]
        if (length(num_cols) > 0) summary_df[num_cols] <- lapply(summary_df[num_cols], smart_round)
        datatable(summary_df, options = list(pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                        columnDefs = list(list(width = '80px', targets = '_all'))), rownames = FALSE)
      } else {
        datatable(head(df, 50), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
      }
    }, error = function(e) {
      datatable(head(df, 50), options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
    })
  })

  # ============================================
  # Exposures NPV table
  # ============================================

  output$npv_table <- renderDT({
    req(rv$results)
    npv_cols <- c("company_id", "company_name", "sector", "technology", "country_iso2",
                  "exposure_value_usd", "term",
                  "crispy_perc_value_change", "net_present_value_baseline", "net_present_value_shock")
    npv_cols <- intersect(npv_cols, names(rv$results))
    if (length(npv_cols) == 0) npv_cols <- names(rv$results)
    npv_df <- rv$results[, npv_cols, drop = FALSE]
    num_cols <- npv_cols[sapply(npv_df, is.numeric)]
    if (length(num_cols) > 0) npv_df[num_cols] <- lapply(npv_df[num_cols], smart_round)
    datatable(npv_df,
              options = list(pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                             columnDefs = list(list(width = '80px', targets = '_all'))),
              rownames = FALSE)
  })

  # ============================================
  # Exposures PD table (with PD change + EL change columns + color)
  # ============================================

  output$pd_table <- renderDT({
    req(rv$results)
    df <- rv$results

    # Select base columns
    base_cols <- c("company_id", "company_name", "sector", "technology", "country_iso2",
                   "exposure_value_usd", "term", "loss_given_default",
                   "pd_baseline", "pd_shock")
    base_cols <- intersect(base_cols, names(df))
    if (length(base_cols) == 0) base_cols <- names(df)

    display_df <- df[, base_cols, drop = FALSE]

    # Round pd_baseline and pd_shock for display
    if ("pd_baseline" %in% names(display_df)) display_df$pd_baseline <- smart_round(display_df$pd_baseline)
    if ("pd_shock" %in% names(display_df)) display_df$pd_shock <- smart_round(display_df$pd_shock)

    # Compute PD Change columns
    has_pd <- all(c("pd_baseline", "pd_shock") %in% names(display_df))
    if (has_pd) {
      display_df$PD_Change <- smart_round(display_df$pd_shock - display_df$pd_baseline)
      display_df$PD_Change_Pct <- smart_round(
        ifelse(display_df$pd_baseline != 0,
               (display_df$pd_shock - display_df$pd_baseline) / display_df$pd_baseline * 100,
               NA_real_)
      )
    }

    # Compute EL columns from the full results (which have expected_loss_* computed)
    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    if (has_el) {
      display_df$EL_Baseline <- smart_round(df$expected_loss_baseline)
      display_df$EL_Shock <- smart_round(df$expected_loss_shock)
      display_df$EL_Change <- smart_round(df$expected_loss_shock - df$expected_loss_baseline)
      display_df$EL_Change_Pct <- smart_round(
        ifelse(df$expected_loss_baseline != 0,
               (df$expected_loss_shock - df$expected_loss_baseline) /
                 df$expected_loss_baseline * 100,
               NA_real_)
      )
    }

    # Build the DT with color styling
    dt <- datatable(
      display_df,
      options = list(pageLength = 20, scrollX = TRUE, autoWidth = FALSE,
                     columnDefs = list(list(width = '80px', targets = '_all'))),
      rownames = FALSE
    )

    # Color gradient: red (positive change = worse) -> blue (neutral) -> green (negative change = better)
    if (has_pd && "PD_Change" %in% names(display_df)) {
      brks_pd <- c(-Inf, -0.05, -0.02, -0.01, -0.005, -0.001, 0.001, 0.005, 0.01, 0.02, 0.05, Inf)
      clrs_pd <- c("#2D6A2E", "#3D8B3E", "#5DAA5E", "#7DC07E", "#A0D8A0",
                   "#7AADCA",
                   "#E0A0A0", "#D48080", "#C45555", "#A83535", "#8B2020")
      dt <- dt %>%
        formatStyle("PD_Change",
                    backgroundColor = styleInterval(brks_pd[-c(1, length(brks_pd))], clrs_pd),
                    color = "white", fontWeight = "bold")
    }

    if (has_pd && "PD_Change_Pct" %in% names(display_df)) {
      brks_pct <- c(-Inf, -50, -20, -10, -5, -1, 1, 5, 10, 20, 50, Inf)
      clrs_pct <- c("#2D6A2E", "#3D8B3E", "#5DAA5E", "#7DC07E", "#A0D8A0",
                    "#7AADCA",
                    "#E0A0A0", "#D48080", "#C45555", "#A83535", "#8B2020")
      dt <- dt %>%
        formatStyle("PD_Change_Pct",
                    backgroundColor = styleInterval(brks_pct[-c(1, length(brks_pct))], clrs_pct),
                    color = "white", fontWeight = "bold")
    }

    if (has_el && "EL_Change" %in% names(display_df)) {
      # For EL: negative values = more loss = worse (red), positive = better (green)
      # Note: EL values are negative (loss), so EL_Change being more negative = worse
      el_range <- range(display_df$EL_Change, na.rm = TRUE)
      el_max <- max(abs(el_range))
      if (el_max > 0) {
        brks_el <- c(-Inf, -el_max*0.5, -el_max*0.2, -el_max*0.1, -el_max*0.01,
                     el_max*0.01, el_max*0.1, el_max*0.2, el_max*0.5, Inf)
        clrs_el <- c("#8B2020", "#A83535", "#C45555", "#D48080",
                     "#7AADCA",
                     "#7DC07E", "#5DAA5E", "#3D8B3E", "#2D6A2E")
        dt <- dt %>%
          formatStyle("EL_Change",
                      backgroundColor = styleInterval(brks_el[-c(1, length(brks_el))], clrs_el),
                      color = "white", fontWeight = "bold")
      }
    }

    if (has_el && "EL_Change_Pct" %in% names(display_df)) {
      brks_el_pct <- c(-Inf, -50, -20, -10, -5, -1, 1, 5, 10, 20, 50, Inf)
      clrs_el_pct <- c("#8B2020", "#A83535", "#C45555", "#D48080", "#E0A0A0",
                       "#7AADCA",
                       "#A0D8A0", "#7DC07E", "#5DAA5E", "#3D8B3E", "#2D6A2E")
      dt <- dt %>%
        formatStyle("EL_Change_Pct",
                    backgroundColor = styleInterval(brks_el_pct[-c(1, length(brks_el_pct))], clrs_el_pct),
                    color = "white", fontWeight = "bold")
    }

    dt
  })

  # ============================================
  # Portfolio-level PD & EL aggregates (Exposures PD tab)
  # ============================================

  output$pd_portfolio_summary <- renderUI({
    req(rv$results)
    df <- rv$results

    has_pd <- all(c("pd_baseline", "pd_shock") %in% names(df))
    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_exp <- "exposure_value_usd" %in% names(df)

    if (!has_pd) return(tags$p("PD columns not available in results."))

    # Exposure-weighted average PD change
    if (has_exp) {
      total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)
      pd_change <- df$pd_shock - df$pd_baseline

      if (total_exposure > 0) {
        weighted_pd_change <- sum(pd_change * df$exposure_value_usd, na.rm = TRUE) / total_exposure
        weighted_pd_baseline <- sum(df$pd_baseline * df$exposure_value_usd, na.rm = TRUE) / total_exposure
        weighted_pd_change_pct <- if (weighted_pd_baseline != 0) {
          weighted_pd_change / weighted_pd_baseline * 100
        } else NA_real_
      } else {
        weighted_pd_change <- mean(pd_change, na.rm = TRUE)
        weighted_pd_change_pct <- NA_real_
        total_exposure <- 0
      }
    } else {
      weighted_pd_change <- mean(df$pd_shock - df$pd_baseline, na.rm = TRUE)
      weighted_pd_change_pct <- NA_real_
      total_exposure <- 0
    }

    pd_class <- if (weighted_pd_change > 0.001) "negative" else if (weighted_pd_change < -0.001) "positive" else "neutral"

    # EL aggregates
    el_tags <- if (has_el) {
      total_el_baseline <- sum(df$expected_loss_baseline, na.rm = TRUE)
      total_el_shock <- sum(df$expected_loss_shock, na.rm = TRUE)
      total_el_change <- total_el_shock - total_el_baseline
      total_el_change_pct <- if (total_el_baseline != 0) {
        total_el_change / total_el_baseline * 100
      } else NA_real_

      # EL values are negative (losses), so more negative shock = worse = "negative" class
      el_class <- if (total_el_change < 0) "negative" else if (total_el_change > 0) "positive" else "neutral"

      tagList(
        column(4,
          div(class = "portfolio-aggregate",
            h4("Total EL (Baseline)"),
            tags$span(class = "agg-value neutral", format_number(total_el_baseline))
          )
        ),
        column(4,
          div(class = "portfolio-aggregate",
            h4("Total EL (Shock)"),
            tags$span(class = paste("agg-value", el_class), format_number(total_el_shock))
          )
        ),
        column(4,
          div(class = "portfolio-aggregate",
            h4("Total EL Change"),
            tags$span(class = paste("agg-value", el_class),
                     paste0(format_number(total_el_change),
                            if (!is.na(total_el_change_pct)) paste0(" (", smart_round(total_el_change_pct), "%)") else ""))
          )
        )
      )
    } else NULL

    tagList(
      fluidRow(
        column(4,
          div(class = "portfolio-aggregate",
            h4("Exposure-Weighted PD Change (abs)"),
            tags$span(class = paste("agg-value", pd_class),
                     paste0(smart_round(weighted_pd_change * 100), " pp"))
          )
        ),
        column(4,
          div(class = "portfolio-aggregate",
            h4("Exposure-Weighted PD Change (%)"),
            tags$span(class = paste("agg-value", pd_class),
                     if (!is.na(weighted_pd_change_pct)) paste0(smart_round(weighted_pd_change_pct), "%") else "N/A")
          )
        ),
        column(4,
          div(class = "portfolio-aggregate",
            h4("Total Exposure"),
            tags$span(class = "agg-value neutral", format_number(total_exposure))
          )
        )
      ),
      if (!is.null(el_tags)) fluidRow(el_tags)
    )
  })

  # ============================================
  # Summary plots - Sector-level NPV and PD
  # ============================================

  output$plot_sector_npv <- renderPlotly({
    req(rv$results)
    df <- rv$results
    npv_col <- intersect(names(df), c("crispy_perc_value_change", "net_present_value_change"))
    req(length(npv_col) > 0, "sector" %in% names(df))

    plot_data <- df %>%
      group_by(sector) %>%
      summarise(avg_npv_change = mean(.data[[npv_col[1]]], na.rm = TRUE) * 100, .groups = "drop")

    p <- ggplot(plot_data, aes(x = reorder(sector, avg_npv_change), y = avg_npv_change, fill = avg_npv_change < 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = TRISK_HEX_GREEN), guide = "none") +
      labs(title = "NPV Change by Sector", x = "", y = "Average NPV Change (%)") +
      trisk_plot_theme()

    ggplotly(p)
  })

  output$plot_sector_pd <- renderPlotly({
    req(rv$results)
    df <- rv$results
    pd_base_col <- intersect(names(df), c("pd_baseline", "crispy_pd_baseline"))
    pd_shock_col <- intersect(names(df), c("pd_shock", "crispy_pd_shock"))
    req(length(pd_base_col) > 0, length(pd_shock_col) > 0, "sector" %in% names(df))

    plot_data <- df %>%
      group_by(sector) %>%
      summarise(
        Baseline = mean(.data[[pd_base_col[1]]], na.rm = TRUE) * 100,
        Shock = mean(.data[[pd_shock_col[1]]], na.rm = TRUE) * 100,
        .groups = "drop"
      ) %>%
      pivot_longer(cols = c(Baseline, Shock), names_to = "scenario", values_to = "pd")

    p <- ggplot(plot_data, aes(x = sector, y = pd, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Baseline" = TRISK_HEX_GREEN, "Shock" = TRISK_HEX_RED)) +
      labs(title = "PD by Sector: Baseline vs Shock", x = "", y = "Avg PD (%)") +
      trisk_plot_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

  # ============================================
  # HORIZON ANALYSIS (Multi-Year Time-Series)
  # ============================================

  # Combine all shock-year results into a single long-form dataframe
  horizon_data <- reactive({
    req(rv$results_by_year)
    if (length(rv$results_by_year) < 2) return(NULL)
    bind_rows(rv$results_by_year, .id = "shock_year_chr") %>%
      mutate(shock_year = as.integer(shock_year_chr))
  })

  # Main Horizon Analysis UI
  output$horizon_analysis_ui <- renderUI({
    if (is.null(rv$results_by_year) || length(rv$results_by_year) < 2) {
      return(tags$div(
        style = "text-align: center; padding: 40px; color: #666;",
        icon("clock", style = "font-size: 48px; color: #DDD0D4;"),
        tags$h4("Horizon Analysis requires 2+ shock years"),
        tags$p("Select 2 or more shock years on the Configure tab, then re-run."),
        actionLink("goto_config_horizon", "Configure shock years",
                    icon = icon("cog"), style = "font-size: 14px;"),
        tags$p(style = "font-size: 13px; color: #999; margin-top: 8px;",
               "Tip: select years like 2030, 2035, 2040, 2045, 2050 for a full term structure.")
      ))
    }

    years <- sort(as.integer(names(rv$results_by_year)))

    tagList(
      # Header with summary
      tags$div(style = "margin-bottom: 15px; padding: 12px; background: #F0E6EA; border-radius: 8px;",
        fluidRow(
          column(8,
            tags$h4(style = "margin: 0 0 4px 0; font-weight: 700;",
                    icon("chart-area"), " Multi-Horizon Risk Analysis"),
            tags$p(style = "margin: 0; font-size: 13px; color: #666;",
                   paste0(length(years), " shock years analyzed: ",
                          paste(years, collapse = ", "),
                          " | ", nrow(rv$results_by_year[[1]]), " companies"))
          ),
          column(4, style = "text-align: right; padding-top: 8px;",
            downloadButton("download_horizon_csv", "Export Multi-Horizon CSV",
                          class = "btn-sm", style = paste0("background:", BRAND_CORAL, "; color: white; border: none;"))
          )
        )
      ),

      # Row 1: Portfolio-level time-series line charts
      fluidRow(
        box(width = 4, title = "Average PD (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_pd_line", height = "300px")),
        box(width = 4, title = "Average NPV Change Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_npv_line", height = "300px")),
        box(width = 4, title = "Total EL (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_el_line", height = "300px"))
      ),

      # Row 2: Sector-faceted evolution
      fluidRow(
        box(width = 12, title = "PD Evolution by Sector", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            plotlyOutput("horizon_sector_facet", height = "400px"))
      ),

      # Row 3: Company sparklines + Risk migration matrix
      fluidRow(
        box(width = 7, title = "Company Risk Trajectories", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            plotlyOutput("horizon_company_trajectories", height = "400px")),
        box(width = 5, title = "Risk Migration Matrix", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(style = "font-size: 12px; color: #666;",
                   "How companies move between PD risk buckets from earliest to latest horizon."),
            plotlyOutput("horizon_migration_heatmap", height = "350px"))
      ),

      # Row 4: Animated chart + Term structure
      fluidRow(
        box(width = 7, title = "Animated Portfolio Transition", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            plotlyOutput("horizon_animated", height = "400px")),
        box(width = 5, title = "PD Term Structure", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(style = "font-size: 12px; color: #666;",
                   "PD curve across shock years — analogous to a credit term structure."),
            plotlyOutput("horizon_term_structure", height = "350px"))
      ),

      # Row 5: Year-over-year table
      fluidRow(
        box(width = 12, title = "Year-over-Year Metrics", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput("horizon_yoy_table"))
      )
    )
  })

  # ---- Horizon: Portfolio PD line chart ----
  output$horizon_pd_line <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    agg <- hd %>% group_by(shock_year) %>%
      summarise(avg_pd = mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop")
    plot_ly(agg, x = ~shock_year, y = ~avg_pd, type = "scatter", mode = "lines+markers",
            line = list(color = BRAND_CORAL, width = 3),
            marker = list(color = BRAND_CORAL, size = 10),
            hovertemplate = "Year: %{x}<br>Avg PD: %{y:.4f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5),
             yaxis = list(title = "Avg PD (Shock) %"),
             margin = list(t = 10))
  })

  # ---- Horizon: Portfolio NPV line chart ----
  output$horizon_npv_line <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    agg <- hd %>% group_by(shock_year) %>%
      summarise(avg_npv = mean(crispy_perc_value_change, na.rm = TRUE) * 100, .groups = "drop")
    plot_ly(agg, x = ~shock_year, y = ~avg_npv, type = "scatter", mode = "lines+markers",
            line = list(color = STATUS_BLUE, width = 3),
            marker = list(color = STATUS_BLUE, size = 10),
            hovertemplate = "Year: %{x}<br>Avg NPV Change: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5),
             yaxis = list(title = "Avg NPV Change %"),
             margin = list(t = 10))
  })

  # ---- Horizon: Portfolio EL line chart ----
  output$horizon_el_line <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    agg <- hd %>% group_by(shock_year) %>%
      summarise(total_el = sum(expected_loss_shock, na.rm = TRUE), .groups = "drop")
    plot_ly(agg, x = ~shock_year, y = ~total_el, type = "scatter", mode = "lines+markers",
            line = list(color = STATUS_RED, width = 3),
            marker = list(color = STATUS_RED, size = 10),
            hovertemplate = "Year: %{x}<br>Total EL: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5),
             yaxis = list(title = "Total EL (Shock)"),
             margin = list(t = 10))
  })

  # ---- Horizon: Sector-faceted PD evolution ----
  output$horizon_sector_facet <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    has_sector <- "sector" %in% names(hd)
    if (!has_sector) return(plotly_empty())

    agg <- hd %>% group_by(shock_year, sector) %>%
      summarise(avg_pd = mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop")

    p <- plot_ly(agg, x = ~shock_year, y = ~avg_pd, color = ~sector,
                 type = "scatter", mode = "lines+markers",
                 hovertemplate = "%{fullData.name}<br>Year: %{x}<br>PD: %{y:.4f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5),
             yaxis = list(title = "Avg PD (Shock) %"),
             legend = list(orientation = "h", y = -0.2),
             margin = list(t = 10, b = 60))
    p
  })

  # ---- Horizon: Company trajectories (spaghetti + mean) ----
  output$horizon_company_trajectories <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    id_col <- if ("company_name" %in% names(hd)) "company_name" else "company_id"

    company_data <- hd %>%
      group_by(across(all_of(id_col)), shock_year) %>%
      summarise(pd = mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop")

    avg_data <- hd %>% group_by(shock_year) %>%
      summarise(pd = mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop") %>%
      mutate(!!id_col := "Portfolio Average")

    p <- plot_ly()
    # Individual company lines (thin, transparent)
    companies <- unique(company_data[[id_col]])
    for (comp in companies) {
      cd <- company_data[company_data[[id_col]] == comp, ]
      p <- p %>% add_trace(data = cd, x = ~shock_year, y = ~pd,
                           type = "scatter", mode = "lines",
                           line = list(width = 1.5, color = "rgba(200,150,160,0.5)"),
                           name = comp, legendgroup = "companies",
                           showlegend = (comp == companies[1]),
                           hovertemplate = paste0(comp, "<br>Year: %{x}<br>PD: %{y:.4f}%<extra></extra>"))
    }
    # Portfolio average (thick, dark)
    p <- p %>% add_trace(data = avg_data, x = ~shock_year, y = ~pd,
                         type = "scatter", mode = "lines+markers",
                         line = list(width = 4, color = BRAND_CORAL),
                         marker = list(size = 8, color = BRAND_CORAL),
                         name = "Portfolio Avg", legendgroup = "avg",
                         hovertemplate = "Portfolio Avg<br>Year: %{x}<br>PD: %{y:.4f}%<extra></extra>")
    p %>% layout(xaxis = list(title = "Shock Year", dtick = 5),
                 yaxis = list(title = "PD (Shock) %"),
                 legend = list(orientation = "h", y = -0.15),
                 margin = list(t = 10, b = 50))
  })

  # ---- Horizon: Risk migration matrix ----
  output$horizon_migration_heatmap <- renderPlotly({
    req(rv$results_by_year)
    years <- sort(as.integer(names(rv$results_by_year)))
    if (length(years) < 2) return(plotly_empty())

    first_year <- rv$results_by_year[[as.character(years[1])]]
    last_year <- rv$results_by_year[[as.character(years[length(years)])]]

    # Assign risk buckets
    pd_bucket <- function(pd) {
      cut(pd, breaks = c(-Inf, 0.001, 0.005, 0.01, 0.05, Inf),
          labels = c("Very Low\n(<0.1%)", "Low\n(0.1-0.5%)", "Medium\n(0.5-1%)",
                     "High\n(1-5%)", "Very High\n(>5%)"),
          right = TRUE)
    }

    id_col <- if ("company_id" %in% names(first_year)) "company_id" else "company_name"
    from_df <- data.frame(id = first_year[[id_col]], from = pd_bucket(first_year$pd_shock))
    to_df <- data.frame(id = last_year[[id_col]], to = pd_bucket(last_year$pd_shock))
    migration <- merge(from_df, to_df, by = "id")

    # Build transition matrix
    bucket_levels <- c("Very Low\n(<0.1%)", "Low\n(0.1-0.5%)", "Medium\n(0.5-1%)",
                       "High\n(1-5%)", "Very High\n(>5%)")
    migration$from <- factor(migration$from, levels = bucket_levels)
    migration$to <- factor(migration$to, levels = bucket_levels)

    mtx <- table(migration$from, migration$to)
    mtx_df <- as.data.frame.matrix(mtx)

    plot_ly(x = colnames(mtx_df), y = rownames(mtx_df), z = as.matrix(mtx_df),
            type = "heatmap",
            colorscale = list(c(0, "#FBF5F2"), c(0.5, "#EF7173"), c(1, "#8B2020")),
            hovertemplate = "From: %{y}<br>To: %{x}<br>Count: %{z}<extra></extra>") %>%
      layout(xaxis = list(title = paste0("Risk Bucket (", years[length(years)], ")"), tickangle = 0),
             yaxis = list(title = paste0("Risk Bucket (", years[1], ")"), autorange = "reversed"),
             margin = list(t = 10, l = 100, b = 80))
  })

  # ---- Horizon: Animated scatter (NPV vs PD by year) ----
  output$horizon_animated <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    if (!"crispy_perc_value_change" %in% names(hd)) return(plotly_empty())
    id_col <- if ("company_name" %in% names(hd)) "company_name" else "company_id"
    has_sector <- "sector" %in% names(hd)

    plot_df <- hd %>%
      mutate(npv_pct = crispy_perc_value_change * 100,
             pd_pct = pd_shock * 100,
             exp_size = sqrt(exposure_value_usd) / 100)

    if (has_sector) {
      p <- plot_ly(plot_df, x = ~npv_pct, y = ~pd_pct, size = ~exp_size,
                   color = ~sector, frame = ~shock_year,
                   text = ~paste0(get(id_col), "\nNPV: ", round(npv_pct, 2), "%\nPD: ", round(pd_pct, 4), "%"),
                   hoverinfo = "text",
                   type = "scatter", mode = "markers",
                   marker = list(opacity = 0.7, sizemode = "diameter", sizeref = 2))
    } else {
      p <- plot_ly(plot_df, x = ~npv_pct, y = ~pd_pct, size = ~exp_size,
                   frame = ~shock_year,
                   text = ~paste0(get(id_col), "\nNPV: ", round(npv_pct, 2), "%\nPD: ", round(pd_pct, 4), "%"),
                   hoverinfo = "text",
                   type = "scatter", mode = "markers",
                   marker = list(opacity = 0.7, color = BRAND_CORAL, sizemode = "diameter", sizeref = 2))
    }
    p %>% layout(xaxis = list(title = "NPV Change %"),
                 yaxis = list(title = "PD (Shock) %"),
                 margin = list(t = 10)) %>%
      animation_opts(frame = 800, transition = 400, redraw = FALSE) %>%
      animation_slider(currentvalue = list(prefix = "Shock Year: "))
  })

  # ---- Horizon: PD Term Structure ----
  output$horizon_term_structure <- renderPlotly({
    hd <- horizon_data()
    req(hd)

    has_sector <- "sector" %in% names(hd)
    if (has_sector) {
      agg <- hd %>% group_by(shock_year, sector) %>%
        summarise(
          avg_pd = mean(pd_shock, na.rm = TRUE) * 100,
          p25 = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
          p75 = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      # Show one line per sector + confidence ribbon for the portfolio
      portfolio_agg <- hd %>% group_by(shock_year) %>%
        summarise(
          avg_pd = mean(pd_shock, na.rm = TRUE) * 100,
          p25 = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
          p75 = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      p <- plot_ly() %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~p75, type = "scatter", mode = "lines",
                  line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~p25, type = "scatter", mode = "lines",
                  fill = "tonexty", fillcolor = "rgba(239,113,115,0.2)",
                  line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~avg_pd, type = "scatter", mode = "lines+markers",
                  line = list(color = BRAND_CORAL, width = 3, dash = "dash"),
                  marker = list(color = BRAND_CORAL, size = 8),
                  name = "Portfolio Avg",
                  hovertemplate = "Portfolio<br>Year: %{x}<br>PD: %{y:.4f}%<br>(IQR band shown)<extra></extra>")
    } else {
      portfolio_agg <- hd %>% group_by(shock_year) %>%
        summarise(
          avg_pd = mean(pd_shock, na.rm = TRUE) * 100,
          p25 = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
          p75 = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
          .groups = "drop"
        )
      p <- plot_ly() %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~p75, type = "scatter", mode = "lines",
                  line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~p25, type = "scatter", mode = "lines",
                  fill = "tonexty", fillcolor = "rgba(239,113,115,0.2)",
                  line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
        add_trace(data = portfolio_agg, x = ~shock_year, y = ~avg_pd, type = "scatter", mode = "lines+markers",
                  line = list(color = BRAND_CORAL, width = 3),
                  marker = list(color = BRAND_CORAL, size = 8),
                  name = "Portfolio Avg",
                  hovertemplate = "Year: %{x}<br>Avg PD: %{y:.4f}%<br>(IQR band shown)<extra></extra>")
    }
    p %>% layout(xaxis = list(title = "Shock Year", dtick = 5),
                 yaxis = list(title = "PD (Shock) %"),
                 legend = list(orientation = "h", y = -0.2),
                 margin = list(t = 10, b = 50))
  })

  # ---- Horizon: Year-over-Year metrics table ----
  output$horizon_yoy_table <- renderDT({
    req(rv$results_by_year)
    years <- sort(as.integer(names(rv$results_by_year)))
    if (length(years) < 2) return(NULL)

    rows <- lapply(years, function(yr) {
      df <- rv$results_by_year[[as.character(yr)]]
      data.frame(
        `Shock Year` = yr,
        `Avg PD (%)` = round(mean(df$pd_shock, na.rm = TRUE) * 100, 4),
        `Max PD (%)` = round(max(df$pd_shock, na.rm = TRUE) * 100, 4),
        `Avg NPV Change (%)` = round(mean(df$crispy_perc_value_change, na.rm = TRUE) * 100, 2),
        `Total EL` = round(sum(df$expected_loss_shock, na.rm = TRUE), 0),
        `Total EL Change` = round(sum(df$expected_loss_shock - df$expected_loss_baseline, na.rm = TRUE), 0),
        Companies = nrow(df),
        check.names = FALSE
      )
    })
    yoy_df <- bind_rows(rows)

    # Add YoY delta columns
    if (nrow(yoy_df) >= 2) {
      yoy_df$`PD Delta (pp)` <- c(NA, round(diff(yoy_df$`Avg PD (%)`), 4))
      yoy_df$`NPV Delta (pp)` <- c(NA, round(diff(yoy_df$`Avg NPV Change (%)`), 2))
    }

    datatable(yoy_df, options = list(dom = "t", pageLength = 20, scrollX = TRUE,
                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
              rownames = FALSE) %>%
      formatStyle("Avg PD (%)",
                  background = styleColorBar(range(yoy_df$`Avg PD (%)`, na.rm = TRUE), BRAND_CORAL_LT),
                  backgroundSize = "98% 60%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center") %>%
      formatStyle("Avg NPV Change (%)",
                  color = styleInterval(0, c(STATUS_RED, STATUS_GREEN)),
                  fontWeight = "bold")
  })

  # ---- Horizon: CSV Export ----
  output$download_horizon_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_multi_horizon_", rv$run_id, ".csv")
    },
    content = function(file) {
      req(rv$results_by_year)
      combined <- bind_rows(rv$results_by_year, .id = "shock_year")
      write.csv(combined, file, row.names = FALSE)
    }
  )

  # ============================================
  # SCENARIO COMPARISON DASHBOARD
  # ============================================

  # Reactive: combine all scenario results into a single long-form dataframe
  scenario_comparison_data <- reactive({
    req(rv$results_by_scenario)
    scen_results <- rv$results_by_scenario
    if (length(scen_results) < 2) return(NULL)  # Need 2+ scenarios for comparison

    all_rows <- list()
    for (scen_code in names(scen_results)) {
      year_list <- scen_results[[scen_code]]
      for (yr_key in names(year_list)) {
        df <- year_list[[yr_key]]
        df$target_scenario <- scen_code
        df$scenario_label <- scenario_label(scen_code)
        df$scenario_category <- scenario_category(scen_code)
        df$shock_year <- as.integer(yr_key)
        all_rows[[paste0(scen_code, "_", yr_key)]] <- df
      }
    }
    bind_rows(all_rows)
  })

  # Reactive: portfolio-level summary per scenario (aggregated across companies)
  scenario_portfolio_summary <- reactive({
    scd <- scenario_comparison_data()
    req(scd)

    scd %>%
      group_by(target_scenario, scenario_label, scenario_category, shock_year) %>%
      summarise(
        n_companies = n(),
        avg_pd_shock = mean(pd_shock, na.rm = TRUE),
        max_pd_shock = max(pd_shock, na.rm = TRUE),
        avg_pd_change = mean(pd_shock - pd_baseline, na.rm = TRUE),
        avg_npv_change = if ("crispy_perc_value_change" %in% names(scd))
          mean(crispy_perc_value_change, na.rm = TRUE) else NA_real_,
        total_el_shock = if ("expected_loss_shock" %in% names(scd))
          sum(expected_loss_shock, na.rm = TRUE) else NA_real_,
        total_el_change = if ("expected_loss_difference" %in% names(scd))
          sum(expected_loss_difference, na.rm = TRUE) else NA_real_,
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # ---- Scenario Comparison UI ----
  output$scenario_comparison_ui <- renderUI({
    scd <- scenario_comparison_data()
    if (is.null(scd)) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #999;",
        icon("layer-group", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Scenario Comparison requires 2+ target scenarios"),
        p("Select multiple target scenarios on the Configure tab to enable cross-scenario analysis.",
          style = "font-size: 14px;"),
        actionLink("goto_config_scenario", "Configure target scenarios",
                    icon = icon("cog"), style = "font-size: 14px;")
      ))
    }

    n_scen <- length(unique(scd$target_scenario))
    n_yrs <- length(unique(scd$shock_year))
    n_co <- length(unique(scd$company_id))

    tagList(
      # Header
      div(class = "section-header section-header--dark",
        fluidRow(
          column(8,
            h4(icon("layer-group"), paste0(" Scenario Comparison: ", n_scen, " scenarios"),
               style = "margin: 0; font-weight: 700;"),
            tags$small(paste0(n_yrs, " shock year(s) | ", n_co, " companies | ",
                             n_scen * n_yrs, " total runs"))
          ),
          column(4, style = "text-align: right;",
            downloadButton("download_scenario_csv", "Export CSV",
                          class = "btn btn-xs btn-export")
          )
        )
      ),

      # Row 1: Scenario Ranking Table + Tornado Chart
      fluidRow(
        box(title = "Scenario Impact Ranking", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Scenarios ranked by portfolio severity (highest avg PD shock first)."),
            DTOutput("scenario_ranking_table", height = "300px")
        ),
        box(title = "Sensitivity Tornado", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Range of portfolio metrics across scenarios \u2014 which has the biggest impact?"),
            plotlyOutput("scenario_tornado", height = "300px")
        )
      ),

      # Row 2: Grouped bar chart + Spider/radar
      fluidRow(
        box(title = "PD by Scenario & Sector", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("scenario_sector_bars", height = "350px")
        ),
        box(title = "Scenario Radar", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Normalized portfolio metrics (0\u20131 scale) overlaid per scenario."),
            plotlyOutput("scenario_radar", height = "350px")
        )
      ),

      # Row 3: Company-level heatmap
      fluidRow(
        box(title = "Company \u00D7 Scenario Heatmap", width = 12, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Rows = companies, Columns = scenarios. Cell color = PD shock intensity."),
            plotlyOutput("scenario_company_heatmap", height = "400px")
        )
      ),

      # ---- SCENARIO DISTRIBUTION (Probabilistic) ----
      div(class = "section-header section-header--purple", style = "margin: 20px 0 16px 0;",
        h4(icon("chart-area"), " Scenario Distribution \u2014 Probabilistic Analysis",
           style = "margin: 0; font-weight: 700;"),
        tags$small("Treating selected scenarios as a distribution of plausible outcomes. ",
                   "Confidence bands show the range of uncertainty across scenario pathways.")
      ),

      # Row 4: Fan chart + Violin plots
      fluidRow(
        box(title = "PD Fan Chart \u2014 Uncertainty Over Time", width = 7, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Median PD with P10\u2013P90 and P25\u2013P75 confidence bands across all scenarios."),
            plotlyOutput("dist_fan_chart", height = "380px")
        ),
        box(title = "PD Distribution by Scenario", width = 5, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Company-level PD spread per scenario (violin + box plot)."),
            plotlyOutput("dist_violin", height = "380px")
        )
      ),

      # Row 5: Exceedance curve + NPV fan chart
      fluidRow(
        box(title = "PD Exceedance Curve", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Probability that portfolio avg PD exceeds a given threshold, across scenarios."),
            plotlyOutput("dist_exceedance", height = "350px")
        ),
        box(title = "NPV Change Fan Chart", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Median NPV change with P10\u2013P90 bands across scenarios."),
            plotlyOutput("dist_npv_fan", height = "350px")
        )
      ),

      # Row 6: Summary statistics table
      fluidRow(
        box(title = "Distribution Summary Statistics", width = 12, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Cross-scenario quantiles for key portfolio metrics at each shock year."),
            DTOutput("dist_summary_table", height = "250px")
        )
      )
    )
  })

  # ---- Scenario Ranking Table ----
  output$scenario_ranking_table <- renderDT({
    sps <- scenario_portfolio_summary()
    req(sps)

    # Use earliest year for ranking
    earliest_yr <- min(sps$shock_year)
    ranking <- sps %>%
      filter(shock_year == max(sps$shock_year)) %>%
      arrange(desc(avg_pd_shock)) %>%
      mutate(
        rank = row_number(),
        avg_pd_pct = round(avg_pd_shock * 100, 3),
        max_pd_pct = round(max_pd_shock * 100, 3),
        avg_npv_pct = round(avg_npv_change * 100, 2),
        el_change = round(total_el_change, 0)
      ) %>%
      select(Rank = rank, Scenario = scenario_label, Category = scenario_category,
             `Avg PD (%)` = avg_pd_pct, `Max PD (%)` = max_pd_pct,
             `NPV Change (%)` = avg_npv_pct, `EL Change ($)` = el_change)

    datatable(ranking, rownames = FALSE, options = list(
      dom = "t", pageLength = 20, ordering = FALSE,
      scrollY = "250px", scrollCollapse = TRUE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )) %>%
      formatStyle("Avg PD (%)", backgroundColor = styleInterval(
        c(0.5, 2, 5), c("#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")))
  })

  # ---- Sensitivity Tornado Chart ----
  output$scenario_tornado <- renderPlotly({
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr)

    metrics <- data.frame(
      metric = c("Avg PD (%)", "Max PD (%)", "NPV Change (%)", "EL Change ($K)"),
      min_val = c(min(sps_yr$avg_pd_shock * 100), min(sps_yr$max_pd_shock * 100),
                  min(sps_yr$avg_npv_change * 100), min(sps_yr$total_el_change / 1000)),
      max_val = c(max(sps_yr$avg_pd_shock * 100), max(sps_yr$max_pd_shock * 100),
                  max(sps_yr$avg_npv_change * 100), max(sps_yr$total_el_change / 1000)),
      median_val = c(median(sps_yr$avg_pd_shock * 100), median(sps_yr$max_pd_shock * 100),
                     median(sps_yr$avg_npv_change * 100), median(sps_yr$total_el_change / 1000)),
      stringsAsFactors = FALSE
    )
    metrics$range <- metrics$max_val - metrics$min_val
    metrics <- metrics[order(metrics$range, decreasing = FALSE), ]
    metrics$metric <- factor(metrics$metric, levels = metrics$metric)

    plot_ly() %>%
      add_segments(data = metrics, x = ~min_val, xend = ~max_val, y = ~metric, yend = ~metric,
                   line = list(color = BRAND_CORAL, width = 14), showlegend = FALSE,
                   hoverinfo = "text",
                   text = ~paste0(metric, "\nMin: ", round(min_val, 2), " | Max: ", round(max_val, 2))) %>%
      add_markers(data = metrics, x = ~median_val, y = ~metric, marker = list(
        color = "#1A1A1A", size = 10, symbol = "diamond"), name = "Median",
        hoverinfo = "text", text = ~paste0("Median: ", round(median_val, 2))) %>%
      layout(xaxis = list(title = "Value", zeroline = TRUE),
             yaxis = list(title = ""),
             margin = list(l = 120),
             showlegend = FALSE)
  })

  # ---- Scenario × Sector Grouped Bar Chart ----
  output$scenario_sector_bars <- renderPlotly({
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)
    sector_scen <- scd %>%
      filter(shock_year == latest_yr) %>%
      group_by(scenario_label, sector) %>%
      summarise(avg_pd = mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop")

    plot_ly(sector_scen, x = ~sector, y = ~avg_pd, color = ~scenario_label,
            type = "bar", hoverinfo = "text",
            text = ~paste0(scenario_label, "\n", sector, ": ", round(avg_pd, 3), "%")) %>%
      layout(barmode = "group",
             xaxis = list(title = "Sector"),
             yaxis = list(title = "Avg PD Shock (%)"),
             legend = list(orientation = "h", y = -0.2))
  })

  # ---- Scenario Radar Chart ----
  output$scenario_radar <- renderPlotly({
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr)

    # Normalize each metric to 0-1 range
    normalize <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[2] == rng[1]) return(rep(0.5, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }

    radar_df <- sps_yr %>%
      mutate(
        pd_norm = normalize(avg_pd_shock),
        npv_norm = normalize(abs(avg_npv_change)),
        el_norm = normalize(abs(total_el_change)),
        max_pd_norm = normalize(max_pd_shock)
      )

    dims <- c("pd_norm", "npv_norm", "el_norm", "max_pd_norm")
    dim_labels <- c("Avg PD", "NPV Impact", "EL Impact", "Max PD")

    p <- plot_ly(type = "scatterpolar", fill = "toself")
    colors <- c(BRAND_CORAL, STATUS_BLUE, STATUS_GREEN, "#9C27B0", "#FF9800", "#795548")

    for (i in seq_len(nrow(radar_df))) {
      vals <- as.numeric(radar_df[i, dims])
      vals <- c(vals, vals[1])  # close the polygon
      col <- colors[((i - 1) %% length(colors)) + 1]
      p <- p %>% add_trace(
        r = vals, theta = c(dim_labels, dim_labels[1]),
        name = radar_df$scenario_label[i],
        line = list(color = col, width = 2),
        fillcolor = paste0(col, "33"),
        opacity = 0.7
      )
    }
    p %>% layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
      legend = list(orientation = "h", y = -0.15, font = list(size = 10)),
      margin = list(t = 30)
    )
  })

  # ---- Company × Scenario Heatmap ----
  output$scenario_company_heatmap <- renderPlotly({
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)

    # After portfolio enrichment, companies may have multiple rows (one per technology).
    # Aggregate to company-level using exposure-weighted mean PD.
    has_exposure <- "exposure_value_usd" %in% names(scd)
    heatmap_df <- scd %>%
      filter(shock_year == latest_yr) %>%
      group_by(company_name, scenario_label) %>%
      summarise(
        pd_shock = if (has_exposure) {
          weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE)
        } else {
          mean(pd_shock, na.rm = TRUE)
        },
        .groups = "drop"
      ) %>%
      mutate(pd_pct = round(pd_shock * 100, 3))

    # Pivot to matrix
    mat <- heatmap_df %>%
      select(company_name, scenario_label, pd_pct) %>%
      tidyr::pivot_wider(names_from = scenario_label, values_from = pd_pct)

    companies <- mat$company_name
    scenarios <- setdiff(names(mat), "company_name")
    z_matrix <- as.matrix(mat[, scenarios])

    # Build hover text matrix (same dims as z_matrix)
    text_matrix <- matrix("", nrow = length(companies), ncol = length(scenarios))
    for (r in seq_along(companies)) {
      for (cc in seq_along(scenarios)) {
        text_matrix[r, cc] <- paste0(companies[r], "\n", scenarios[cc], "\nPD: ", z_matrix[r, cc], "%")
      }
    }

    plot_ly(x = scenarios, y = companies, z = z_matrix,
            type = "heatmap",
            colorscale = list(c(0, "#e8f5e9"), c(0.3, "#fff9c4"), c(0.6, "#ffccbc"), c(1, "#c62828")),
            colorbar = list(title = "PD Shock (%)"),
            hoverinfo = "text",
            text = text_matrix) %>%
      layout(xaxis = list(title = "", tickangle = -30),
             yaxis = list(title = ""),
             margin = list(b = 120, l = 120))
  })

  # ========================================================
  # SCENARIO DISTRIBUTION — Probabilistic Analysis
  # ========================================================

  # Reactive: distribution quantiles at each shock year
  dist_quantiles <- reactive({
    sps <- scenario_portfolio_summary()
    req(sps)
    if (length(unique(sps$target_scenario)) < 2) return(NULL)

    sps %>%
      group_by(shock_year) %>%
      summarise(
        pd_p10 = quantile(avg_pd_shock, 0.10, na.rm = TRUE),
        pd_p25 = quantile(avg_pd_shock, 0.25, na.rm = TRUE),
        pd_median = median(avg_pd_shock, na.rm = TRUE),
        pd_p75 = quantile(avg_pd_shock, 0.75, na.rm = TRUE),
        pd_p90 = quantile(avg_pd_shock, 0.90, na.rm = TRUE),
        pd_mean = mean(avg_pd_shock, na.rm = TRUE),
        npv_p10 = quantile(avg_npv_change, 0.10, na.rm = TRUE),
        npv_p25 = quantile(avg_npv_change, 0.25, na.rm = TRUE),
        npv_median = median(avg_npv_change, na.rm = TRUE),
        npv_p75 = quantile(avg_npv_change, 0.75, na.rm = TRUE),
        npv_p90 = quantile(avg_npv_change, 0.90, na.rm = TRUE),
        el_p10 = quantile(total_el_change, 0.10, na.rm = TRUE),
        el_p25 = quantile(total_el_change, 0.25, na.rm = TRUE),
        el_median = median(total_el_change, na.rm = TRUE),
        el_p75 = quantile(total_el_change, 0.75, na.rm = TRUE),
        el_p90 = quantile(total_el_change, 0.90, na.rm = TRUE),
        n_scenarios = n(),
        .groups = "drop"
      )
  })

  # ---- PD Fan Chart ----
  output$dist_fan_chart <- renderPlotly({
    dq <- dist_quantiles()
    req(dq)

    sps <- scenario_portfolio_summary()
    yrs <- sort(dq$shock_year)

    p <- plot_ly() %>%
      # P10-P90 band (lightest)
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~pd_p10 * 100, ymax = ~pd_p90 * 100,
                  fillcolor = "rgba(245, 61, 63, 0.12)", line = list(width = 0),
                  name = "P10\u2013P90", showlegend = TRUE) %>%
      # P25-P75 band (darker)
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~pd_p25 * 100, ymax = ~pd_p75 * 100,
                  fillcolor = "rgba(245, 61, 63, 0.30)", line = list(width = 0),
                  name = "P25\u2013P75", showlegend = TRUE) %>%
      # Median line
      add_lines(data = dq, x = ~shock_year, y = ~pd_median * 100,
                line = list(color = BRAND_CORAL, width = 3, dash = "solid"),
                name = "Median") %>%
      # Mean line (dashed)
      add_lines(data = dq, x = ~shock_year, y = ~pd_mean * 100,
                line = list(color = "#1A1A1A", width = 2, dash = "dot"),
                name = "Mean")

    # Individual scenario traces (thin, semi-transparent)
    scen_labels <- unique(sps$scenario_label)
    colors_scen <- c("#2196F3", "#4CAF50", "#FF9800", "#9C27B0", "#795548",
                     "#607D8B", "#E91E63", "#00BCD4", "#8BC34A", "#FFC107")
    for (i in seq_along(scen_labels)) {
      scen_data <- sps %>% filter(scenario_label == scen_labels[i])
      col <- colors_scen[((i - 1) %% length(colors_scen)) + 1]
      p <- p %>% add_lines(data = scen_data, x = ~shock_year, y = ~avg_pd_shock * 100,
                           line = list(color = col, width = 1.2, dash = "solid"),
                           opacity = 0.5, name = scen_labels[i],
                           legendgroup = "scenarios", showlegend = (i <= 6))
    }

    p %>% layout(
      xaxis = list(title = "Shock Year", dtick = 5),
      yaxis = list(title = "Portfolio Avg PD (%)"),
      legend = list(orientation = "h", y = -0.2, font = list(size = 9)),
      hovermode = "x unified"
    )
  })

  # ---- PD Violin / Box Plot ----
  output$dist_violin <- renderPlotly({
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)
    violin_df <- scd %>%
      filter(shock_year == latest_yr) %>%
      mutate(pd_pct = pd_shock * 100)

    plot_ly(violin_df, x = ~scenario_label, y = ~pd_pct,
            type = "violin", box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "all", jitter = 0.3, pointpos = -1.5,
            marker = list(size = 4, opacity = 0.6),
            fillcolor = "rgba(245, 61, 63, 0.15)",
            line = list(color = BRAND_CORAL),
            hoverinfo = "y") %>%
      layout(xaxis = list(title = "", tickangle = -25),
             yaxis = list(title = "PD Shock (%)"),
             showlegend = FALSE,
             margin = list(b = 100))
  })

  # ---- PD Exceedance Curve ----
  output$dist_exceedance <- renderPlotly({
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr) %>% arrange(avg_pd_shock)
    n <- nrow(sps_yr)

    if (n < 2) return(plotly_empty())

    # Build exceedance: probability of exceeding threshold x
    pd_vals <- sort(sps_yr$avg_pd_shock * 100)
    exceed_probs <- 1 - (seq_along(pd_vals) - 0.5) / n

    plot_ly() %>%
      add_lines(x = pd_vals, y = exceed_probs * 100,
                line = list(color = BRAND_CORAL, width = 3, shape = "hv"),
                fill = "tozeroy", fillcolor = "rgba(245, 61, 63, 0.12)",
                name = "Exceedance") %>%
      add_markers(x = pd_vals, y = exceed_probs * 100,
                  marker = list(color = BRAND_CORAL, size = 8),
                  text = paste0("PD > ", round(pd_vals, 3), "%: ",
                               round(exceed_probs * 100, 1), "% of scenarios"),
                  hoverinfo = "text", showlegend = FALSE) %>%
      layout(xaxis = list(title = "Portfolio Avg PD Threshold (%)"),
             yaxis = list(title = "Exceedance Probability (%)", range = c(0, 105)),
             showlegend = FALSE)
  })

  # ---- NPV Change Fan Chart ----
  output$dist_npv_fan <- renderPlotly({
    dq <- dist_quantiles()
    req(dq)
    sps <- scenario_portfolio_summary()

    p <- plot_ly() %>%
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~npv_p10 * 100, ymax = ~npv_p90 * 100,
                  fillcolor = "rgba(90, 142, 174, 0.15)", line = list(width = 0),
                  name = "P10\u2013P90") %>%
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~npv_p25 * 100, ymax = ~npv_p75 * 100,
                  fillcolor = "rgba(90, 142, 174, 0.35)", line = list(width = 0),
                  name = "P25\u2013P75") %>%
      add_lines(data = dq, x = ~shock_year, y = ~npv_median * 100,
                line = list(color = STATUS_BLUE, width = 3),
                name = "Median")

    # Individual scenario traces
    scen_labels <- unique(sps$scenario_label)
    for (i in seq_along(scen_labels)) {
      scen_data <- sps %>% filter(scenario_label == scen_labels[i])
      p <- p %>% add_lines(data = scen_data, x = ~shock_year, y = ~avg_npv_change * 100,
                           line = list(width = 1, dash = "dot"),
                           opacity = 0.4, name = scen_labels[i],
                           legendgroup = "scen2", showlegend = (i <= 4))
    }

    p %>% layout(
      xaxis = list(title = "Shock Year", dtick = 5),
      yaxis = list(title = "Average NPV Change (%)"),
      legend = list(orientation = "h", y = -0.2, font = list(size = 9)),
      hovermode = "x unified"
    )
  })

  # ---- Distribution Summary Statistics Table ----
  output$dist_summary_table <- renderDT({
    dq <- dist_quantiles()
    req(dq)

    display_df <- dq %>%
      mutate(
        `Shock Year` = shock_year,
        `N Scenarios` = n_scenarios,
        `PD P10 (%)` = round(pd_p10 * 100, 4),
        `PD P25 (%)` = round(pd_p25 * 100, 4),
        `PD Median (%)` = round(pd_median * 100, 4),
        `PD P75 (%)` = round(pd_p75 * 100, 4),
        `PD P90 (%)` = round(pd_p90 * 100, 4),
        `NPV P10 (%)` = round(npv_p10 * 100, 2),
        `NPV Median (%)` = round(npv_median * 100, 2),
        `NPV P90 (%)` = round(npv_p90 * 100, 2),
        `EL Median ($)` = round(el_median, 0),
        `EL P90 ($)` = round(el_p90, 0)
      ) %>%
      select(`Shock Year`, `N Scenarios`,
             `PD P10 (%)`, `PD P25 (%)`, `PD Median (%)`, `PD P75 (%)`, `PD P90 (%)`,
             `NPV P10 (%)`, `NPV Median (%)`, `NPV P90 (%)`,
             `EL Median ($)`, `EL P90 ($)`)

    datatable(display_df, rownames = FALSE, options = list(
      dom = "t", pageLength = 20,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )) %>%
      formatStyle("PD P90 (%)", backgroundColor = styleInterval(
        c(1, 3, 5), c("#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")))
  })

  # ---- Export scenario comparison data ----
  output$download_scenario_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_scenario_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      scd <- scenario_comparison_data()
      if (!is.null(scd)) write.csv(scd, file, row.names = FALSE)
    }
  )

  # ============================================
  # ATTRIBUTION / WATERFALL DASHBOARD
  # ============================================

  # Reactive: attribution data — builds the decomposition of PD/NPV/EL changes
  # by sector, technology, and company, using the primary results (first scenario, first year).
  # For multi-scenario runs, we also build cross-scenario decompositions.
  attribution_data <- reactive({
    req(rv$results)
    df <- rv$results
    req(all(c("pd_baseline", "pd_shock", "exposure_value_usd", "sector") %in% names(df)))

    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_npv <- "crispy_perc_value_change" %in% names(df)
    has_tech <- "technology" %in% names(df)
    has_company <- any(c("company_name", "company_id") %in% names(df))

    total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)

    # --- Company-level attribution ---
    # After portfolio enrichment, results may have multiple rows per company
    # (one per technology). Aggregate to true company-level for attribution.
    company_label_col <- if (has_company && "company_name" %in% names(df)) "company_name" else "company_id"
    row_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        pd_contribution = pd_change * weight,
        company_label = .data[[company_label_col]],
        el_change = if (has_el) expected_loss_shock - expected_loss_baseline else NA_real_,
        npv_change_pct = if (has_npv) crispy_perc_value_change * 100 else NA_real_
      )

    company_df <- row_df %>%
      group_by(company_label, sector) %>%
      summarise(
        # Weighted means MUST come before exposure_value_usd is overwritten by sum()
        pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
        pd_shock    = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
        pd_change   = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
        npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
        exposure_value_usd = sum(exposure_value_usd, na.rm = TRUE),
        weight      = sum(weight, na.rm = TRUE),
        pd_contribution = sum(pd_contribution, na.rm = TRUE),
        el_change   = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    # Guard NA labels
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      paste0("Company_", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )

    # --- Sector-level attribution ---
    sector_df <- company_df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE),
        avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
        avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        pd_contribution = sum(pd_contribution, na.rm = TRUE),
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(abs(pd_contribution)))

    # --- Technology-level attribution (if available) ---
    # Use row_df (pre-company-aggregation) since it still has the technology column
    tech_df <- if (has_tech) {
      row_df %>%
        group_by(sector, technology) %>%
        summarise(
          n_companies = n_distinct(company_label),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          pd_contribution = sum(pd_contribution, na.rm = TRUE),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(desc(abs(pd_contribution)))
    } else NULL

    # --- Portfolio-level waterfall steps ---
    portfolio_pd_baseline <- weighted.mean(df$pd_baseline, df$exposure_value_usd, na.rm = TRUE)
    portfolio_pd_shock <- weighted.mean(df$pd_shock, df$exposure_value_usd, na.rm = TRUE)
    portfolio_pd_change <- portfolio_pd_shock - portfolio_pd_baseline

    list(
      company = company_df,
      company_detail = row_df,   # per-row (technology-level) for detailed table
      sector = sector_df,
      tech = tech_df,
      portfolio_pd_baseline = portfolio_pd_baseline,
      portfolio_pd_shock = portfolio_pd_shock,
      portfolio_pd_change = portfolio_pd_change,
      total_exposure = total_exposure,
      has_el = has_el,
      has_npv = has_npv,
      has_tech = has_tech
    )
  })

  # ---- Attribution Dashboard UI ----
  output$attribution_ui <- renderUI({
    ad <- attribution_data()
    if (is.null(ad)) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #999;",
        icon("chart-bar", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Run analysis to view attribution breakdown"),
        p("Results are needed to decompose risk changes by sector, technology, and company.",
          style = "font-size: 14px;"),
        actionLink("goto_run_attribution", "Go to Run Analysis",
                    icon = icon("play"), style = "font-size: 14px;")
      ))
    }

    n_sectors <- nrow(ad$sector)
    n_co <- nrow(ad$company)

    tagList(
      # Header
      div(class = "section-header section-header--green",
        fluidRow(
          column(8,
            h4(icon("chart-bar"), " Risk Attribution Dashboard",
               style = "margin: 0; font-weight: 700;"),
            tags$small(paste0(n_sectors, " sectors | ", n_co, " companies | ",
                             "Portfolio PD change: ", round(ad$portfolio_pd_change * 100, 4), " pp"),
                      style = "opacity: 0.85;")
          ),
          column(4, style = "text-align: right;",
            downloadButton("download_attribution_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),

      # Row 1: Waterfall chart (top-down PD decomposition)
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("exchange-alt"), " PD Waterfall: Baseline \u2192 Sector Contributions \u2192 Shocked PD",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Each bar shows how much each sector contributes to the total portfolio PD change (exposure-weighted).",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_waterfall", height = "400px")
          )
        )
      ),

      # Row 2: Company movers + Sector×Driver heatmap
      fluidRow(
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("sort-amount-down"), " Top Risk Movers (Companies)",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Companies with the largest marginal contribution to portfolio PD change.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_company_movers", height = "380px")
          )
        ),
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("chart-pie"), " Marginal Contribution to Portfolio Risk",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Each company's share of the total portfolio PD change (absolute).",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_marginal_contribution", height = "380px")
          )
        )
      ),

      # Row 3: Sector×Technology heatmap + EL waterfall
      fluidRow(
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("th"), " Sector \u00D7 Technology Attribution",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("PD contribution by sector and technology combination.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_sector_tech_heatmap", height = "380px")
          )
        ),
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("money-bill-wave"), " Expected Loss Attribution by Sector",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Change in expected loss (EL) decomposed by sector.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_el_waterfall", height = "380px")
          )
        )
      ),

      # Row 4: Attribution summary table
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("table"), " Detailed Attribution Table",
               style = "font-weight: 600; margin-top: 0;"),
            DTOutput("attr_detail_table")
          )
        )
      )
    )
  })

  # ---- Waterfall Chart: Baseline PD -> Sector contributions -> Shocked PD ----
  output$attr_waterfall <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    sector_df <- ad$sector
    baseline <- ad$portfolio_pd_baseline * 100  # convert to percentage points
    shocked <- ad$portfolio_pd_shock * 100

    # Build waterfall data: start with baseline, add sector contributions, end with total
    wf_labels <- c("Portfolio\nBaseline PD", sector_df$sector, "Portfolio\nShocked PD")
    wf_values <- c(baseline, sector_df$pd_contribution * 100, shocked)
    wf_measure <- c("absolute", rep("relative", nrow(sector_df)), "total")

    # Colors: baseline=blue, positive contributions=red, negative=green, total=dark
    wf_colors <- c(STATUS_BLUE)
    for (v in sector_df$pd_contribution) {
      wf_colors <- c(wf_colors, if (v > 0) BRAND_CORAL else STATUS_GREEN)
    }
    wf_colors <- c(wf_colors, "#1A1A1A")

    # Hover text
    wf_text <- c(
      paste0("Baseline PD: ", round(baseline, 4), "%"),
      paste0(sector_df$sector,
             "\nContribution: ", ifelse(sector_df$pd_contribution >= 0, "+", ""),
             round(sector_df$pd_contribution * 100, 4), " pp",
             "\nWeight: ", round(sector_df$weight * 100, 1), "%",
             "\nCompanies: ", sector_df$n_companies),
      paste0("Shocked PD: ", round(shocked, 4), "%",
             "\nTotal change: ", ifelse(shocked - baseline >= 0, "+", ""),
             round(shocked - baseline, 4), " pp")
    )

    plot_ly(
      x = wf_labels,
      y = wf_values,
      type = "waterfall",
      measure = wf_measure,
      text = wf_text,
      hoverinfo = "text",
      connector = list(line = list(color = "#DDD0D4", width = 1, dash = "dot")),
      increasing = list(marker = list(color = BRAND_CORAL)),
      decreasing = list(marker = list(color = STATUS_GREEN)),
      totals = list(marker = list(color = "#1A1A1A")),
      textposition = "outside",
      texttemplate = "%{y:.4f}"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11),
                     categoryorder = "array", categoryarray = wf_labels),
        yaxis = list(title = "Portfolio PD (%)", tickformat = ".4f"),
        margin = list(b = 100, t = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Top Company Risk Movers (horizontal bar chart) ----
  output$attr_company_movers <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    company_df <- ad$company %>%
      arrange(desc(abs(pd_contribution))) %>%
      head(15)

    # Sort by contribution for display (largest positive at top)
    company_df <- company_df %>%
      arrange(pd_contribution)

    colors <- ifelse(company_df$pd_contribution >= 0, BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      company_df$company_label,
      "\nSector: ", company_df$sector,
      "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp",
      "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
      "\nWeight: ", round(company_df$weight * 100, 1), "%",
      "\nContribution: ", ifelse(company_df$pd_contribution >= 0, "+", ""),
      round(company_df$pd_contribution * 100, 4), " pp"
    )

    plot_ly(
      y = factor(company_df$company_label, levels = company_df$company_label),
      x = company_df$pd_contribution * 100,
      type = "bar",
      orientation = "h",
      marker = list(color = colors, line = list(width = 0)),
      text = hover_text,
      hoverinfo = "text",
      texttemplate = "%{x:.4f}",
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Contribution to Portfolio PD Change (pp)", zeroline = TRUE,
                     zerolinecolor = "#999", zerolinewidth = 1),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 120, r = 50, t = 10, b = 50),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Marginal Contribution (treemap-style bar chart) ----
  output$attr_marginal_contribution <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    company_df <- ad$company %>%
      mutate(abs_contribution = abs(pd_contribution)) %>%
      arrange(desc(abs_contribution)) %>%
      head(20)

    total_abs <- sum(company_df$abs_contribution)

    company_df <- company_df %>%
      mutate(
        share_pct = abs_contribution / total_abs * 100,
        direction = ifelse(pd_contribution >= 0, "Risk Increase", "Risk Decrease")
      )

    # Cumulative share for visual ordering
    company_df <- company_df %>%
      arrange(desc(share_pct))

    colors <- ifelse(company_df$direction == "Risk Increase", BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      company_df$company_label,
      "\nSector: ", company_df$sector,
      "\nShare of total PD change: ", round(company_df$share_pct, 1), "%",
      "\nDirection: ", company_df$direction,
      "\nContribution: ", round(company_df$pd_contribution * 100, 4), " pp"
    )

    plot_ly(
      labels = company_df$company_label,
      values = company_df$share_pct,
      parents = company_df$sector,
      type = "treemap",
      marker = list(
        colors = colors,
        line = list(color = "white", width = 1)
      ),
      text = hover_text,
      hoverinfo = "text",
      textinfo = "label+percent parent",
      textfont = list(size = 11)
    ) %>%
      layout(
        margin = list(l = 5, r = 5, t = 5, b = 5)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Sector × Technology Attribution Heatmap ----
  output$attr_sector_tech_heatmap <- renderPlotly({
    ad <- attribution_data()
    req(ad, ad$has_tech, ad$tech)

    tech_df <- ad$tech
    if (is.null(tech_df) || nrow(tech_df) == 0) {
      return(plotly_empty() %>% layout(title = "No technology data"))
    }

    sectors <- sort(unique(tech_df$sector))
    techs <- sort(unique(tech_df$technology))

    # Build matrix
    z_matrix <- matrix(0, nrow = length(techs), ncol = length(sectors))
    text_matrix <- matrix("", nrow = length(techs), ncol = length(sectors))

    for (r in seq_along(techs)) {
      for (cc in seq_along(sectors)) {
        match_row <- tech_df %>%
          filter(sector == sectors[cc], technology == techs[r])
        if (nrow(match_row) > 0) {
          z_matrix[r, cc] <- match_row$pd_contribution[1] * 100
          text_matrix[r, cc] <- paste0(
            techs[r], " / ", sectors[cc],
            "\nPD Contribution: ", round(match_row$pd_contribution[1] * 100, 4), " pp",
            "\nCompanies: ", match_row$n_companies[1],
            "\nExposure: $", format(round(match_row$total_exposure[1]), big.mark = ",")
          )
        } else {
          text_matrix[r, cc] <- paste0(techs[r], " / ", sectors[cc], "\nNo data")
        }
      }
    }

    # Custom diverging color scale: green (negative/good) -> white -> red (positive/bad)
    colorscale <- list(
      list(0, STATUS_GREEN),
      list(0.5, "#FFFFFF"),
      list(1, BRAND_CORAL)
    )

    plot_ly(
      x = sectors,
      y = techs,
      z = z_matrix,
      type = "heatmap",
      colorscale = colorscale,
      zmid = 0,
      text = text_matrix,
      hoverinfo = "text",
      colorbar = list(title = "PD Contr.\n(pp)", len = 0.6)
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 100, b = 80, t = 10)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Expected Loss Attribution by Sector (waterfall) ----
  output$attr_el_waterfall <- renderPlotly({
    ad <- attribution_data()
    req(ad, ad$has_el)

    sector_df <- ad$sector %>%
      filter(!is.na(el_change)) %>%
      arrange(desc(abs(el_change)))

    if (nrow(sector_df) == 0) {
      return(plotly_empty() %>% layout(title = "No EL data available"))
    }

    total_el_change <- sum(sector_df$el_change, na.rm = TRUE)

    # Build waterfall: sector contributions + total
    wf_labels <- c(sector_df$sector, "Total EL\nChange")
    wf_values <- c(sector_df$el_change, total_el_change)
    wf_measure <- c(rep("relative", nrow(sector_df)), "total")

    hover_text <- c(
      paste0(sector_df$sector,
             "\nEL Change: $", format(round(sector_df$el_change), big.mark = ","),
             "\nExposure: $", format(round(sector_df$total_exposure), big.mark = ","),
             "\nCompanies: ", sector_df$n_companies),
      paste0("Total EL Change: $", format(round(total_el_change), big.mark = ","))
    )

    plot_ly(
      x = wf_labels,
      y = wf_values,
      type = "waterfall",
      measure = wf_measure,
      text = hover_text,
      hoverinfo = "text",
      connector = list(line = list(color = "#DDD0D4", width = 1, dash = "dot")),
      # For EL: negative change (more loss) = bad = red, positive = green
      increasing = list(marker = list(color = STATUS_GREEN)),
      decreasing = list(marker = list(color = BRAND_CORAL)),
      totals = list(marker = list(color = "#333333")),
      textposition = "outside",
      texttemplate = "$%{y:,.0f}"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11)),
        yaxis = list(title = "EL Change ($)", tickformat = "$,.0f"),
        margin = list(b = 100, t = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Detailed Attribution Table ----
  output$attr_detail_table <- renderDT({
    ad <- attribution_data()
    req(ad)

    # Use technology-level detail rows for the full breakdown table
    detail_df <- ad$company_detail

    display_df <- detail_df %>%
      arrange(desc(abs(pd_contribution))) %>%
      transmute(
        Company = company_label,
        Sector = sector,
        Technology = if (ad$has_tech) technology else "N/A",
        `Exposure ($)` = round(exposure_value_usd, 0),
        `Weight (%)` = round(weight * 100, 2),
        `PD Baseline (%)` = round(pd_baseline * 100, 4),
        `PD Shock (%)` = round(pd_shock * 100, 4),
        `PD Change (pp)` = round(pd_change * 100, 4),
        `Contribution (pp)` = round(pd_contribution * 100, 4),
        `Contribution (%)` = if (abs(ad$portfolio_pd_change) > 1e-10)
          round(pd_contribution / ad$portfolio_pd_change * 100, 1) else NA_real_
      )

    if (ad$has_el) {
      display_df$`EL Change ($)` <- round(detail_df$el_change, 0)
    }
    if (ad$has_npv) {
      display_df$`NPV Change (%)` <- round(detail_df$npv_change_pct, 2)
    }

    datatable(display_df, rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(list(8, "desc")),  # Sort by Contribution (pp) descending
        columnDefs = list(
          list(className = "dt-right", targets = 3:ncol(display_df) - 1)
        )
      )
    ) %>%
      formatCurrency("Exposure ($)", currency = "$", digits = 0) %>%
      formatStyle(
        "PD Change (pp)",
        backgroundColor = styleInterval(
          c(-0.01, 0, 0.01, 0.05),
          c("#c8e6c9", "#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Contribution (pp)",
        backgroundColor = styleInterval(
          c(-0.005, 0, 0.005, 0.02),
          c("#c8e6c9", "#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")
        ),
        fontWeight = "bold"
      )
  })

  # ---- Export Attribution CSV ----
  output$download_attribution_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_attribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      ad <- attribution_data()
      if (!is.null(ad)) {
        export_df <- ad$company_detail %>%
          select(any_of(c("company_label", "company_id", "sector", "technology",
                         "exposure_value_usd", "weight", "pd_baseline", "pd_shock",
                         "pd_change", "pd_contribution", "el_change", "npv_change_pct")))
        write.csv(export_df, file, row.names = FALSE)
      }
    }
  )

  # ============================================
  # CONCENTRATION RISK CENTER
  # ============================================

  # Reactive: concentration data — builds all aggregations needed for
  # heatmaps, treemaps, scatter, Lorenz curve, geographic view, and scorecard.
  concentration_data <- reactive({
    req(rv$results)
    df <- rv$results
    req(all(c("pd_baseline", "pd_shock", "exposure_value_usd", "sector") %in% names(df)))

    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_npv <- "crispy_perc_value_change" %in% names(df)
    has_tech <- "technology" %in% names(df)
    has_geo <- "country_iso2" %in% names(df)
    has_company <- any(c("company_name", "company_id") %in% names(df))

    total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)

    # --- Company-level enrichment ---
    company_label_col <- if (has_company && "company_name" %in% names(df)) "company_name" else "company_id"
    company_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        company_label = .data[[company_label_col]]
      )
    # Guard NA labels: replace with row-index fallback
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      paste0("Company_", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )
    if (has_el) {
      company_df <- company_df %>%
        mutate(el_change = expected_loss_shock - expected_loss_baseline)
    }
    if (has_npv) {
      company_df <- company_df %>%
        mutate(npv_change_pct = crispy_perc_value_change * 100)
    }

    # --- Concentration metrics ---
    weights <- company_df$weight
    weights <- weights[!is.na(weights)]
    weights_sorted <- sort(weights, decreasing = TRUE)
    n <- length(weights)

    hhi <- sum(weights^2, na.rm = TRUE) * 10000  # HHI on 0-10000 scale
    cr5 <- sum(head(weights_sorted, 5), na.rm = TRUE) * 100
    cr10 <- sum(head(weights_sorted, 10), na.rm = TRUE) * 100
    max_single <- if (n > 0) max(weights, na.rm = TRUE) * 100 else 0

    # Gini coefficient
    gini <- if (n > 1) {
      sorted_w <- sort(weights)
      total_w <- sum(sorted_w)
      if (total_w > 0) {
        numerator <- sum((2 * seq_along(sorted_w) - n - 1) * sorted_w)
        numerator / (n * total_w)
      } else 0
    } else 0

    # --- Sector-level aggregation ---
    sector_df <- company_df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE),
        avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
        avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(total_exposure))

    # --- Geography-level aggregation ---
    geo_df <- if (has_geo) {
      company_df %>%
        group_by(country_iso2) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
          avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
          avg_pd_change = avg_pd_shock - avg_pd_baseline,
          .groups = "drop"
        ) %>%
        arrange(desc(total_exposure))
    } else NULL

    # --- Sector × Geography cross-tab ---
    cross_df <- if (has_geo) {
      company_df %>%
        group_by(sector, country_iso2) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
    } else NULL

    # --- Sector × Technology cross-tab ---
    cross_tech_df <- if (has_tech) {
      company_df %>%
        group_by(sector, technology) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          .groups = "drop"
        )
    } else NULL

    # --- Lorenz curve data (normalized so last value = 100%) ---
    cum_exp_raw <- cumsum(sort(weights))
    cum_exp_norm <- if (length(cum_exp_raw) > 0 && cum_exp_raw[length(cum_exp_raw)] > 0) {
      cum_exp_raw / cum_exp_raw[length(cum_exp_raw)]
    } else cum_exp_raw
    lorenz_df <- data.frame(
      cum_companies = c(0, seq_len(n)) / n * 100,
      cum_exposure = c(0, cum_exp_norm) * 100
    )

    list(
      company = company_df,
      sector = sector_df,
      geo = geo_df,
      cross = cross_df,
      cross_tech = cross_tech_df,
      lorenz = lorenz_df,
      total_exposure = total_exposure,
      n_companies = n,
      hhi = hhi,
      cr5 = cr5,
      cr10 = cr10,
      max_single = max_single,
      gini = gini,
      has_el = has_el,
      has_npv = has_npv,
      has_tech = has_tech,
      has_geo = has_geo
    )
  })

  # ---- Concentration UI ----
  output$concentration_ui <- renderUI({
    cd <- concentration_data()
    if (is.null(cd)) {
      return(div(style = "text-align: center; padding: 60px 20px; color: #999;",
        icon("th-large", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Run analysis to view concentration risk"),
        p("Results are needed to assess portfolio concentration by sector, geography, and company.",
          style = "font-size: 14px;"),
        actionLink("goto_run_concentration", "Go to Run Analysis",
                    icon = icon("play"), style = "font-size: 14px;")
      ))
    }

    # HHI traffic light
    hhi_color <- if (cd$hhi < 1500) STATUS_GREEN else if (cd$hhi < 2500) "#D4A017" else BRAND_CORAL
    hhi_label <- if (cd$hhi < 1500) "Low" else if (cd$hhi < 2500) "Moderate" else "High"
    gini_color <- if (cd$gini < 0.4) STATUS_GREEN else if (cd$gini < 0.6) "#D4A017" else BRAND_CORAL
    cr5_color <- if (cd$cr5 < 50) STATUS_GREEN else if (cd$cr5 < 75) "#D4A017" else BRAND_CORAL

    tagList(
      # Header
      div(class = "section-header section-header--blue",
        fluidRow(
          column(8,
            h4(icon("th-large"), " Concentration Risk Center",
               style = "margin: 0; font-weight: 700;"),
            tags$small(paste0(cd$n_companies, " companies | ",
                             nrow(cd$sector), " sectors | ",
                             "Total exposure: $", format_number(cd$total_exposure)),
                      style = "opacity: 0.85;")
          ),
          column(4, style = "text-align: right;",
            downloadButton("download_concentration_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),

      # Row 1: Concentration Scorecard
      fluidRow(
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", hhi_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("HHI Index", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(format(round(cd$hhi), big.mark = ","), style = paste0("margin: 0; color: ", hhi_color, "; font-weight: 700;")),
            tags$span(hhi_label, style = paste0("font-size: 12px; font-weight: 600; color: ", hhi_color, ";"))
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", cr5_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Top-5 Concentration (CR5)", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(paste0(round(cd$cr5, 1), "%"), style = paste0("margin: 0; color: ", cr5_color, "; font-weight: 700;")),
            tags$small(paste0("Top-10: ", round(cd$cr10, 1), "%"), style = "color: #888;")
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", gini_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Gini Coefficient", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(round(cd$gini, 3), style = paste0("margin: 0; color: ", gini_color, "; font-weight: 700;")),
            tags$span(if (cd$gini < 0.4) "Even" else if (cd$gini < 0.6) "Moderate" else "Concentrated",
                     style = paste0("font-size: 12px; font-weight: 600; color: ", gini_color, ";"))
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", STATUS_BLUE,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Max Single-Name Weight", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(paste0(round(cd$max_single, 1), "%"), style = paste0("margin: 0; color: ", STATUS_BLUE, "; font-weight: 700;")),
            tags$small(paste0(cd$n_companies, " companies total"), style = "color: #888;")
          )
        )
      ),

      # Row 2: Primary heatmap with controls + Top-10 donut
      fluidRow(
        column(8,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            fluidRow(
              column(6,
                h5(icon("th"), " Concentration Heatmap",
                   style = "font-weight: 600; margin-top: 0;")
              ),
              column(3,
                selectInput("conc_heatmap_metric", NULL,
                           choices = c("PD Change" = "pd_change",
                                      "Exposure ($)" = "exposure",
                                      "EL Change ($)" = "el_change",
                                      "NPV Change (%)" = "npv_change"),
                           selected = "pd_change",
                           width = "100%")
              ),
              column(3,
                selectInput("conc_heatmap_dims", NULL,
                           choices = c("Sector \u00D7 Geography" = "sector_geo",
                                      "Sector \u00D7 Technology" = "sector_tech",
                                      "Sector \u00D7 Company" = "sector_company"),
                           selected = "sector_geo",
                           width = "100%")
              )
            ),
            plotlyOutput("conc_heatmap", height = "420px")
          )
        ),
        column(4,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("chart-pie"), " Top-10 Exposure Concentration",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Largest single-name exposures as % of portfolio.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_donut", height = "370px")
          )
        )
      ),

      # Row 3: Treemap + Lorenz curve
      fluidRow(
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("project-diagram"), " Exposure Treemap (Sector \u2192 Company)",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Size = exposure weight, color = PD change magnitude.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_treemap", height = "400px")
          )
        ),
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("chart-area"), " Lorenz Curve (Exposure Concentration)",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small(paste0("Gini = ", round(cd$gini, 3),
                             " | Perfect equality = 0, maximum concentration = 1."),
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_lorenz", height = "400px")
          )
        )
      ),

      # Row 4: Risk-return scatter + Geographic choropleth
      fluidRow(
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("bullseye"), " Risk-Return Scatter",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Each bubble = one company. x = PD change, y = exposure weight, size = abs EL change.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_scatter", height = "400px")
          )
        ),
        column(6,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("globe"), " Geographic Concentration",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Country-level exposure and PD change.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_geo_map", height = "400px")
          )
        )
      ),

      # Row 5: Dual-axis baseline vs shocked concentration
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("columns"), " Concentration Shift: Baseline vs Shocked",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Side-by-side sector concentration before and after the transition shock.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_dual_bar", height = "380px")
          )
        )
      )
    )
  })

  # ---- Concentration Heatmap (dynamic dimensions + metrics) ----
  output$conc_heatmap <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    metric <- input$conc_heatmap_metric
    dims <- input$conc_heatmap_dims

    # Select cross-tab data based on chosen dimensions
    if (dims == "sector_geo") {
      if (!isTRUE(cd$has_geo) || is.null(cd$cross)) {
        return(plotly_empty() %>%
          layout(title = list(text = "No geography data — try Sector \u00D7 Technology or Sector \u00D7 Company",
                             font = list(size = 13, color = "#999"))))
      }
      cross <- cd$cross
      row_col <- "country_iso2"
      row_label <- "Geography"
    } else if (dims == "sector_tech") {
      if (!isTRUE(cd$has_tech) || is.null(cd$cross_tech)) {
        return(plotly_empty() %>%
          layout(title = list(text = "No technology data — try Sector \u00D7 Geography or Sector \u00D7 Company",
                             font = list(size = 13, color = "#999"))))
      }
      cross <- cd$cross_tech
      row_col <- "technology"
      row_label <- "Technology"
    } else {
      # sector_company fallback: always available
      cross <- cd$company %>%
        group_by(sector, company_label) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          el_change = if (cd$has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (cd$has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
      # Limit to top 20 companies for readability
      top_companies <- cd$company %>%
        arrange(desc(exposure_value_usd)) %>%
        head(20) %>%
        pull(company_label)
      cross <- cross %>% filter(company_label %in% top_companies)
      row_col <- "company_label"
      row_label <- "Company"
    }
    if (is.null(cross) || nrow(cross) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected dimensions"))
    }

    rows <- sort(unique(cross[[row_col]]))
    cols <- sort(unique(cross$sector))

    # Select metric
    metric_col <- switch(metric,
      "pd_change" = "avg_pd_change",
      "exposure" = "total_exposure",
      "el_change" = "el_change",
      "npv_change" = "npv_change_pct"
    )
    metric_label <- switch(metric,
      "pd_change" = "PD Change (pp)",
      "exposure" = "Exposure ($)",
      "el_change" = "EL Change ($)",
      "npv_change" = "NPV Change (%)"
    )

    # Check if metric column exists
    if (!metric_col %in% names(cross)) {
      return(plotly_empty() %>% layout(title = paste("Metric", metric_label, "not available")))
    }

    # Build matrix
    z_matrix <- matrix(NA, nrow = length(rows), ncol = length(cols))
    text_matrix <- matrix("", nrow = length(rows), ncol = length(cols))

    for (r in seq_along(rows)) {
      for (cc in seq_along(cols)) {
        match_row <- cross %>%
          filter(.data[[row_col]] == rows[r], sector == cols[cc])
        if (nrow(match_row) > 0) {
          val <- match_row[[metric_col]][1]
          z_matrix[r, cc] <- if (metric == "pd_change") val * 100 else val
          text_matrix[r, cc] <- paste0(
            rows[r], " / ", cols[cc],
            "\n", metric_label, ": ",
            if (metric == "pd_change") paste0(round(val * 100, 4), " pp")
            else if (metric == "exposure") paste0("$", format(round(val), big.mark = ","))
            else if (metric == "el_change") paste0("$", format(round(val), big.mark = ","))
            else paste0(round(val, 2), "%"),
            "\nCompanies: ", match_row$n_companies[1]
          )
        } else {
          text_matrix[r, cc] <- paste0(rows[r], " / ", cols[cc], "\nNo data")
        }
      }
    }

    # Color scale depends on metric
    if (metric %in% c("pd_change", "el_change")) {
      colorscale <- list(list(0, STATUS_GREEN), list(0.5, "#FFFFFF"), list(1, BRAND_CORAL))
      use_zmid <- 0
    } else if (metric == "npv_change") {
      colorscale <- list(list(0, BRAND_CORAL), list(0.5, "#FFFFFF"), list(1, STATUS_GREEN))
      use_zmid <- 0
    } else {
      # Exposure: sequential scale
      colorscale <- list(list(0, "#FBF5F2"), list(0.5, "#5A8EAE"), list(1, "#1A4D6E"))
      use_zmid <- NULL
    }

    trace_args <- list(
      x = cols,
      y = rows,
      z = z_matrix,
      type = "heatmap",
      colorscale = colorscale,
      text = text_matrix,
      hoverinfo = "text",
      colorbar = list(title = metric_label, len = 0.6)
    )
    if (!is.null(use_zmid)) trace_args$zmid <- use_zmid

    do.call(plot_ly, trace_args) %>%
      layout(
        xaxis = list(title = "Sector", tickangle = -30, tickfont = list(size = 10)),
        yaxis = list(title = row_label, tickfont = list(size = 10)),
        margin = list(l = 80, b = 80, t = 10)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Top-10 Exposure Donut ----
  output$conc_donut <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    n_top <- min(10, nrow(cd$company))
    company_df <- cd$company %>%
      arrange(desc(weight)) %>%
      head(n_top)

    # Guard against NA labels — replace with company_id or index
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      if ("company_id" %in% names(company_df)) paste0("ID: ", company_df$company_id)
      else paste0("Company ", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )

    other_weight <- max(0, 1 - sum(company_df$weight, na.rm = TRUE))
    n_others <- cd$n_companies - n_top

    # Build labels/values — only add "Others" slice if there are remaining companies
    if (n_others > 0 && other_weight > 0.001) {
      labels <- c(as.character(company_df$company_label), "Others")
      values <- c(company_df$weight * 100, other_weight * 100)
    } else {
      labels <- as.character(company_df$company_label)
      values <- company_df$weight * 100
    }

    palette <- c(BRAND_CORAL, BRAND_CORAL_LT, STATUS_BLUE, "#5A8EAE", "#7BADC4",
                 STATUS_GREEN, "#8BB85F", "#A8C97B", "#D4A017", "#E8C547")
    colors <- if (n_others > 0 && other_weight > 0.001) {
      c(rep(palette, length.out = n_top), "#CCCCCC")
    } else {
      rep(palette, length.out = n_top)
    }

    hover_text <- paste0(company_df$company_label,
             "\nSector: ", company_df$sector,
             "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
             "\nWeight: ", round(company_df$weight * 100, 1), "%",
             "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp")
    if (n_others > 0 && other_weight > 0.001) {
      hover_text <- c(hover_text,
        paste0("Others\n", n_others, " companies\nWeight: ", round(other_weight * 100, 1), "%"))
    }

    plot_ly(
      labels = labels,
      values = values,
      type = "pie",
      hole = 0.5,
      marker = list(colors = colors, line = list(color = "white", width = 1.5)),
      text = hover_text,
      hoverinfo = "text",
      textinfo = "label+percent",
      textfont = list(size = 10),
      insidetextorientation = "radial"
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 10, b = 10),
        annotations = list(
          list(text = paste0("<b>Top ", n_top, "</b><br>", round(sum(company_df$weight, na.rm = TRUE) * 100, 1), "%"),
               x = 0.5, y = 0.5, font = list(size = 14, color = "#333"),
               showarrow = FALSE)
        )
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Exposure Treemap (Sector -> Company) ----
  output$conc_treemap <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    # Take top 30 companies by exposure for readability
    top_co <- cd$company %>%
      arrange(desc(exposure_value_usd)) %>%
      head(30)

    if (nrow(top_co) == 0) {
      return(plotly_empty() %>% layout(title = "No company data available"))
    }

    # Color by PD change magnitude (diverging)
    pd_range <- max(abs(top_co$pd_change), na.rm = TRUE)
    if (is.na(pd_range) || pd_range == 0) pd_range <- 1  # avoid division by zero

    hover_text <- paste0(
      top_co$company_label,
      "\nSector: ", top_co$sector,
      "\nExposure: $", format(round(top_co$exposure_value_usd), big.mark = ","),
      "\nWeight: ", round(top_co$weight * 100, 1), "%",
      "\nPD Change: ", round(top_co$pd_change * 100, 4), " pp"
    )

    # Numeric color: -1 (green) to +1 (red)
    color_vals <- top_co$pd_change / pd_range

    # Ensure labels are non-NA strings for treemap
    treemap_labels <- ifelse(is.na(top_co$company_label),
                             paste0("Company_", seq_len(nrow(top_co))),
                             as.character(top_co$company_label))

    # Plotly treemap requires parent nodes to exist as entries in the labels array.
    # Add sector root nodes (with parent = "") so the hierarchy renders.
    sectors <- unique(top_co$sector)
    sector_weights <- sapply(sectors, function(s) {
      sum(top_co$weight[top_co$sector == s], na.rm = TRUE) * 100
    })
    sector_pd <- sapply(sectors, function(s) {
      rows <- top_co[top_co$sector == s, ]
      weighted.mean(rows$pd_change, rows$exposure_value_usd, na.rm = TRUE)
    })
    sector_color_vals <- sector_pd / pd_range

    sector_hover <- paste0(
      sectors,
      "\nTotal weight: ", round(sector_weights, 1), "%",
      "\nAvg PD Change: ", round(sector_pd * 100, 4), " pp"
    )

    # Combine: sector roots first, then company leaves
    all_labels <- c(sectors, treemap_labels)
    all_parents <- c(rep("", length(sectors)), as.character(top_co$sector))
    all_values <- c(rep(0, length(sectors)), top_co$weight * 100)  # sector values=0 so they don't double-count
    all_colors <- c(sector_color_vals, color_vals)
    all_text <- c(sector_hover, hover_text)

    plot_ly(
      labels = all_labels,
      parents = all_parents,
      values = all_values,
      type = "treemap",
      branchvalues = "remainder",
      marker = list(
        color = all_colors,
        colorscale = list(list(0, STATUS_GREEN), list(0.5, "#FFFFCC"), list(1, BRAND_CORAL)),
        cmid = 0,
        colorbar = list(title = "PD Change\n(norm.)", len = 0.5),
        line = list(color = "white", width = 1)
      ),
      text = all_text,
      hoverinfo = "text",
      textinfo = "label+value",
      texttemplate = "%{label}<br>%{value:.1f}%",
      textfont = list(size = 10)
    ) %>%
      layout(
        margin = list(l = 5, r = 5, t = 5, b = 5)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Lorenz Curve ----
  output$conc_lorenz <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    lorenz <- cd$lorenz

    plot_ly() %>%
      # Equality line
      add_trace(
        x = c(0, 100), y = c(0, 100),
        type = "scatter", mode = "lines",
        line = list(color = "#CCCCCC", width = 1.5, dash = "dash"),
        name = "Perfect Equality",
        hoverinfo = "skip"
      ) %>%
      # Lorenz curve
      add_trace(
        x = lorenz$cum_companies,
        y = lorenz$cum_exposure,
        type = "scatter", mode = "lines",
        line = list(color = STATUS_BLUE, width = 2.5),
        fill = "tozeroy",
        fillcolor = "rgba(90,142,174,0.15)",
        name = "Portfolio",
        hovertemplate = paste0(
          "Bottom %{x:.1f}% of companies<br>",
          "Hold %{y:.1f}% of exposure<extra></extra>"
        )
      ) %>%
      # Gini annotation
      add_annotations(
        x = 70, y = 25,
        text = paste0("<b>Gini = ", round(cd$gini, 3), "</b>"),
        showarrow = FALSE,
        font = list(size = 16, color = STATUS_BLUE)
      ) %>%
      layout(
        xaxis = list(title = "Cumulative % of Companies (smallest first)",
                     range = c(0, 100), ticksuffix = "%"),
        yaxis = list(title = "Cumulative % of Exposure",
                     range = c(0, 100), ticksuffix = "%"),
        showlegend = TRUE,
        legend = list(x = 0.05, y = 0.95),
        margin = list(l = 60, r = 20, t = 10, b = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Risk-Return Scatter (bubble) ----
  output$conc_scatter <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    company_df <- cd$company

    # Compute bubble sizes — normalize to visible range [4, 20] pixels
    bubble_raw <- if (cd$has_el) {
      abs(company_df$el_change)
    } else {
      company_df$exposure_value_usd / cd$total_exposure * 100
    }
    bubble_raw[is.na(bubble_raw)] <- 0
    bmax <- max(bubble_raw, na.rm = TRUE)
    bubble_size <- if (bmax > 0) {
      (bubble_raw / bmax) * 16 + 4  # range [4, 20]
    } else {
      rep(8, length(bubble_raw))  # default 8px if all zeros
    }
    size_label <- if (cd$has_el) "Abs EL Change" else "Weight (%)"

    colors <- ifelse(company_df$pd_change >= 0, BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      company_df$company_label,
      "\nSector: ", company_df$sector,
      "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp",
      "\nWeight: ", round(company_df$weight * 100, 2), "%",
      "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
      if (cd$has_el) paste0("\nEL Change: $", format(round(company_df$el_change), big.mark = ",")) else ""
    )

    plot_ly(
      x = company_df$pd_change * 100,
      y = company_df$weight * 100,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = bubble_size,  # already scaled to [4, 20] range
        color = colors,
        opacity = 0.7,
        line = list(color = "white", width = 0.5)
      ),
      text = hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "PD Change (pp)", zeroline = TRUE,
                     zerolinecolor = "#999", zerolinewidth = 1),
        yaxis = list(title = "Exposure Weight (%)", zeroline = FALSE),
        margin = list(l = 60, r = 20, t = 10, b = 60),
        showlegend = FALSE,
        # Add quadrant shading annotation
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 100, y0 = 0, y1 = 100,
               fillcolor = "rgba(232,75,77,0.05)", line = list(width = 0),
               layer = "below"),
          list(type = "rect", x0 = -100, x1 = 0, y0 = 0, y1 = 100,
               fillcolor = "rgba(107,159,59,0.05)", line = list(width = 0),
               layer = "below")
        ),
        annotations = list(
          list(x = 0.95, y = 0.95, xref = "paper", yref = "paper",
               text = "High exposure +\nPD increase", showarrow = FALSE,
               font = list(size = 10, color = BRAND_CORAL), opacity = 0.6),
          list(x = 0.05, y = 0.95, xref = "paper", yref = "paper",
               text = "High exposure +\nPD decrease", showarrow = FALSE,
               font = list(size = 10, color = STATUS_GREEN), opacity = 0.6)
        )
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Geographic Choropleth ----
  output$conc_geo_map <- renderPlotly({
    cd <- concentration_data()
    req(cd, cd$has_geo, cd$geo)

    geo_df <- cd$geo
    if (is.null(geo_df) || nrow(geo_df) == 0) {
      return(plotly_empty() %>% layout(title = "No geography data"))
    }

    hover_text <- paste0(
      geo_df$country_iso2,
      "\nCompanies: ", geo_df$n_companies,
      "\nExposure: $", format(round(geo_df$total_exposure), big.mark = ","),
      "\nWeight: ", round(geo_df$weight * 100, 1), "%",
      "\nAvg PD Change: ", round(geo_df$avg_pd_change * 100, 4), " pp"
    )

    # ISO-2 to ISO-3 conversion for plotly choropleth
    iso2_to_iso3 <- c(
      AF="AFG",AL="ALB",DZ="DZA",AD="AND",AO="AGO",AG="ATG",AR="ARG",AM="ARM",
      AU="AUS",AT="AUT",AZ="AZE",BS="BHS",BH="BHR",BD="BGD",BB="BRB",BY="BLR",
      BE="BEL",BZ="BLZ",BJ="BEN",BT="BTN",BO="BOL",BA="BIH",BW="BWA",BR="BRA",
      BN="BRN",BG="BGR",BF="BFA",BI="BDI",KH="KHM",CM="CMR",CA="CAN",CF="CAF",
      TD="TCD",CL="CHL",CN="CHN",CO="COL",KM="COM",CD="COD",CG="COG",CR="CRI",
      CI="CIV",HR="HRV",CU="CUB",CY="CYP",CZ="CZE",DK="DNK",DJ="DJI",DM="DMA",
      DO="DOM",EC="ECU",EG="EGY",SV="SLV",GQ="GNQ",ER="ERI",EE="EST",ET="ETH",
      FJ="FJI",FI="FIN",FR="FRA",GA="GAB",GM="GMB",GE="GEO",DE="DEU",GH="GHA",
      GR="GRC",GT="GTM",GN="GIN",GW="GNB",GY="GUY",HT="HTI",HN="HND",HU="HUN",
      IS="ISL",IN="IND",ID="IDN",IR="IRN",IQ="IRQ",IE="IRL",IL="ISR",IT="ITA",
      JM="JAM",JP="JPN",JO="JOR",KZ="KAZ",KE="KEN",KW="KWT",KG="KGZ",LA="LAO",
      LV="LVA",LB="LBN",LS="LSO",LR="LBR",LY="LBY",LT="LTU",LU="LUX",MG="MDG",
      MW="MWI",MY="MYS",MV="MDV",ML="MLI",MT="MLT",MR="MRT",MU="MUS",MX="MEX",
      MD="MDA",MN="MNG",ME="MNE",MA="MAR",MZ="MOZ",MM="MMR","NA"="NAM",NP="NPL",
      NL="NLD",NZ="NZL",NI="NIC",NE="NER",NG="NGA",NO="NOR",OM="OMN",PK="PAK",
      PA="PAN",PG="PNG",PY="PRY",PE="PER",PH="PHL",PL="POL",PT="PRT",QA="QAT",
      RO="ROU",RU="RUS",RW="RWA",SA="SAU",SN="SEN",RS="SRB",SL="SLE",SG="SGP",
      SK="SVK",SI="SVN",SO="SOM",ZA="ZAF",KR="KOR",SS="SSD",ES="ESP",LK="LKA",
      SD="SDN",SR="SUR",SZ="SWZ",SE="SWE",CH="CHE",SY="SYR",TW="TWN",TJ="TJK",
      TZ="TZA",TH="THA",TL="TLS",TG="TGO",TT="TTO",TN="TUN",TR="TUR",TM="TKM",
      UG="UGA",UA="UKR",AE="ARE",GB="GBR",US="USA",UY="URY",UZ="UZB",VE="VEN",
      VN="VNM",YE="YEM",ZM="ZMB",ZW="ZWE"
    )

    geo_df$iso3 <- ifelse(
      toupper(geo_df$country_iso2) %in% names(iso2_to_iso3),
      iso2_to_iso3[toupper(geo_df$country_iso2)],
      geo_df$country_iso2  # fallback: pass as-is
    )

    # For small country counts (<=5), use a bar chart for clarity
    # (small countries like DE are nearly invisible on world choropleth)
    if (nrow(geo_df) <= 5) {
      geo_df <- geo_df %>% arrange(desc(total_exposure))
      bar_colors <- ifelse(geo_df$avg_pd_change >= 0, BRAND_CORAL, STATUS_GREEN)
      plot_ly() %>%
        add_bars(
          x = geo_df$country_iso2,
          y = geo_df$total_exposure,
          name = "Exposure ($)",
          marker = list(color = STATUS_BLUE, opacity = 0.6),
          hovertemplate = paste0(
            "%{x}<br>Exposure: $%{y:,.0f}",
            "<br>Weight: ", round(geo_df$weight * 100, 1), "%",
            "<br>Companies: ", geo_df$n_companies, "<extra></extra>"
          )
        ) %>%
        add_bars(
          x = geo_df$country_iso2,
          y = geo_df$avg_pd_change * 100,
          name = "PD Change (pp)",
          yaxis = "y2",
          marker = list(color = bar_colors, opacity = 0.8),
          hovertemplate = paste0(
            "%{x}<br>PD Change: %{y:.4f} pp<extra></extra>"
          )
        ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = "Country"),
          yaxis = list(title = "Exposure ($)", side = "left", tickformat = "$,.0f"),
          yaxis2 = list(title = "PD Change (pp)", side = "right", overlaying = "y",
                        tickformat = ".4f"),
          legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
          margin = list(t = 50, b = 50),
          annotations = list(
            list(text = paste0(nrow(geo_df), " countries in portfolio"),
                 xref = "paper", yref = "paper", x = 0.5, y = -0.15,
                 showarrow = FALSE, font = list(size = 11, color = "#999"))
          )
        ) %>%
        config(displayModeBar = FALSE)
    } else {
      # Full choropleth for many countries
      plot_ly(
        type = "choropleth",
        locations = geo_df$iso3,
        locationmode = "ISO-3",
        z = geo_df$avg_pd_change * 100,
        text = hover_text,
        hoverinfo = "text",
        colorscale = list(list(0, STATUS_GREEN), list(0.5, "#FFFFCC"), list(1, BRAND_CORAL)),
        zmid = 0,
        colorbar = list(title = "PD Change\n(pp)", len = 0.6),
        marker = list(line = list(color = "#333", width = 1))
      ) %>%
        layout(
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "#DDD0D4",
            projection = list(type = "natural earth"),
            fitbounds = "locations",
            bgcolor = "#FAFAFA"
          ),
          margin = list(l = 0, r = 0, t = 10, b = 0)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })

  # ---- Dual Bar: Baseline vs Shocked concentration ----
  output$conc_dual_bar <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    sector_df <- cd$sector

    # Baseline concentration: exposure weighted by baseline PD
    # Shocked concentration: exposure weighted by shock PD
    # Show both side by side as grouped bar
    baseline_weighted <- sector_df$avg_pd_baseline * sector_df$weight * 100
    shock_weighted <- sector_df$avg_pd_shock * sector_df$weight * 100

    plot_ly(x = sector_df$sector) %>%
      add_bars(
        y = baseline_weighted,
        name = "Baseline (weighted PD \u00D7 weight)",
        marker = list(color = STATUS_BLUE, opacity = 0.7),
        hovertemplate = paste0(
          "%{x}<br>",
          "Baseline contribution: %{y:.4f} pp<br>",
          "Avg PD: ", round(sector_df$avg_pd_baseline * 100, 4), " pp<br>",
          "Weight: ", round(sector_df$weight * 100, 1), "%<extra></extra>"
        )
      ) %>%
      add_bars(
        y = shock_weighted,
        name = "Shocked (weighted PD \u00D7 weight)",
        marker = list(color = BRAND_CORAL, opacity = 0.7),
        hovertemplate = paste0(
          "%{x}<br>",
          "Shock contribution: %{y:.4f} pp<br>",
          "Avg PD: ", round(sector_df$avg_pd_shock * 100, 4), " pp<br>",
          "Weight: ", round(sector_df$weight * 100, 1), "%<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11)),
        yaxis = list(title = "PD Contribution (pp = avg PD \u00D7 exposure weight)"),
        legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
        margin = list(b = 80, t = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Export Concentration CSV ----
  output$download_concentration_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_concentration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      cd <- concentration_data()
      if (!is.null(cd)) {
        export_df <- cd$company %>%
          select(any_of(c("company_label", "company_id", "sector", "technology",
                         "country_iso2", "exposure_value_usd", "weight",
                         "pd_baseline", "pd_shock", "pd_change",
                         "el_change", "npv_change_pct")))
        # Add concentration metrics as attributes in separate rows at top
        metrics_row <- data.frame(
          company_label = c("## CONCENTRATION METRICS ##"),
          company_id = c(NA),
          sector = c(paste0("HHI=", round(cd$hhi), " | CR5=", round(cd$cr5, 1),
                           "% | CR10=", round(cd$cr10, 1), "% | Gini=", round(cd$gini, 3))),
          stringsAsFactors = FALSE
        )
        write.csv(export_df, file, row.names = FALSE)
      }
    }
  )

  # ============================================
  # INTEGRATION PAGE - PD Integration
  # ============================================

  # Method description
  output$pd_method_description <- renderUI({
    method <- input$pd_integration_method
    desc <- switch(method,
      "absolute" = tagList(
        tags$b("Absolute Change Application"),
        tags$p("TRISK-adjusted PD = Internal_PD + (PD_shock - PD_baseline)"),
        tags$p("Adds the raw PD change from TRISK to your internal PD estimate.")
      ),
      "relative" = tagList(
        tags$b("Relative Change Application"),
        tags$p("TRISK-adjusted PD = Internal_PD * (1 + PD_change%)"),
        tags$p("Scales your internal PD by the relative percentage shift from the TRISK model.")
      ),
      "zscore" = tagList(
        tags$b("Z-Score Integration (Basel IRB)"),
        tags$p(HTML("TRISK-adjusted PD = &Phi;(&Phi;<sup>-1</sup>(Internal_PD) + &Phi;<sup>-1</sup>(PD_shock) - &Phi;<sup>-1</sup>(PD_baseline))")),
        tags$p("Uses the Vasicek one-factor model approach. Combines PDs in the normal quantile
               (z-score) space, preserving the non-linear relationship at distribution tails.
               Recommended for Basel IRB-aligned institutions.")
      )
    )
    desc
  })

  # PD input table (editable Internal_PD column)
  output$pd_integration_table <- renderDT({
    req(rv$results)
    df <- rv$results

    # Build display with key identifiers + PD columns
    id_cols <- intersect(c("company_id", "company_name", "sector", "technology"), names(df))
    pd_cols <- intersect(c("exposure_value_usd", "pd_baseline", "pd_shock"), names(df))
    show_cols <- c(id_cols, pd_cols)

    display_df <- df[, show_cols, drop = FALSE]

    # Round pd_baseline and pd_shock for display
    if ("pd_baseline" %in% names(display_df)) display_df$pd_baseline <- smart_round(display_df$pd_baseline)
    if ("pd_shock" %in% names(display_df)) display_df$pd_shock <- smart_round(display_df$pd_shock)

    # Add computed PD change
    if (all(c("pd_baseline", "pd_shock") %in% names(display_df))) {
      display_df$PD_Change <- smart_round(display_df$pd_shock - display_df$pd_baseline)
      display_df$PD_Change_Pct <- smart_round(
        ifelse(display_df$pd_baseline != 0,
               (display_df$pd_shock - display_df$pd_baseline) / display_df$pd_baseline * 100,
               NA_real_))
    }

    # Editable Internal_PD column:
    # Priority: (1) user-uploaded/edited values in rv$internal_pd, (2) pd_baseline, (3) zero
    if (!is.null(rv$internal_pd) && length(rv$internal_pd) == nrow(df)) {
      display_df$Internal_PD <- smart_round(rv$internal_pd)
    } else if ("pd_baseline" %in% names(df)) {
      display_df$Internal_PD <- smart_round(df$pd_baseline)
    } else {
      display_df$Internal_PD <- 0
    }

    datatable(
      display_df,
      editable = list(target = "cell", disable = list(columns = seq_len(ncol(display_df) - 1) - 1)),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Observe edits to Internal_PD in the PD integration table
  observeEvent(input$pd_integration_table_cell_edit, {
    info <- input$pd_integration_table_cell_edit
    req(rv$results)

    # Initialize internal_pd vector if needed
    if (is.null(rv$internal_pd)) {
      if ("pd_baseline" %in% names(rv$results)) {
        rv$internal_pd <- rv$results$pd_baseline
      } else {
        rv$internal_pd <- rep(0, nrow(rv$results))
      }
    }

    # The editable column is the last one (Internal_PD)
    row_idx <- info$row
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && row_idx >= 1 && row_idx <= length(rv$internal_pd)) {
      rv$internal_pd[row_idx] <- new_val
    }
  })

  # Upload Internal PDs from CSV
  observeEvent(input$upload_internal_pd, {
    req(input$upload_internal_pd, rv$results)
    tryCatch({
      uploaded <- read.csv(input$upload_internal_pd$datapath, stringsAsFactors = FALSE)
      names(uploaded) <- tolower(names(uploaded))

      # Schema + value validation
      validation_err <- validate_internal_pd_csv(uploaded)
      if (!is.null(validation_err)) {
        showNotification(paste("Validation error:", validation_err),
                        type = "error", duration = 8)
        return()
      }

      # Match by company_id
      df <- rv$results
      internal_pd <- if ("pd_baseline" %in% names(df)) df$pd_baseline else rep(0, nrow(df))

      matched <- 0
      for (i in seq_len(nrow(df))) {
        cid <- as.character(df$company_id[i])
        idx <- which(as.character(uploaded$company_id) == cid)
        if (length(idx) > 0) {
          val <- as.numeric(uploaded$internal_pd[idx[1]])
          if (!is.na(val)) {
            internal_pd[i] <- val
            matched <- matched + 1
          }
        }
      }

      rv$internal_pd <- internal_pd
      showNotification(
        paste0("Internal PDs loaded: ", matched, " of ", nrow(df), " counterparties matched."),
        type = if (matched > 0) "message" else "warning", duration = 6
      )
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message), type = "error", duration = 8)
    })
  })

  # Internal PD upload status
  output$internal_pd_upload_status <- renderUI({
    if (!is.null(rv$internal_pd)) {
      # Safety: if results row count changed, warn and reset
      if (!is.null(rv$results) && length(rv$internal_pd) != nrow(rv$results)) {
        rv$internal_pd <- NULL
        return(tags$span(class = "text-warning", icon("triangle-exclamation"),
                        "Internal PDs reset (portfolio size changed)"))
      }
      n_custom <- if (!is.null(rv$results) && "pd_baseline" %in% names(rv$results)) {
        sum(rv$internal_pd != rv$results$pd_baseline, na.rm = TRUE)
      } else 0
      if (n_custom > 0) {
        tagList(
          tags$span(class = "text-success", icon("check-circle"),
                   paste0(n_custom, " custom PD values loaded")),
          if (length(rv$run_history) > 0)
            tags$br(), tags$span(class = "text-muted", style = "font-size: 11px;",
                                icon("clock-rotate-left"), " Preserved from previous run")
        )
      } else {
        tags$span(class = "text-muted", icon("info-circle"),
                 "Using pd_baseline as default")
      }
    }
  })

  # Apply PD integration
  observeEvent(input$apply_pd_integration, {
    req(rv$results)
    df <- rv$results
    method <- input$pd_integration_method

    # Get internal PD values
    internal_pd <- if (!is.null(rv$internal_pd)) {
      rv$internal_pd
    } else if ("pd_baseline" %in% names(df)) {
      df$pd_baseline
    } else {
      rep(0, nrow(df))
    }

    pd_baseline <- if ("pd_baseline" %in% names(df)) df$pd_baseline else rep(0, nrow(df))
    pd_shock <- if ("pd_shock" %in% names(df)) df$pd_shock else rep(0, nrow(df))

    pd_change <- pd_shock - pd_baseline
    pd_change_pct <- ifelse(pd_baseline != 0, pd_change / pd_baseline, 0)

    adjusted_pd <- switch(method,
      "absolute" = {
        internal_pd + pd_change
      },
      "relative" = {
        internal_pd * (1 + pd_change_pct)
      },
      "zscore" = {
        # Basel IRB z-score integration
        # adjusted = Phi(Phi^{-1}(internal) + Phi^{-1}(shock) - Phi^{-1}(baseline))
        # Clip inputs to (0.0001, 0.9999) to avoid Inf from qnorm
        clip_pd <- function(x) pmin(pmax(x, 0.0001), 0.9999)
        z_internal <- qnorm(clip_pd(internal_pd))
        z_shock <- qnorm(clip_pd(pd_shock))
        z_baseline <- qnorm(clip_pd(pd_baseline))
        pnorm(z_internal + z_shock - z_baseline)
      }
    )

    # Floor at 0, cap at 1
    adjusted_pd <- pmin(pmax(adjusted_pd, 0), 1)

    # Build result dataframe
    id_cols <- intersect(c("company_id", "company_name", "sector", "technology",
                           "exposure_value_usd"), names(df))
    result_df <- df[, id_cols, drop = FALSE]
    result_df$Internal_PD <- smart_round(internal_pd)
    result_df$PD_Baseline <- smart_round(pd_baseline)
    result_df$PD_Shock <- smart_round(pd_shock)
    result_df$PD_Change <- smart_round(pd_change)
    result_df$PD_Change_Pct <- smart_round(pd_change_pct * 100)
    result_df$TRISK_Adjusted_PD <- smart_round(adjusted_pd)
    result_df$PD_Adjustment <- smart_round(adjusted_pd - internal_pd)

    rv$pd_integration_result <- result_df

    showNotification(
      paste("PD integration complete using", switch(method,
        "absolute" = "Absolute Change",
        "relative" = "Relative Change",
        "zscore" = "Z-Score (Basel IRB)"
      ), "method."),
      type = "message", duration = 5
    )
  })

  # PD integration results table (adaptive color coding + friendly column names)
  output$pd_integration_results <- renderDT({
    req(rv$pd_integration_result)
    df <- rv$pd_integration_result

    # Rename columns in-place for cleaner display
    pd_renames <- c(
      "company_id"       = "Company ID",
      "company_name"     = "Company",
      "sector"           = "Sector",
      "technology"       = "Technology",
      "exposure_value_usd" = "Exposure (USD)",
      "Internal_PD"      = "Internal PD",
      "PD_Baseline"      = "TRISK PD Baseline",
      "PD_Shock"         = "TRISK PD Shock",
      "PD_Change"        = "PD Change",
      "PD_Change_Pct"    = "PD Change %",
      "TRISK_Adjusted_PD"= "Adjusted PD",
      "PD_Adjustment"    = "PD Adjustment"
    )
    display_df <- df
    for (old_name in names(pd_renames)) {
      if (old_name %in% names(display_df)) {
        names(display_df)[names(display_df) == old_name] <- pd_renames[old_name]
      }
    }

    # Data-adaptive breakpoints for PD Adjustment
    pd_adj_vals <- display_df[["PD Adjustment"]][!is.na(display_df[["PD Adjustment"]])]
    pd_adj_max <- if (length(pd_adj_vals) > 0) max(abs(pd_adj_vals)) else 0.01
    if (pd_adj_max == 0) pd_adj_max <- 0.01

    brks_pd <- c(-pd_adj_max, -pd_adj_max * 0.5, -pd_adj_max * 0.1,
                  pd_adj_max * 0.1, pd_adj_max * 0.5, pd_adj_max)
    clrs_pd <- c("#1B4332", "#2D6A2E", "#5DAA5E", "#7AADCA",
                 "#C45555", "#8B2020", "#5C0A0A")

    num_cols <- which(sapply(display_df, is.numeric)) - 1

    dt <- datatable(
      display_df,
      options = list(pageLength = 20, scrollX = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = num_cols))),
      rownames = FALSE
    ) %>%
      formatStyle("Adjusted PD",
                  backgroundColor = "#FFFDE7",
                  fontWeight = "bold") %>%
      formatStyle("PD Adjustment",
                  backgroundColor = styleInterval(brks_pd, clrs_pd),
                  color = "white", fontWeight = "bold")
    dt
  })

  # Portfolio-level PD integration aggregates (ValueBox-style cards above detail table)
  output$pd_integration_portfolio_summary <- renderUI({
    req(rv$pd_integration_result)
    df <- rv$pd_integration_result

    exp_col <- if ("exposure_value_usd" %in% names(df)) "exposure_value_usd" else NULL
    total_exp <- if (!is.null(exp_col)) sum(df[[exp_col]], na.rm = TRUE) else 0

    if (!is.null(exp_col) && total_exp > 0) {
      w_internal_pd <- sum(df$Internal_PD * df[[exp_col]], na.rm = TRUE) / total_exp
      w_adjusted_pd <- sum(df$TRISK_Adjusted_PD * df[[exp_col]], na.rm = TRUE) / total_exp
      w_pd_adjustment <- w_adjusted_pd - w_internal_pd
    } else {
      w_internal_pd <- mean(df$Internal_PD, na.rm = TRUE)
      w_adjusted_pd <- mean(df$TRISK_Adjusted_PD, na.rm = TRUE)
      w_pd_adjustment <- w_adjusted_pd - w_internal_pd
    }

    adj_pct <- if (w_internal_pd != 0) {
      raw_pct <- w_pd_adjustment / w_internal_pd * 100
      # Cap display at 9999% to avoid overflow in ValueBox
      if (abs(raw_pct) > 9999) {
        paste0(" (>", if (raw_pct > 0) "+" else "-", "9999%)")
      } else {
        paste0(" (", round(raw_pct, 1), "%)")
      }
    } else ""

    # Color: red if PD increased, green if decreased, grey if neutral
    adj_color <- if (w_pd_adjustment > 0.0001) "red" else if (w_pd_adjustment < -0.0001) "green" else "navy"

    tagList(
      h4(icon("chart-bar"), "Portfolio-Level PD Aggregates",
         style = "margin-top: 15px; margin-bottom: 10px; font-weight: 600;"),
      fluidRow(
        valueBox(
          value = paste0(smart_round(w_internal_pd * 100), "%"),
          subtitle = "Wtd Avg Internal PD",
          icon = icon("shield-halved"),
          color = "navy",
          width = 3
        ),
        valueBox(
          value = paste0(smart_round(w_adjusted_pd * 100), "%"),
          subtitle = "Wtd Avg TRISK-Adjusted PD",
          icon = icon("shield-virus"),
          color = adj_color,
          width = 3
        ),
        valueBox(
          value = paste0(smart_round(w_pd_adjustment * 100), " pp", adj_pct),
          subtitle = "Weighted Average PD Adjustment",
          icon = icon("arrow-trend-up"),
          color = adj_color,
          width = 3
        ),
        valueBox(
          value = format_number(total_exp),
          subtitle = "Total Exposure",
          icon = icon("coins"),
          color = "navy",
          width = 3
        )
      ),
      hr()
    )
  })

  # ============================================
  # INTEGRATION PAGE - EL Integration
  # ============================================

  output$el_method_description <- renderUI({
    method <- input$el_integration_method
    desc <- switch(method,
      "absolute" = tagList(
        tags$b("Absolute Change Application"),
        tags$p("TRISK-adjusted EL = Internal_EL + (EL_shock - EL_baseline)"),
        tags$p("Adds the raw EL change from TRISK to your internal Expected Loss estimate.")
      ),
      "relative" = tagList(
        tags$b("Relative Change Application"),
        tags$p("TRISK-adjusted EL = Internal_EL * (1 + EL_change%)"),
        tags$p("Scales your internal EL by the relative percentage shift from the TRISK model.")
      )
    )
    desc
  })

  # EL input table (editable Internal_EL column)
  output$el_integration_table <- renderDT({
    req(rv$results)
    df <- rv$results

    id_cols <- intersect(c("company_id", "company_name", "sector", "technology"), names(df))
    el_cols <- intersect(c("exposure_value_usd", "loss_given_default",
                           "expected_loss_baseline", "expected_loss_shock"), names(df))
    show_cols <- c(id_cols, el_cols)

    display_df <- df[, show_cols, drop = FALSE]

    if (all(c("expected_loss_baseline", "expected_loss_shock") %in% names(display_df))) {
      display_df$EL_Change <- smart_round(display_df$expected_loss_shock - display_df$expected_loss_baseline)
      display_df$EL_Change_Pct <- smart_round(
        ifelse(display_df$expected_loss_baseline != 0,
               (display_df$expected_loss_shock - display_df$expected_loss_baseline) /
                 display_df$expected_loss_baseline * 100,
               NA_real_))
    }

    # Editable Internal_EL column:
    # Priority: (1) user-uploaded/edited values in rv$internal_el, (2) expected_loss_baseline, (3) zero
    if (!is.null(rv$internal_el) && length(rv$internal_el) == nrow(df)) {
      display_df$Internal_EL <- smart_round(rv$internal_el)
    } else if ("expected_loss_baseline" %in% names(df)) {
      display_df$Internal_EL <- smart_round(df$expected_loss_baseline)
    } else {
      display_df$Internal_EL <- 0
    }

    datatable(
      display_df,
      editable = list(target = "cell", disable = list(columns = seq_len(ncol(display_df) - 1) - 1)),
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    )
  })

  # Observe edits to Internal_EL
  observeEvent(input$el_integration_table_cell_edit, {
    info <- input$el_integration_table_cell_edit
    req(rv$results)

    if (is.null(rv$internal_el)) {
      if ("expected_loss_baseline" %in% names(rv$results)) {
        rv$internal_el <- rv$results$expected_loss_baseline
      } else {
        rv$internal_el <- rep(0, nrow(rv$results))
      }
    }

    row_idx <- info$row
    new_val <- as.numeric(info$value)
    if (!is.na(new_val) && row_idx >= 1 && row_idx <= length(rv$internal_el)) {
      rv$internal_el[row_idx] <- new_val
    }
  })

  # Upload Internal ELs from CSV
  observeEvent(input$upload_internal_el, {
    req(input$upload_internal_el, rv$results)
    tryCatch({
      uploaded <- read.csv(input$upload_internal_el$datapath, stringsAsFactors = FALSE)
      names(uploaded) <- tolower(names(uploaded))

      # Schema + value validation
      validation_err <- validate_internal_el_csv(uploaded)
      if (!is.null(validation_err)) {
        showNotification(paste("Validation error:", validation_err),
                        type = "error", duration = 8)
        return()
      }

      df <- rv$results
      internal_el <- if ("expected_loss_baseline" %in% names(df)) df$expected_loss_baseline else rep(0, nrow(df))

      matched <- 0
      for (i in seq_len(nrow(df))) {
        cid <- as.character(df$company_id[i])
        idx <- which(as.character(uploaded$company_id) == cid)
        if (length(idx) > 0) {
          val <- as.numeric(uploaded$internal_el[idx[1]])
          if (!is.na(val)) {
            internal_el[i] <- val
            matched <- matched + 1
          }
        }
      }

      rv$internal_el <- internal_el
      showNotification(
        paste0("Internal ELs loaded: ", matched, " of ", nrow(df), " counterparties matched."),
        type = if (matched > 0) "message" else "warning", duration = 6
      )
    }, error = function(e) {
      showNotification(paste("Error reading CSV:", e$message), type = "error", duration = 8)
    })
  })

  # Internal EL upload status
  output$internal_el_upload_status <- renderUI({
    if (!is.null(rv$internal_el)) {
      # Safety: if results row count changed, warn and reset
      if (!is.null(rv$results) && length(rv$internal_el) != nrow(rv$results)) {
        rv$internal_el <- NULL
        return(tags$span(class = "text-warning", icon("triangle-exclamation"),
                        "Internal ELs reset (portfolio size changed)"))
      }
      n_custom <- if (!is.null(rv$results) && "expected_loss_baseline" %in% names(rv$results)) {
        sum(rv$internal_el != rv$results$expected_loss_baseline, na.rm = TRUE)
      } else 0
      if (n_custom > 0) {
        tagList(
          tags$span(class = "text-success", icon("check-circle"),
                   paste0(n_custom, " custom EL values loaded")),
          if (length(rv$run_history) > 0)
            tags$br(), tags$span(class = "text-muted", style = "font-size: 11px;",
                                icon("clock-rotate-left"), " Preserved from previous run")
        )
      } else {
        tags$span(class = "text-muted", icon("info-circle"),
                 "Using expected_loss_baseline as default")
      }
    }
  })

  # Apply EL integration
  observeEvent(input$apply_el_integration, {
    req(rv$results)
    df <- rv$results
    method <- input$el_integration_method

    internal_el <- if (!is.null(rv$internal_el)) {
      rv$internal_el
    } else if ("expected_loss_baseline" %in% names(df)) {
      df$expected_loss_baseline
    } else {
      rep(0, nrow(df))
    }

    el_baseline <- if ("expected_loss_baseline" %in% names(df)) df$expected_loss_baseline else rep(0, nrow(df))
    el_shock <- if ("expected_loss_shock" %in% names(df)) df$expected_loss_shock else rep(0, nrow(df))

    el_change <- el_shock - el_baseline
    el_change_pct <- ifelse(el_baseline != 0, el_change / el_baseline, 0)

    adjusted_el <- switch(method,
      "absolute" = {
        internal_el + el_change
      },
      "relative" = {
        internal_el * (1 + el_change_pct)
      }
    )

    id_cols <- intersect(c("company_id", "company_name", "sector", "technology",
                           "exposure_value_usd"), names(df))
    result_df <- df[, id_cols, drop = FALSE]
    result_df$Internal_EL <- smart_round(internal_el)
    result_df$EL_Baseline <- smart_round(el_baseline)
    result_df$EL_Shock <- smart_round(el_shock)
    result_df$EL_Change <- smart_round(el_change)
    result_df$EL_Change_Pct <- smart_round(el_change_pct * 100)
    result_df$TRISK_Adjusted_EL <- smart_round(adjusted_el)
    result_df$EL_Adjustment <- smart_round(adjusted_el - internal_el)

    rv$el_integration_result <- result_df

    showNotification(
      paste("EL integration complete using", switch(method,
        "absolute" = "Absolute Change",
        "relative" = "Relative Change"
      ), "method."),
      type = "message", duration = 5
    )
  })

  # EL integration results table (adaptive color coding + friendly column names)
  output$el_integration_results <- renderDT({
    req(rv$el_integration_result)
    df <- rv$el_integration_result

    # Rename columns in-place for cleaner display
    col_renames <- c(
      "company_id"       = "Company ID",
      "company_name"     = "Company",
      "sector"           = "Sector",
      "technology"       = "Technology",
      "exposure_value_usd" = "Exposure (USD)",
      "Internal_EL"      = "Internal EL",
      "EL_Baseline"      = "TRISK EL Baseline",
      "EL_Shock"         = "TRISK EL Shock",
      "EL_Change"        = "EL Change",
      "EL_Change_Pct"    = "EL Change %",
      "TRISK_Adjusted_EL"= "Adjusted EL",
      "EL_Adjustment"    = "EL Adjustment"
    )
    display_df <- df
    for (old_name in names(col_renames)) {
      if (old_name %in% names(display_df)) {
        names(display_df)[names(display_df) == old_name] <- col_renames[old_name]
      }
    }

    # Data-adaptive breakpoints for EL Adjustment
    el_adj_vals <- display_df[["EL Adjustment"]][!is.na(display_df[["EL Adjustment"]])]
    el_adj_max <- if (length(el_adj_vals) > 0) max(abs(el_adj_vals)) else 1
    if (el_adj_max == 0) el_adj_max <- 1

    brks_adj <- c(-el_adj_max, -el_adj_max * 0.5, -el_adj_max * 0.1,
                   el_adj_max * 0.1, el_adj_max * 0.5, el_adj_max)
    clrs_adj <- c("#1B4332", "#2D6A2E", "#5DAA5E", "#7AADCA",
                  "#C45555", "#8B2020", "#5C0A0A")

    num_cols <- which(sapply(display_df, is.numeric)) - 1

    dt <- datatable(
      display_df,
      options = list(pageLength = 20, scrollX = TRUE,
                     columnDefs = list(list(className = "dt-right", targets = num_cols))),
      rownames = FALSE
    ) %>%
      formatStyle("Adjusted EL",
                  backgroundColor = "#FFFDE7",
                  fontWeight = "bold") %>%
      formatStyle("EL Adjustment",
                  backgroundColor = styleInterval(brks_adj, clrs_adj),
                  color = "white", fontWeight = "bold")

    # Also color-code EL Change % if present
    if ("EL Change %" %in% names(display_df)) {
      dt <- dt %>%
        formatStyle("EL Change %",
                    color = styleInterval(c(0), c("#2D6A2E", "#8B2020")),
                    fontWeight = "bold")
    }

    dt
  })

  # Portfolio-level EL integration aggregates (ValueBox-style cards above detail table)
  output$el_integration_portfolio_summary <- renderUI({
    req(rv$el_integration_result)
    df <- rv$el_integration_result

    total_internal_el <- sum(df$Internal_EL, na.rm = TRUE)
    total_adjusted_el <- sum(df$TRISK_Adjusted_EL, na.rm = TRUE)
    total_el_adjustment <- total_adjusted_el - total_internal_el
    total_el_adj_pct <- if (total_internal_el != 0) {
      total_el_adjustment / total_internal_el * 100
    } else NA_real_

    exp_col <- if ("exposure_value_usd" %in% names(df)) "exposure_value_usd" else NULL
    total_exp <- if (!is.null(exp_col)) sum(df[[exp_col]], na.rm = TRUE) else 0

    # EL as basis points of exposure (key regulatory metric)
    el_bps <- if (total_exp > 0) {
      abs(total_adjusted_el) / total_exp * 10000  # convert to bps
    } else NA_real_

    # EL values are negative, so more negative adjustment = worse
    adj_color <- if (total_el_adjustment < -0.01) "red" else if (total_el_adjustment > 0.01) "green" else "navy"

    el_adj_pct_str <- if (!is.na(total_el_adj_pct)) {
      if (abs(total_el_adj_pct) > 9999) {
        paste0(" (>", if (total_el_adj_pct > 0) "+" else "-", "9999%)")
      } else {
        paste0(" (", round(total_el_adj_pct, 1), "%)")
      }
    } else ""

    el_adj_label <- paste0(format_number(total_el_adjustment), el_adj_pct_str)

    bps_label <- if (!is.na(el_bps)) paste0(round(el_bps, 1), " bps") else "N/A"

    tagList(
      h4(icon("chart-bar"), "Portfolio-Level EL Aggregates",
         style = "margin-top: 15px; margin-bottom: 10px; font-weight: 600;"),
      fluidRow(
        valueBox(
          value = format_number(total_internal_el),
          subtitle = "Total Internal EL",
          icon = icon("landmark"),
          color = "navy",
          width = 3
        ),
        valueBox(
          value = format_number(total_adjusted_el),
          subtitle = "Total TRISK-Adjusted EL",
          icon = icon("scale-unbalanced"),
          color = adj_color,
          width = 3
        ),
        valueBox(
          value = el_adj_label,
          subtitle = "Total EL Adjustment",
          icon = icon("arrow-trend-up"),
          color = adj_color,
          width = 3
        ),
        valueBox(
          value = format_number(total_exp),
          subtitle = "Total Exposure",
          icon = icon("coins"),
          color = "navy",
          width = 3
        )
      ),
      # Second row: EL as bps of exposure
      fluidRow(
        valueBox(
          value = tags$span(bps_label,
                            title = "Adjusted EL as basis points of total exposure (|EL| / Exposure * 10,000)",
                            style = "cursor: help;"),
          subtitle = "Adjusted EL / Exposure",
          icon = icon("percent"),
          color = if (!is.na(el_bps) && el_bps > 50) "red" else if (!is.na(el_bps) && el_bps > 20) "yellow" else "green",
          width = 3
        )
      ),
      hr()
    )
  })

  # Sector/Technology EL Breakdown (collapsible summary table)
  output$el_sector_breakdown <- renderUI({
    req(rv$el_integration_result)
    df <- rv$el_integration_result

    # Check for grouping columns
    grp_cols <- intersect(c("sector", "technology"), names(df))
    if (length(grp_cols) == 0) return(NULL)

    # Aggregate by sector (or sector+technology if both present)
    grp_col <- grp_cols[1]  # primary grouping by sector

    sectors <- unique(df[[grp_col]])
    breakdown_rows <- lapply(sectors, function(s) {
      sub_df <- df[df[[grp_col]] == s, , drop = FALSE]
      n <- nrow(sub_df)
      int_el <- sum(sub_df$Internal_EL, na.rm = TRUE)
      adj_el <- sum(sub_df$TRISK_Adjusted_EL, na.rm = TRUE)
      el_adj <- adj_el - int_el
      exp_val <- if ("exposure_value_usd" %in% names(sub_df)) sum(sub_df$exposure_value_usd, na.rm = TRUE) else 0
      el_bps <- if (exp_val > 0) abs(adj_el) / exp_val * 10000 else NA_real_

      # Direction indicator
      dir_icon <- if (el_adj < -0.01) {
        tags$span(style = "color: #C44245;", icon("arrow-down"), " ")
      } else if (el_adj > 0.01) {
        tags$span(style = "color: #6B9F3B;", icon("arrow-up"), " ")
      } else {
        tags$span(style = "color: #666;", icon("minus"), " ")
      }

      tags$tr(
        tags$td(style = "padding: 6px 12px; font-weight: 600;", s),
        tags$td(style = "padding: 6px 12px; text-align: right;", n),
        tags$td(style = "padding: 6px 12px; text-align: right;", format_number(exp_val)),
        tags$td(style = "padding: 6px 12px; text-align: right;", format_number(int_el)),
        tags$td(style = "padding: 6px 12px; text-align: right;", format_number(adj_el)),
        tags$td(style = "padding: 6px 12px; text-align: right;", dir_icon, format_number(el_adj)),
        tags$td(style = "padding: 6px 12px; text-align: right;",
                if (!is.na(el_bps)) paste0(round(el_bps, 1), " bps") else "N/A")
      )
    })

    tagList(
      tags$div(
        style = "margin-bottom: 15px;",
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-weight: 600; font-size: 15px; padding: 8px 0; color: #1A1A1A;",
            icon("layer-group"), " EL Breakdown by Sector"
          ),
          tags$div(
            style = "margin-top: 8px; overflow-x: auto;",
            tags$table(
              class = "table table-striped table-hover",
              style = "width: 100%; font-size: 13px; border-collapse: collapse;",
              tags$thead(
                tags$tr(
                  style = "background: #F0E6EA; border-bottom: 2px solid #DDD0D4;",
                  tags$th(style = "padding: 8px 12px;", "Sector"),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "Count"),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "Exposure"),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "Internal EL"),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "Adjusted EL"),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "EL Adj."),
                  tags$th(style = "padding: 8px 12px; text-align: right;", "EL/Exposure")
                )
              ),
              tags$tbody(breakdown_rows)
            )
          )
        )
      )
    )
  })

  # EL Adjustment Bar Chart (plotly) — visual impact by sector or counterparty
  output$el_adjustment_chart <- renderPlotly({
    req(rv$el_integration_result)
    df <- rv$el_integration_result

    # Determine grouping: use sector if available, otherwise counterparty
    has_sector <- "sector" %in% names(df)
    has_company <- "company_name" %in% names(df)

    if (has_sector) {
      # Aggregate by sector
      chart_df <- df %>%
        group_by(sector) %>%
        summarise(
          EL_Adjustment = sum(EL_Adjustment, na.rm = TRUE),
          Exposure = if ("exposure_value_usd" %in% names(df)) sum(exposure_value_usd, na.rm = TRUE) else 0,
          n = n(),
          .groups = "drop"
        ) %>%
        arrange(EL_Adjustment)

      chart_df$color <- ifelse(chart_df$EL_Adjustment < 0, BRAND_CORAL, STATUS_GREEN)
      chart_df$hover_text <- paste0(
        "<b>", chart_df$sector, "</b><br>",
        "EL Adjustment: ", sapply(chart_df$EL_Adjustment, format_number), "<br>",
        "Exposure: ", sapply(chart_df$Exposure, format_number), "<br>",
        "Counterparties: ", chart_df$n
      )

      p <- plot_ly(chart_df,
                   y = ~reorder(sector, EL_Adjustment),
                   x = ~EL_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color),
                   text = ~hover_text,
                   hoverinfo = "text") %>%
        layout(
          title = list(text = "EL Adjustment by Sector", font = list(size = 14)),
          xaxis = list(title = "EL Adjustment", zeroline = TRUE,
                       zerolinecolor = "#999", zerolinewidth = 1.5),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          margin = list(l = 120, r = 20, t = 40, b = 40),
          showlegend = FALSE
        )
    } else if (has_company) {
      # Show by individual counterparty (top 15 by absolute EL adjustment)
      chart_df <- df %>%
        arrange(desc(abs(EL_Adjustment))) %>%
        head(15) %>%
        arrange(EL_Adjustment)

      chart_df$color <- ifelse(chart_df$EL_Adjustment < 0, BRAND_CORAL, STATUS_GREEN)
      chart_df$hover_text <- paste0(
        "<b>", chart_df$company_name, "</b><br>",
        "EL Adjustment: ", sapply(chart_df$EL_Adjustment, format_number)
      )

      p <- plot_ly(chart_df,
                   y = ~reorder(company_name, EL_Adjustment),
                   x = ~EL_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color),
                   text = ~hover_text,
                   hoverinfo = "text") %>%
        layout(
          title = list(text = "Top 15 Counterparties by EL Adjustment", font = list(size = 14)),
          xaxis = list(title = "EL Adjustment", zeroline = TRUE,
                       zerolinecolor = "#999", zerolinewidth = 1.5),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          margin = list(l = 150, r = 20, t = 40, b = 40),
          showlegend = FALSE
        )
    } else {
      # Fallback: show all counterparties by index
      chart_df <- df %>%
        mutate(label = paste0("Row ", row_number())) %>%
        arrange(EL_Adjustment)

      chart_df$color <- ifelse(chart_df$EL_Adjustment < 0, BRAND_CORAL, STATUS_GREEN)

      p <- plot_ly(chart_df,
                   y = ~reorder(label, EL_Adjustment),
                   x = ~EL_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color)) %>%
        layout(
          title = list(text = "EL Adjustment by Counterparty", font = list(size = 14)),
          xaxis = list(title = "EL Adjustment"),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          showlegend = FALSE
        )
    }

    p
  })

  # ============================================
  # Downloads
  # ============================================

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".xlsx")
    },
    content = function(file) {
      req(rv$results)

      df <- rv$results
      grp_cols <- intersect(c("sector", "technology"), names(df))

      summary_df <- if (length(grp_cols) > 0) {
        tryCatch({
          df %>%
            group_by(across(all_of(grp_cols))) %>%
            summarise(n_rows = n(), .groups = "drop")
        }, error = function(e) df)
      } else {
        df
      }

      sheets <- list(
        "Summary" = summary_df,
        "Full Results" = df
      )

      # Add multi-horizon results if available
      if (!is.null(rv$results_by_year) && length(rv$results_by_year) > 1) {
        sheets[["Multi-Horizon"]] <- bind_rows(rv$results_by_year, .id = "shock_year")
      }

      # Add multi-scenario comparison if available
      if (!is.null(rv$results_by_scenario) && length(rv$results_by_scenario) > 1) {
        all_scen_rows <- list()
        for (scen_code in names(rv$results_by_scenario)) {
          for (yr_key in names(rv$results_by_scenario[[scen_code]])) {
            df_s <- rv$results_by_scenario[[scen_code]][[yr_key]]
            df_s$target_scenario <- scen_code
            df_s$shock_year <- as.integer(yr_key)
            all_scen_rows[[paste0(scen_code, "_", yr_key)]] <- df_s
          }
        }
        sheets[["Scenario Comparison"]] <- bind_rows(all_scen_rows)
      }

      # Add integration results if available
      if (!is.null(rv$pd_integration_result)) {
        sheets[["PD Integration"]] <- rv$pd_integration_result
      }
      if (!is.null(rv$el_integration_result)) {
        sheets[["EL Integration"]] <- rv$el_integration_result
      }

      # Guard against OOM: writexl builds the entire workbook in memory.
      # For large exports (>100K rows), write CSV instead which streams to disk.
      total_rows <- sum(vapply(sheets, nrow, integer(1)))
      if (total_rows > 100000) {
        showNotification(
          paste0("Large export (", format(total_rows, big.mark = ","),
                 " rows) — writing CSV instead of Excel to avoid memory issues."),
          type = "warning", duration = 8)
        data.table::fwrite(sheets[["Full Results"]], file)
      } else {
        writexl::write_xlsx(sheets, file)
      }
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".csv")
    },
    content = function(file) {
      req(rv$results)
      write_csv(rv$results, file)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".json")
    },
    content = function(file) {
      req(rv$results)
      jsonlite::write_json(rv$results, file, pretty = TRUE)
    }
  )

  output$download_config <- downloadHandler(
    filename = function() {
      paste0("trisk_config_", rv$run_id, ".json")
    },
    content = function(file) {
      config <- list(
        run_id = rv$run_id,
        timestamp = as.character(Sys.time()),
        parameters = list(
          baseline_scenarios = paste(input$baseline_scenario, collapse = ", "),
          target_scenarios = paste(input$target_scenarios, collapse = ", "),
          scenario_geography = input$scenario_geography,
          shock_years = paste(input$shock_years, collapse = ", "),
          risk_free_rate = input$risk_free_rate,
          discount_rate = input$discount_rate,
          growth_rate = input$growth_rate,
          market_passthrough = input$market_passthrough,
          carbon_price_model = input$carbon_price_model
        ),
        data_summary = list(
          portfolio_rows = nrow(rv$portfolio),
          assets_rows = nrow(rv$assets),
          financial_rows = nrow(rv$financial)
        ),
        versions = list(
          trisk_model = as.character(packageVersion("trisk.model")),
          trisk_analysis = as.character(packageVersion("trisk.analysis")),
          R = R.version$version.string
        )
      )
      jsonlite::write_json(config, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  # ============================================
  # Metadata
  # ============================================

  output$metadata_summary <- renderText({
    if (is.null(rv$run_id)) {
      "No analysis run yet."
    } else {
      paste(
        "Run ID:", rv$run_id,
        "\nTimestamp:", as.character(Sys.time()),
        "\nBaseline(s):", paste(input$baseline_scenario, collapse = ", "),
        "\nTarget(s):", paste(input$target_scenarios, collapse = ", "),
        "\nGeography:", input$scenario_geography,
        "\nResults rows:", if (!is.null(rv$results)) as.character(nrow(rv$results)) else "N/A"
      )
    }
  })

  output$version_info <- renderText({
    paste(
      "trisk.model:", as.character(packageVersion("trisk.model")),
      "\ntrisk.analysis:", as.character(packageVersion("trisk.analysis")),
      "\nR:", R.version$version.string,
      "\nShiny:", as.character(packageVersion("shiny")),
      "\nContainer build:", Sys.getenv("BUILD_DATE", "unknown")
    )
  })
}
