# mod_upload.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_upload <- function(input, output, session, rv, log_message) {

  # ============================================

  observeEvent(input$load_mock_data, {
    tryCatch({
      log_message("Loading mock data from trisk packages...")

      rv$assets <- load_mock_data("assets")
      if (!is.null(rv$assets)) log_message(paste("  Assets:", nrow(rv$assets), "rows"))

      rv$financial <- load_mock_data("financial")
      if (!is.null(rv$financial)) log_message(paste("  Financial:", nrow(rv$financial), "rows"))

      # Always load demo scenarios/carbon so demo data works as a cohesive package.
      # The demo portfolio + assets are designed to work with demo scenarios, not the
      # pre-bundled GCS scenarios. Users can still upload their own scenarios separately.
      rv$scenarios <- load_mock_data("scenarios")
      if (!is.null(rv$scenarios)) log_message(paste("  Scenarios:", nrow(rv$scenarios), "rows,", length(unique(rv$scenarios$scenario)), "scenarios"))

      rv$carbon <- load_mock_data("carbon")
      if (!is.null(rv$carbon)) log_message(paste("  Carbon:", nrow(rv$carbon), "rows"))

      rv$portfolio <- load_mock_data("portfolio")
      if (!is.null(rv$portfolio)) log_message(paste("  Portfolio:", nrow(rv$portfolio), "rows"))

      loaded <- sum(!sapply(list(rv$assets, rv$financial, rv$scenarios, rv$carbon, rv$portfolio), is.null))

      # Scenario dropdowns auto-update via the observe({req(rv$scenarios)}) block.

      if (loaded == 5) {
        showNotification("All demo data loaded successfully! Go to Configure.", type = "message", duration = 5)
        log_message("All mock data loaded successfully.")
        audit_log("upload", list(type = "mock_data", datasets_loaded = loaded))
      } else {
        showNotification(paste("Loaded", loaded, "of 5 datasets. Some may be missing."), type = "warning", duration = 5)
        log_message(paste("Warning: only", loaded, "of 5 datasets loaded."))
      }
    }, error = function(e) {
      message(paste("ERROR (mock data load):", conditionMessage(e)))
      showNotification("Could not load demo data. Please try again.", type = "error")
      log_message("ERROR loading mock data (see server log for details)")
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
  # File upload handlers (with security controls)
  # ============================================

  observeEvent(input$portfolio_file, {
    req(input$portfolio_file)
    tryCatch({
      file_check <- validate_file_type(input$portfolio_file$datapath, input$portfolio_file$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      df <- tibble::as_tibble(data.table::fread(input$portfolio_file$datapath))
      if (nrow(df) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
      rv$portfolio <- strip_columns(df, "portfolio")
      audit_log("upload", list(type = "portfolio", rows = nrow(rv$portfolio)))
      showNotification(paste("Portfolio loaded:", nrow(rv$portfolio), "rows"), type = "message")
    }, error = function(e) {
      message(paste("ERROR (portfolio upload):", conditionMessage(e)))
      showNotification("Could not parse the uploaded file. Please check the format.", type = "error")
    })
  })

  observeEvent(input$assets_file, {
    req(input$assets_file)
    tryCatch({
      file_check <- validate_file_type(input$assets_file$datapath, input$assets_file$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      df <- tibble::as_tibble(data.table::fread(input$assets_file$datapath))
      if (nrow(df) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
      rv$assets <- strip_columns(df, "assets")
      audit_log("upload", list(type = "assets", rows = nrow(rv$assets)))
      showNotification(paste("Assets loaded:", nrow(rv$assets), "rows"), type = "message")
    }, error = function(e) {
      message(paste("ERROR (assets upload):", conditionMessage(e)))
      showNotification("Could not parse the uploaded file. Please check the format.", type = "error")
    })
  })

  observeEvent(input$financial_file, {
    req(input$financial_file)
    tryCatch({
      file_check <- validate_file_type(input$financial_file$datapath, input$financial_file$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      df <- tibble::as_tibble(data.table::fread(input$financial_file$datapath))
      if (nrow(df) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
      rv$financial <- strip_columns(df, "financial")
      audit_log("upload", list(type = "financial", rows = nrow(rv$financial)))
      showNotification(paste("Financial data loaded:", nrow(rv$financial), "rows"), type = "message")
    }, error = function(e) {
      message(paste("ERROR (financial upload):", conditionMessage(e)))
      showNotification("Could not parse the uploaded file. Please check the format.", type = "error")
    })
  })

  observeEvent(input$scenarios_file, {
    req(input$scenarios_file)
    tryCatch({
      file_check <- validate_file_type(input$scenarios_file$datapath, input$scenarios_file$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      df <- tibble::as_tibble(data.table::fread(input$scenarios_file$datapath))
      if (nrow(df) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
      rv$scenarios <- strip_columns(df, "scenarios")
      audit_log("upload", list(type = "scenarios", rows = nrow(rv$scenarios)))
      showNotification(
        paste("Scenarios loaded:", length(unique(rv$scenarios$scenario)), "scenarios"),
        type = "message"
      )
    }, error = function(e) {
      message(paste("ERROR (scenarios upload):", conditionMessage(e)))
      showNotification("Could not parse the uploaded file. Please check the format.", type = "error")
    })
  })

  # ============================================
  # Sector detection — drives dynamic scenario UI
  # ============================================
  observeEvent(rv$portfolio, {
    port <- rv$portfolio
    if (is.null(port)) {
      rv$detected_sectors <- NULL
      rv$scenario_groups <- "core"
      return()
    }
    tryCatch({
      # Detect sectors from the portfolio's own sector column
      assets <- isolate(rv$assets)
      if ("sector" %in% names(port)) {
        sectors <- unique(port$sector)
      } else if (!is.null(assets) && "sector" %in% names(assets)) {
        # Fallback: derive sectors from assets data for portfolio companies
        portfolio_companies <- unique(port$company_id)
        sectors <- unique(assets$sector[assets$company_id %in% portfolio_companies])
      } else {
        sectors <- character(0)
      }
      rv$detected_sectors <- sectors
      rv$scenario_groups <- detect_scenario_groups(sectors)
      log_message(paste("Detected portfolio sectors:", paste(sectors, collapse = ", ")))
      log_message(paste("Active scenario groups:", paste(rv$scenario_groups, collapse = ", ")))
    }, error = function(e) {
      message(paste("ERROR (sector detection):", conditionMessage(e)))
    })
  }, ignoreNULL = FALSE)

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
  # Dynamic Upload Boxes (status changes on success)
  # ============================================

  output$portfolio_upload_box <- renderUI({
    loaded <- !is.null(rv$portfolio)
    box(
      title = "Portfolio Data",
      status = if (loaded) "success" else "primary",
      solidHeader = TRUE,
      width = 6,
      fileInput("portfolio_file", "Upload Portfolio CSV",
                accept = c(".csv", ".CSV"),
                placeholder = "No file selected"),
      helpText("Required columns: company_id, company_name, country_iso2,
               exposure_value_usd, term, loss_given_default"),
      uiOutput("portfolio_status"),
      uiOutput("portfolio_audit"),
      downloadLink("download_portfolio_template", "Download Template CSV",
                   class = "btn btn-xs btn-default mb-8"),
      div(class = "data-preview", DTOutput("portfolio_preview"))
    )
  })

  output$assets_upload_box <- renderUI({
    loaded <- !is.null(rv$assets)
    box(
      title = "Assets Data",
      status = if (loaded) "success" else "primary",
      solidHeader = TRUE,
      width = 6,
      fileInput("assets_file", "Upload Assets CSV",
                accept = c(".csv", ".CSV"),
                placeholder = "No file selected"),
      helpText("Company-level production and asset data with production trajectories"),
      uiOutput("assets_status"),
      uiOutput("assets_audit"),
      downloadLink("download_assets_template", "Download Template CSV",
                   class = "btn btn-xs btn-default mb-8"),
      div(class = "data-preview", DTOutput("assets_preview"))
    )
  })

  output$financial_upload_box <- renderUI({
    loaded <- !is.null(rv$financial)
    box(
      title = "Financial Features Data",
      status = if (loaded) "success" else "primary",
      solidHeader = TRUE,
      width = 6,
      fileInput("financial_file", "Upload Financial Features CSV",
                accept = c(".csv", ".CSV"),
                placeholder = "No file selected"),
      helpText("PD, net profit margin, debt/equity ratio, volatility per company"),
      uiOutput("financial_status"),
      uiOutput("financial_audit"),
      downloadLink("download_financial_template", "Download Template CSV",
                   class = "btn btn-xs btn-default mb-8"),
      div(class = "data-preview", DTOutput("financial_preview"))
    )
  })

  output$scenarios_upload_box <- renderUI({
    has_preloaded <- !is.null(rv$scenarios)
    box(
      title = "Scenarios Data",
      status = if (has_preloaded) "success" else "warning",
      solidHeader = TRUE,
      width = 6,
      if (has_preloaded) {
        tagList(
          tags$p(class = "status-ok",
            icon("check-circle"),
            strong(paste0(" ", length(unique(rv$scenarios$scenario)), " scenarios loaded"))
          ),
          tags$p(paste("Geographies:", paste(sort(unique(rv$scenarios$scenario_geography)), collapse = ", "))),
          hr(),
          tags$p("Or upload custom scenarios:"),
          fileInput("scenarios_file", NULL, accept = c(".csv", ".CSV"))
        )
      } else {
        tagList(
          tags$p(class = "status-missing",
            icon("exclamation-triangle"),
            strong(" No pre-loaded scenarios")
          ),
          fileInput("scenarios_file", "Upload Scenarios CSV",
                    accept = c(".csv", ".CSV")),
          helpText("Scenarios are pre-loaded at build time. Upload here only to override with custom data.")
        )
      }
    )
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

  # ============================================
  # Consolidated Data Quality Dashboard
  # ============================================
  output$data_quality_dashboard <- renderUI({
    datasets <- list(
      Portfolio = rv$portfolio,
      Assets   = rv$assets,
      Financial = rv$financial,
      Scenarios = rv$scenarios
    )
    loaded <- !sapply(datasets, is.null)
    if (!any(loaded)) return(NULL)

    auditable_types <- c("Portfolio" = "portfolio", "Assets" = "assets", "Financial" = "financial")
    total_rows <- 0L
    total_issues <- 0L
    ds_cards <- list()

    for (label in names(datasets)) {
      df <- datasets[[label]]
      if (is.null(df)) {
        ds_cards[[label]] <- tags$div(class = "dq-badge dq-badge--missing",
          icon("times-circle"),
          tags$span(class = "dq-badge-label", label),
          tags$span(class = "dq-badge-detail", "Not loaded")
        )
        next
      }

      n_rows <- nrow(df)
      total_rows <- total_rows + n_rows

      type_key <- auditable_types[label]
      n_issues <- 0L
      if (!is.na(type_key) && !is.null(type_key)) {
        audit <- audit_dataset(df, type_key)
        if (!is.null(audit)) n_issues <- nrow(audit$issues)
      }
      total_issues <- total_issues + n_issues

      badge_class <- if (n_issues == 0) "dq-badge--good"
                     else if (n_issues <= 10) "dq-badge--warning"
                     else "dq-badge--danger"
      badge_icon <- if (n_issues == 0) icon("check-circle")
                    else if (n_issues <= 10) icon("exclamation-triangle")
                    else icon("times-circle")

      detail_text <- paste0(format(n_rows, big.mark = ","), " rows")
      if (n_issues > 0) {
        detail_text <- paste0(detail_text, " \u00B7 ", n_issues, " issue", if (n_issues != 1) "s" else "")
      }

      ds_cards[[label]] <- tags$div(class = paste("dq-badge", badge_class),
        badge_icon,
        tags$span(class = "dq-badge-label", label),
        tags$span(class = "dq-badge-detail", detail_text)
      )
    }

    tagList(
      tags$div(class = "dq-dashboard",
        tags$div(class = "dq-summary-row",
          tags$div(class = "dq-stat",
            tags$span(class = "dq-stat-value", format(total_rows, big.mark = ",")),
            tags$span(class = "dq-stat-label", "Total Rows")
          ),
          tags$div(class = "dq-stat",
            tags$span(class = "dq-stat-value", sum(loaded)),
            tags$span(class = "dq-stat-label",
              paste0("of ", length(datasets), " Datasets"))
          ),
          tags$div(class = "dq-stat",
            tags$span(class = paste0("dq-stat-value",
              if (total_issues > 0) " status-missing" else " status-ok"),
              total_issues),
            tags$span(class = "dq-stat-label", "Issues Found")
          )
        ),
        tags$div(class = "dq-badges-row",
          ds_cards
        )
      )
    )
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

  # Reactive flag for conditionalPanel (must use renderText, not reactive, for output$)
  output$has_data_issues <- renderText({
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
    as.character(issues_count > 0)
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
      write.csv(sanitize_export(all_issues), file, row.names = FALSE)
    }
  )

}
