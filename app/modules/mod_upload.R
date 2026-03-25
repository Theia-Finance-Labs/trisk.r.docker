# mod_upload.R
# Data Upload: file handlers, validation, previews, audits, templates, error reports

setup_upload <- function(input, output, session, rv, log_message) {

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
      showNotification("Error loading mock data. Check log for details.", type = "error")
      message("[ERROR] Mock data load failed: ", e$message)
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
      df <- read_csv(input$portfolio_file$datapath, show_col_types = FALSE)
      err <- validate_portfolio(df)
      if (!is.null(err)) {
        showNotification(paste("Portfolio rejected:", err), type = "error", duration = 8)
        return()
      }
      if (nrow(df) > 100000) {
        showNotification("Portfolio exceeds 100,000 row limit. Please reduce the file size.", type = "error")
        return()
      }
      rv$portfolio <- df
      showNotification(paste("Portfolio loaded:", nrow(df), "rows"), type = "message")
    }, error = function(e) {
      showNotification("Error loading portfolio. Check the file format and try again.", type = "error")
      message("[ERROR] Portfolio load failed: ", e$message)
    })
  })

  observeEvent(input$assets_file, {
    req(input$assets_file)
    tryCatch({
      df <- read_csv(input$assets_file$datapath, show_col_types = FALSE)
      err <- validate_assets(df)
      if (!is.null(err)) {
        showNotification(paste("Assets rejected:", err), type = "error", duration = 8)
        return()
      }
      if (nrow(df) > 1000000) {
        showNotification("Assets exceeds 1,000,000 row limit. Please reduce the file size.", type = "error")
        return()
      }
      rv$assets <- df
      showNotification(paste("Assets loaded:", nrow(df), "rows"), type = "message")
    }, error = function(e) {
      showNotification("Error loading assets data. Ensure the CSV has the required columns.", type = "error")
      message("[ERROR] Assets load failed: ", e$message)
    })
  })

  observeEvent(input$financial_file, {
    req(input$financial_file)
    tryCatch({
      df <- read_csv(input$financial_file$datapath, show_col_types = FALSE)
      err <- validate_financial(df)
      if (!is.null(err)) {
        showNotification(paste("Financial data rejected:", err), type = "error", duration = 8)
        return()
      }
      if (nrow(df) > 100000) {
        showNotification("Financial data exceeds 100,000 row limit. Please reduce the file size.", type = "error")
        return()
      }
      rv$financial <- df
      showNotification(paste("Financial data loaded:", nrow(df), "rows"), type = "message")
    }, error = function(e) {
      showNotification("Error loading financial data. Check the file format and try again.", type = "error")
      message("[ERROR] Financial data load failed: ", e$message)
    })
  })

  observeEvent(input$scenarios_file, {
    req(input$scenarios_file)
    tryCatch({
      df <- read_csv(input$scenarios_file$datapath, show_col_types = FALSE)
      # Minimal schema check: must have 'scenario' column at minimum
      if (!"scenario" %in% names(df)) {
        showNotification("Scenarios rejected: missing required column 'scenario'", type = "error", duration = 8)
        return()
      }
      if (nrow(df) > 500000) {
        showNotification("Scenarios exceeds 500,000 row limit. Please reduce the file size.", type = "error")
        return()
      }
      rv$scenarios <- df
      showNotification(
        paste("Scenarios loaded:", length(unique(df$scenario)), "scenarios"),
        type = "message"
      )
    }, error = function(e) {
      showNotification("Error loading scenarios. Check the file format and try again.", type = "error")
      message("[ERROR] Scenarios load failed: ", e$message)
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
          if (audit$n_duplicate_id > 0) tags$span(class = "status-missing", audit$n_duplicate_id) else "0"
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
        tags$small(tags$b("Columns with missing values:"), class = "status-missing"),
        tags$ul(class = "audit-list", null_items)
      )
    }

    div(class = "audit-card",
      div(class = "mb-6", schema_badge),
      tags$table(class = "table table-condensed fs-12 mb-4",
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

}
