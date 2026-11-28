# mod_integration.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_integration <- function(input, output, session, rv) {

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
      file_check <- validate_file_type(input$upload_internal_el$datapath, input$upload_internal_el$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      uploaded <- read.csv(input$upload_internal_el$datapath, stringsAsFactors = FALSE)
      if (nrow(uploaded) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
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

      uploaded_map <- stats::setNames(
        as.numeric(uploaded$internal_el),
        as.character(uploaded$company_id)
      )
      result_ids <- as.character(df$company_id)
      matched_vals <- uploaded_map[result_ids]
      has_match <- !is.na(matched_vals)
      internal_el[has_match] <- matched_vals[has_match]
      matched <- sum(has_match)

      rv$internal_el <- internal_el
      showNotification(
        paste0("Internal ELs loaded: ", matched, " of ", nrow(df), " counterparties matched."),
        type = if (matched > 0) "message" else "warning", duration = 6
      )
    }, error = function(e) {
      message(paste("ERROR (EL CSV read):", conditionMessage(e)))
      showNotification("Could not read the EL CSV file. Please check the file format.", type = "error", duration = 8)
    })
  })

  # Reset stale internal EL when results row count changes
  observe({
    req(rv$results, rv$internal_el)
    if (length(rv$internal_el) != nrow(rv$results)) {
      rv$internal_el <- NULL
    }
  })

  # Internal EL upload status
  output$internal_el_upload_status <- renderUI({
    if (!is.null(rv$internal_el)) {
      if (!is.null(rv$results) && length(rv$internal_el) != nrow(rv$results)) {
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
    clrs_adj <- c("#1B4332", "#2D6A2E", "#5DAA5E", "#BAB6B5",
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
        "<b>", htmltools::htmlEscape(chart_df$sector), "</b><br>",
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
        "<b>", htmltools::htmlEscape(chart_df$company_name), "</b><br>",
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
      audit_log("export", list(format = "xlsx", run_id = rv$run_id, rows = nrow(rv$results)))

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

      # Sanitize all sheets to export-safe columns
      sheets <- lapply(sheets, sanitize_export)

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
      audit_log("export", list(format = "csv", run_id = rv$run_id, rows = nrow(rv$results)))
      write_csv(sanitize_export(rv$results), file)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".json")
    },
    content = function(file) {
      req(rv$results)
      audit_log("export", list(format = "json", run_id = rv$run_id, rows = nrow(rv$results)))
      jsonlite::write_json(sanitize_export(rv$results), file, pretty = TRUE)
    }
  )

  output$download_config <- downloadHandler(
    filename = function() {
      paste0("trisk_config_", rv$run_id, ".json")
    },
    content = function(file) {
      audit_log("export", list(format = "config_json", run_id = rv$run_id))
      config <- list(
        run_id = rv$run_id,
        timestamp = as.character(Sys.time()),
        parameters = list(
          baseline_scenario = input$baseline_scenario %||% "",
          target_scenario = input$target_scenario %||% "",
          scenario_pairs = lapply(rv$scenario_groups, function(key) {
            entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
            if (key == "core") {
              bl <- input$baseline_scenario; tgt <- input$target_scenario
            } else {
              bl <- input[[paste0("baseline_scenario_", key)]]
              tgt <- input[[paste0("target_scenario_", key)]]
            }
            list(group = key, label = entry$label,
                 baseline = bl %||% "", target = tgt %||% "",
                 sectors = entry$sectors)
          }),
          scenario_geography = input$scenario_geography,
          shock_years = paste(input$shock_years, collapse = ", "),
          risk_free_rate = input$risk_free_rate,
          discount_rate = input$discount_rate,
          growth_rate = input$growth_rate,
          market_passthrough_power = input$market_passthrough_power,
          market_passthrough_primary = input$market_passthrough_primary,
          carbon_price_model = input$carbon_price_model
        ),
        data_summary = list(
          portfolio_rows = nrow(rv$portfolio),
          assets_rows = nrow(rv$assets),
          financial_rows = nrow(rv$financial)
        ),
        versions = list(
          trisk_model = as.character(packageVersion("trisk.model")),
          trisk_analysis = as.character(packageVersion("trisk.analysis"))
        )
      )
      jsonlite::write_json(config, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  # ============================================

}
