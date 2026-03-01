# mod_integration.R
# PD and EL Integration: internal PD/EL adjustment methods, editable tables, CSV upload

setup_integration <- function(input, output, session, rv) {
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
            tags$br(), tags$span(class = "text-muted fs-11",
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
      # NOTE: needs CSS class .portfolio-summary-heading { margin-top: 15px; margin-bottom: 10px; font-weight: 600; }
      h4(icon("chart-bar"), "Portfolio-Level PD Aggregates",
         class = "portfolio-summary-heading"),
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
            tags$br(), tags$span(class = "text-muted fs-11",
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
         class = "portfolio-summary-heading"),
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
                            class = "cursor-help"),
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
      # NOTE: needs CSS class .dir-danger { color: #C44245; } .dir-success { color: #6B9F3B; }
      dir_icon <- if (el_adj < -0.01) {
        tags$span(class = "text-danger", icon("arrow-down"), " ")
      } else if (el_adj > 0.01) {
        tags$span(class = "text-success", icon("arrow-up"), " ")
      } else {
        tags$span(class = "fg-secondary", icon("minus"), " ")
      }

      tags$tr(
        tags$td(class = "td-pad fw-600", s),
        tags$td(class = "td-right", n),
        tags$td(class = "td-right", format_number(exp_val)),
        tags$td(class = "td-right", format_number(int_el)),
        tags$td(class = "td-right", format_number(adj_el)),
        tags$td(class = "td-right", dir_icon, format_number(el_adj)),
        tags$td(class = "td-right",
                if (!is.na(el_bps)) paste0(round(el_bps, 1), " bps") else "N/A")
      )
    })

    # NOTE: needs CSS classes:
    # .breakdown-summary { cursor: pointer; font-weight: 600; font-size: 15px; padding: 8px 0; color: #1A1A1A; }
    # .breakdown-body { margin-top: 8px; overflow-x: auto; }
    # .breakdown-table { width: 100%; font-size: 13px; border-collapse: collapse; }
    # .breakdown-thead { background: #F0E6EA; border-bottom: 2px solid #DDD0D4; }
    # .th-pad { padding: 8px 12px; }
    tagList(
      tags$div(
        class = "mb-15",
        tags$details(
          tags$summary(
            class = "breakdown-summary",
            icon("layer-group"), " EL Breakdown by Sector"
          ),
          tags$div(
            class = "breakdown-body",
            tags$table(
              class = "table table-striped table-hover breakdown-table",
              tags$thead(
                tags$tr(
                  class = "breakdown-thead",
                  tags$th(class = "th-pad", "Sector"),
                  tags$th(class = "th-right", "Count"),
                  tags$th(class = "th-right", "Exposure"),
                  tags$th(class = "th-right", "Internal EL"),
                  tags$th(class = "th-right", "Adjusted EL"),
                  tags$th(class = "th-right", "EL Adj."),
                  tags$th(class = "th-right", "EL/Exposure")
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
}
