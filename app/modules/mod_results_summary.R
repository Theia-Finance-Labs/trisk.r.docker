# mod_results_summary.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_results_summary <- function(input, output, session, rv) {

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

  # NPV value box removed — NPV is an intermediate metric, not user-facing

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
      return(valueBox("---", "PD Change, Exposure-Weighted (%)", icon = icon("balance-scale"), color = "blue"))
    }
    val <- NA_real_
    df <- rv$results
    if (all(c("pd_baseline", "pd_shock") %in% names(df))) {
      exp_col <- if ("exposure_value_usd" %in% names(df)) df$exposure_value_usd else rep(1, nrow(df))
      total_exp <- sum(exp_col, na.rm = TRUE)
      if (total_exp > 0) {
        weighted_baseline <- sum(df$pd_baseline * exp_col, na.rm = TRUE) / total_exp
        weighted_change   <- sum((df$pd_shock - df$pd_baseline) * exp_col, na.rm = TRUE) / total_exp
        val <- if (weighted_baseline != 0) weighted_change / weighted_baseline * 100 else 0
      }
    }
    display_val <- if (!is.na(val)) paste0(smart_round(val), "%") else "N/A"
    valueBox(tags$span(display_val,
                       title = paste0("Full precision: ", smart_round(val), "%"),
                       style = "cursor: help;"),
             "PD Change, Exposure-Weighted (%)",
             icon = if (!is.na(val) && val > 0) icon("arrow-up") else icon("arrow-down"),
             color = if (!is.na(val) && val > 0.01) "red" else if (!is.na(val) && val < -0.01) "green" else "blue")
  })

  # Portfolio-level EL Change value box
  output$vb_el_change <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "Total EL Change", icon = icon("exclamation-circle"), color = "blue"))
    }
    val <- 0
    df <- rv$results
    if (all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))) {
      # Compute as abs(shock) - abs(baseline) so positive = loss increased (aligns with PD direction)
      val <- sum(abs(df$expected_loss_shock) - abs(df$expected_loss_baseline), na.rm = TRUE)
    }
    display_val <- format_number(abs(val))
    is_increase <- val > 0  # positive = EL increased (more expected loss)
    valueBox(tags$span(paste0(if (is_increase) "+" else "-", display_val),
                       title = paste0("Full precision: ", round(val, 2)),
                       style = "cursor: help;"),
             "Total EL Change",
             icon = if (is_increase) icon("arrow-up") else icon("arrow-down"),
             color = if (is_increase) "red" else "green")
  })

  # ============================================
  # Section headers for results tabs
  # ============================================

  output$summary_header <- renderUI({
    req(rv$results)
    df <- rv$results
    n_co <- length(unique(df$company_id))
    n_sectors <- if ("sector" %in% names(df)) length(unique(df$sector)) else 0
    total_exp <- if ("exposure_value_usd" %in% names(df)) sum(df$exposure_value_usd, na.rm = TRUE) else 0
    div(class = "section-header section-header--dark",
      fluidRow(
        column(8,
          h4(icon("chart-pie"), " Portfolio Summary", class = "section-title"),
          tags$small(paste0(n_co, " companies | ", n_sectors, " sectors | Total exposure: $",
                           format_number(total_exp)), class = "opacity-85")
        ),
        column(4, class = "text-right",
          downloadButton("download_summary_csv", "Export CSV", class = "btn-sm btn-export")
        )
      )
    )
  })

  output$pd_analysis_header <- renderUI({
    req(rv$results)
    df <- rv$results
    n_co <- length(unique(df$company_id))
    n_sectors <- if ("sector" %in% names(df)) length(unique(df$sector)) else 0
    avg_pd <- if ("pd_shock" %in% names(df)) smart_round(mean(df$pd_shock, na.rm = TRUE) * 100) else "N/A"
    div(class = "section-header section-header--dark",
      fluidRow(
        column(8,
          h4(icon("shield-alt"), " Credit Risk Analysis", class = "section-title"),
          tags$small(paste0(n_co, " companies | ", n_sectors, " sectors | Avg shocked PD: ",
                           avg_pd, "%"), class = "opacity-85")
        ),
        column(4, class = "text-right",
          downloadButton("download_pd_csv", "Export CSV", class = "btn-sm btn-export")
        )
      )
    )
  })

  # NPV analysis header removed — NPV tab no longer shown

  # Download handlers for section headers
  output$download_summary_csv <- downloadHandler(
    filename = function() paste0("trisk_summary_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$results)
      write.csv(sanitize_export(rv$results), file, row.names = FALSE)
    }
  )

  output$download_pd_csv <- downloadHandler(
    filename = function() paste0("trisk_pd_analysis_", Sys.Date(), ".csv"),
    content = function(file) {
      req(rv$results)
      pd_cols <- c("company_id", "company_name", "sector", "technology", "country_iso2",
                   "exposure_value_usd", "term", "loss_given_default", "pd_baseline", "pd_shock")
      pd_cols <- intersect(pd_cols, names(rv$results))
      if (length(pd_cols) == 0) pd_cols <- names(rv$results)
      write.csv(sanitize_export(rv$results[, pd_cols, drop = FALSE]), file, row.names = FALSE)
    }
  )

  # NPV download handler removed

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

  # NPV table removed — NPV tab no longer shown

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
                   "#BAB6B5",
                   "#E0A0A0", "#D48080", "#C45555", "#A83535", "#8B2020")
      dt <- dt %>%
        formatStyle("PD_Change",
                    backgroundColor = styleInterval(brks_pd[-c(1, length(brks_pd))], clrs_pd),
                    color = "white", fontWeight = "bold")
    }

    if (has_pd && "PD_Change_Pct" %in% names(display_df)) {
      brks_pct <- c(-Inf, -50, -20, -10, -5, -1, 1, 5, 10, 20, 50, Inf)
      clrs_pct <- c("#2D6A2E", "#3D8B3E", "#5DAA5E", "#7DC07E", "#A0D8A0",
                    "#BAB6B5",
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
                     "#BAB6B5",
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
                       "#BAB6B5",
                       "#A0D8A0", "#7DC07E", "#5DAA5E", "#3D8B3E", "#2D6A2E")
      dt <- dt %>%
        formatStyle("EL_Change_Pct",
                    backgroundColor = styleInterval(brks_el_pct[-c(1, length(brks_el_pct))], clrs_el_pct),
                    color = "white", fontWeight = "bold")
    }

    dt
  })

  # ---- PD Distribution: horizontal bar chart of PD change per company ----
  output$plot_pd_distribution <- renderPlotly({
    req(rv$results)
    df <- rv$results

    req("pd_baseline" %in% names(df), "pd_shock" %in% names(df))

    tc <- plotly_theme_colors(input)

    has_company_name <- "company_name" %in% names(df) && any(!is.na(df$company_name))
    id_col <- if (has_company_name) "company_name" else "company_id"
    sector_col <- if ("sector" %in% names(df)) "sector" else NULL

    # Aggregate to company level (results may have multiple rows per company for different technologies)
    plot_df <- df %>%
      mutate(
        company_label = if (has_company_name) as.character(.data[[id_col]])
                        else paste0("Company ", .data[[id_col]])
      ) %>%
      group_by(company_label) %>%
      summarise(
        pd_baseline = safe_weighted_mean(pd_baseline, exposure_value_usd),
        pd_shock = safe_weighted_mean(pd_shock, exposure_value_usd),
        exposure_value_usd = sum(exposure_value_usd, na.rm = TRUE),
        sector = first(sector),
        .groups = "drop"
      ) %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        pd_change_bps = pd_change * 10000,
        direction = ifelse(pd_change >= 0, "PD Increase (Deterioration)", "PD Decrease (Improvement)")
      ) %>%
      filter(!is.na(pd_change), is.finite(pd_change)) %>%
      arrange(pd_change)

    req(nrow(plot_df) > 0)

    # Build hover text
    hover_text <- paste0(
      htmltools::htmlEscape(as.character(plot_df$company_label)),
      if (!is.null(sector_col)) paste0("\nSector: ", htmltools::htmlEscape(as.character(plot_df[[sector_col]]))) else "",
      "\nPD Baseline: ", smart_round(plot_df$pd_baseline * 100), "%",
      "\nPD Shock: ", smart_round(plot_df$pd_shock * 100), "%",
      "\nPD Change: ", ifelse(plot_df$pd_change >= 0, "+", ""),
      round(plot_df$pd_change_bps, 1), " bps"
    )

    colors <- ifelse(plot_df$pd_change >= 0, BRAND_CORAL, STATUS_GREEN)

    plot_df$company_factor <- factor(plot_df$company_label, levels = plot_df$company_label)
    plot_df$bar_color <- ifelse(plot_df$pd_change >= 0, "PD Increase", "PD Decrease")

    p <- ggplot(plot_df, aes(x = company_factor, y = pd_change_bps, fill = bar_color,
                             text = hover_text)) +
      geom_col(width = 0.6) +
      geom_hline(yintercept = 0, color = "#999", linewidth = 0.5) +
      scale_fill_manual(values = c("PD Increase" = BRAND_CORAL, "PD Decrease" = STATUS_GREEN),
                        guide = "none") +
      labs(y = "PD Change (basis points)", x = "") +
      coord_flip() +
      trisk_plot_theme() +
      theme(axis.text.y = element_text(size = 10))

    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL,
        margin = list(l = 140, r = 60, t = 10, b = 50),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
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
  # Summary tab - Unified value cards row (6 KPI cards)
  # ============================================

  output$value_cards_row <- renderUI({
    # Helper to safely pick first available column
    safe_col <- function(df, ...) {
      candidates <- c(...)
      for (col in candidates) {
        if (col %in% names(df)) return(df[[col]])
      }
      return(NULL)
    }
    # Helper to build a single portfolio-aggregate card
    make_card <- function(title, value_text, color_class = "neutral", tooltip = title) {
      column(2,
        div(class = "portfolio-aggregate",
          h4(title, title = tooltip),
          tags$span(class = paste("agg-value", color_class), value_text)
        )
      )
    }

    if (is.null(rv$results)) {
      return(fluidRow(
        make_card("Companies Analyzed", "---"),
        make_card("Total Exposure", "---"),
        make_card("Max PD Shock (%)", "---"),
        make_card("PD Change (%)", "---"),
        make_card("Total EL Change", "---")
      ))
    }

    df <- rv$results
    has_pd <- all(c("pd_baseline", "pd_shock") %in% names(df))
    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_exp <- "exposure_value_usd" %in% names(df)

    # 1. Companies Analyzed
    n_companies <- {
      col <- safe_col(df, "company_id", "company_name")
      if (!is.null(col)) length(unique(col)) else nrow(df)
    }

    # 2. Total Exposure
    total_exposure <- {
      col <- safe_col(df, "exposure_value_usd", "exposure")
      if (!is.null(col)) sum(col, na.rm = TRUE) else 0
    }

    # 3. Max PD Shock
    max_pd <- {
      col <- safe_col(df, "pd_shock", "crispy_pd_shock")
      if (!is.null(col)) max(col, na.rm = TRUE) * 100 else 0
    }
    max_pd_class <- if (max_pd > 5) "negative" else "neutral"

    # 5. PD Change (%) — exposure-weighted relative
    pd_change_pct <- NA_real_
    if (has_pd && has_exp) {
      exp_col <- df$exposure_value_usd
      total_exp <- sum(exp_col, na.rm = TRUE)
      if (total_exp > 0) {
        weighted_pd_baseline <- sum(df$pd_baseline * exp_col, na.rm = TRUE) / total_exp
        if (weighted_pd_baseline != 0) {
          weighted_pd_change <- sum((df$pd_shock - df$pd_baseline) * exp_col, na.rm = TRUE) / total_exp
          pd_change_pct <- weighted_pd_change / weighted_pd_baseline * 100
        }
      }
    }
    pd_pct_class <- if (!is.na(pd_change_pct) && pd_change_pct > 0.01) "negative" else
      if (!is.na(pd_change_pct) && pd_change_pct < -0.01) "positive" else "neutral"

    # 6. Total EL Change
    el_change <- 0
    if (has_el) {
      el_change <- sum(df$expected_loss_shock - df$expected_loss_baseline, na.rm = TRUE)
    }
    # EL is negative by convention; more negative = EL increased = BAD
    el_class <- if (el_change < 0) "negative" else if (el_change > 0) "positive" else "neutral"

    fluidRow(
      make_card("Companies Analyzed", as.character(n_companies),
                tooltip = "Number of unique companies in the portfolio"),
      make_card("Total Exposure", format_number(total_exposure),
                tooltip = "Sum of all loan exposure amounts in USD"),
      make_card("Max PD Shock (%)",
                paste0(display_round(max_pd), "%"), max_pd_class,
                tooltip = "Highest single-company probability of default under shock scenario"),
      make_card("PD Change (%)",
                if (!is.na(pd_change_pct)) paste0(smart_round(pd_change_pct), "%") else "N/A",
                pd_pct_class,
                tooltip = "Exposure-weighted portfolio PD change as percentage of baseline PD"),
      make_card("Total EL Change", format_number(abs(el_change)), el_class,
                tooltip = "Total change in expected loss across the portfolio")
    )
  })

  # Sector NPV chart removed — NPV is an intermediate metric


  # ============================================
  # Combined multi-year data (used by PD analysis & horizon plots)
  # ============================================

  # All shock-year results combined (works even with 1 year)
  all_years_data <- reactive({
    req(rv$results_by_year)
    bind_rows(rv$results_by_year, .id = "shock_year_chr") %>%
      mutate(shock_year = as.integer(shock_year_chr))
  })

  # ============================================
  # ENHANCED PD ANALYSIS PLOTS
  # ============================================

  # Plot 1: PD Change by Sector — bar chart with CIs and per-year shock lines
  output$plot_pd_sector_enhanced <- renderPlotly({
    req(rv$results_by_year)
    df <- all_years_data()
    req(nrow(df) > 0, "sector" %in% names(df), "pd_baseline" %in% names(df), "pd_shock" %in% names(df))

    # Compute relative PD change (%) for every company × year
    df <- df %>% mutate(
      pd_change_pct = ifelse(pd_baseline != 0,
                             (pd_shock - pd_baseline) / pd_baseline * 100,
                             0)
    )

    # Sector-level stats: mean, SE, 95% CI across all companies and years
    sector_stats <- df %>%
      group_by(sector) %>%
      summarise(
        mean_pd = mean(pd_change_pct, na.rm = TRUE),
        sd_pd   = sd(pd_change_pct, na.rm = TRUE),
        n       = n(),
        se_pd   = sd_pd / sqrt(n),
        ci_lo   = mean_pd - 1.96 * se_pd,
        ci_hi   = mean_pd + 1.96 * se_pd,
        .groups = "drop"
      )

    # Per-year averages per sector (for the thin horizontal lines)
    year_sector_means <- df %>%
      group_by(sector, shock_year) %>%
      summarise(year_mean = mean(pd_change_pct, na.rm = TRUE), .groups = "drop")

    # Sort sectors by mean PD change
    sector_order <- sector_stats %>% arrange(mean_pd) %>% pull(sector)
    sector_stats$sector <- factor(sector_stats$sector, levels = sector_order)
    year_sector_means$sector <- factor(year_sector_means$sector, levels = sector_order)

    # Color: red for deterioration (positive change), green for improvement (negative)
    sector_stats$bar_color <- ifelse(sector_stats$mean_pd >= 0, TRISK_HEX_RED, TRISK_HEX_GREEN)

    p <- ggplot(sector_stats, aes(x = sector, y = mean_pd)) +
      geom_bar(stat = "identity", aes(fill = mean_pd >= 0), width = 0.6) +
      geom_errorbar(aes(ymin = ci_lo, ymax = ci_hi), width = 0.2, linewidth = 0.6, color = "gray30") +
      # Per-year averages as thin black tick marks
      geom_point(data = year_sector_means, aes(x = sector, y = year_mean),
                 shape = 95, size = 8, color = "black", stroke = 1.2) +
      scale_fill_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = TRISK_HEX_GREEN), guide = "none") +
      coord_flip() +
      labs(
        title = "PD Change by Sector",
        subtitle = paste0(
          "Bars = mean across shock years | ",
          "Whiskers = 95% CI (\u00b11.96 SE) | ",
          "Ticks (\u2501) = per-year avg"
        ),
        x = "", y = "Avg PD Change (%)"
      ) +
      trisk_plot_theme() +
      theme(plot.subtitle = element_text(size = 9, color = "gray50"))

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        showlegend = FALSE,
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })



  # ============================================

}
