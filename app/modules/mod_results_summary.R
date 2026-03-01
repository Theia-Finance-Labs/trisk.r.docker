# mod_results_summary.R
# Results Summary: value boxes, summary tables, sector plots, memoized aggregations

setup_results_summary <- function(input, output, session, rv) {

  # ============================================
  # Memoized summary reactives
  # ============================================
  # Centralized aggregations consumed by multiple render outputs.
  # Each is cached by run_id so re-selecting tabs does not recompute.

  # ---- Primary results: sector-level summary ----
  results_by_sector <- reactive({
    req(rv$results)
    df <- rv$results
    req("sector" %in% names(df))

    npv_col <- intersect(names(df), c("crispy_perc_value_change", "net_present_value_change"))
    pd_base_col <- intersect(names(df), c("pd_baseline", "crispy_pd_baseline"))
    pd_shock_col <- intersect(names(df), c("pd_shock", "crispy_pd_shock"))

    agg <- df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        avg_pd_baseline = if (length(pd_base_col) > 0) mean(.data[[pd_base_col[1]]], na.rm = TRUE) else NA_real_,
        avg_pd_shock    = if (length(pd_shock_col) > 0) mean(.data[[pd_shock_col[1]]], na.rm = TRUE) else NA_real_,
        avg_npv_change  = if (length(npv_col) > 0) mean(.data[[npv_col[1]]], na.rm = TRUE) else NA_real_,
        total_exposure  = if ("exposure_value_usd" %in% names(df)) sum(exposure_value_usd, na.rm = TRUE) else NA_real_,
        total_el_shock  = if ("expected_loss_shock" %in% names(df)) sum(expected_loss_shock, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )
    agg
  }) %>% bindCache(rv$run_id)

  # ---- Primary results: sector × technology summary ----
  results_by_tech <- reactive({
    req(rv$results)
    df <- rv$results
    req("sector" %in% names(df), "technology" %in% names(df))

    npv_col <- intersect(names(df), c("crispy_perc_value_change", "net_present_value_change"))
    pd_shock_col <- intersect(names(df), c("pd_shock", "crispy_pd_shock"))

    df %>%
      group_by(sector, technology) %>%
      summarise(
        n_rows         = n(),
        avg_npv_change = if (length(npv_col) > 0) smart_round(mean(.data[[npv_col[1]]], na.rm = TRUE) * 100) else NA_real_,
        avg_pd_shock   = if (length(pd_shock_col) > 0) smart_round(mean(.data[[pd_shock_col[1]]], na.rm = TRUE) * 100) else NA_real_,
        .groups = "drop"
      )
  }) %>% bindCache(rv$run_id)

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
                       class = "cursor-help"),
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
                       class = "cursor-help"),
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
                       class = "cursor-help"),
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
                       class = "cursor-help"),
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
        # Use centralized results_by_tech() — single group_by instead of 3 separate ones
        summary_df <- results_by_tech()
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
    agg <- results_by_sector()
    req(agg, !all(is.na(agg$avg_npv_change)))

    plot_data <- agg %>% mutate(avg_npv_change = avg_npv_change * 100)

    p <- ggplot(plot_data, aes(x = reorder(sector, avg_npv_change), y = avg_npv_change, fill = avg_npv_change < 0)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      scale_fill_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = TRISK_HEX_GREEN), guide = "none") +
      labs(title = "NPV Change by Sector", x = "", y = "Average NPV Change (%)") +
      trisk_plot_theme()

    ggplotly(p)
  })

  output$plot_sector_pd <- renderPlotly({
    agg <- results_by_sector()
    req(agg, !all(is.na(agg$avg_pd_baseline)), !all(is.na(agg$avg_pd_shock)))

    plot_data <- agg %>%
      transmute(sector,
                Baseline = avg_pd_baseline * 100,
                Shock    = avg_pd_shock * 100) %>%
      pivot_longer(cols = c(Baseline, Shock), names_to = "scenario", values_to = "pd")

    p <- ggplot(plot_data, aes(x = sector, y = pd, fill = scenario)) +
      geom_bar(stat = "identity", position = "dodge") +
      scale_fill_manual(values = c("Baseline" = TRISK_HEX_GREEN, "Shock" = TRISK_HEX_RED)) +
      labs(title = "PD by Sector: Baseline vs Shock", x = "", y = "Avg PD (%)") +
      trisk_plot_theme() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))

    ggplotly(p)
  })

}
