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
  # Results outputs - Unified value cards row
  # ============================================

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

  output$value_cards_row <- renderUI({
    if (is.null(rv$results)) {
      return(fluidRow(
        make_card("Companies Analyzed", "---"),
        make_card("Total Exposure", "---"),
        make_card("Max PD Shock (%)", "---"),
        make_card("PD Change (pp)", "---"),
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

    # 4. PD Change (pp) — exposure-weighted
    pd_change_pp <- 0
    if (has_pd) {
      exp_col <- if (has_exp) df$exposure_value_usd else rep(1, nrow(df))
      total_exp <- sum(exp_col, na.rm = TRUE)
      if (total_exp > 0) {
        pd_change_pp <- sum((df$pd_shock - df$pd_baseline) * exp_col, na.rm = TRUE) / total_exp * 100
      }
    }
    pd_pp_class <- if (pd_change_pp > 0.01) "negative" else if (pd_change_pp < -0.01) "positive" else "neutral"

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
    el_class <- if (el_change > 0) "negative" else if (el_change < 0) "positive" else "neutral"

    fluidRow(
      make_card("Companies Analyzed", as.character(n_companies),
                tooltip = "Number of unique companies in the portfolio"),
      make_card("Total Exposure", format_number(total_exposure),
                tooltip = "Sum of all loan exposure amounts in USD"),
      make_card("Max PD Shock (%)",
                paste0(display_round(max_pd), "%"), max_pd_class,
                tooltip = "Highest single-company probability of default under shock scenario"),
      make_card("PD Change (pp)",
                paste0(display_round(pd_change_pp), " pp"), pd_pp_class,
                tooltip = "Exposure-weighted portfolio PD change in percentage points"),
      make_card("PD Change (%)",
                if (!is.na(pd_change_pct)) paste0(smart_round(pd_change_pct), "%") else "N/A",
                pd_pct_class,
                tooltip = "Exposure-weighted portfolio PD change as percentage of baseline PD"),
      make_card("Total EL Change", format_number(el_change), el_class,
                tooltip = "Total change in expected loss across the portfolio")
    )
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
                        columnDefs = list(list(width = '80px', targets = '_all')),
                        headerCallback = make_header_tooltips(names(summary_df))), rownames = FALSE)
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
                             columnDefs = list(list(width = '80px', targets = '_all')),
                             headerCallback = make_header_tooltips(names(npv_df))),
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
                     columnDefs = list(list(width = '80px', targets = '_all')),
                     headerCallback = make_header_tooltips(names(display_df))),
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

  # (pd_portfolio_summary removed — all metrics now in unified value_cards_row above)

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

    tc <- plotly_theme_colors(input)
    ggplotly(p) %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
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

    tc <- plotly_theme_colors(input)
    ggplotly(p) %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

}
