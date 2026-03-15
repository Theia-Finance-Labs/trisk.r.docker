# mod_results_horizon.R
# Horizon Analysis: multi-year time-series charts, YoY table, CSV export

setup_results_horizon <- function(input, output, session, rv) {

  # Combine all shock-year results into a single long-form dataframe
  horizon_data <- reactive({
    req(rv$results_by_year)
    if (length(rv$results_by_year) < 2) return(NULL)
    bind_rows(rv$results_by_year, .id = "shock_year_chr") %>%
      mutate(shock_year = as.integer(shock_year_chr))
  })

  # ---- Horizon: portfolio-level aggregation by shock year ----
  horizon_by_year <- reactive({
    hd <- horizon_data()
    req(hd)
    hd %>%
      group_by(shock_year) %>%
      summarise(
        avg_pd     = if ("exposure_value_usd" %in% names(hd))
          weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE) * 100
        else mean(pd_shock, na.rm = TRUE) * 100,
        avg_npv    = if ("crispy_perc_value_change" %in% names(hd)) mean(crispy_perc_value_change, na.rm = TRUE) * 100 else NA_real_,
        total_el   = if ("expected_loss_shock" %in% names(hd)) sum(expected_loss_shock, na.rm = TRUE) else NA_real_,
        pd_p25     = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
        pd_p75     = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }) %>% bindCache(rv$run_id)

  # ---- Horizon: sector-level aggregation by shock year × sector ----
  horizon_by_sector <- reactive({
    hd <- horizon_data()
    req(hd, "sector" %in% names(hd))
    hd %>%
      group_by(shock_year, sector) %>%
      summarise(
        avg_pd   = if ("exposure_value_usd" %in% names(hd))
          weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE) * 100
        else mean(pd_shock, na.rm = TRUE) * 100,
        pd_p25   = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
        pd_p75   = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }) %>% bindCache(rv$run_id)

  # Main Horizon Analysis UI
  output$horizon_analysis_ui <- renderUI({
    if (is.null(rv$results_by_year) || length(rv$results_by_year) < 2) {
      return(tags$div(
        class = "empty-state-40",
        icon("clock", class = "empty-state-icon-muted"),
        tags$h4("Horizon Analysis requires 2+ shock years"),
        tags$p("Select 2 or more shock years on the Configure tab, then re-run."),
        actionLink("goto_config_horizon", "Configure shock years",
                    icon = icon("cog"), class = "fs-14"),
        tags$p(class = "fs-13 fg-muted mt-8",
               "Tip: select years like 2030, 2035, 2040, 2045, 2050 for a full term structure.")
      ))
    }

    years <- sort(as.integer(names(rv$results_by_year)))

    tagList(
      # Header with summary
      div(class = "section-header section-header--dark",
        fluidRow(
          column(8,
            h4(icon("chart-area"), " Multi-Horizon Risk Analysis",
               class = "section-title"),
            tags$small(paste0(length(years), " shock years: ",
                             paste(years, collapse = ", "),
                             " | ", nrow(rv$results_by_year[[1]]), " companies"),
                      class = "opacity-85")
          ),
          column(4, class = "text-right",
            downloadButton("download_horizon_csv", "Export Multi-Horizon CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),

      # Row 1: Year-over-year table (moved to top)
      fluidRow(
        box(width = 12, title = "Year-over-Year Metrics", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput("horizon_yoy_table"))
      ),

      # Row 2: Portfolio-level time-series line charts
      fluidRow(
        box(width = 4, title = "Average PD (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_pd_line", height = "300px")),
        box(width = 4, title = "Average NPV Change Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_npv_line", height = "300px")),
        box(width = 4, title = "Total EL (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_el_line", height = "300px"))
      ),

      # Row 3: PD Evolution by Sector (clustered bar chart)
      fluidRow(
        box(width = 12, title = "PD Evolution by Sector", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            plotlyOutput("horizon_sector_facet", height = "400px"))
      ),

      # Row 4: Company PD Scatter + Risk migration matrix
      fluidRow(
        box(width = 7, title = "Company PD Scatter by Year", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(class = "history-meta",
                   "Each dot = one company at one shock year. Size reflects exposure weight."),
            plotlyOutput("horizon_company_pd_scatter", height = "400px")),
        box(width = 5, title = "Risk Migration Matrix", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(class = "history-meta",
                   "How companies move between PD risk buckets from earliest to latest horizon."),
            plotlyOutput("horizon_migration_heatmap", height = "350px"))
      ),

      # Row 5: PD Term Structure
      fluidRow(
        box(width = 12, title = "PD Term Structure", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(class = "history-meta",
                   "PD curve across shock years — analogous to a credit term structure."),
            plotlyOutput("horizon_term_structure", height = "350px"))
      )
    )
  })

  # ---- Horizon: Portfolio PD line chart ----
  output$horizon_pd_line <- renderPlotly({
    tc <- plotly_theme_colors(input)
    agg <- horizon_by_year()
    req(agg)
    plot_ly(agg, x = ~shock_year, y = ~avg_pd, type = "scatter", mode = "lines+markers",
            line = list(color = BRAND_CORAL, width = 3),
            marker = list(color = BRAND_CORAL, size = 10),
            hovertemplate = "Year: %{x}<br>Avg PD: %{y:.4f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
             yaxis = list(title = "Avg PD (Shock) %", gridcolor = tc$gridcolor),
             margin = list(t = 10),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: Portfolio NPV line chart ----
  output$horizon_npv_line <- renderPlotly({
    tc <- plotly_theme_colors(input)
    agg <- horizon_by_year()
    req(agg, !all(is.na(agg$avg_npv)))
    plot_ly(agg, x = ~shock_year, y = ~avg_npv, type = "scatter", mode = "lines+markers",
            line = list(color = STATUS_BLUE, width = 3),
            marker = list(color = STATUS_BLUE, size = 10),
            hovertemplate = "Year: %{x}<br>Avg NPV Change: %{y:.2f}%<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
             yaxis = list(title = "Avg NPV Change %", gridcolor = tc$gridcolor),
             margin = list(t = 10),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: Portfolio EL line chart ----
  output$horizon_el_line <- renderPlotly({
    tc <- plotly_theme_colors(input)
    agg <- horizon_by_year()
    req(agg, !all(is.na(agg$total_el)))
    plot_ly(agg, x = ~shock_year, y = ~total_el, type = "scatter", mode = "lines+markers",
            line = list(color = STATUS_RED, width = 3),
            marker = list(color = STATUS_RED, size = 10),
            hovertemplate = "Year: %{x}<br>Total EL: %{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
             yaxis = list(title = "Total EL (Shock)", gridcolor = tc$gridcolor),
             margin = list(t = 10),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: Sector PD evolution (clustered bar chart) ----
  output$horizon_sector_facet <- renderPlotly({
    tc <- plotly_theme_colors(input)
    agg <- horizon_by_sector()
    req(agg)

    # Convert shock_year to factor for discrete x-axis (clustered bars)
    agg$year_factor <- factor(agg$shock_year)

    plot_ly(agg, x = ~year_factor, y = ~avg_pd, color = ~sector,
            type = "bar",
            hovertemplate = "%{fullData.name}<br>Year: %{x}<br>PD: %{y:.4f}%<extra></extra>") %>%
      layout(barmode = "group",
             xaxis = list(title = "Shock Year", gridcolor = tc$gridcolor),
             yaxis = list(title = "Exp-Wtd Avg PD (Shock) %", gridcolor = tc$gridcolor),
             legend = list(orientation = "h", y = -0.2),
             margin = list(t = 10, b = 60),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: Company PD Scatter by Year ----
  output$horizon_company_pd_scatter <- renderPlotly({
    tc <- plotly_theme_colors(input)
    hd <- horizon_data()
    req(hd)
    id_col <- if ("company_name" %in% names(hd)) "company_name" else "company_id"
    has_sector <- "sector" %in% names(hd)

    total_exp <- sum(hd$exposure_value_usd, na.rm = TRUE)
    scatter_df <- hd %>%
      mutate(
        pd_pct = pd_shock * 100,
        year_factor = factor(shock_year),
        weight = exposure_value_usd / total_exp,
        company_label = .data[[id_col]]
      )

    # Scale marker sizes: weight -> [4, 18] pixel range
    w_max <- max(scatter_df$weight, na.rm = TRUE)
    scatter_df$marker_size <- if (w_max > 0) {
      (scatter_df$weight / w_max) * 14 + 4
    } else {
      rep(8, nrow(scatter_df))
    }

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(scatter_df$company_label)),
      if (has_sector) paste0("\nSector: ", htmltools::htmlEscape(as.character(scatter_df$sector))) else "",
      "\nYear: ", scatter_df$shock_year,
      "\nPD: ", round(scatter_df$pd_pct, 4), "%",
      "\nWeight: ", round(scatter_df$weight * 100, 2), "%"
    )

    if (has_sector) {
      plot_ly(scatter_df, x = ~year_factor, y = ~pd_pct, color = ~sector,
              type = "scatter", mode = "markers",
              marker = list(size = ~marker_size, opacity = 0.6,
                           line = list(color = "white", width = 0.5)),
              text = hover_text, hoverinfo = "text")
    } else {
      plot_ly(scatter_df, x = ~year_factor, y = ~pd_pct,
              type = "scatter", mode = "markers",
              marker = list(size = ~marker_size, opacity = 0.6,
                           color = BRAND_CORAL,
                           line = list(color = "white", width = 0.5)),
              text = hover_text, hoverinfo = "text")
    } %>%
      layout(xaxis = list(title = "Shock Year", gridcolor = tc$gridcolor),
             yaxis = list(title = "PD (Shock) %", gridcolor = tc$gridcolor),
             legend = list(orientation = "h", y = -0.15),
             margin = list(t = 10, b = 50),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: Risk migration matrix ----
  output$horizon_migration_heatmap <- renderPlotly({
    tc <- plotly_theme_colors(input)
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
      layout(xaxis = list(title = paste0("Risk Bucket (", years[length(years)], ")"), tickangle = 0, gridcolor = tc$gridcolor),
             yaxis = list(title = paste0("Risk Bucket (", years[1], ")"), autorange = "reversed", gridcolor = tc$gridcolor),
             margin = list(t = 10, l = 100, b = 80),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Horizon: PD Term Structure ----
  output$horizon_term_structure <- renderPlotly({
    tc <- plotly_theme_colors(input)
    portfolio_agg <- horizon_by_year()
    req(portfolio_agg)

    # Dashed line when sector breakdown exists (visual cue that sectors are also available)
    hd <- horizon_data()
    has_sector <- !is.null(hd) && "sector" %in% names(hd)
    dash_style <- if (has_sector) "dash" else "solid"

    p <- plot_ly() %>%
      add_trace(data = portfolio_agg, x = ~shock_year, y = ~pd_p75, type = "scatter", mode = "lines",
                line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(data = portfolio_agg, x = ~shock_year, y = ~pd_p25, type = "scatter", mode = "lines",
                fill = "tonexty", fillcolor = "rgba(239,113,115,0.2)",
                line = list(width = 0), showlegend = FALSE, hoverinfo = "skip") %>%
      add_trace(data = portfolio_agg, x = ~shock_year, y = ~avg_pd, type = "scatter", mode = "lines+markers",
                line = list(color = BRAND_CORAL, width = 3, dash = dash_style),
                marker = list(color = BRAND_CORAL, size = 8),
                name = "Portfolio Avg",
                hovertemplate = "Portfolio<br>Year: %{x}<br>PD: %{y:.4f}%<br>(IQR band shown)<extra></extra>")

    p %>% layout(xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
                 yaxis = list(title = "PD (Shock) %", gridcolor = tc$gridcolor),
                 legend = list(orientation = "h", y = -0.2),
                 margin = list(t = 10, b = 50),
                 font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
                 paper_bgcolor = tc$paper_bgcolor,
                 plot_bgcolor = tc$plot_bgcolor,
                 hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
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
      combined <- sanitize_export(bind_rows(rv$results_by_year, .id = "shock_year"))
      write.csv(combined, file, row.names = FALSE)
    }
  )

}
