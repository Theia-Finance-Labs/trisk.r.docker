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
        avg_pd     = mean(pd_shock, na.rm = TRUE) * 100,
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
        avg_pd   = mean(pd_shock, na.rm = TRUE) * 100,
        pd_p25   = quantile(pd_shock, 0.25, na.rm = TRUE) * 100,
        pd_p75   = quantile(pd_shock, 0.75, na.rm = TRUE) * 100,
        .groups = "drop"
      )
  }) %>% bindCache(rv$run_id)

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
    agg <- horizon_by_year()
    req(agg)
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
    agg <- horizon_by_year()
    req(agg, !all(is.na(agg$avg_npv)))
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
    agg <- horizon_by_year()
    req(agg, !all(is.na(agg$total_el)))
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
    agg <- horizon_by_sector()
    req(agg)

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

    avg_data <- horizon_by_year() %>%
      select(shock_year, pd = avg_pd) %>%
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
      combined <- sanitize_export(bind_rows(rv$results_by_year, .id = "shock_year"))
      write.csv(combined, file, row.names = FALSE)
    }
  )

}
