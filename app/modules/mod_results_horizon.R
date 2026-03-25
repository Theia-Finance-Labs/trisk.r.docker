# mod_results_horizon.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_results_horizon <- function(input, output, session, rv) {

  # HORIZON ANALYSIS (Multi-Year Time-Series)
  # ============================================

  # Combine all shock-year results (requires ≥ 2 years for horizon view)
  horizon_data <- reactive({
    req(rv$results_by_year)
    if (length(rv$results_by_year) < 2) return(NULL)
    all_years_data()
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
      tags$p(class = "fs-12 fg-secondary mb-8", tags$i(class = "fa fa-info-circle mr-6"), "Shows how risk evolves across different shock years. A later shock year means companies have longer to accumulate misalignment, amplifying the transition impact."),

      # Row 1: Year-over-year metrics table (moved to top for quick reference)
      fluidRow(
        box(width = 12, title = "Year-over-Year Metrics", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            DTOutput("horizon_yoy_table"))
      ),

      # Row 2: Portfolio-level time-series line charts
      fluidRow(
        box(width = 6, title = "Average PD (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_pd_line", height = "300px")),
        box(width = 6, title = "Total EL (Shock) Over Time", status = "danger", solidHeader = FALSE,
            plotlyOutput("horizon_el_line", height = "300px"))
      ),

      # Row 3: Sector-faceted evolution
      fluidRow(
        box(width = 12, title = "PD Evolution by Sector", status = "info", solidHeader = FALSE,
            collapsible = TRUE,
            plotlyOutput("horizon_sector_facet", height = "400px"))
      ),

      # Row 4: PD Term Structure
      fluidRow(
        box(width = 12, title = "PD Term Structure", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(style = "font-size: 12px; color: #666;",
                   "PD curve across shock years — analogous to a credit term structure. Shaded band = interquartile range (P25–P75)."),
            plotlyOutput("horizon_term_structure", height = "380px"))
      ),

      # Row 5: Risk Migration Matrix + Company PD Trajectories
      fluidRow(
        box(width = 6, title = "Risk Migration Matrix", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(style = "font-size: 12px; color: #666;",
                   "How companies migrate between PD risk buckets from the earliest to latest shock year. Analogous to a credit rating transition matrix."),
            plotlyOutput("horizon_migration_heatmap", height = "380px")),
        box(width = 6, title = "Company PD Trajectories", status = "warning", solidHeader = FALSE,
            collapsible = TRUE,
            tags$p(style = "font-size: 12px; color: #666;",
                   "Individual company PD paths across shock years (thin lines) vs portfolio average (bold). Shows dispersion of risk outcomes."),
            plotlyOutput("horizon_company_trajectories", height = "380px"))
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
      layout(xaxis = list(title = "Shock Year", tickvals = agg$shock_year, ticktext = as.character(agg$shock_year)),
             yaxis = list(title = "Avg PD (Shock) %"),
             margin = list(t = 10))
  })

  # Horizon NPV line chart removed

  # ---- Horizon: Portfolio EL line chart ----
  output$horizon_el_line <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    agg <- hd %>% group_by(shock_year) %>%
      summarise(total_el = sum(abs(expected_loss_shock), na.rm = TRUE), .groups = "drop")
    plot_ly(agg, x = ~shock_year, y = ~total_el, type = "scatter", mode = "lines+markers",
            line = list(color = STATUS_RED, width = 3),
            marker = list(color = STATUS_RED, size = 10),
            hovertemplate = "Year: %{x}<br>Total EL: $%{y:,.0f}<extra></extra>") %>%
      layout(xaxis = list(title = "Shock Year", tickvals = agg$shock_year, ticktext = as.character(agg$shock_year)),
             yaxis = list(title = "Total Expected Loss ($)"),
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
      layout(xaxis = list(title = "Shock Year", tickvals = unique(agg$shock_year), ticktext = as.character(unique(agg$shock_year))),
             yaxis = list(title = "Avg PD (Shock) %"),
             legend = list(orientation = "h", y = -0.2),
             margin = list(t = 10, b = 60))
    p
  })

  # ---- Horizon: Company PD Trajectories (spaghetti plot) ----
  output$horizon_company_trajectories <- renderPlotly({
    hd <- horizon_data()
    req(hd)
    id_col <- if ("company_name" %in% names(hd) && any(!is.na(hd$company_name))) "company_name" else "company_id"
    all_years <- sort(unique(hd$shock_year))

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
    p %>% layout(xaxis = list(title = "Shock Year", tickvals = all_years, ticktext = as.character(all_years)),
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

    # Assign risk buckets (aligned with Basel IRB rating grades)
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
            colorscale = list(c(0, "#F5F0F2"), c(0.5, "#EF7173"), c(1, "#C0393B")),
            hovertemplate = "From: %{y}<br>To: %{x}<br>Count: %{z}<extra></extra>") %>%
      layout(xaxis = list(title = paste0("PD Bucket (", years[length(years)], ")"), tickangle = 0),
             yaxis = list(title = paste0("PD Bucket (", years[1], ")"), autorange = "reversed"),
             margin = list(t = 10, l = 100, b = 80))
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
    all_years <- unique(portfolio_agg$shock_year)
    p %>% layout(xaxis = list(title = "Shock Year", tickvals = all_years, ticktext = as.character(all_years)),
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
        `Total EL ($)` = round(sum(abs(df$expected_loss_shock), na.rm = TRUE), 0),
        `EL Change ($)` = round(sum(abs(df$expected_loss_shock) - abs(df$expected_loss_baseline), na.rm = TRUE), 0),
        Companies = nrow(df),
        check.names = FALSE
      )
    })
    yoy_df <- bind_rows(rows)

    # Add YoY delta columns
    if (nrow(yoy_df) >= 2) {
      yoy_df$`PD Delta (pp)` <- c(NA, round(diff(yoy_df$`Avg PD (%)`), 4))
    }

    datatable(yoy_df, options = list(dom = "t", pageLength = 20, scrollX = TRUE,
                  columnDefs = list(list(className = "dt-center", targets = "_all"))),
              rownames = FALSE) %>%
      formatStyle("Avg PD (%)",
                  background = styleColorBar(range(yoy_df$`Avg PD (%)`, na.rm = TRUE), BRAND_CORAL_LT),
                  backgroundSize = "98% 60%", backgroundRepeat = "no-repeat",
                  backgroundPosition = "center")
  })

  # ---- Horizon: CSV Export ----
  output$download_horizon_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_multi_horizon_", rv$run_id, ".csv")
    },
    content = function(file) {
      req(rv$results_by_year)
      combined <- bind_rows(rv$results_by_year, .id = "shock_year")
      write.csv(sanitize_export(combined), file, row.names = FALSE)
    }
  )


}
