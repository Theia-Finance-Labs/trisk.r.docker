# mod_results_scenarios.R
# Scenario Comparison + Distribution Analysis dashboards

setup_results_scenarios <- function(input, output, session, rv) {

  # ============================================
  # SCENARIO COMPARISON DASHBOARD
  # ============================================

  # Reactive: combine all scenario results into a single long-form dataframe
  scenario_comparison_data <- reactive({
    req(rv$results_by_scenario)
    scen_results <- rv$results_by_scenario
    if (length(scen_results) < 2) return(NULL)  # Need 2+ scenarios for comparison

    all_rows <- list()
    for (scen_code in names(scen_results)) {
      year_list <- scen_results[[scen_code]]
      for (yr_key in names(year_list)) {
        df <- year_list[[yr_key]]
        df$target_scenario <- scen_code
        df$scenario_label <- scenario_label(scen_code)
        df$scenario_category <- scenario_category(scen_code)
        df$shock_year <- as.integer(yr_key)
        all_rows[[paste0(scen_code, "_", yr_key)]] <- df
      }
    }
    bind_rows(all_rows)
  })

  # Reactive: portfolio-level summary per scenario (aggregated across companies)
  scenario_portfolio_summary <- reactive({
    scd <- scenario_comparison_data()
    req(scd)

    scd %>%
      group_by(target_scenario, scenario_label, scenario_category, shock_year) %>%
      summarise(
        n_companies = n(),
        avg_pd_shock = if ("exposure_value_usd" %in% names(scd))
          weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE)
        else mean(pd_shock, na.rm = TRUE),
        max_pd_shock = max(pd_shock, na.rm = TRUE),
        avg_pd_change = if ("exposure_value_usd" %in% names(scd))
          weighted.mean(pd_shock - pd_baseline, exposure_value_usd, na.rm = TRUE)
        else mean(pd_shock - pd_baseline, na.rm = TRUE),
        avg_npv_change = if ("crispy_perc_value_change" %in% names(scd))
          mean(crispy_perc_value_change, na.rm = TRUE) else NA_real_,
        total_el_shock = if ("expected_loss_shock" %in% names(scd))
          sum(expected_loss_shock, na.rm = TRUE) else NA_real_,
        total_el_change = if ("expected_loss_difference" %in% names(scd))
          sum(expected_loss_difference, na.rm = TRUE) else NA_real_,
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        .groups = "drop"
      )
  })

  # ---- Scenario Comparison UI ----
  output$scenario_comparison_ui <- renderUI({
    scd <- scenario_comparison_data()
    if (is.null(scd)) {
      return(div(class = "empty-state",
        icon("layer-group", class = "empty-state-icon"),
        h4("Scenario Comparison requires 2+ target scenarios"),
        p("Select multiple target scenarios on the Configure tab to enable cross-scenario analysis.",
          class = "fs-14"),
        actionLink("goto_config_scenario", "Configure target scenarios",
                    icon = icon("cog"), class = "fs-14")
      ))
    }

    n_scen <- length(unique(scd$target_scenario))
    n_yrs <- length(unique(scd$shock_year))
    n_co <- length(unique(scd$company_id))

    tagList(
      # Header
      div(class = "section-header section-header--dark",
        fluidRow(
          column(8,
            h4(icon("layer-group"), paste0(" Scenario Comparison: ", n_scen, " scenarios"),
               class = "section-title"),
            tags$small(paste0(n_yrs, " shock year(s) | ", n_co, " companies | ",
                             n_scen * n_yrs, " total runs"))
          ),
          column(4, class = "text-right",
            downloadButton("download_scenario_csv", "Export CSV",
                          class = "btn btn-xs btn-export")
          )
        )
      ),

      # Row 1: Distribution Summary Statistics (moved to top)
      fluidRow(
        box(title = "Distribution Summary Statistics", width = 12, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Cross-scenario quantiles for key portfolio metrics at each shock year."),
            DTOutput("dist_summary_table", height = "250px")
        )
      ),

      # ---- SCENARIO DISTRIBUTION (Probabilistic) ----
      div(class = "section-header section-header--purple mt-20",
        h4(icon("chart-area"), " Scenario Distribution \u2014 Probabilistic Analysis",
           class = "section-title"),
        tags$small("Treating selected scenarios as a distribution of plausible outcomes. ",
                   "Confidence bands show the range of uncertainty across scenario pathways.")
      ),

      # Row 2: Fan chart + Violin plots
      fluidRow(
        box(title = "PD Fan Chart \u2014 Uncertainty Over Time", width = 7, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Median PD with P10\u2013P90 and P25\u2013P75 confidence bands across all scenarios."),
            plotlyOutput("dist_fan_chart", height = "380px")
        ),
        box(title = "PD Distribution by Scenario", width = 5, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Company-level PD spread per scenario (violin + box plot)."),
            plotlyOutput("dist_violin", height = "380px")
        )
      ),

      # Row 3: Exceedance curve + NPV fan chart
      fluidRow(
        box(title = "PD Exceedance Curve", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Probability that portfolio avg PD exceeds a given threshold, across scenarios."),
            plotlyOutput("dist_exceedance", height = "350px")
        ),
        box(title = "NPV Change Fan Chart", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Median NPV change with P10\u2013P90 bands across scenarios."),
            plotlyOutput("dist_npv_fan", height = "350px")
        )
      ),

      # Row 4: Scenario Ranking Table + Tornado Chart
      fluidRow(
        box(title = "Scenario Impact Ranking", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Scenarios ranked by portfolio severity (highest avg PD shock first)."),
            DTOutput("scenario_ranking_table", height = "300px")
        ),
        box(title = "Sensitivity Tornado", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Range of portfolio metrics across scenarios \u2014 which has the biggest impact?"),
            plotlyOutput("scenario_tornado", height = "300px")
        )
      ),

      # Row 5: Grouped bar chart + Spider/radar
      fluidRow(
        box(title = "PD by Scenario & Sector", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            plotlyOutput("scenario_sector_bars", height = "350px")
        ),
        box(title = "Scenario Radar", width = 6, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Normalized portfolio metrics (0\u20131 scale) overlaid per scenario."),
            plotlyOutput("scenario_radar", height = "350px")
        )
      ),

      # Row 6: Company-level heatmap
      fluidRow(
        box(title = "Company \u00D7 Scenario Heatmap", width = 12, status = "danger",
            solidHeader = TRUE, collapsible = TRUE,
            tags$small("Rows = companies, Columns = scenarios. Cell color = PD shock intensity."),
            plotlyOutput("scenario_company_heatmap", height = "400px")
        )
      )
    )
  })

  # ---- Scenario Ranking Table ----
  output$scenario_ranking_table <- renderDT({
    sps <- scenario_portfolio_summary()
    req(sps)

    # Use earliest year for ranking
    earliest_yr <- min(sps$shock_year)
    ranking <- sps %>%
      filter(shock_year == max(sps$shock_year)) %>%
      arrange(desc(avg_pd_shock)) %>%
      mutate(
        rank = row_number(),
        avg_pd_pct = round(avg_pd_shock * 100, 3),
        max_pd_pct = round(max_pd_shock * 100, 3),
        avg_npv_pct = round(avg_npv_change * 100, 2),
        el_change = round(total_el_change, 0)
      ) %>%
      select(Rank = rank, Scenario = scenario_label, Code = target_scenario,
             Category = scenario_category,
             `Avg PD (%)` = avg_pd_pct, `Max PD (%)` = max_pd_pct,
             `NPV Change (%)` = avg_npv_pct, `EL Change ($)` = el_change)

    datatable(ranking, rownames = FALSE, options = list(
      dom = "t", pageLength = 20, ordering = FALSE,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )) %>%
      formatStyle("Avg PD (%)", backgroundColor = styleInterval(
        c(0.5, 2, 5), c("#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")))
  })

  # ---- Sensitivity Tornado Chart ----
  output$scenario_tornado <- renderPlotly({
    tc <- plotly_theme_colors(input)
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr)

    metrics <- data.frame(
      metric = c("Avg PD (%)", "Max PD (%)", "NPV Change (%)"),
      min_val = c(min(sps_yr$avg_pd_shock * 100), min(sps_yr$max_pd_shock * 100),
                  min(sps_yr$avg_npv_change * 100)),
      max_val = c(max(sps_yr$avg_pd_shock * 100), max(sps_yr$max_pd_shock * 100),
                  max(sps_yr$avg_npv_change * 100)),
      median_val = c(median(sps_yr$avg_pd_shock * 100), median(sps_yr$max_pd_shock * 100),
                     median(sps_yr$avg_npv_change * 100)),
      stringsAsFactors = FALSE
    )
    metrics$range <- metrics$max_val - metrics$min_val
    metrics <- metrics[order(metrics$range, decreasing = FALSE), ]
    metrics$metric <- factor(metrics$metric, levels = metrics$metric)

    plot_ly() %>%
      add_segments(data = metrics, x = ~min_val, xend = ~max_val, y = ~metric, yend = ~metric,
                   line = list(color = BRAND_CORAL, width = 14), showlegend = FALSE,
                   hoverinfo = "text",
                   text = ~paste0(metric, "\nMin: ", round(min_val, 2), " | Max: ", round(max_val, 2))) %>%
      add_markers(data = metrics, x = ~median_val, y = ~metric, marker = list(
        color = "#1A1A1A", size = 10, symbol = "diamond"), name = "Median",
        hoverinfo = "text", text = ~paste0("Median: ", round(median_val, 2))) %>%
      layout(xaxis = list(title = "Value", zeroline = TRUE, gridcolor = tc$gridcolor),
             yaxis = list(title = "", gridcolor = tc$gridcolor),
             margin = list(l = 120),
             showlegend = FALSE,
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Scenario × Sector Grouped Bar Chart ----
  output$scenario_sector_bars <- renderPlotly({
    tc <- plotly_theme_colors(input)
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)
    sector_scen <- scd %>%
      filter(shock_year == latest_yr) %>%
      group_by(scenario_label, sector) %>%
      summarise(avg_pd = if ("exposure_value_usd" %in% names(scd))
          weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE) * 100
        else mean(pd_shock, na.rm = TRUE) * 100, .groups = "drop")

    plot_ly(sector_scen, x = ~sector, y = ~avg_pd, color = ~scenario_label,
            type = "bar", hoverinfo = "text",
            text = ~paste0(scenario_label, "\n", sector, ": ", round(avg_pd, 3), "%")) %>%
      layout(barmode = "group",
             xaxis = list(title = "Sector", gridcolor = tc$gridcolor),
             yaxis = list(title = "Avg PD Shock (%)", gridcolor = tc$gridcolor),
             legend = list(orientation = "h", y = -0.2),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Scenario Radar Chart ----
  output$scenario_radar <- renderPlotly({
    tc <- plotly_theme_colors(input)
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr)

    # Normalize each metric to 0-1 range
    normalize <- function(x) {
      rng <- range(x, na.rm = TRUE)
      if (rng[2] == rng[1]) return(rep(0.5, length(x)))
      (x - rng[1]) / (rng[2] - rng[1])
    }

    radar_df <- sps_yr %>%
      mutate(
        pd_norm = normalize(avg_pd_shock),
        npv_norm = normalize(abs(avg_npv_change)),
        el_norm = normalize(abs(total_el_change)),
        max_pd_norm = normalize(max_pd_shock)
      )

    dims <- c("pd_norm", "npv_norm", "el_norm", "max_pd_norm")
    dim_labels <- c("Avg PD", "NPV Impact", "EL Impact", "Max PD")

    p <- plot_ly(type = "scatterpolar", fill = "toself")
    colors <- c(BRAND_CORAL, STATUS_BLUE, STATUS_GREEN, "#9C27B0", "#FF9800", "#795548")

    for (i in seq_len(nrow(radar_df))) {
      vals <- as.numeric(radar_df[i, dims])
      vals <- c(vals, vals[1])  # close the polygon
      col <- colors[((i - 1) %% length(colors)) + 1]
      p <- p %>% add_trace(
        r = vals, theta = c(dim_labels, dim_labels[1]),
        name = radar_df$scenario_label[i],
        line = list(color = col, width = 2),
        fillcolor = paste0(col, "33"),
        opacity = 0.7
      )
    }
    p %>% layout(
      polar = list(radialaxis = list(visible = TRUE, range = c(0, 1))),
      legend = list(orientation = "h", y = -0.15, font = list(size = 10)),
      margin = list(t = 30),
      font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
      paper_bgcolor = tc$paper_bgcolor,
      plot_bgcolor = tc$plot_bgcolor,
      hoverlabel = PLOTLY_HOVERLABEL
    ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Company × Scenario Heatmap ----
  output$scenario_company_heatmap <- renderPlotly({
    tc <- plotly_theme_colors(input)
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)
    heatmap_df <- scd %>%
      filter(shock_year == latest_yr) %>%
      select(company_name, scenario_label, pd_shock) %>%
      mutate(pd_pct = round(pd_shock * 100, 3))

    # Pivot to matrix
    mat <- heatmap_df %>%
      select(company_name, scenario_label, pd_pct) %>%
      tidyr::pivot_wider(names_from = scenario_label, values_from = pd_pct)

    companies <- mat$company_name
    scenarios <- setdiff(names(mat), "company_name")
    z_matrix <- as.matrix(mat[, scenarios])

    # Build hover text matrix (same dims as z_matrix)
    text_matrix <- matrix("", nrow = length(companies), ncol = length(scenarios))
    for (r in seq_along(companies)) {
      for (cc in seq_along(scenarios)) {
        text_matrix[r, cc] <- paste0(companies[r], "\n", scenarios[cc], "\nPD: ", z_matrix[r, cc], "%")
      }
    }

    plot_ly(x = scenarios, y = companies, z = z_matrix,
            type = "heatmap",
            colorscale = list(c(0, "#e8f5e9"), c(0.3, "#fff9c4"), c(0.6, "#ffccbc"), c(1, "#c62828")),
            colorbar = list(title = "PD Shock (%)"),
            hoverinfo = "text",
            text = text_matrix) %>%
      layout(xaxis = list(title = "", tickangle = -30, gridcolor = tc$gridcolor),
             yaxis = list(title = "", gridcolor = tc$gridcolor),
             margin = list(b = 120, l = 120),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ========================================================
  # SCENARIO DISTRIBUTION — Probabilistic Analysis
  # ========================================================

  # Reactive: distribution quantiles at each shock year
  dist_quantiles <- reactive({
    sps <- scenario_portfolio_summary()
    req(sps)
    if (length(unique(sps$target_scenario)) < 2) return(NULL)

    sps %>%
      group_by(shock_year) %>%
      summarise(
        pd_p10 = quantile(avg_pd_shock, 0.10, na.rm = TRUE),
        pd_p25 = quantile(avg_pd_shock, 0.25, na.rm = TRUE),
        pd_median = median(avg_pd_shock, na.rm = TRUE),
        pd_p75 = quantile(avg_pd_shock, 0.75, na.rm = TRUE),
        pd_p90 = quantile(avg_pd_shock, 0.90, na.rm = TRUE),
        pd_mean = mean(avg_pd_shock, na.rm = TRUE),
        npv_p10 = quantile(avg_npv_change, 0.10, na.rm = TRUE),
        npv_p25 = quantile(avg_npv_change, 0.25, na.rm = TRUE),
        npv_median = median(avg_npv_change, na.rm = TRUE),
        npv_p75 = quantile(avg_npv_change, 0.75, na.rm = TRUE),
        npv_p90 = quantile(avg_npv_change, 0.90, na.rm = TRUE),
        el_p10 = quantile(total_el_change, 0.10, na.rm = TRUE),
        el_p25 = quantile(total_el_change, 0.25, na.rm = TRUE),
        el_median = median(total_el_change, na.rm = TRUE),
        el_p75 = quantile(total_el_change, 0.75, na.rm = TRUE),
        el_p90 = quantile(total_el_change, 0.90, na.rm = TRUE),
        n_scenarios = n(),
        .groups = "drop"
      )
  })

  # ---- PD Fan Chart ----
  output$dist_fan_chart <- renderPlotly({
    tc <- plotly_theme_colors(input)
    dq <- dist_quantiles()
    req(dq)

    sps <- scenario_portfolio_summary()
    yrs <- sort(dq$shock_year)

    p <- plot_ly() %>%
      # P10-P90 band (lightest)
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~pd_p10 * 100, ymax = ~pd_p90 * 100,
                  fillcolor = "rgba(245, 61, 63, 0.12)", line = list(width = 0),
                  name = "P10\u2013P90", showlegend = TRUE) %>%
      # P25-P75 band (darker)
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~pd_p25 * 100, ymax = ~pd_p75 * 100,
                  fillcolor = "rgba(245, 61, 63, 0.30)", line = list(width = 0),
                  name = "P25\u2013P75", showlegend = TRUE) %>%
      # Median line
      add_lines(data = dq, x = ~shock_year, y = ~pd_median * 100,
                line = list(color = BRAND_CORAL, width = 3, dash = "solid"),
                name = "Median") %>%
      # Mean line (dashed)
      add_lines(data = dq, x = ~shock_year, y = ~pd_mean * 100,
                line = list(color = "#1A1A1A", width = 2, dash = "dot"),
                name = "Mean")

    # Individual scenario traces (thin, semi-transparent)
    scen_labels <- unique(sps$scenario_label)
    colors_scen <- c("#2196F3", "#4CAF50", "#FF9800", "#9C27B0", "#795548",
                     "#607D8B", "#E91E63", "#00BCD4", "#8BC34A", "#FFC107")
    for (i in seq_along(scen_labels)) {
      scen_data <- sps %>% filter(scenario_label == scen_labels[i])
      col <- colors_scen[((i - 1) %% length(colors_scen)) + 1]
      p <- p %>% add_lines(data = scen_data, x = ~shock_year, y = ~avg_pd_shock * 100,
                           line = list(color = col, width = 1.2, dash = "solid"),
                           opacity = 0.5, name = scen_labels[i],
                           legendgroup = "scenarios", showlegend = (i <= 6))
    }

    p %>% layout(
      xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
      yaxis = list(title = "Portfolio Avg PD (%)", gridcolor = tc$gridcolor),
      legend = list(orientation = "h", y = -0.2, font = list(size = 9)),
      hovermode = "x unified",
      font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
      paper_bgcolor = tc$paper_bgcolor,
      plot_bgcolor = tc$plot_bgcolor,
      hoverlabel = PLOTLY_HOVERLABEL
    ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- PD Violin / Box Plot ----
  output$dist_violin <- renderPlotly({
    tc <- plotly_theme_colors(input)
    scd <- scenario_comparison_data()
    req(scd)

    latest_yr <- max(scd$shock_year)
    violin_df <- scd %>%
      filter(shock_year == latest_yr) %>%
      mutate(pd_pct = pd_shock * 100)

    plot_ly(violin_df, x = ~scenario_label, y = ~pd_pct,
            type = "violin", box = list(visible = TRUE),
            meanline = list(visible = TRUE),
            points = "all", jitter = 0.3, pointpos = -1.5,
            marker = list(size = 4, opacity = 0.6),
            fillcolor = "rgba(245, 61, 63, 0.15)",
            line = list(color = BRAND_CORAL),
            hoverinfo = "y") %>%
      layout(xaxis = list(title = "", tickangle = -25, gridcolor = tc$gridcolor),
             yaxis = list(title = "PD Shock (%)", gridcolor = tc$gridcolor),
             showlegend = FALSE,
             margin = list(b = 100),
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- PD Exceedance Curve ----
  output$dist_exceedance <- renderPlotly({
    tc <- plotly_theme_colors(input)
    sps <- scenario_portfolio_summary()
    req(sps)

    latest_yr <- max(sps$shock_year)
    sps_yr <- sps %>% filter(shock_year == latest_yr) %>% arrange(avg_pd_shock)
    n <- nrow(sps_yr)

    if (n < 2) return(plotly_empty())

    # Build exceedance: probability of exceeding threshold x
    pd_vals <- sort(sps_yr$avg_pd_shock * 100)
    exceed_probs <- 1 - (seq_along(pd_vals) - 0.5) / n

    plot_ly() %>%
      add_lines(x = pd_vals, y = exceed_probs * 100,
                line = list(color = BRAND_CORAL, width = 3, shape = "hv"),
                fill = "tozeroy", fillcolor = "rgba(245, 61, 63, 0.12)",
                name = "Exceedance") %>%
      add_markers(x = pd_vals, y = exceed_probs * 100,
                  marker = list(color = BRAND_CORAL, size = 8),
                  text = paste0("PD > ", round(pd_vals, 3), "%: ",
                               round(exceed_probs * 100, 1), "% of scenarios"),
                  hoverinfo = "text", showlegend = FALSE) %>%
      layout(xaxis = list(title = "Portfolio Avg PD Threshold (%)", gridcolor = tc$gridcolor),
             yaxis = list(title = "Exceedance Probability (%)", range = c(0, 105), gridcolor = tc$gridcolor),
             showlegend = FALSE,
             font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
             paper_bgcolor = tc$paper_bgcolor,
             plot_bgcolor = tc$plot_bgcolor,
             hoverlabel = PLOTLY_HOVERLABEL) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- NPV Change Fan Chart ----
  output$dist_npv_fan <- renderPlotly({
    tc <- plotly_theme_colors(input)
    dq <- dist_quantiles()
    req(dq)
    sps <- scenario_portfolio_summary()

    p <- plot_ly() %>%
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~npv_p10 * 100, ymax = ~npv_p90 * 100,
                  fillcolor = "rgba(90, 142, 174, 0.15)", line = list(width = 0),
                  name = "P10\u2013P90") %>%
      add_ribbons(data = dq, x = ~shock_year,
                  ymin = ~npv_p25 * 100, ymax = ~npv_p75 * 100,
                  fillcolor = "rgba(90, 142, 174, 0.35)", line = list(width = 0),
                  name = "P25\u2013P75") %>%
      add_lines(data = dq, x = ~shock_year, y = ~npv_median * 100,
                line = list(color = STATUS_BLUE, width = 3),
                name = "Median")

    # Individual scenario traces
    scen_labels <- unique(sps$scenario_label)
    for (i in seq_along(scen_labels)) {
      scen_data <- sps %>% filter(scenario_label == scen_labels[i])
      p <- p %>% add_lines(data = scen_data, x = ~shock_year, y = ~avg_npv_change * 100,
                           line = list(width = 1, dash = "dot"),
                           opacity = 0.4, name = scen_labels[i],
                           legendgroup = "scen2", showlegend = (i <= 4))
    }

    p %>% layout(
      xaxis = list(title = "Shock Year", dtick = 5, gridcolor = tc$gridcolor),
      yaxis = list(title = "Average NPV Change (%)", gridcolor = tc$gridcolor),
      legend = list(orientation = "h", y = -0.2, font = list(size = 9)),
      hovermode = "x unified",
      font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
      paper_bgcolor = tc$paper_bgcolor,
      plot_bgcolor = tc$plot_bgcolor,
      hoverlabel = PLOTLY_HOVERLABEL
    ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Distribution Summary Statistics Table ----
  output$dist_summary_table <- renderDT({
    dq <- dist_quantiles()
    req(dq)

    display_df <- dq %>%
      mutate(
        `Shock Year` = shock_year,
        `N Scenarios` = n_scenarios,
        `PD P10 (%)` = round(pd_p10 * 100, 4),
        `PD P25 (%)` = round(pd_p25 * 100, 4),
        `PD Median (%)` = round(pd_median * 100, 4),
        `PD P75 (%)` = round(pd_p75 * 100, 4),
        `PD P90 (%)` = round(pd_p90 * 100, 4),
        `NPV P10 (%)` = round(npv_p10 * 100, 2),
        `NPV Median (%)` = round(npv_median * 100, 2),
        `NPV P90 (%)` = round(npv_p90 * 100, 2),
        `EL Median ($)` = round(el_median, 0),
        `EL P90 ($)` = round(el_p90, 0)
      ) %>%
      select(`Shock Year`, `N Scenarios`,
             `PD P10 (%)`, `PD P25 (%)`, `PD Median (%)`, `PD P75 (%)`, `PD P90 (%)`,
             `NPV P10 (%)`, `NPV Median (%)`, `NPV P90 (%)`,
             `EL Median ($)`, `EL P90 ($)`)

    datatable(display_df, rownames = FALSE, options = list(
      dom = "t", pageLength = 20,
      columnDefs = list(list(className = "dt-center", targets = "_all"))
    )) %>%
      formatStyle("PD P90 (%)", backgroundColor = styleInterval(
        c(1, 3, 5), c("#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")))
  })

  # ---- Export scenario comparison data ----
  output$download_scenario_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_scenario_comparison_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      scd <- scenario_comparison_data()
      if (!is.null(scd)) write.csv(sanitize_export(scd), file, row.names = FALSE)
    }
  )

}
