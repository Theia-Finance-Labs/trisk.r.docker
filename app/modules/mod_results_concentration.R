# mod_results_concentration.R
# Concentration Risk Center: HHI, CR5, Gini, Lorenz, heatmaps, treemap, scatter, geo map

setup_results_concentration <- function(input, output, session, rv) {
  # ============================================
  # CONCENTRATION RISK CENTER
  # ============================================

  # Reactive: concentration data — builds all aggregations needed for
  # heatmaps, treemaps, scatter, Lorenz curve, geographic view, and scorecard.
  concentration_data <- reactive({
    req(rv$results)
    df <- rv$results
    req(all(c("pd_baseline", "pd_shock", "exposure_value_usd", "sector") %in% names(df)))

    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_npv <- "crispy_perc_value_change" %in% names(df)
    has_tech <- "technology" %in% names(df)
    has_geo <- "country_iso2" %in% names(df)
    has_company <- any(c("company_name", "company_id") %in% names(df))

    total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)

    # --- Company-level enrichment ---
    company_label_col <- if (has_company && "company_name" %in% names(df)) "company_name" else "company_id"
    company_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        company_label = .data[[company_label_col]]
      )
    # Guard NA labels: replace with row-index fallback
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      paste0("Company_", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )
    if (has_el) {
      company_df <- company_df %>%
        mutate(el_change = expected_loss_shock - expected_loss_baseline)
    }
    if (has_npv) {
      company_df <- company_df %>%
        mutate(npv_change_pct = crispy_perc_value_change * 100)
    }

    # --- Concentration metrics ---
    weights <- company_df$weight
    weights <- weights[!is.na(weights)]
    weights_sorted <- sort(weights, decreasing = TRUE)
    n <- length(weights)

    hhi <- sum(weights^2, na.rm = TRUE) * 10000  # HHI on 0-10000 scale
    cr5 <- sum(head(weights_sorted, 5), na.rm = TRUE) * 100
    cr10 <- sum(head(weights_sorted, 10), na.rm = TRUE) * 100
    max_single <- if (n > 0) max(weights, na.rm = TRUE) * 100 else 0

    # Gini coefficient
    gini <- if (n > 1) {
      sorted_w <- sort(weights)
      total_w <- sum(sorted_w)
      if (total_w > 0) {
        numerator <- sum((2 * seq_along(sorted_w) - n - 1) * sorted_w)
        numerator / (n * total_w)
      } else 0
    } else 0

    # --- Sector-level aggregation ---
    sector_df <- company_df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE),
        avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
        avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(total_exposure))

    # --- Geography-level aggregation ---
    geo_df <- if (has_geo) {
      company_df %>%
        group_by(country_iso2) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
          avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
          avg_pd_change = avg_pd_shock - avg_pd_baseline,
          .groups = "drop"
        ) %>%
        arrange(desc(total_exposure))
    } else NULL

    # --- Sector × Geography cross-tab ---
    cross_df <- if (has_geo) {
      company_df %>%
        group_by(sector, country_iso2) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
    } else NULL

    # --- Sector × Technology cross-tab ---
    cross_tech_df <- if (has_tech) {
      company_df %>%
        group_by(sector, technology) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          .groups = "drop"
        )
    } else NULL

    # --- Lorenz curve data (normalized so last value = 100%) ---
    cum_exp_raw <- cumsum(sort(weights))
    cum_exp_norm <- if (length(cum_exp_raw) > 0 && cum_exp_raw[length(cum_exp_raw)] > 0) {
      cum_exp_raw / cum_exp_raw[length(cum_exp_raw)]
    } else cum_exp_raw
    lorenz_df <- data.frame(
      cum_companies = c(0, seq_len(n)) / n * 100,
      cum_exposure = c(0, cum_exp_norm) * 100
    )

    list(
      company = company_df,
      sector = sector_df,
      geo = geo_df,
      cross = cross_df,
      cross_tech = cross_tech_df,
      lorenz = lorenz_df,
      total_exposure = total_exposure,
      n_companies = n,
      hhi = hhi,
      cr5 = cr5,
      cr10 = cr10,
      max_single = max_single,
      gini = gini,
      has_el = has_el,
      has_npv = has_npv,
      has_tech = has_tech,
      has_geo = has_geo
    )
  })

  # ---- Concentration UI ----
  output$concentration_ui <- renderUI({
    cd <- concentration_data()
    if (is.null(cd)) {
      return(div(class = "empty-state",
        icon("th-large", class = "empty-state-icon"),
        h4("Run analysis to view concentration risk"),
        p("Results are needed to assess portfolio concentration by sector, geography, and company.",
          class = "fs-14"),
        actionLink("goto_run_concentration", "Go to Run Analysis",
                    icon = icon("play"), class = "fs-14")
      ))
    }

    # HHI traffic light — use CSS status classes instead of inline colors
    hhi_status <- if (cd$hhi < 1500) "metric-good" else if (cd$hhi < 2500) "metric-warning" else "metric-danger"
    hhi_label <- if (cd$hhi < 1500) "Low" else if (cd$hhi < 2500) "Moderate" else "High"
    gini_status <- if (cd$gini < 0.4) "metric-good" else if (cd$gini < 0.6) "metric-warning" else "metric-danger"
    cr5_status <- if (cd$cr5 < 50) "metric-good" else if (cd$cr5 < 75) "metric-warning" else "metric-danger"

    tagList(
      # Header
      div(class = "section-header section-header--blue",
        fluidRow(
          column(8,
            h4(icon("th-large"), " Concentration Risk Center",
               class = "section-title"),
            tags$small(paste0(cd$n_companies, " companies | ",
                             nrow(cd$sector), " sectors | ",
                             "Total exposure: $", format_number(cd$total_exposure)),
                      class = "opacity-85")
          ),
          column(4, class = "text-right",
            downloadButton("download_concentration_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),

      # Row 1: Concentration Scorecard
      # NOTE: needs CSS classes:
      # .metric-card { background: white; padding: 12px; border-radius: 8px; text-align: center; border-left: 4px solid transparent; margin-bottom: 16px; }
      # .metric-good { border-left-color: <green>; color: <green>; } (for h3 and status spans)
      # .metric-warning { border-left-color: <amber>; color: <amber>; }
      # .metric-danger { border-left-color: <red>; color: <red>; }
      # .metric-info { border-left-color: <blue>; color: <blue>; }
      # .metric-status-label { font-size: 12px; font-weight: 600; }
      fluidRow(
        column(3,
          div(class = paste("metric-card", hhi_status),
            tags$small("HHI Index", class = "card-subtitle"),
            h3(format(round(cd$hhi), big.mark = ","), class = "section-title"),
            tags$span(hhi_label, class = "metric-status-label")
          )
        ),
        column(3,
          div(class = paste("metric-card", cr5_status),
            tags$small("Top-5 Concentration (CR5)", class = "card-subtitle"),
            h3(paste0(round(cd$cr5, 1), "%"), class = "section-title"),
            tags$small(paste0("Top-10: ", round(cd$cr10, 1), "%"), class = "fg-muted")
          )
        ),
        column(3,
          div(class = paste("metric-card", gini_status),
            tags$small("Gini Coefficient", class = "card-subtitle"),
            h3(round(cd$gini, 3), class = "section-title"),
            tags$span(if (cd$gini < 0.4) "Even" else if (cd$gini < 0.6) "Moderate" else "Concentrated",
                     class = "metric-status-label")
          )
        ),
        column(3,
          div(class = "metric-card metric-info",
            tags$small("Max Single-Name Weight", class = "card-subtitle"),
            h3(paste0(round(cd$max_single, 1), "%"), class = "section-title"),
            tags$small(paste0(cd$n_companies, " companies total"), class = "fg-muted")
          )
        )
      ),

      # Row 2: Primary heatmap with controls + Top-10 donut
      fluidRow(
        column(8,
          div(class = "card-surface mb-16",
            fluidRow(
              column(6,
                h5(icon("th"), " Concentration Heatmap",
                   class = "fw-600 mt-0")
              ),
              column(3,
                selectInput("conc_heatmap_metric", NULL,
                           choices = c("PD Change" = "pd_change",
                                      "Exposure ($)" = "exposure",
                                      "EL Change ($)" = "el_change",
                                      "NPV Change (%)" = "npv_change"),
                           selected = "pd_change",
                           width = "100%")
              ),
              column(3,
                selectInput("conc_heatmap_dims", NULL,
                           choices = c("Sector \u00D7 Geography" = "sector_geo",
                                      "Sector \u00D7 Technology" = "sector_tech",
                                      "Sector \u00D7 Company" = "sector_company"),
                           selected = "sector_geo",
                           width = "100%")
              )
            ),
            plotlyOutput("conc_heatmap", height = "420px")
          )
        ),
        column(4,
          div(class = "card-surface mb-16",
            h5(icon("chart-pie"), " Top-10 Exposure Concentration",
               class = "fw-600 mt-0"),
            tags$small("Largest single-name exposures as % of portfolio.",
                      class = "card-subtitle"),
            plotlyOutput("conc_donut", height = "370px")
          )
        )
      ),

      # Row 3: Treemap + Lorenz curve
      fluidRow(
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("project-diagram"), " Exposure Treemap (Sector \u2192 Company)",
               class = "fw-600 mt-0"),
            tags$small("Size = exposure weight, color = PD change magnitude.",
                      class = "card-subtitle"),
            plotlyOutput("conc_treemap", height = "400px")
          )
        ),
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("chart-area"), " Lorenz Curve (Exposure Concentration)",
               class = "fw-600 mt-0"),
            tags$small(paste0("Gini = ", round(cd$gini, 3),
                             " | Perfect equality = 0, maximum concentration = 1."),
                      class = "card-subtitle"),
            plotlyOutput("conc_lorenz", height = "400px")
          )
        )
      ),

      # Row 4: Risk-return scatter + Geographic choropleth
      fluidRow(
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("bullseye"), " Risk-Return Scatter",
               class = "fw-600 mt-0"),
            tags$small("Each bubble = one company. x = PD change, y = exposure weight, size = abs EL change.",
                      class = "card-subtitle"),
            plotlyOutput("conc_scatter", height = "400px")
          )
        ),
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("globe"), " Geographic Concentration",
               class = "fw-600 mt-0"),
            tags$small("Country-level exposure and PD change.",
                      class = "card-subtitle"),
            plotlyOutput("conc_geo_map", height = "400px")
          )
        )
      ),

      # Row 5: Dual-axis baseline vs shocked concentration
      fluidRow(
        column(12,
          div(class = "card-surface mb-16",
            h5(icon("columns"), " Concentration Shift: Baseline vs Shocked",
               class = "fw-600 mt-0"),
            tags$small("Side-by-side sector concentration before and after the transition shock.",
                      class = "card-subtitle"),
            plotlyOutput("conc_dual_bar", height = "380px")
          )
        )
      )
    )
  })

  # ---- Concentration Heatmap (dynamic dimensions + metrics) ----
  output$conc_heatmap <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    metric <- input$conc_heatmap_metric
    dims <- input$conc_heatmap_dims

    # Select cross-tab data based on chosen dimensions
    if (dims == "sector_geo") {
      if (!isTRUE(cd$has_geo) || is.null(cd$cross)) {
        return(plotly_empty() %>%
          layout(title = list(text = "No geography data — try Sector \u00D7 Technology or Sector \u00D7 Company",
                             font = list(size = 13, color = "#999"))))
      }
      cross <- cd$cross
      row_col <- "country_iso2"
      row_label <- "Geography"
    } else if (dims == "sector_tech") {
      if (!isTRUE(cd$has_tech) || is.null(cd$cross_tech)) {
        return(plotly_empty() %>%
          layout(title = list(text = "No technology data — try Sector \u00D7 Geography or Sector \u00D7 Company",
                             font = list(size = 13, color = "#999"))))
      }
      cross <- cd$cross_tech
      row_col <- "technology"
      row_label <- "Technology"
    } else {
      # sector_company fallback: always available
      cross <- cd$company %>%
        group_by(sector, company_label) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          el_change = if (cd$has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (cd$has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        )
      # Limit to top 20 companies for readability
      top_companies <- cd$company %>%
        arrange(desc(exposure_value_usd)) %>%
        head(20) %>%
        pull(company_label)
      cross <- cross %>% filter(company_label %in% top_companies)
      row_col <- "company_label"
      row_label <- "Company"
    }
    if (is.null(cross) || nrow(cross) == 0) {
      return(plotly_empty() %>% layout(title = "No data for selected dimensions"))
    }

    rows <- sort(unique(cross[[row_col]]))
    cols <- sort(unique(cross$sector))

    # Select metric
    metric_col <- switch(metric,
      "pd_change" = "avg_pd_change",
      "exposure" = "total_exposure",
      "el_change" = "el_change",
      "npv_change" = "npv_change_pct"
    )
    metric_label <- switch(metric,
      "pd_change" = "PD Change (pp)",
      "exposure" = "Exposure ($)",
      "el_change" = "EL Change ($)",
      "npv_change" = "NPV Change (%)"
    )

    # Check if metric column exists
    if (!metric_col %in% names(cross)) {
      return(plotly_empty() %>% layout(title = paste("Metric", metric_label, "not available")))
    }

    # Build matrix
    z_matrix <- matrix(NA, nrow = length(rows), ncol = length(cols))
    text_matrix <- matrix("", nrow = length(rows), ncol = length(cols))

    for (r in seq_along(rows)) {
      for (cc in seq_along(cols)) {
        match_row <- cross %>%
          filter(.data[[row_col]] == rows[r], sector == cols[cc])
        if (nrow(match_row) > 0) {
          val <- match_row[[metric_col]][1]
          z_matrix[r, cc] <- if (metric == "pd_change") val * 100 else val
          text_matrix[r, cc] <- paste0(
            htmltools::htmlEscape(as.character(rows[r])), " / ", htmltools::htmlEscape(as.character(cols[cc])),
            "\n", metric_label, ": ",
            if (metric == "pd_change") paste0(round(val * 100, 4), " pp")
            else if (metric == "exposure") paste0("$", format(round(val), big.mark = ","))
            else if (metric == "el_change") paste0("$", format(round(val), big.mark = ","))
            else paste0(round(val, 2), "%"),
            "\nCompanies: ", match_row$n_companies[1]
          )
        } else {
          text_matrix[r, cc] <- paste0(htmltools::htmlEscape(as.character(rows[r])), " / ", htmltools::htmlEscape(as.character(cols[cc])), "\nNo data")
        }
      }
    }

    # Color scale depends on metric
    if (metric %in% c("pd_change", "el_change")) {
      colorscale <- list(list(0, STATUS_GREEN), list(0.5, "#FFFFFF"), list(1, BRAND_CORAL))
      use_zmid <- 0
    } else if (metric == "npv_change") {
      colorscale <- list(list(0, BRAND_CORAL), list(0.5, "#FFFFFF"), list(1, STATUS_GREEN))
      use_zmid <- 0
    } else {
      # Exposure: sequential scale
      colorscale <- list(list(0, "#FBF5F2"), list(0.5, "#5A8EAE"), list(1, "#1A4D6E"))
      use_zmid <- NULL
    }

    trace_args <- list(
      x = cols,
      y = rows,
      z = z_matrix,
      type = "heatmap",
      colorscale = colorscale,
      text = text_matrix,
      hoverinfo = "text",
      colorbar = list(title = metric_label, len = 0.6)
    )
    if (!is.null(use_zmid)) trace_args$zmid <- use_zmid

    do.call(plot_ly, trace_args) %>%
      layout(
        xaxis = list(title = "Sector", tickangle = -30, tickfont = list(size = 10)),
        yaxis = list(title = row_label, tickfont = list(size = 10)),
        margin = list(l = 80, b = 80, t = 10)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Top-10 Exposure Donut ----
  output$conc_donut <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    n_top <- min(10, nrow(cd$company))
    company_df <- cd$company %>%
      arrange(desc(weight)) %>%
      head(n_top)

    # Guard against NA labels — replace with company_id or index
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      if ("company_id" %in% names(company_df)) paste0("ID: ", company_df$company_id)
      else paste0("Company ", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )

    other_weight <- max(0, 1 - sum(company_df$weight, na.rm = TRUE))
    n_others <- cd$n_companies - n_top

    # Build labels/values — only add "Others" slice if there are remaining companies
    if (n_others > 0 && other_weight > 0.001) {
      labels <- c(as.character(company_df$company_label), "Others")
      values <- c(company_df$weight * 100, other_weight * 100)
    } else {
      labels <- as.character(company_df$company_label)
      values <- company_df$weight * 100
    }

    palette <- c(BRAND_CORAL, BRAND_CORAL_LT, STATUS_BLUE, "#5A8EAE", "#7BADC4",
                 STATUS_GREEN, "#8BB85F", "#A8C97B", "#D4A017", "#E8C547")
    colors <- if (n_others > 0 && other_weight > 0.001) {
      c(rep(palette, length.out = n_top), "#CCCCCC")
    } else {
      rep(palette, length.out = n_top)
    }

    hover_text <- paste0(htmltools::htmlEscape(as.character(company_df$company_label)),
             "\nSector: ", htmltools::htmlEscape(as.character(company_df$sector)),
             "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
             "\nWeight: ", round(company_df$weight * 100, 1), "%",
             "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp")
    if (n_others > 0 && other_weight > 0.001) {
      hover_text <- c(hover_text,
        paste0("Others\n", n_others, " companies\nWeight: ", round(other_weight * 100, 1), "%"))
    }

    plot_ly(
      labels = labels,
      values = values,
      type = "pie",
      hole = 0.5,
      marker = list(colors = colors, line = list(color = "white", width = 1.5)),
      text = hover_text,
      hoverinfo = "text",
      textinfo = "label+percent",
      textfont = list(size = 10),
      insidetextorientation = "radial"
    ) %>%
      layout(
        showlegend = FALSE,
        margin = list(l = 10, r = 10, t = 10, b = 10),
        annotations = list(
          list(text = paste0("<b>Top ", n_top, "</b><br>", round(sum(company_df$weight, na.rm = TRUE) * 100, 1), "%"),
               x = 0.5, y = 0.5, font = list(size = 14, color = "#333"),
               showarrow = FALSE)
        )
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Exposure Treemap (Sector -> Company) ----
  output$conc_treemap <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    # Take top 30 companies by exposure for readability
    top_co <- cd$company %>%
      arrange(desc(exposure_value_usd)) %>%
      head(30)

    if (nrow(top_co) == 0) {
      return(plotly_empty() %>% layout(title = "No company data available"))
    }

    # Color by PD change magnitude (diverging)
    pd_range <- max(abs(top_co$pd_change), na.rm = TRUE)
    if (is.na(pd_range) || pd_range == 0) pd_range <- 1  # avoid division by zero

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(top_co$company_label)),
      "\nSector: ", htmltools::htmlEscape(as.character(top_co$sector)),
      "\nExposure: $", format(round(top_co$exposure_value_usd), big.mark = ","),
      "\nWeight: ", round(top_co$weight * 100, 1), "%",
      "\nPD Change: ", round(top_co$pd_change * 100, 4), " pp"
    )

    # Numeric color: -1 (green) to +1 (red)
    color_vals <- top_co$pd_change / pd_range

    # Ensure labels are non-NA strings for treemap
    treemap_labels <- ifelse(is.na(top_co$company_label),
                             paste0("Company_", seq_len(nrow(top_co))),
                             as.character(top_co$company_label))

    # Plotly treemap requires parent nodes to exist as entries in the labels array.
    # Add sector root nodes (with parent = "") so the hierarchy renders.
    sectors <- unique(top_co$sector)
    sector_weights <- sapply(sectors, function(s) {
      sum(top_co$weight[top_co$sector == s], na.rm = TRUE) * 100
    })
    sector_pd <- sapply(sectors, function(s) {
      rows <- top_co[top_co$sector == s, ]
      weighted.mean(rows$pd_change, rows$exposure_value_usd, na.rm = TRUE)
    })
    sector_color_vals <- sector_pd / pd_range

    sector_hover <- paste0(
      htmltools::htmlEscape(as.character(sectors)),
      "\nTotal weight: ", round(sector_weights, 1), "%",
      "\nAvg PD Change: ", round(sector_pd * 100, 4), " pp"
    )

    # Combine: sector roots first, then company leaves
    all_labels <- c(sectors, treemap_labels)
    all_parents <- c(rep("", length(sectors)), as.character(top_co$sector))
    all_values <- c(rep(0, length(sectors)), top_co$weight * 100)  # sector values=0 so they don't double-count
    all_colors <- c(sector_color_vals, color_vals)
    all_text <- c(sector_hover, hover_text)

    plot_ly(
      labels = all_labels,
      parents = all_parents,
      values = all_values,
      type = "treemap",
      branchvalues = "remainder",
      marker = list(
        color = all_colors,
        colorscale = list(list(0, STATUS_GREEN), list(0.5, "#FFFFCC"), list(1, BRAND_CORAL)),
        cmid = 0,
        colorbar = list(title = "PD Change\n(norm.)", len = 0.5),
        line = list(color = "white", width = 1)
      ),
      text = all_text,
      hoverinfo = "text",
      textinfo = "label+value",
      texttemplate = "%{label}<br>%{value:.1f}%",
      textfont = list(size = 10)
    ) %>%
      layout(
        margin = list(l = 5, r = 5, t = 5, b = 5)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Lorenz Curve ----
  output$conc_lorenz <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    lorenz <- cd$lorenz

    plot_ly() %>%
      # Equality line
      add_trace(
        x = c(0, 100), y = c(0, 100),
        type = "scatter", mode = "lines",
        line = list(color = "#CCCCCC", width = 1.5, dash = "dash"),
        name = "Perfect Equality",
        hoverinfo = "skip"
      ) %>%
      # Lorenz curve
      add_trace(
        x = lorenz$cum_companies,
        y = lorenz$cum_exposure,
        type = "scatter", mode = "lines",
        line = list(color = STATUS_BLUE, width = 2.5),
        fill = "tozeroy",
        fillcolor = "rgba(90,142,174,0.15)",
        name = "Portfolio",
        hovertemplate = paste0(
          "Bottom %{x:.1f}% of companies<br>",
          "Hold %{y:.1f}% of exposure<extra></extra>"
        )
      ) %>%
      # Gini annotation
      add_annotations(
        x = 70, y = 25,
        text = paste0("<b>Gini = ", round(cd$gini, 3), "</b>"),
        showarrow = FALSE,
        font = list(size = 16, color = STATUS_BLUE)
      ) %>%
      layout(
        xaxis = list(title = "Cumulative % of Companies (smallest first)",
                     range = c(0, 100), ticksuffix = "%"),
        yaxis = list(title = "Cumulative % of Exposure",
                     range = c(0, 100), ticksuffix = "%"),
        showlegend = TRUE,
        legend = list(x = 0.05, y = 0.95),
        margin = list(l = 60, r = 20, t = 10, b = 60)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Risk-Return Scatter (bubble) ----
  output$conc_scatter <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    company_df <- cd$company

    # Compute bubble sizes — normalize to visible range [4, 20] pixels
    bubble_raw <- if (cd$has_el) {
      abs(company_df$el_change)
    } else {
      company_df$exposure_value_usd / cd$total_exposure * 100
    }
    bubble_raw[is.na(bubble_raw)] <- 0
    bmax <- max(bubble_raw, na.rm = TRUE)
    bubble_size <- if (bmax > 0) {
      (bubble_raw / bmax) * 16 + 4  # range [4, 20]
    } else {
      rep(8, length(bubble_raw))  # default 8px if all zeros
    }
    size_label <- if (cd$has_el) "Abs EL Change" else "Weight (%)"

    colors <- ifelse(company_df$pd_change >= 0, BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(company_df$company_label)),
      "\nSector: ", htmltools::htmlEscape(as.character(company_df$sector)),
      "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp",
      "\nWeight: ", round(company_df$weight * 100, 2), "%",
      "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
      if (cd$has_el) paste0("\nEL Change: $", format(round(company_df$el_change), big.mark = ",")) else ""
    )

    plot_ly(
      x = company_df$pd_change * 100,
      y = company_df$weight * 100,
      type = "scatter",
      mode = "markers",
      marker = list(
        size = bubble_size,  # already scaled to [4, 20] range
        color = colors,
        opacity = 0.7,
        line = list(color = "white", width = 0.5)
      ),
      text = hover_text,
      hoverinfo = "text"
    ) %>%
      layout(
        xaxis = list(title = "PD Change (pp)", zeroline = TRUE,
                     zerolinecolor = "#999", zerolinewidth = 1),
        yaxis = list(title = "Exposure Weight (%)", zeroline = FALSE),
        margin = list(l = 60, r = 20, t = 10, b = 60),
        showlegend = FALSE,
        # Add quadrant shading annotation
        shapes = list(
          list(type = "rect", x0 = 0, x1 = 100, y0 = 0, y1 = 100,
               fillcolor = "rgba(232,75,77,0.05)", line = list(width = 0),
               layer = "below"),
          list(type = "rect", x0 = -100, x1 = 0, y0 = 0, y1 = 100,
               fillcolor = "rgba(107,159,59,0.05)", line = list(width = 0),
               layer = "below")
        ),
        annotations = list(
          list(x = 0.95, y = 0.95, xref = "paper", yref = "paper",
               text = "High exposure +\nPD increase", showarrow = FALSE,
               font = list(size = 10, color = BRAND_CORAL), opacity = 0.6),
          list(x = 0.05, y = 0.95, xref = "paper", yref = "paper",
               text = "High exposure +\nPD decrease", showarrow = FALSE,
               font = list(size = 10, color = STATUS_GREEN), opacity = 0.6)
        )
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Geographic Choropleth ----
  output$conc_geo_map <- renderPlotly({
    cd <- concentration_data()
    req(cd, cd$has_geo, cd$geo)

    geo_df <- cd$geo
    if (is.null(geo_df) || nrow(geo_df) == 0) {
      return(plotly_empty() %>% layout(title = "No geography data"))
    }

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(geo_df$country_iso2)),
      "\nCompanies: ", geo_df$n_companies,
      "\nExposure: $", format(round(geo_df$total_exposure), big.mark = ","),
      "\nWeight: ", round(geo_df$weight * 100, 1), "%",
      "\nAvg PD Change: ", round(geo_df$avg_pd_change * 100, 4), " pp"
    )

    # ISO-2 to ISO-3 conversion for plotly choropleth
    iso2_to_iso3 <- c(
      AF="AFG",AL="ALB",DZ="DZA",AD="AND",AO="AGO",AG="ATG",AR="ARG",AM="ARM",
      AU="AUS",AT="AUT",AZ="AZE",BS="BHS",BH="BHR",BD="BGD",BB="BRB",BY="BLR",
      BE="BEL",BZ="BLZ",BJ="BEN",BT="BTN",BO="BOL",BA="BIH",BW="BWA",BR="BRA",
      BN="BRN",BG="BGR",BF="BFA",BI="BDI",KH="KHM",CM="CMR",CA="CAN",CF="CAF",
      TD="TCD",CL="CHL",CN="CHN",CO="COL",KM="COM",CD="COD",CG="COG",CR="CRI",
      CI="CIV",HR="HRV",CU="CUB",CY="CYP",CZ="CZE",DK="DNK",DJ="DJI",DM="DMA",
      DO="DOM",EC="ECU",EG="EGY",SV="SLV",GQ="GNQ",ER="ERI",EE="EST",ET="ETH",
      FJ="FJI",FI="FIN",FR="FRA",GA="GAB",GM="GMB",GE="GEO",DE="DEU",GH="GHA",
      GR="GRC",GT="GTM",GN="GIN",GW="GNB",GY="GUY",HT="HTI",HN="HND",HU="HUN",
      IS="ISL",IN="IND",ID="IDN",IR="IRN",IQ="IRQ",IE="IRL",IL="ISR",IT="ITA",
      JM="JAM",JP="JPN",JO="JOR",KZ="KAZ",KE="KEN",KW="KWT",KG="KGZ",LA="LAO",
      LV="LVA",LB="LBN",LS="LSO",LR="LBR",LY="LBY",LT="LTU",LU="LUX",MG="MDG",
      MW="MWI",MY="MYS",MV="MDV",ML="MLI",MT="MLT",MR="MRT",MU="MUS",MX="MEX",
      MD="MDA",MN="MNG",ME="MNE",MA="MAR",MZ="MOZ",MM="MMR","NA"="NAM",NP="NPL",
      NL="NLD",NZ="NZL",NI="NIC",NE="NER",NG="NGA",NO="NOR",OM="OMN",PK="PAK",
      PA="PAN",PG="PNG",PY="PRY",PE="PER",PH="PHL",PL="POL",PT="PRT",QA="QAT",
      RO="ROU",RU="RUS",RW="RWA",SA="SAU",SN="SEN",RS="SRB",SL="SLE",SG="SGP",
      SK="SVK",SI="SVN",SO="SOM",ZA="ZAF",KR="KOR",SS="SSD",ES="ESP",LK="LKA",
      SD="SDN",SR="SUR",SZ="SWZ",SE="SWE",CH="CHE",SY="SYR",TW="TWN",TJ="TJK",
      TZ="TZA",TH="THA",TL="TLS",TG="TGO",TT="TTO",TN="TUN",TR="TUR",TM="TKM",
      UG="UGA",UA="UKR",AE="ARE",GB="GBR",US="USA",UY="URY",UZ="UZB",VE="VEN",
      VN="VNM",YE="YEM",ZM="ZMB",ZW="ZWE"
    )

    geo_df$iso3 <- ifelse(
      toupper(geo_df$country_iso2) %in% names(iso2_to_iso3),
      iso2_to_iso3[toupper(geo_df$country_iso2)],
      geo_df$country_iso2  # fallback: pass as-is
    )

    # For small country counts (<=5), use a bar chart for clarity
    # (small countries like DE are nearly invisible on world choropleth)
    if (nrow(geo_df) <= 5) {
      geo_df <- geo_df %>% arrange(desc(total_exposure))
      bar_colors <- ifelse(geo_df$avg_pd_change >= 0, BRAND_CORAL, STATUS_GREEN)
      plot_ly() %>%
        add_bars(
          x = geo_df$country_iso2,
          y = geo_df$total_exposure,
          name = "Exposure ($)",
          marker = list(color = STATUS_BLUE, opacity = 0.6),
          hovertemplate = paste0(
            "%{x}<br>Exposure: $%{y:,.0f}",
            "<br>Weight: ", round(geo_df$weight * 100, 1), "%",
            "<br>Companies: ", geo_df$n_companies, "<extra></extra>"
          )
        ) %>%
        add_bars(
          x = geo_df$country_iso2,
          y = geo_df$avg_pd_change * 100,
          name = "PD Change (pp)",
          yaxis = "y2",
          marker = list(color = bar_colors, opacity = 0.8),
          hovertemplate = paste0(
            "%{x}<br>PD Change: %{y:.4f} pp<extra></extra>"
          )
        ) %>%
        layout(
          barmode = "group",
          xaxis = list(title = "Country"),
          yaxis = list(title = "Exposure ($)", side = "left", tickformat = "$,.0f"),
          yaxis2 = list(title = "PD Change (pp)", side = "right", overlaying = "y",
                        tickformat = ".4f"),
          legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
          margin = list(t = 50, b = 50),
          annotations = list(
            list(text = paste0(nrow(geo_df), " countries in portfolio"),
                 xref = "paper", yref = "paper", x = 0.5, y = -0.15,
                 showarrow = FALSE, font = list(size = 11, color = "#999"))
          )
        ) %>%
        config(displayModeBar = FALSE)
    } else {
      # Full choropleth for many countries
      plot_ly(
        type = "choropleth",
        locations = geo_df$iso3,
        locationmode = "ISO-3",
        z = geo_df$avg_pd_change * 100,
        text = hover_text,
        hoverinfo = "text",
        colorscale = list(list(0, STATUS_GREEN), list(0.5, "#FFFFCC"), list(1, BRAND_CORAL)),
        zmid = 0,
        colorbar = list(title = "PD Change\n(pp)", len = 0.6),
        marker = list(line = list(color = "#333", width = 1))
      ) %>%
        layout(
          geo = list(
            showframe = FALSE,
            showcoastlines = TRUE,
            coastlinecolor = "#DDD0D4",
            projection = list(type = "natural earth"),
            fitbounds = "locations",
            bgcolor = "#FAFAFA"
          ),
          margin = list(l = 0, r = 0, t = 10, b = 0)
        ) %>%
        config(displayModeBar = FALSE)
    }
  })

  # ---- Dual Bar: Baseline vs Shocked concentration ----
  output$conc_dual_bar <- renderPlotly({
    cd <- concentration_data()
    req(cd)

    sector_df <- cd$sector

    # Baseline concentration: exposure weighted by baseline PD
    # Shocked concentration: exposure weighted by shock PD
    # Show both side by side as grouped bar
    baseline_weighted <- sector_df$avg_pd_baseline * sector_df$weight * 100
    shock_weighted <- sector_df$avg_pd_shock * sector_df$weight * 100

    plot_ly(x = sector_df$sector) %>%
      add_bars(
        y = baseline_weighted,
        name = "Baseline (weighted PD \u00D7 weight)",
        marker = list(color = STATUS_BLUE, opacity = 0.7),
        hovertemplate = paste0(
          "%{x}<br>",
          "Baseline contribution: %{y:.4f} pp<br>",
          "Avg PD: ", round(sector_df$avg_pd_baseline * 100, 4), " pp<br>",
          "Weight: ", round(sector_df$weight * 100, 1), "%<extra></extra>"
        )
      ) %>%
      add_bars(
        y = shock_weighted,
        name = "Shocked (weighted PD \u00D7 weight)",
        marker = list(color = BRAND_CORAL, opacity = 0.7),
        hovertemplate = paste0(
          "%{x}<br>",
          "Shock contribution: %{y:.4f} pp<br>",
          "Avg PD: ", round(sector_df$avg_pd_shock * 100, 4), " pp<br>",
          "Weight: ", round(sector_df$weight * 100, 1), "%<extra></extra>"
        )
      ) %>%
      layout(
        barmode = "group",
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11)),
        yaxis = list(title = "PD Contribution (pp = avg PD \u00D7 exposure weight)"),
        legend = list(orientation = "h", y = 1.12, x = 0.5, xanchor = "center"),
        margin = list(b = 80, t = 50)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Export Concentration CSV ----
  output$download_concentration_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_concentration_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      cd <- concentration_data()
      if (!is.null(cd)) {
        export_df <- cd$company %>%
          select(any_of(c("company_label", "company_id", "sector", "technology",
                         "country_iso2", "exposure_value_usd", "weight",
                         "pd_baseline", "pd_shock", "pd_change",
                         "el_change", "npv_change_pct")))
        # Add concentration metrics as attributes in separate rows at top
        metrics_row <- data.frame(
          company_label = c("## CONCENTRATION METRICS ##"),
          company_id = c(NA),
          sector = c(paste0("HHI=", round(cd$hhi), " | CR5=", round(cd$cr5, 1),
                           "% | CR10=", round(cd$cr10, 1), "% | Gini=", round(cd$gini, 3))),
          stringsAsFactors = FALSE
        )
        write.csv(export_df, file, row.names = FALSE)
      }
    }
  )
}
