# mod_results_concentration.R
# Auto-extracted from server.R during modularization (2026-03-25)

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
    has_valid_company_name <- "company_name" %in% names(df) && any(!is.na(df$company_name))
    company_label_col <- if (has_valid_company_name) "company_name" else if ("company_id" %in% names(df)) "company_id" else NULL
    company_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        company_label = if (!is.null(company_label_col)) htmltools::htmlEscape(as.character(.data[[company_label_col]])) else paste0("Company_", row_number())
      )
    # Guard NA labels: replace with row-index fallback
    company_df$company_label <- ifelse(
      is.na(company_df$company_label) | company_df$company_label == "",
      paste0("Company_", seq_len(nrow(company_df))),
      company_df$company_label
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
        avg_pd_baseline = safe_weighted_mean(pd_baseline, exposure_value_usd),
        avg_pd_shock = safe_weighted_mean(pd_shock, exposure_value_usd),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) safe_weighted_mean(npv_change_pct, exposure_value_usd) else NA_real_,
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
          avg_pd_baseline = safe_weighted_mean(pd_baseline, exposure_value_usd),
          avg_pd_shock = safe_weighted_mean(pd_shock, exposure_value_usd),
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
          avg_pd_change = safe_weighted_mean(pd_change, exposure_value_usd),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (has_npv) safe_weighted_mean(npv_change_pct, exposure_value_usd) else NA_real_,
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
          avg_pd_change = safe_weighted_mean(pd_change, exposure_value_usd),
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
      return(div(style = "text-align: center; padding: 60px 20px; color: #999;",
        icon("th-large", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Run analysis to view concentration risk"),
        p("Results are needed to assess portfolio concentration by sector, geography, and company.",
          style = "font-size: 14px;"),
        actionLink("goto_run_concentration", "Go to Run Analysis",
                    icon = icon("play"), style = "font-size: 14px;")
      ))
    }

    # HHI traffic light
    hhi_color <- if (cd$hhi < 1500) STATUS_GREEN else if (cd$hhi < 2500) "#D4A017" else BRAND_CORAL
    hhi_label <- if (cd$hhi < 1500) "Low" else if (cd$hhi < 2500) "Moderate" else "High"
    # gini_color removed (Gini card replaced with CR10 card)
    cr5_color <- if (cd$cr5 < 50) STATUS_GREEN else if (cd$cr5 < 75) "#D4A017" else BRAND_CORAL

    tagList(
      # Header
      div(class = "section-header section-header--blue",
        fluidRow(
          column(8,
            h4(icon("th-large"), " Concentration Risk Center",
               style = "margin: 0; font-weight: 700;"),
            tags$small(paste0(cd$n_companies, " companies | ",
                             nrow(cd$sector), " sectors | ",
                             "Total exposure: $", format_number(cd$total_exposure)),
                      style = "opacity: 0.85;")
          ),
          column(4, style = "text-align: right;",
            downloadButton("download_concentration_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),
      tags$p(class = "fs-12 fg-secondary mb-8", tags$i(class = "fa fa-info-circle mr-6"), "Measures how concentrated your climate transition risk is across names and sectors. High concentration means a few exposures dominate your portfolio's risk profile."),

      # Row 1: Concentration Scorecard
      fluidRow(
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", hhi_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("HHI Index", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(format(round(cd$hhi), big.mark = ","), style = paste0("margin: 0; color: ", hhi_color, "; font-weight: 700;")),
            tags$span(hhi_label, style = paste0("font-size: 12px; font-weight: 600; color: ", hhi_color, ";"))
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", cr5_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Top-5 Concentration (CR5)", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(paste0(round(cd$cr5, 1), "%"), style = paste0("margin: 0; color: ", cr5_color, "; font-weight: 700;")),
            tags$small(paste0("Top-10: ", round(cd$cr10, 1), "%"), style = "color: #888;")
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", cr5_color,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Top-10 Concentration (CR10)", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(paste0(round(cd$cr10, 1), "%"), style = paste0("margin: 0; color: ", cr5_color, "; font-weight: 700;")),
            tags$small(paste0("# Sectors: ", nrow(cd$sector)), style = "color: #888;")
          )
        ),
        column(3,
          div(style = paste0("background: white; border-left: 4px solid ", STATUS_BLUE,
                            "; border-radius: 8px; padding: 16px; margin-bottom: 16px; text-align: center;"),
            tags$small("Max Single-Name Weight", style = "color: #666; display: block; margin-bottom: 4px;"),
            h3(paste0(round(cd$max_single, 1), "%"), style = paste0("margin: 0; color: ", STATUS_BLUE, "; font-weight: 700;")),
            tags$small(paste0(cd$n_companies, " companies total"), style = "color: #888;")
          )
        )
      ),

      # Row 2: Primary heatmap with controls + Top-10 donut
      fluidRow(
        column(8,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            fluidRow(
              column(6,
                h5(icon("th"), " Concentration Heatmap",
                   style = "font-weight: 600; margin-top: 0;")
              ),
              column(3,
                selectInput("conc_heatmap_metric", NULL,
                           choices = c("PD Change" = "pd_change",
                                      "Exposure ($)" = "exposure",
                                      "EL Change ($)" = "el_change"),
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
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("chart-pie"), " Top-10 Exposure Concentration",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Largest single-name exposures as % of portfolio.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("conc_donut", height = "370px")
          )
        )
      ),

      # Row 3: (Treemap + Lorenz removed — heatmap + donut are more analytical)
      # Row 4: Risk-return scatter moved to ui.R (static element avoids renderUI timing issue)
      # Row 5: (Dual bar removed — redundant with summary + waterfall)
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
          avg_pd_change = safe_weighted_mean(pd_change, exposure_value_usd),
          el_change = if (cd$has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          npv_change_pct = if (cd$has_npv) safe_weighted_mean(npv_change_pct, exposure_value_usd) else NA_real_,
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
      "el_change" = "el_change"
    )
    metric_label <- switch(metric,
      "pd_change" = "PD Change (pp)",
      "exposure" = "Exposure ($)",
      "el_change" = "EL Change ($)"
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
            rows[r], " / ", cols[cc],
            "\n", metric_label, ": ",
            if (metric == "pd_change") paste0(round(val * 100, 4), " pp")
            else if (metric == "exposure") paste0("$", format(round(val), big.mark = ","))
            else if (metric == "el_change") paste0("$", format(round(val), big.mark = ","))
            else paste0(round(val, 2), "%"),
            "\nCompanies: ", match_row$n_companies[1]
          )
        } else {
          text_matrix[r, cc] <- paste0(rows[r], " / ", cols[cc], "\nNo data")
        }
      }
    }

    # Color scale depends on metric
    if (metric %in% c("pd_change", "el_change")) {
      colorscale <- list(list(0, STATUS_GREEN), list(0.5, "#FFFFFF"), list(1, BRAND_CORAL))
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

    hover_text <- paste0(company_df$company_label,
             "\nSector: ", company_df$sector,
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
      company_df$company_label,
      "\nSector: ", company_df$sector,
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
  # outputOptions not needed — conc_scatter is now a static element in ui.R

  # ---- Geographic Choropleth ----
  output$conc_geo_map <- renderPlotly({
    cd <- concentration_data()
    req(cd, cd$has_geo, cd$geo)

    geo_df <- cd$geo
    if (is.null(geo_df) || nrow(geo_df) == 0) {
      return(plotly_empty() %>% layout(title = "No geography data"))
    }

    hover_text <- paste0(
      geo_df$country_iso2,
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

  # conc_dual_bar removed — redundant with summary + waterfall (see git history)

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
        write.csv(sanitize_export(export_df), file, row.names = FALSE)
      }
    }
  )

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
        tags$p("Combines PDs in the normal quantile (z-score) space using the Vasicek one-factor
               model. This preserves non-linear tail behavior \u2014 critical for regulatory capital,
               where the difference between 2% and 5% PD is much larger than 0.1% vs 0.4%."),
        tags$p(class = "fs-12 fg-secondary", tags$em("Recommended for Basel III/IV IRB banks. Consistent with the
               single-factor Gaussian copula framework underpinning regulatory capital formulas (CRR Art. 153)."))
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
      file_check <- validate_file_type(input$upload_internal_pd$datapath, input$upload_internal_pd$name)
      if (!is.null(file_check)) {
        showNotification(file_check, type = "error")
        return()
      }
      uploaded <- read.csv(input$upload_internal_pd$datapath, stringsAsFactors = FALSE)
      if (nrow(uploaded) > MAX_UPLOAD_ROWS) {
        showNotification(paste0("File exceeds row limit (", format(MAX_UPLOAD_ROWS, big.mark = ","), " max)."), type = "error")
        return()
      }
      names(uploaded) <- tolower(names(uploaded))

      # Schema + value validation
      validation_err <- validate_internal_pd_csv(uploaded)
      if (!is.null(validation_err)) {
        showNotification(paste("Validation error:", validation_err),
                        type = "error", duration = 8)
        return()
      }

      # Match by company_id (vectorized join instead of O(n^2) loop)
      df <- rv$results
      internal_pd <- if ("pd_baseline" %in% names(df)) df$pd_baseline else rep(0, nrow(df))

      uploaded_map <- stats::setNames(
        as.numeric(uploaded$internal_pd),
        as.character(uploaded$company_id)
      )
      result_ids <- as.character(df$company_id)
      matched_vals <- uploaded_map[result_ids]
      has_match <- !is.na(matched_vals)
      internal_pd[has_match] <- matched_vals[has_match]
      matched <- sum(has_match)

      rv$internal_pd <- internal_pd
      showNotification(
        paste0("Internal PDs loaded: ", matched, " of ", nrow(df), " counterparties matched."),
        type = if (matched > 0) "message" else "warning", duration = 6
      )
    }, error = function(e) {
      message(paste("ERROR (PD CSV read):", conditionMessage(e)))
      showNotification("Could not read the PD CSV file. Please check the file format.", type = "error", duration = 8)
    })
  })

  # Reset stale internal PD when results row count changes (separate observer, not inside renderUI)
  observe({
    req(rv$results, rv$internal_pd)
    if (length(rv$internal_pd) != nrow(rv$results)) {
      rv$internal_pd <- NULL
    }
  })

  # Internal PD upload status
  output$internal_pd_upload_status <- renderUI({
    if (!is.null(rv$internal_pd)) {
      if (!is.null(rv$results) && length(rv$internal_pd) != nrow(rv$results)) {
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
            tags$br(), tags$span(class = "text-muted", style = "font-size: 11px;",
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
    clrs_pd <- c("#1B4332", "#2D6A2E", "#5DAA5E", "#BAB6B5",
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
      h4(icon("chart-bar"), "Portfolio-Level PD Aggregates",
         style = "margin-top: 15px; margin-bottom: 10px; font-weight: 600;"),
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

  # Sector/Technology PD Breakdown (collapsible summary table)
  output$pd_sector_breakdown <- renderUI({
    req(rv$pd_integration_result)
    df <- rv$pd_integration_result

    grp_cols <- intersect(c("sector", "technology"), names(df))
    if (length(grp_cols) == 0) return(NULL)

    grp_col <- grp_cols[1]
    sectors <- unique(df[[grp_col]])

    breakdown_rows <- lapply(sectors, function(s) {
      sub_df <- df[df[[grp_col]] == s, , drop = FALSE]
      n <- nrow(sub_df)
      avg_internal <- mean(sub_df$Internal_PD, na.rm = TRUE)
      avg_adjusted <- mean(sub_df$TRISK_Adjusted_PD, na.rm = TRUE)
      avg_pd_adj <- mean(sub_df$PD_Adjustment, na.rm = TRUE)

      dir_icon <- if (avg_pd_adj > 1e-6) {
        tags$span(class = "text-danger", icon("arrow-up"), " ")
      } else if (avg_pd_adj < -1e-6) {
        tags$span(class = "text-success", icon("arrow-down"), " ")
      } else {
        tags$span(class = "fg-secondary", icon("minus"), " ")
      }

      tags$tr(
        tags$td(class = "td-pad fw-600", s),
        tags$td(class = "td-right", n),
        tags$td(class = "td-right", smart_round(avg_internal)),
        tags$td(class = "td-right", smart_round(avg_adjusted)),
        tags$td(class = "td-right", dir_icon, smart_round(avg_pd_adj)),
        tags$td(class = "td-right",
                paste0(smart_round(avg_pd_adj * 100), " pp"))
      )
    })

    tagList(
      tags$div(
        class = "mb-15",
        tags$details(
          tags$summary(
            class = "breakdown-summary",
            icon("layer-group"), " PD Breakdown by Sector"
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
                  tags$th(class = "th-right", "Avg Internal PD"),
                  tags$th(class = "th-right", "Avg Adjusted PD"),
                  tags$th(class = "th-right", "Avg PD Adj."),
                  tags$th(class = "th-right", "Avg PD Adj. (pp)")
                )
              ),
              tags$tbody(breakdown_rows)
            )
          )
        )
      )
    )
  })

  # PD Adjustment Bar Chart (plotly)
  output$pd_adjustment_chart <- renderPlotly({
    req(rv$pd_integration_result)
    df <- rv$pd_integration_result

    has_sector <- "sector" %in% names(df)
    has_company <- "company_name" %in% names(df)

    if (has_sector) {
      chart_df <- df %>%
        group_by(sector) %>%
        summarise(
          PD_Adjustment = mean(PD_Adjustment, na.rm = TRUE),
          n = n(),
          .groups = "drop"
        ) %>%
        arrange(PD_Adjustment)

      chart_df$color <- ifelse(chart_df$PD_Adjustment > 0, BRAND_CORAL, STATUS_GREEN)
      chart_df$hover_text <- paste0(
        "<b>", htmltools::htmlEscape(as.character(chart_df$sector)), "</b><br>",
        "Avg PD Adjustment: ", sapply(chart_df$PD_Adjustment, smart_round), "<br>",
        "Counterparties: ", chart_df$n
      )

      p <- plot_ly(chart_df,
                   y = ~reorder(sector, PD_Adjustment),
                   x = ~PD_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color),
                   text = ~hover_text,
                   hoverinfo = "text") %>%
        layout(
          title = list(text = "Avg PD Adjustment by Sector", font = list(size = 14)),
          xaxis = list(title = "PD Adjustment", zeroline = TRUE,
                       zerolinecolor = "#999", zerolinewidth = 1.5),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          margin = list(l = 120, r = 20, t = 40, b = 40),
          showlegend = FALSE
        )
    } else if (has_company) {
      chart_df <- df %>%
        arrange(desc(abs(PD_Adjustment))) %>%
        head(15) %>%
        arrange(PD_Adjustment)

      chart_df$color <- ifelse(chart_df$PD_Adjustment > 0, BRAND_CORAL, STATUS_GREEN)
      chart_df$hover_text <- paste0(
        "<b>", htmltools::htmlEscape(as.character(chart_df$company_name)), "</b><br>",
        "PD Adjustment: ", sapply(chart_df$PD_Adjustment, smart_round)
      )

      p <- plot_ly(chart_df,
                   y = ~reorder(company_name, PD_Adjustment),
                   x = ~PD_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color),
                   text = ~hover_text,
                   hoverinfo = "text") %>%
        layout(
          title = list(text = "Top 15 Counterparties by PD Adjustment", font = list(size = 14)),
          xaxis = list(title = "PD Adjustment", zeroline = TRUE,
                       zerolinecolor = "#999", zerolinewidth = 1.5),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          margin = list(l = 150, r = 20, t = 40, b = 40),
          showlegend = FALSE
        )
    } else {
      chart_df <- df %>%
        mutate(label = paste0("Row ", row_number())) %>%
        arrange(PD_Adjustment)

      chart_df$color <- ifelse(chart_df$PD_Adjustment > 0, BRAND_CORAL, STATUS_GREEN)

      p <- plot_ly(chart_df,
                   y = ~reorder(label, PD_Adjustment),
                   x = ~PD_Adjustment,
                   type = "bar",
                   orientation = "h",
                   marker = list(color = ~color)) %>%
        layout(
          title = list(text = "PD Adjustment by Counterparty", font = list(size = 14)),
          xaxis = list(title = "PD Adjustment"),
          yaxis = list(title = ""),
          plot_bgcolor = "#FEFEFE",
          paper_bgcolor = "#FEFEFE",
          showlegend = FALSE
        )
    }

    p
  })


}
