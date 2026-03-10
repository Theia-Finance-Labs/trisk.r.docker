# mod_results_attribution.R
# Attribution / Waterfall Dashboard: risk decomposition by company, sector, technology

setup_results_attribution <- function(input, output, session, rv) {

  # Reactive: attribution data — builds the decomposition of PD/NPV/EL changes
  # by sector, technology, and company, using the primary results (first scenario, first year).
  # For multi-scenario runs, we also build cross-scenario decompositions.
  attribution_data <- reactive({
    req(rv$results)
    df <- rv$results
    req(all(c("pd_baseline", "pd_shock", "exposure_value_usd", "sector") %in% names(df)))

    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_npv <- "crispy_perc_value_change" %in% names(df)
    has_tech <- "technology" %in% names(df)
    has_company <- any(c("company_name", "company_id") %in% names(df))

    total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)

    # --- Company-level attribution ---
    company_label_col <- if (has_company && "company_name" %in% names(df)) "company_name" else "company_id"
    company_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        pd_contribution = pd_change * weight,  # weighted contribution to portfolio PD change
        company_label = .data[[company_label_col]]
      )
    # Guard NA labels
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

    # --- Sector-level attribution ---
    sector_df <- company_df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE),
        avg_pd_baseline = weighted.mean(pd_baseline, exposure_value_usd, na.rm = TRUE),
        avg_pd_shock = weighted.mean(pd_shock, exposure_value_usd, na.rm = TRUE),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        pd_contribution = sum(pd_contribution, na.rm = TRUE),
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) weighted.mean(npv_change_pct, exposure_value_usd, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(abs(pd_contribution)))

    # --- Technology-level attribution (if available) ---
    tech_df <- if (has_tech) {
      company_df %>%
        group_by(sector, technology) %>%
        summarise(
          n_companies = n(),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = weighted.mean(pd_change, exposure_value_usd, na.rm = TRUE),
          pd_contribution = sum(pd_contribution, na.rm = TRUE),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(desc(abs(pd_contribution)))
    } else NULL

    # --- Portfolio-level waterfall steps ---
    portfolio_pd_baseline <- weighted.mean(df$pd_baseline, df$exposure_value_usd, na.rm = TRUE)
    portfolio_pd_shock <- weighted.mean(df$pd_shock, df$exposure_value_usd, na.rm = TRUE)
    portfolio_pd_change <- portfolio_pd_shock - portfolio_pd_baseline

    list(
      company = company_df,
      sector = sector_df,
      tech = tech_df,
      portfolio_pd_baseline = portfolio_pd_baseline,
      portfolio_pd_shock = portfolio_pd_shock,
      portfolio_pd_change = portfolio_pd_change,
      total_exposure = total_exposure,
      has_el = has_el,
      has_npv = has_npv,
      has_tech = has_tech
    )
  })

  # ---- Attribution Dashboard UI ----
  output$attribution_ui <- renderUI({
    ad <- attribution_data()
    if (is.null(ad)) {
      return(div(class = "empty-state",
        icon("chart-bar", class = "empty-state-icon"),
        h4("Run analysis to view attribution breakdown"),
        p("Results are needed to decompose risk changes by sector, technology, and company.",
          class = "fs-14"),
        actionLink("goto_run_attribution", "Go to Run Analysis",
                    icon = icon("play"), class = "fs-14")
      ))
    }

    n_sectors <- nrow(ad$sector)
    n_co <- nrow(ad$company)

    tagList(
      # Header
      div(class = "section-header section-header--green",
        fluidRow(
          column(8,
            h4(icon("chart-bar"), " Risk Attribution Dashboard",
               class = "section-title"),
            tags$small(paste0(n_sectors, " sectors | ", n_co, " companies | ",
                             "Portfolio PD change: ", round(ad$portfolio_pd_change * 100, 4), " pp"),
                      class = "opacity-85")
          ),
          column(4, class = "text-right",
            downloadButton("download_attribution_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),

      # Row 1: Waterfall chart (top-down PD decomposition)
      fluidRow(
        column(12,
          div(class = "card-surface mb-16",
            h5(icon("exchange-alt"), " PD Waterfall: Baseline \u2192 Sector Contributions \u2192 Shocked PD",
               class = "fw-600 mt-0"),
            tags$small("Each bar shows how much each sector contributes to the total portfolio PD change (exposure-weighted).",
                      class = "card-subtitle"),
            plotlyOutput("attr_waterfall", height = "400px")
          )
        )
      ),

      # Row 2: Company movers + Sector×Driver heatmap
      fluidRow(
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("sort-amount-down"), " Top Risk Movers (Companies)",
               class = "fw-600 mt-0"),
            tags$small("Companies with the largest marginal contribution to portfolio PD change.",
                      class = "card-subtitle"),
            plotlyOutput("attr_company_movers", height = "380px")
          )
        ),
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("chart-pie"), " Marginal Contribution to Portfolio Risk",
               class = "fw-600 mt-0"),
            tags$small("Each company's share of the total portfolio PD change (absolute).",
                      class = "card-subtitle"),
            plotlyOutput("attr_marginal_contribution", height = "380px")
          )
        )
      ),

      # Row 3: Sector×Technology heatmap + EL waterfall
      fluidRow(
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("th"), " Sector \u00D7 Technology Attribution",
               class = "fw-600 mt-0"),
            tags$small("PD contribution by sector and technology combination.",
                      class = "card-subtitle"),
            plotlyOutput("attr_sector_tech_heatmap", height = "380px")
          )
        ),
        column(6,
          div(class = "card-surface mb-16",
            h5(icon("money-bill-wave"), " Expected Loss Attribution by Sector",
               class = "fw-600 mt-0"),
            tags$small("Change in expected loss (EL) decomposed by sector.",
                      class = "card-subtitle"),
            plotlyOutput("attr_el_waterfall", height = "380px")
          )
        )
      ),

      # Row 4: Attribution summary table
      fluidRow(
        column(12,
          div(class = "card-surface mb-16",
            h5(icon("table"), " Detailed Attribution Table",
               class = "fw-600 mt-0"),
            DTOutput("attr_detail_table")
          )
        )
      )
    )
  })

  # ---- Waterfall Chart: Baseline PD -> Sector contributions -> Shocked PD ----
  output$attr_waterfall <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    sector_df <- ad$sector
    baseline <- ad$portfolio_pd_baseline * 100  # convert to percentage points
    shocked <- ad$portfolio_pd_shock * 100

    # Build waterfall data: start with baseline, add sector contributions, end with total
    wf_labels <- c("Portfolio\nBaseline PD", sector_df$sector, "Portfolio\nShocked PD")
    wf_values <- c(baseline, sector_df$pd_contribution * 100, shocked)
    wf_measure <- c("absolute", rep("relative", nrow(sector_df)), "total")

    # Colors: baseline=blue, positive contributions=red, negative=green, total=dark
    wf_colors <- c(STATUS_BLUE)
    for (v in sector_df$pd_contribution) {
      wf_colors <- c(wf_colors, if (v > 0) BRAND_CORAL else STATUS_GREEN)
    }
    wf_colors <- c(wf_colors, "#1A1A1A")

    # Hover text
    wf_text <- c(
      paste0("Baseline PD: ", round(baseline, 4), "%"),
      paste0(htmltools::htmlEscape(as.character(sector_df$sector)),
             "\nContribution: ", ifelse(sector_df$pd_contribution >= 0, "+", ""),
             round(sector_df$pd_contribution * 100, 4), " pp",
             "\nWeight: ", round(sector_df$weight * 100, 1), "%",
             "\nCompanies: ", sector_df$n_companies),
      paste0("Shocked PD: ", round(shocked, 4), "%",
             "\nTotal change: ", ifelse(shocked - baseline >= 0, "+", ""),
             round(shocked - baseline, 4), " pp")
    )

    plot_ly(
      x = wf_labels,
      y = wf_values,
      type = "waterfall",
      measure = wf_measure,
      text = wf_text,
      hoverinfo = "text",
      connector = list(line = list(color = "#DDD0D4", width = 1, dash = "dot")),
      increasing = list(marker = list(color = BRAND_CORAL)),
      decreasing = list(marker = list(color = STATUS_GREEN)),
      totals = list(marker = list(color = "#1A1A1A")),
      textposition = "outside",
      texttemplate = "%{y:.4f}"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11),
                     categoryorder = "array", categoryarray = wf_labels),
        yaxis = list(title = "Portfolio PD (%)", tickformat = ".4f"),
        margin = list(b = 100, t = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Top Company Risk Movers (horizontal bar chart) ----
  output$attr_company_movers <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    company_df <- ad$company %>%
      arrange(desc(abs(pd_contribution))) %>%
      head(15)

    # Sort by contribution for display (largest positive at top)
    company_df <- company_df %>%
      arrange(pd_contribution)

    colors <- ifelse(company_df$pd_contribution >= 0, BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(company_df$company_label)),
      "\nSector: ", htmltools::htmlEscape(as.character(company_df$sector)),
      "\nPD Change: ", round(company_df$pd_change * 100, 4), " pp",
      "\nExposure: $", format(round(company_df$exposure_value_usd), big.mark = ","),
      "\nWeight: ", round(company_df$weight * 100, 1), "%",
      "\nContribution: ", ifelse(company_df$pd_contribution >= 0, "+", ""),
      round(company_df$pd_contribution * 100, 4), " pp"
    )

    plot_ly(
      y = factor(company_df$company_label, levels = company_df$company_label),
      x = company_df$pd_contribution * 100,
      type = "bar",
      orientation = "h",
      marker = list(color = colors, line = list(width = 0)),
      text = hover_text,
      hoverinfo = "text",
      texttemplate = "%{x:.4f}",
      textposition = "outside"
    ) %>%
      layout(
        xaxis = list(title = "Contribution to Portfolio PD Change (pp)", zeroline = TRUE,
                     zerolinecolor = "#999", zerolinewidth = 1),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 120, r = 50, t = 10, b = 50),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Marginal Contribution (treemap-style bar chart) ----
  output$attr_marginal_contribution <- renderPlotly({
    ad <- attribution_data()
    req(ad)

    company_df <- ad$company %>%
      mutate(abs_contribution = abs(pd_contribution)) %>%
      arrange(desc(abs_contribution)) %>%
      head(20)

    total_abs <- sum(company_df$abs_contribution)

    company_df <- company_df %>%
      mutate(
        share_pct = abs_contribution / total_abs * 100,
        direction = ifelse(pd_contribution >= 0, "Risk Increase", "Risk Decrease")
      )

    # Cumulative share for visual ordering
    company_df <- company_df %>%
      arrange(desc(share_pct))

    colors <- ifelse(company_df$direction == "Risk Increase", BRAND_CORAL, STATUS_GREEN)

    hover_text <- paste0(
      htmltools::htmlEscape(as.character(company_df$company_label)),
      "\nSector: ", htmltools::htmlEscape(as.character(company_df$sector)),
      "\nShare of total PD change: ", round(company_df$share_pct, 1), "%",
      "\nDirection: ", company_df$direction,
      "\nContribution: ", round(company_df$pd_contribution * 100, 4), " pp"
    )

    plot_ly(
      labels = company_df$company_label,
      values = company_df$share_pct,
      parents = company_df$sector,
      type = "treemap",
      marker = list(
        colors = colors,
        line = list(color = "white", width = 1)
      ),
      text = hover_text,
      hoverinfo = "text",
      textinfo = "label+percent parent",
      textfont = list(size = 11)
    ) %>%
      layout(
        margin = list(l = 5, r = 5, t = 5, b = 5)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Sector × Technology Attribution Heatmap ----
  output$attr_sector_tech_heatmap <- renderPlotly({
    ad <- attribution_data()
    req(ad, ad$has_tech, ad$tech)

    tech_df <- ad$tech
    if (is.null(tech_df) || nrow(tech_df) == 0) {
      return(plotly_empty() %>% layout(title = "No technology data"))
    }

    sectors <- sort(unique(tech_df$sector))
    techs <- sort(unique(tech_df$technology))

    # Build matrix
    z_matrix <- matrix(0, nrow = length(techs), ncol = length(sectors))
    text_matrix <- matrix("", nrow = length(techs), ncol = length(sectors))

    for (r in seq_along(techs)) {
      for (cc in seq_along(sectors)) {
        match_row <- tech_df %>%
          filter(sector == sectors[cc], technology == techs[r])
        if (nrow(match_row) > 0) {
          z_matrix[r, cc] <- match_row$pd_contribution[1] * 100
          text_matrix[r, cc] <- paste0(
            htmltools::htmlEscape(as.character(techs[r])), " / ", htmltools::htmlEscape(as.character(sectors[cc])),
            "\nPD Contribution: ", round(match_row$pd_contribution[1] * 100, 4), " pp",
            "\nCompanies: ", match_row$n_companies[1],
            "\nExposure: $", format(round(match_row$total_exposure[1]), big.mark = ",")
          )
        } else {
          text_matrix[r, cc] <- paste0(htmltools::htmlEscape(as.character(techs[r])), " / ", htmltools::htmlEscape(as.character(sectors[cc])), "\nNo data")
        }
      }
    }

    # Custom diverging color scale: green (negative/good) -> white -> red (positive/bad)
    colorscale <- list(
      list(0, STATUS_GREEN),
      list(0.5, "#FFFFFF"),
      list(1, BRAND_CORAL)
    )

    plot_ly(
      x = sectors,
      y = techs,
      z = z_matrix,
      type = "heatmap",
      colorscale = colorscale,
      zmid = 0,
      text = text_matrix,
      hoverinfo = "text",
      colorbar = list(title = "PD Contr.\n(pp)", len = 0.6)
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 10)),
        yaxis = list(title = "", tickfont = list(size = 10)),
        margin = list(l = 100, b = 80, t = 10)
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Expected Loss Attribution by Sector (waterfall) ----
  output$attr_el_waterfall <- renderPlotly({
    ad <- attribution_data()
    req(ad, ad$has_el)

    sector_df <- ad$sector %>%
      filter(!is.na(el_change)) %>%
      arrange(desc(abs(el_change)))

    if (nrow(sector_df) == 0) {
      return(plotly_empty() %>% layout(title = "No EL data available"))
    }

    total_el_change <- sum(sector_df$el_change, na.rm = TRUE)

    # Build waterfall: sector contributions + total
    wf_labels <- c(sector_df$sector, "Total EL\nChange")
    wf_values <- c(sector_df$el_change, total_el_change)
    wf_measure <- c(rep("relative", nrow(sector_df)), "total")

    hover_text <- c(
      paste0(htmltools::htmlEscape(as.character(sector_df$sector)),
             "\nEL Change: $", format(round(sector_df$el_change), big.mark = ","),
             "\nExposure: $", format(round(sector_df$total_exposure), big.mark = ","),
             "\nCompanies: ", sector_df$n_companies),
      paste0("Total EL Change: $", format(round(total_el_change), big.mark = ","))
    )

    plot_ly(
      x = wf_labels,
      y = wf_values,
      type = "waterfall",
      measure = wf_measure,
      text = hover_text,
      hoverinfo = "text",
      connector = list(line = list(color = "#DDD0D4", width = 1, dash = "dot")),
      # For EL: negative change (more loss) = bad = red, positive = green
      increasing = list(marker = list(color = STATUS_GREEN)),
      decreasing = list(marker = list(color = BRAND_CORAL)),
      totals = list(marker = list(color = "#333333")),
      textposition = "outside",
      texttemplate = "$%{y:,.0f}"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11)),
        yaxis = list(title = "EL Change ($)", tickformat = "$,.0f"),
        margin = list(b = 100, t = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Detailed Attribution Table ----
  output$attr_detail_table <- renderDT({
    ad <- attribution_data()
    req(ad)

    company_df <- ad$company

    display_df <- company_df %>%
      arrange(desc(abs(pd_contribution))) %>%
      transmute(
        Company = company_label,
        Sector = sector,
        Technology = if (ad$has_tech) technology else "N/A",
        `Exposure ($)` = round(exposure_value_usd, 0),
        `Weight (%)` = round(weight * 100, 2),
        `PD Baseline (%)` = round(pd_baseline * 100, 4),
        `PD Shock (%)` = round(pd_shock * 100, 4),
        `PD Change (pp)` = round(pd_change * 100, 4),
        `Contribution (pp)` = round(pd_contribution * 100, 4),
        `Contribution (%)` = if (abs(ad$portfolio_pd_change) > 1e-10)
          round(pd_contribution / ad$portfolio_pd_change * 100, 1) else NA_real_
      )

    if (ad$has_el) {
      display_df$`EL Change ($)` <- round(company_df$el_change, 0)
    }
    if (ad$has_npv) {
      display_df$`NPV Change (%)` <- round(company_df$npv_change_pct, 2)
    }

    datatable(display_df, rownames = FALSE,
      options = list(
        pageLength = 20,
        scrollX = TRUE,
        order = list(list(8, "desc")),  # Sort by Contribution (pp) descending
        columnDefs = list(
          list(className = "dt-right", targets = 3:ncol(display_df) - 1)
        )
      )
    ) %>%
      formatCurrency("Exposure ($)", currency = "$", digits = 0) %>%
      formatStyle(
        "PD Change (pp)",
        backgroundColor = styleInterval(
          c(-0.01, 0, 0.01, 0.05),
          c("#c8e6c9", "#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Contribution (pp)",
        backgroundColor = styleInterval(
          c(-0.005, 0, 0.005, 0.02),
          c("#c8e6c9", "#e8f5e9", "#fff9c4", "#ffccbc", "#ef9a9a")
        ),
        fontWeight = "bold"
      )
  })

  # ---- Export Attribution CSV ----
  output$download_attribution_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_attribution_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv")
    },
    content = function(file) {
      ad <- attribution_data()
      if (!is.null(ad)) {
        export_df <- ad$company %>%
          select(any_of(c("company_label", "company_id", "sector", "technology",
                         "exposure_value_usd", "weight", "pd_baseline", "pd_shock",
                         "pd_change", "pd_contribution", "el_change", "npv_change_pct")))
        write.csv(export_df, file, row.names = FALSE)
      }
    }
  )

}
