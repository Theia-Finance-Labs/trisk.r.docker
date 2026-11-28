# mod_results_attribution.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_results_attribution <- function(input, output, session, rv) {

  # ATTRIBUTION / WATERFALL DASHBOARD
  # ============================================

  # Reactive: attribution data — builds the decomposition of PD/NPV/EL changes
  # by sector, technology, and company, using the primary results (first scenario, first year).
  # For multi-scenario runs, we also build cross-scenario decompositions.
  attribution_data <- reactive({
    req(rv$results)
    df <- rv$results
    req(all(c("pd_baseline", "pd_shock", "exposure_value_usd", "sector") %in% names(df)))

    has_el <- all(c("expected_loss_baseline", "expected_loss_shock") %in% names(df))
    has_npv <- "trisk_perc_value_change" %in% names(df)
    has_tech <- "technology" %in% names(df)
    has_company <- any(c("company_name", "company_id") %in% names(df))

    total_exposure <- sum(df$exposure_value_usd, na.rm = TRUE)

    # --- Company-level attribution ---
    # After portfolio enrichment, results may have multiple rows per company
    # (one per technology). Aggregate to true company-level for attribution.
    has_valid_company_name_attr <- "company_name" %in% names(df) && any(!is.na(df$company_name))
    company_label_col <- if (has_valid_company_name_attr) "company_name" else "company_id"
    row_df <- df %>%
      mutate(
        pd_change = pd_shock - pd_baseline,
        weight = exposure_value_usd / total_exposure,
        pd_contribution = pd_change * weight,
        company_label = htmltools::htmlEscape(as.character(.data[[company_label_col]])),
        el_change = if (has_el) expected_loss_shock - expected_loss_baseline else NA_real_,
        npv_change_pct = if (has_npv) trisk_perc_value_change * 100 else NA_real_
      )

    company_df <- row_df %>%
      group_by(company_label, sector) %>%
      summarise(
        # Weighted means MUST come before exposure_value_usd is overwritten by sum()
        pd_baseline = safe_weighted_mean(pd_baseline, exposure_value_usd),
        pd_shock    = safe_weighted_mean(pd_shock, exposure_value_usd),
        pd_change   = safe_weighted_mean(pd_change, exposure_value_usd),
        npv_change_pct = if (has_npv) safe_weighted_mean(npv_change_pct, exposure_value_usd) else NA_real_,
        exposure_value_usd = sum(exposure_value_usd, na.rm = TRUE),
        weight      = sum(weight, na.rm = TRUE),
        pd_contribution = sum(pd_contribution, na.rm = TRUE),
        el_change   = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        .groups = "drop"
      )

    # Guard NA labels
    company_df$company_label <- ifelse(
      is.na(company_df$company_label),
      paste0("Company_", seq_len(nrow(company_df))),
      as.character(company_df$company_label)
    )

    # --- Sector-level attribution ---
    sector_df <- company_df %>%
      group_by(sector) %>%
      summarise(
        n_companies = n(),
        total_exposure = sum(exposure_value_usd, na.rm = TRUE),
        weight = sum(weight, na.rm = TRUE),
        avg_pd_baseline = safe_weighted_mean(pd_baseline, exposure_value_usd),
        avg_pd_shock = safe_weighted_mean(pd_shock, exposure_value_usd),
        avg_pd_change = avg_pd_shock - avg_pd_baseline,
        pd_contribution = sum(pd_contribution, na.rm = TRUE),
        el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
        npv_change_pct = if (has_npv) safe_weighted_mean(npv_change_pct, exposure_value_usd) else NA_real_,
        .groups = "drop"
      ) %>%
      arrange(desc(abs(pd_contribution)))

    # --- Technology-level attribution (if available) ---
    # Use row_df (pre-company-aggregation) since it still has the technology column
    tech_df <- if (has_tech) {
      row_df %>%
        group_by(sector, technology) %>%
        summarise(
          n_companies = n_distinct(company_label),
          total_exposure = sum(exposure_value_usd, na.rm = TRUE),
          weight = sum(weight, na.rm = TRUE),
          avg_pd_change = safe_weighted_mean(pd_change, exposure_value_usd),
          pd_contribution = sum(pd_contribution, na.rm = TRUE),
          el_change = if (has_el) sum(el_change, na.rm = TRUE) else NA_real_,
          .groups = "drop"
        ) %>%
        arrange(desc(abs(pd_contribution)))
    } else NULL

    # --- Portfolio-level waterfall steps ---
    portfolio_pd_baseline <- safe_weighted_mean(df$pd_baseline, df$exposure_value_usd)
    portfolio_pd_shock <- safe_weighted_mean(df$pd_shock, df$exposure_value_usd)
    portfolio_pd_change <- portfolio_pd_shock - portfolio_pd_baseline

    list(
      company = company_df,
      company_detail = row_df,   # per-row (technology-level) for detailed table
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
      return(div(style = "text-align: center; padding: 60px 20px; color: #999;",
        icon("chart-bar", style = "font-size: 48px; margin-bottom: 15px;"),
        h4("Run analysis to view attribution breakdown"),
        p("Results are needed to decompose risk changes by sector, technology, and company.",
          style = "font-size: 14px;"),
        actionLink("goto_run_attribution", "Go to Run Analysis",
                    icon = icon("play"), style = "font-size: 14px;")
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
               style = "margin: 0; font-weight: 700;"),
            tags$small(paste0(n_sectors, " sectors | ", n_co, " companies | ",
                             "Portfolio PD change: ", round(ad$portfolio_pd_change * 100, 4), " pp"),
                      style = "opacity: 0.85;")
          ),
          column(4, style = "text-align: right;",
            downloadButton("download_attribution_csv", "Export CSV",
                          class = "btn-sm btn-export")
          )
        )
      ),
      tags$p(class = "fs-12 fg-secondary mb-8", tags$i(class = "fa fa-info-circle mr-6"), "Decomposes the portfolio PD change into sector and company contributions. The waterfall shows how each sector moves the portfolio from baseline PD to shocked PD, exposure-weighted."),

      # Row 1: Waterfall chart (top-down PD decomposition)
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("exchange-alt"), " PD Waterfall: Baseline \u2192 Sector Contributions \u2192 Shocked PD",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Each bar shows how much each sector contributes to the total portfolio PD change (exposure-weighted).",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_waterfall", height = "400px")
          )
        )
      ),

      # Row 2: Company movers (full width — marginal contribution treemap removed)
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("sort-amount-down"), " Top Risk Movers (Companies)",
               style = "font-weight: 600; margin-top: 0;"),
            tags$p(class = "fs-12 fg-secondary mb-8", tags$i(class = "fa fa-info-circle mr-6"), "Companies with the largest exposure-weighted PD change \u2014 the names driving your portfolio's climate transition risk."),
            plotlyOutput("attr_company_movers", height = "380px")
          )
        )
      ),

      # Row 3: EL Waterfall (complements PD waterfall with dollar-impact view)
      if (ad$has_el) fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("money-bill-wave"), " Expected Loss Waterfall by Sector",
               style = "font-weight: 600; margin-top: 0;"),
            tags$small("Shows each sector's contribution to the total Expected Loss increase. Red bars = EL increased (more provisioning needed), green = EL decreased.",
                      style = "color: #666; display: block; margin-bottom: 10px;"),
            plotlyOutput("attr_el_waterfall", height = "400px")
          )
        )
      ),

      # Row 4: Attribution summary table
      fluidRow(
        column(12,
          div(class = "card-surface", style = "margin-bottom: 16px;",
            h5(icon("table"), " Detailed Attribution Table",
               style = "font-weight: 600; margin-top: 0;"),
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
      paste0(sector_df$sector,
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
    req(ad, ad$company)
    req(nrow(ad$company) > 0)
    req(all(c("company_label", "sector", "pd_change", "exposure_value_usd",
              "weight", "pd_contribution") %in% names(ad$company)))

    tc <- plotly_theme_colors(input)

    company_df <- ad$company %>%
      filter(!is.na(pd_contribution), is.finite(pd_contribution)) %>%
      arrange(desc(abs(pd_contribution))) %>%
      head(15)

    req(nrow(company_df) > 0)

    # Sort by contribution for display (largest positive at top)
    company_df <- company_df %>%
      arrange(pd_contribution)

    # Disambiguate labels: append sector if any company appears in multiple sectors
    company_df <- company_df %>%
      mutate(display_label = if (any(duplicated(company_label))) {
        paste0(company_label, " (", sector, ")")
      } else {
        company_label
      })

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
      y = factor(company_df$display_label, levels = company_df$display_label),
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
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL,
        xaxis = list(title = "Contribution to Portfolio PD Change (pp)", zeroline = TRUE,
                     zerolinecolor = "#999", zerolinewidth = 1, gridcolor = tc$gridcolor),
        yaxis = list(title = "", tickfont = list(size = 10), gridcolor = tc$gridcolor),
        margin = list(l = 120, r = 50, t = 10, b = 50),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })



  # ---- Expected Loss Attribution by Sector (waterfall) ----
  output$attr_el_waterfall <- renderPlotly({
    ad <- attribution_data()
    req(ad, ad$has_el)

    # Sort for clean waterfall cascade: EL increases first (el_change < 0 means loss increased,
    # so -el_change > 0), then decreases. This prevents bars from "hovering" in the middle.
    sector_df <- ad$sector %>%
      filter(!is.na(el_change)) %>%
      arrange(el_change)

    if (nrow(sector_df) == 0) {
      return(plotly_empty() %>% layout(title = "No EL data available"))
    }

    total_el_change <- sum(sector_df$el_change, na.rm = TRUE)

    # Negate el_change so increases in loss display as positive (upward) bars
    # el_change is negative when loss increases (EL = -(EAD*PD)), so -el_change > 0 for increased loss
    wf_labels <- c(sector_df$sector, "Total EL\nIncrease")
    wf_values <- c(-sector_df$el_change, -total_el_change)
    wf_measure <- c(rep("relative", nrow(sector_df)), "total")

    hover_text <- c(
      paste0(sector_df$sector,
             "\nEL ", ifelse(sector_df$el_change < 0, "Increase", "Decrease"),
             ": $", format(round(abs(sector_df$el_change)), big.mark = ","),
             "\nExposure: $", format(round(sector_df$total_exposure), big.mark = ","),
             "\nCompanies: ", sector_df$n_companies),
      paste0("Total EL ", ifelse(total_el_change < 0, "Increase", "Decrease"),
             ": $", format(round(abs(total_el_change)), big.mark = ","))
    )

    plot_ly(
      x = wf_labels,
      y = wf_values,
      type = "waterfall",
      measure = wf_measure,
      text = hover_text,
      hoverinfo = "text",
      connector = list(line = list(color = "#DDD0D4", width = 1, dash = "dot")),
      # Positive bars (EL increased) = red; negative bars (EL decreased) = green
      increasing = list(marker = list(color = STATUS_RED)),
      decreasing = list(marker = list(color = STATUS_GREEN)),
      totals = list(marker = list(color = "#333333")),
      textposition = "outside",
      texttemplate = "$%{y:,.0f}"
    ) %>%
      layout(
        xaxis = list(title = "", tickangle = -30, tickfont = list(size = 11),
                     categoryorder = "array", categoryarray = wf_labels),
        yaxis = list(title = "Expected Loss Increase ($)", tickformat = "$,.0f"),
        margin = list(b = 100, t = 30),
        showlegend = FALSE
      ) %>%
      config(displayModeBar = FALSE)
  })

  # ---- Detailed Attribution Table ----
  output$attr_detail_table <- renderDT({
    ad <- attribution_data()
    req(ad)

    # Use technology-level detail rows for the full breakdown table
    detail_df <- ad$company_detail

    display_df <- detail_df %>%
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
      display_df$`EL Change ($)` <- round(detail_df$el_change, 0)
    }
    # NPV column removed from attribution detail table

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
          c("#BAB6B5", "#F5F0F2", "#EF7173", "#E84B4D", "#C0393B")
        ),
        fontWeight = "bold"
      ) %>%
      formatStyle(
        "Contribution (pp)",
        backgroundColor = styleInterval(
          c(-0.005, 0, 0.005, 0.02),
          c("#BAB6B5", "#F5F0F2", "#EF7173", "#E84B4D", "#C0393B")
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
        export_df <- ad$company_detail %>%
          select(any_of(c("company_label", "company_id", "sector", "technology",
                         "exposure_value_usd", "weight", "pd_baseline", "pd_shock",
                         "pd_change", "pd_contribution", "el_change", "npv_change_pct")))
        write.csv(sanitize_export(export_df), file, row.names = FALSE)
      }
    }
  )


}
