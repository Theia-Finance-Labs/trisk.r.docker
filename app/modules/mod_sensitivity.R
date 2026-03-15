# mod_sensitivity.R
# Scenario Sensitivity Analysis: trajectory visualization, multi-scenario runs, distribution plots

setup_sensitivity <- function(input, output, session, rv) {

  # ---- Local helpers ----
  make_card <- function(title, value_text, color_class = "neutral", tooltip = title) {
    column(2,
      div(class = "portfolio-aggregate",
        h4(title, title = tooltip),
        tags$span(class = paste("agg-value", color_class), value_text)
      )
    )
  }

  geo_for_scenario <- function(scenario_code, selected_geo) {
    available_geos <- unique(rv$scenarios$scenario_geography[rv$scenarios$scenario == scenario_code])
    if (selected_geo %in% available_geos) {
      return(list(geography = selected_geo, fallback = FALSE))
    }
    return(list(geography = "Global", fallback = TRUE))
  }

  # ---- Detect sensitivity groups when main analysis completes ----
  observe({
    req(rv$results, rv$target_scenarios_all)
    # Use the core target scenario from the main run
    core_target <- rv$results$target_scenario[1]
    req(core_target)
    rv$sensitivity_config <- build_sensitivity_groups(core_target, rv$target_scenarios_all)
  })

  # ---- Main content UI ----
  output$sensitivity_content_ui <- renderUI({
    # State 1: No main analysis run yet
    if (is.null(rv$results)) {
      return(tagList(
        div(class = "section-header section-header--dark",
          h4(icon("chart-bar"), " Scenario Sensitivity Analysis", class = "section-title"),
          tags$small("Explore how results change across different climate scenarios",
                    class = "opacity-85")
        ),
        tags$div(class = "empty-state-40",
          icon("flask", class = "empty-state-icon-muted"),
          tags$h4("Run the main analysis first"),
          tags$p("Sensitivity analysis builds on your portfolio results.",
                 "Run an analysis on the Portfolio Results tab, then return here."),
          actionLink("goto_run_from_sensitivity", "Go to Run Analysis",
                    icon = icon("play"), class = "fs-14")
        )
      ))
    }

    config <- rv$sensitivity_config
    shock_years <- sort(as.integer(names(rv$results_by_year)))
    core_target <- rv$results$target_scenario[1]
    core_baseline <- rv$results$baseline_scenario[1]

    # ---- Section 1: Scenario Trajectories ----
    trajectory_section <- tagList(
      div(class = "section-header section-header--dark",
        fluidRow(
          column(8,
            h4(icon("chart-line"), " Scenario Trajectories", class = "section-title"),
            tags$small(paste0("Baseline: ", scenario_label(core_baseline),
                             " | Target: ", scenario_label(core_target)),
                      class = "opacity-85")
          ),
          column(4, class = "text-right",
            radioButtons("trajectory_level", NULL,
              choices = c("By Sector" = "sector", "By Technology" = "technology"),
              selected = "sector", inline = TRUE)
          )
        )
      ),
      fluidRow(
        box(width = 12,
          plotlyOutput("plot_trajectories", height = "480px")
        )
      )
    )

    # ---- Section 2: Sensitivity Configuration ----
    sensitivity_config_section <- if (is.null(config) || length(config$groups) == 0) {
      tags$div(class = "empty-state-40",
        icon("exclamation-circle", class = "empty-state-icon-muted"),
        tags$h4("No alternative scenarios found"),
        tags$p("The selected scenario has no sibling scenarios for sensitivity analysis.")
      )
    } else {
      # Build checkbox choices with rich labels
      group_choices <- setNames(names(config$groups), sapply(names(config$groups), function(gk) {
        g <- config$groups[[gk]]
        paste0(g$label, " (", g$n_scenarios, " scenarios",
               if (length(g$iams) > 0) paste0(": ", paste(g$iams, collapse = ", ")) else "",
               ")", if (g$is_primary) " \u2605 YOUR SCENARIO" else "")
      }))
      # Pre-select primary group
      selected_groups <- names(config$groups)[sapply(config$groups, function(g) g$is_primary)]

      tagList(
        div(class = "section-header section-header--dark",
          fluidRow(
            column(8,
              h4(icon("layer-group"), " Sensitivity Configuration", class = "section-title"),
              tags$small(paste0("Found ", length(config$groups), " policy groups from ",
                               config$primary_policy, " family"),
                        class = "opacity-85")
            ),
            column(4, class = "text-right",
              actionButton("run_sensitivity", "Run Sensitivity Analysis",
                          icon = icon("play"),
                          class = "btn-danger btn-lg")
            )
          )
        ),
        fluidRow(
          box(width = 12, title = "Select Policy Groups to Analyze", collapsible = TRUE,
            tags$p(class = "fs-13 fg-muted",
              "Each group contains scenarios with the same policy ambition level. ",
              "Check the groups you want to include in the sensitivity analysis."
            ),
            checkboxGroupInput("sensitivity_groups", NULL,
              choices = group_choices,
              selected = selected_groups
            ),
            tags$hr(),
            tags$small(class = "fg-muted",
              paste0("Shock years: ", paste(shock_years, collapse = ", "),
                    " | Baseline: ", scenario_label(core_baseline),
                    " | Geography: ", input$scenario_geography %||% "Global"))
          )
        )
      )
    }

    # ---- Section 3: Sensitivity Results ----
    results_section <- if (!is.null(rv$sensitivity_summary)) {
      tagList(
        div(class = "section-header section-header--dark",
          h4(icon("chart-area"), " Sensitivity Results", class = "section-title"),
          tags$small("Distribution of key metrics across scenarios", class = "opacity-85")
        ),
        uiOutput("sensitivity_summary_cards"),
        fluidRow(
          box(width = 6, plotlyOutput("plot_sensitivity_pd", height = "400px")),
          box(width = 6, plotlyOutput("plot_sensitivity_el", height = "400px"))
        ),
        fluidRow(
          box(width = 12, plotlyOutput("plot_sensitivity_npv", height = "380px"))
        ),
        fluidRow(
          box(width = 12, title = "Scenario Comparison", collapsible = TRUE,
            DT::DTOutput("sensitivity_comparison_table")
          )
        )
      )
    } else {
      NULL
    }

    tagList(
      trajectory_section,
      tags$hr(style = "margin: 24px 0;"),
      sensitivity_config_section,
      results_section
    )
  })

  # ---- Navigation link ----
  observeEvent(input$goto_run_from_sensitivity, {
    updateTabItems(session, "tabs", "run")
  })

  # ---- Trajectory Plot ----
  output$plot_trajectories <- renderPlotly({
    req(rv$results, rv$scenarios)
    core_target <- rv$results$target_scenario[1]
    core_baseline <- rv$results$baseline_scenario[1]
    selected_geo <- input$scenario_geography %||% "Global"
    level <- input$trajectory_level %||% "sector"
    shock_years <- sort(as.integer(names(rv$results_by_year)))

    # Filter scenarios data to baseline + target, selected geography
    traj_data <- rv$scenarios %>%
      dplyr::filter(
        .data$scenario %in% c(core_baseline, core_target),
        .data$scenario_geography == selected_geo
      )

    req(nrow(traj_data) > 0, "scenario_pathway" %in% names(traj_data))

    # Convert scenario_pathway to numeric
    traj_data <- traj_data %>%
      dplyr::mutate(
        pathway_value = suppressWarnings(as.numeric(.data$scenario_pathway)),
        scenario_label = ifelse(.data$scenario == core_baseline, "Baseline", "Target"),
        year = as.integer(.data$scenario_year)
      ) %>%
      dplyr::filter(!is.na(.data$pathway_value))

    req(nrow(traj_data) > 0)

    if (level == "sector") {
      # Aggregate by sector
      plot_data <- traj_data %>%
        dplyr::group_by(.data$scenario_label, .data$sector, .data$year) %>%
        dplyr::summarise(pathway_value = sum(.data$pathway_value, na.rm = TRUE),
                        .groups = "drop") %>%
        dplyr::mutate(facet_label = .data$sector)
      facet_var <- "sector"
    } else {
      plot_data <- traj_data %>%
        dplyr::mutate(
          tech_label = paste0(.data$sector, " / ", .data$technology),
          facet_label = paste0(.data$sector, " / ", .data$technology)
        )
      facet_var <- "tech_label"
    }

    # Determine units for y-axis
    pathway_units <- unique(na.omit(traj_data$pathway_unit))
    y_label <- if (length(pathway_units) == 1) pathway_units[1] else "Pathway Value"

    p <- ggplot(plot_data, aes(
      x = year, y = pathway_value, color = scenario_label,
      linetype = scenario_label,
      text = paste0(
        facet_label,
        "\n", scenario_label, " (", year, ")",
        "\nValue: ", round(pathway_value, 2)
      )
    )) +
      geom_line(linewidth = 0.8) +
      geom_point(size = 1.2, alpha = 0.5) +
      scale_color_manual(values = c("Baseline" = "gray50", "Target" = TRISK_HEX_RED),
                        name = "Scenario") +
      scale_linetype_manual(values = c("Baseline" = "dashed", "Target" = "solid"),
                           name = "Scenario")

    # Add shock year markers
    for (yr in shock_years) {
      p <- p + geom_vline(xintercept = yr, linetype = "dotted",
                          color = "#2E86C1", linewidth = 0.5, alpha = 0.6)
    }

    if (level == "sector") {
      p <- p + facet_wrap(~ sector, scales = "free_y", ncol = 3)
    } else {
      p <- p + facet_wrap(~ tech_label, scales = "free_y", ncol = 3)
    }

    p <- p +
      labs(
        title = "Scenario Trajectories",
        subtitle = paste0("Baseline vs Target | Dotted blue = shock year(s): ",
                         paste(shock_years, collapse = ", ")),
        x = "Year", y = y_label
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 9, color = "gray50"),
        strip.text = element_text(size = 9, face = "bold"),
        legend.position = "bottom",
        legend.text = element_text(size = 9)
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL,
        legend = list(orientation = "h", y = -0.12, x = 0.5, xanchor = "center")
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Run Sensitivity Analysis ----
  observeEvent(input$run_sensitivity, {
    req(rv$sensitivity_config, rv$results_by_year, rv$portfolio, rv$assets)

    config <- rv$sensitivity_config
    shock_years <- sort(as.integer(names(rv$results_by_year)))
    core_baseline <- rv$results$baseline_scenario[1]
    core_sectors <- SECTOR_SCENARIO_DEFAULTS[["core"]]$sectors
    selected_geo <- input$scenario_geography %||% "Global"

    # Get selected groups from checkboxGroupInput
    selected_keys <- input$sensitivity_groups
    if (is.null(selected_keys) || length(selected_keys) == 0) {
      showNotification("Please select at least one policy group.", type = "warning")
      return()
    }
    selected_scenarios <- unique(unlist(lapply(
      config$groups[selected_keys], function(g) g$scenarios
    )))

    shinyjs::disable("run_sensitivity")
    on.exit(shinyjs::enable("run_sensitivity"))

    withProgress(message = "Running sensitivity analysis...", value = 0, {
      all_results <- list()
      total_runs <- length(selected_scenarios) * length(shock_years)
      run_count <- 0

      for (scen in selected_scenarios) {
        for (yr in shock_years) {
          run_count <- run_count + 1
          incProgress(1 / total_runs,
            detail = paste0(scenario_label(scen), " \u2014 ", yr,
                           " (", run_count, "/", total_runs, ")"))

          # Reuse primary scenario results
          if (scen == config$primary) {
            yr_results <- rv$results_by_year[[as.character(yr)]]
            if (!is.null(yr_results)) {
              # Filter to core sectors only
              yr_results <- yr_results %>%
                dplyr::filter(.data$sector %in% core_sectors)
              yr_results$sensitivity_scenario <- scen
              all_results[[paste0(scen, "_", yr)]] <- yr_results
            }
            next
          }

          tryCatch({
            bl <- baseline_for_scenario(scen, rv$available_baselines, core_baseline)
            geo_result <- geo_for_scenario(scen, selected_geo)

            portfolio_subset <- rv$portfolio %>%
              dplyr::filter(.data$sector %in% core_sectors)
            assets_subset <- rv$assets %>%
              dplyr::filter(.data$sector %in% core_sectors)

            # Year range filter
            pair_scenarios <- rv$scenarios %>%
              dplyr::filter(
                .data$scenario %in% c(bl, scen),
                .data$scenario_geography %in% geo_result$geography
              )
            if (nrow(pair_scenarios) > 0) {
              scen_min <- min(pair_scenarios$scenario_year)
              scen_max <- max(pair_scenarios$scenario_year)
              assets_subset <- assets_subset %>%
                dplyr::filter(.data$production_year >= scen_min,
                              .data$production_year <= scen_max)
            }

            raw <- run_trisk_on_portfolio(
              assets_data = assets_subset,
              scenarios_data = rv$scenarios,
              financial_data = rv$financial,
              carbon_data = rv$carbon,
              portfolio_data = portfolio_subset,
              baseline_scenario = bl,
              target_scenario = scen,
              scenario_geography = geo_result$geography,
              shock_year = yr,
              risk_free_rate = input$risk_free_rate,
              discount_rate = input$discount_rate,
              growth_rate = input$growth_rate,
              market_passthrough = input$market_passthrough
            )

            result_df <- extract_result_df(raw)
            result_df <- compute_el_columns(result_df)
            result_df$shock_year <- yr
            result_df$sensitivity_scenario <- scen
            result_df$baseline_scenario <- bl
            all_results[[paste0(scen, "_", yr)]] <- result_df
          }, error = function(e) {
            message(paste0("Sensitivity: ", scen, " year ", yr, " failed: ",
                          conditionMessage(e)))
          })
        }
      }

      rv$sensitivity_results <- dplyr::bind_rows(all_results)

      # Compute per-scenario summary
      if (nrow(rv$sensitivity_results) > 0) {
        primary_code <- config$primary
        has_exposure <- "exposure_value_usd" %in% names(rv$sensitivity_results)
        has_el_diff  <- "expected_loss_difference" %in% names(rv$sensitivity_results)
        has_npv      <- "crispy_perc_value_change" %in% names(rv$sensitivity_results)

        rv$sensitivity_summary <- rv$sensitivity_results %>%
          dplyr::mutate(
            pd_change_bps = (.data$pd_shock - .data$pd_baseline) * 10000
          ) %>%
          dplyr::group_by(.data$sensitivity_scenario, .data$shock_year) %>%
          dplyr::summarise(
            scenario_label_text = scenario_label(.data$sensitivity_scenario[1]),
            category = scenario_category(.data$sensitivity_scenario[1]),
            policy_type = pathway_to_policy_type(
              parse_scenario_code(.data$sensitivity_scenario[1])$pathway %||% ""
            ),
            avg_pd_change = if (has_exposure)
              weighted.mean(.data$pd_change_bps, .data$exposure_value_usd, na.rm = TRUE)
            else mean(.data$pd_change_bps, na.rm = TRUE),
            total_el_change = if (has_el_diff)
              sum(.data$expected_loss_difference, na.rm = TRUE) else NA_real_,
            avg_npv_change = if (has_npv)
              mean(.data$crispy_perc_value_change, na.rm = TRUE) * 100 else NA_real_,
            n_companies = dplyr::n_distinct(.data$company_id),
            is_primary = .data$sensitivity_scenario[1] == primary_code,
            .groups = "drop"
          )
      }
    })

    showNotification(
      paste0("Sensitivity analysis complete: ", length(unique(rv$sensitivity_results$sensitivity_scenario)),
             " scenarios tested"),
      type = "message", duration = 5
    )
  })

  # ---- Summary Cards ----
  output$sensitivity_summary_cards <- renderUI({
    req(rv$sensitivity_summary)
    ss <- rv$sensitivity_summary

    n_scen <- length(unique(ss$sensitivity_scenario))
    median_pd <- median(ss$avg_pd_change, na.rm = TRUE)
    range_pd <- range(ss$avg_pd_change, na.rm = TRUE)
    median_el <- median(ss$total_el_change, na.rm = TRUE)
    median_npv <- median(ss$avg_npv_change, na.rm = TRUE)
    primary_pd <- ss$avg_pd_change[ss$is_primary][1]

    pd_class <- if (!is.na(median_pd) && median_pd > 1) "negative" else
                if (!is.na(median_pd) && median_pd < -1) "positive" else "neutral"

    fluidRow(
      make_card("Scenarios Tested", as.character(n_scen)),
      make_card("Median PD Change", paste0(round(median_pd, 0), " bps"), pd_class),
      make_card("PD Range", paste0(round(range_pd[1], 0), " to ", round(range_pd[2], 0), " bps")),
      make_card("Your PD Change", paste0(round(primary_pd, 0), " bps"),
                tooltip = paste0("Primary scenario: ", rv$sensitivity_config$primary)),
      make_card("Median EL Change", paste0("$", format(round(median_el), big.mark = ",")),
                tooltip = "Median total expected loss change across scenarios"),
      make_card("Median NPV Change", paste0(round(median_npv, 1), "%"))
    )
  })

  # ---- Violin Plot: PD Change ----
  output$plot_sensitivity_pd <- renderPlotly({
    req(rv$sensitivity_summary)
    ss <- rv$sensitivity_summary
    primary_val <- mean(ss$avg_pd_change[ss$is_primary], na.rm = TRUE)
    use_violin <- min(table(ss$policy_type)) >= 3

    p <- ggplot(ss, aes(x = policy_type, y = avg_pd_change))
    if (use_violin) {
      p <- p + geom_violin(fill = "#C5E1A5", color = TRISK_HEX_GREEN, alpha = 0.3,
                            linewidth = 0.5, scale = "width")
    }
    p <- p + geom_jitter(aes(
        color = is_primary, size = is_primary,
        text = paste0(scenario_label_text,
                     "\nPD Change: ", round(avg_pd_change, 0), " bps",
                     "\nShock Year: ", shock_year)
      ), width = 0.12, alpha = 0.8) +
      geom_hline(yintercept = primary_val, linetype = "dashed",
                 color = TRISK_HEX_RED, alpha = 0.5) +
      scale_color_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = "gray40"), guide = "none") +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2.5), guide = "none") +
      labs(
        title = "PD Change Distribution",
        subtitle = paste0("Dashed line = your scenario (",
                         round(primary_val, 0), " bps) | Each dot = scenario \u00d7 year"),
        x = "", y = "Avg PD Change (bps)"
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 8, color = "gray50"),
        axis.text.x = element_text(size = 8, angle = 25, hjust = 1)
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = "text") %>%
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

  # ---- Violin Plot: Expected Loss ----
  output$plot_sensitivity_el <- renderPlotly({
    req(rv$sensitivity_summary)
    ss <- rv$sensitivity_summary %>% dplyr::filter(!is.na(.data$total_el_change))
    req(nrow(ss) > 0)
    primary_val <- mean(ss$total_el_change[ss$is_primary], na.rm = TRUE)

    use_violin_el <- min(table(ss$policy_type)) >= 3
    p <- ggplot(ss, aes(x = policy_type, y = total_el_change))
    if (use_violin_el) {
      p <- p + geom_violin(fill = "#B3D4FC", color = "#2E86C1", alpha = 0.3,
                            linewidth = 0.5, scale = "width")
    }
    p <- p + geom_jitter(aes(
        color = is_primary, size = is_primary,
        text = paste0(scenario_label_text,
                     "\nEL Change: $", format(round(total_el_change), big.mark = ","),
                     "\nShock Year: ", shock_year)
      ), width = 0.12, alpha = 0.8) +
      geom_hline(yintercept = primary_val, linetype = "dashed",
                 color = TRISK_HEX_RED, alpha = 0.5) +
      scale_color_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = "gray40"), guide = "none") +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2.5), guide = "none") +
      scale_y_continuous(labels = function(x) {
        ifelse(abs(x) >= 1e6, paste0("$", round(x / 1e6, 1), "M"),
        ifelse(abs(x) >= 1e3, paste0("$", round(x / 1e3, 0), "K"),
               paste0("$", round(x))))
      }) +
      labs(
        title = "Expected Loss Change Distribution",
        subtitle = paste0("Dashed line = your scenario | Each dot = scenario \u00d7 year"),
        x = "", y = "Total EL Change (USD)"
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 8, color = "gray50"),
        axis.text.x = element_text(size = 8, angle = 25, hjust = 1)
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = "text") %>%
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

  # ---- Violin Plot: NPV Change ----
  output$plot_sensitivity_npv <- renderPlotly({
    req(rv$sensitivity_summary)
    ss <- rv$sensitivity_summary %>% dplyr::filter(!is.na(.data$avg_npv_change))
    req(nrow(ss) > 0)
    primary_val <- mean(ss$avg_npv_change[ss$is_primary], na.rm = TRUE)

    use_violin_npv <- min(table(ss$policy_type)) >= 3
    p <- ggplot(ss, aes(x = policy_type, y = avg_npv_change))
    if (use_violin_npv) {
      p <- p + geom_violin(fill = "#F8D7DA", color = TRISK_HEX_RED, alpha = 0.3,
                            linewidth = 0.5, scale = "width")
    }
    p <- p + geom_jitter(aes(
        color = is_primary, size = is_primary,
        text = paste0(scenario_label_text,
                     "\nNPV Change: ", round(avg_npv_change, 1), "%",
                     "\nShock Year: ", shock_year)
      ), width = 0.12, alpha = 0.8) +
      geom_hline(yintercept = primary_val, linetype = "dashed",
                 color = TRISK_HEX_RED, alpha = 0.5) +
      scale_color_manual(values = c("TRUE" = TRISK_HEX_RED, "FALSE" = "gray40"), guide = "none") +
      scale_size_manual(values = c("TRUE" = 4, "FALSE" = 2.5), guide = "none") +
      labs(
        title = "NPV Change Distribution",
        subtitle = paste0("Dashed line = your scenario (",
                         round(primary_val, 1), "%) | Each dot = scenario \u00d7 year"),
        x = "", y = "Avg NPV Change (%)"
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 8, color = "gray50"),
        axis.text.x = element_text(size = 8, angle = 25, hjust = 1)
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL,
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })

  # ---- Comparison Table ----
  output$sensitivity_comparison_table <- DT::renderDT({
    req(rv$sensitivity_summary)
    ss <- rv$sensitivity_summary %>%
      dplyr::arrange(.data$policy_type, .data$sensitivity_scenario, .data$shock_year) %>%
      dplyr::transmute(
        Scenario     = .data$scenario_label_text,
        `Policy`     = .data$policy_type,
        Category     = .data$category,
        `Shock Year` = .data$shock_year,
        `PD Change (bps)` = round(.data$avg_pd_change, 0),
        `EL Change ($)` = round(.data$total_el_change, 0),
        `NPV Change (%)` = round(.data$avg_npv_change, 2),
        Companies    = .data$n_companies,
        Primary      = ifelse(.data$is_primary, "\u2605", "")
      )

    DT::datatable(ss, rownames = FALSE, options = list(
      pageLength = 30, dom = "ft", scrollX = TRUE,
      order = list(list(4, "desc"))
    )) %>%
      DT::formatStyle("Primary", fontWeight = "bold", color = TRISK_HEX_RED)
  })
}
