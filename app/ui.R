# ui.R
# User interface for TRISK Shiny application
# Visual style follows 1in1000.com design language (Inter + Space Grotesk fonts, pink/coral palette)

ui <- dashboardPage(
  skin = "red",

  # ============================================
  # Header with logo
  # ============================================
  dashboardHeader(
    title = tags$span(
      tags$span("TRISK Desktop 3.0", class = "header-title"),
      tags$br(),
      tags$span("Climate Transition Risk Stress Testing", class = "header-subtitle")
    ),
    titleWidth = 320,
    tags$li(class = "dropdown",
      tags$button(id = "theme-toggle-btn",
                  class = "theme-toggle",
                  "\u263E Dark")
    )
  ),

  # ============================================
  # Sidebar
  # ============================================
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("1. Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("2. Configure Analysis", tabName = "config", icon = icon("sliders-h")),
      menuItem("3. Run Analysis", tabName = "run", icon = icon("play")),
      menuItem("4. Portfolio Results", tabName = "results", icon = icon("chart-line")),
      menuItem("5. Scenario Analysis", tabName = "sensitivity", icon = icon("chart-bar")),
      menuItem("6. Risk Integration", tabName = "integration", icon = icon("exchange-alt")),
      menuItem("7. Download", tabName = "download", icon = icon("download")),
      hr(),
      menuItem("Documentation", tabName = "docs", icon = icon("book")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    ),
    tags$div(class = "sidebar-logo-wrap",
      tags$img(src = "logo-black.png", height = "160px", class = "opacity-60 sidebar-logo")
    )
  ),

  # ============================================
  # Body
  # ============================================
  dashboardBody(
    useShinyjs(),

    # ---- Vendored Fonts + Custom CSS using 1in1000 palette ----
    # Fonts are bundled locally (app/www/fonts/) for offline/air-gapped deployment.
    # No runtime network dependency on fonts.googleapis.com.
    tags$head(
      # CSP, X-Frame-Options, and other security headers are set by the
      # Caddy reverse proxy (see Caddyfile). Do not add a <meta> CSP here —
      # HTTP headers are more secure and support frame-ancestors.
      tags$link(rel = "stylesheet", href = "fonts.css"),
      tags$link(rel = "stylesheet", href = paste0("trisk.css?v=", as.integer(Sys.time()))),
      tags$script(src = paste0("trisk.js?v=", as.integer(Sys.time())))
    ),

    tabItems(
      # ============================================
      # Welcome Tab
      # ============================================
      tabItem(tabName = "welcome",
        fluidRow(
          box(
            title = "Welcome",
            status = "danger",
            solidHeader = TRUE,
            width = 12,
            h4("TRISK Desktop: desktop tool for climate transition risk stress testing analysis",
               class = "font-heading"),
            p("TRISK helps financial institutions assess the impact of climate transition
              scenarios on their loan portfolios. It uses forward-looking scenario analysis
              to estimate changes in company-level Net Present Value (NPV) and Probability
              of Default (PD) under different climate policy pathways."),
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("How to use this tool"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                tags$ol(
                  tags$li(strong("Upload Data:"), " Upload your portfolio, assets, and financial data (or load demo data from the trisk.model package)."),
                  tags$li(strong("Configure:"), " Select baseline (business-as-usual) and target (transition goal) climate scenarios, set model parameters and shock year."),
                  tags$li(strong("Run:"), " Execute the stress test. The model computes NPV and PD under both scenarios for each company-sector-technology combination."),
                  tags$li(strong("Review Results:"), " Examine NPV changes, PD shifts, Expected Loss changes, and exposure impacts across your portfolio."),
                  tags$li(strong("Integrate:"), " Combine TRISK outputs with your institution's internal PD and Expected Loss estimates using absolute, relative, or Z-score (Basel IRB) methods."),
                  tags$li(strong("Download:"), " Export all results as Excel, CSV, or JSON for your records and reporting.")
                )
              )
            ),
            fluidRow(
              valueBox(
                value = as.character(
                  if (!is.null(scenarios_data_preloaded)) length(unique(scenarios_data_preloaded$scenario)) else 0L
                ),
                subtitle = "Pre-loaded Scenarios",
                icon = icon("chart-area"),
                color = if (!is.null(scenarios_data_preloaded)) "green" else "red",
                width = 4
              ),
              valueBox(
                value = if (!is.null(carbon_data_internal)) "Ready" else "Missing",
                subtitle = "Carbon Price Data",
                icon = icon("leaf"),
                color = if (!is.null(carbon_data_internal)) "green" else "red",
                width = 4
              ),
              valueBox(
                value = as.character(packageVersion("trisk.model")),
                subtitle = "Model Version",
                icon = icon("code-branch"),
                color = "blue",
                width = 4
              )
            )
          )
        )
      ),

      # ============================================
      # Upload Tab
      # ============================================
      tabItem(tabName = "upload",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 1: Load or upload your data"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p("You need four datasets to run the analysis. Click 'Load Demo Data' to use
                  the test datasets bundled with the trisk.model and trisk.analysis R packages,
                  or upload your own CSV files below."),
                tags$ul(
                  tags$li(strong("Portfolio:"), " Your loan book with company_id, company_name, country_iso2, exposure_value_usd, term, and loss_given_default."),
                  tags$li(strong("Assets:"), " Company-level production data: company_id, sector, technology, production_year, capacity, capacity_factor, emission_factor."),
                  tags$li(strong("Financial Features:"), " Company financial metrics: company_id, pd (probability of default), net_profit_margin, debt_equity_ratio, volatility."),
                  tags$li(strong("Scenarios:"), " Climate transition pathways with scenario_geography, scenario, sector, technology, scenario_year, scenario_price.")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Quick Start: Load Demo Data",
            status = "success",
            solidHeader = TRUE,
            width = 12,
            p("Load the mock datasets bundled with the trisk.model and trisk.analysis R packages to test the analysis pipeline."),
            actionButton("load_mock_data", "Load Demo Data from trisk.model",
                        class = "btn-success btn-lg",
                        icon = icon("database")),
            uiOutput("mock_data_status")
          )
        ),
        fluidRow(
          # Portfolio Upload
          uiOutput("portfolio_upload_box"),
          uiOutput("assets_upload_box")
        ),

        fluidRow(
          uiOutput("financial_upload_box"),
          uiOutput("scenarios_upload_box")
        ),

        fluidRow(
          box(
            title = "Data Quality Overview",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            uiOutput("data_quality_dashboard"),
            hr(),
            uiOutput("upload_summary"),
            conditionalPanel(
              condition = "output.has_data_issues",
              hr(),
              downloadButton("download_error_report", "Download Error Report CSV",
                             class = "btn-warning", icon = icon("file-csv"))
            )
          )
        )
      ),

      # ============================================
      # Configuration Tab
      # ============================================
      tabItem(tabName = "config",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 2: Configure your analysis parameters"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p("Select the baseline (business-as-usual) and target (climate shock) scenarios.
                  Then adjust the model parameters that control the discounted cash flow (DCF)
                  valuation and default probability estimation."),
                tags$ul(
                  tags$li(strong("Baseline scenario:"), " Represents business-as-usual, e.g. Current Policies (CP)."),
                  tags$li(strong("Target scenario:"), " Represents the climate transition shock, e.g. Net Zero 2050 (NZ2050)."),
                  tags$li(strong("Scenario geography:"), " Regional scope for scenario pathways (e.g. Global, US, EU)."),
                  tags$li(strong("Shock year:"), " The year when the policy shock is applied in the model.")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Scenarios",
            status = "warning",
            solidHeader = TRUE,
            width = 6,

            # Dynamic sector-aware scenario rows (generated by server.R)
            uiOutput("scenario_rows_ui"),

            selectInput("scenario_geography", "Scenario Geography",
                        choices = NULL, selected = NULL),
            hr(),
            uiOutput("scenario_info"),
            uiOutput("scenario_warnings")

            # --- MULTI-SCENARIO PAIR SYSTEM (temporarily disabled) ---
            # The pair system below supports multiple baseline-target pairs
            # and quick-select buttons. Commented out for single-scenario testing.
            # Re-enable by uncommenting this block and removing the single dropdowns above.
            #
            # fluidRow(
            #   column(5, tags$label("Baseline", class = "control-label")),
            #   column(5, tags$label("Target (Shock)", class = "control-label")),
            #   column(2)
            # ),
            # div(id = "scenario_pairs_container"),
            # div(class = "mb-8",
            #   actionButton("add_pair", "Add Scenario Pair", icon = icon("plus"),
            #                class = "btn btn-default btn-sm")
            # ),
            # div(class = "mb-12",
            #   tags$small(tags$b("Quick select:"), class = "mr-6"),
            #   actionButton("sel_ngfs_nz", HTML("&#x1F30D; Latest NGFS Net Zero"),
            #                class = "btn btn-default btn-xs btn-tag"),
            #   actionButton("sel_sector", HTML("&#x1F3ED; Single-Sector Scenarios"),
            #                class = "btn btn-default btn-xs btn-tag"),
            #   actionButton("sel_ipr", HTML("&#x1F4CA; Latest IPR Scenario"),
            #                class = "btn btn-default btn-xs btn-tag"),
            #   actionButton("sel_clear_targets", "Clear",
            #                class = "btn btn-default btn-xs btn-tag")
            # ),
            # helpText("Quick-select loads curated scenario pairs. ",
            #          "Multiple pairs enable the ",
            #          tags$b("Scenario Comparison"), " tab."),
            # --- END MULTI-SCENARIO PAIR SYSTEM ---
          ),

          column(width = 6,
            box(
              title = "Shock Scenario Year",
              status = "warning",
              solidHeader = TRUE,
              width = 12,
              selectizeInput("shock_years", NULL,
                             choices = seq(2025, 2040, by = 1),
                             selected = config_defaults()$shock_years,
                             multiple = TRUE,
                             options = list(
                               plugins = list("remove_button"),
                               placeholder = "Add shock years (2025\u20132040)..."
                             )),
              helpText("The shock year determines ", tags$b("when"), " the climate transition shock occurs \u2014 ",
                       "the point at which companies are forced from the baseline trajectory onto the target pathway. ",
                       "Multiple years enable the ", tags$b("Horizon Analysis"), " tab for time-series risk evolution.")
            ),
            box(
              title = "Model Parameters",
              status = "warning",
              solidHeader = TRUE,
              collapsible = TRUE,
              collapsed = FALSE,
              width = 12,
              fluidRow(
                column(8, sliderInput("risk_free_rate", "Risk-Free Rate",
                          min = 0, max = 0.1, value = config_defaults()$risk_free_rate, step = 0.001)),
                column(4, numericInput("risk_free_rate_num", "Exact value",
                          value = config_defaults()$risk_free_rate, min = 0, max = 0.1, step = 0.001))
              ),
              helpText("Risk-free interest rate used in DCF discounting."),
              fluidRow(
                column(8, sliderInput("discount_rate", "Discount Rate",
                          min = 0, max = 0.2, value = config_defaults()$discount_rate, step = 0.001)),
                column(4, numericInput("discount_rate_num", "Exact value",
                          value = config_defaults()$discount_rate, min = 0, max = 0.2, step = 0.001))
              ),
              helpText("Discount rate of dividends per year in the DCF model."),
              fluidRow(
                column(8, sliderInput("growth_rate", "Growth Rate",
                          min = 0, max = 0.1, value = config_defaults()$growth_rate, step = 0.001)),
                column(4, numericInput("growth_rate_num", "Exact value",
                          value = config_defaults()$growth_rate, min = 0, max = 0.1, step = 0.001))
              ),
              helpText("Terminal growth rate of profits beyond the final year in the DCF."),
              fluidRow(
                column(8, sliderInput("market_passthrough", "Market Passthrough",
                          min = 0, max = 1, value = config_defaults()$market_passthrough, step = 0.01)),
                column(4, numericInput("market_passthrough_num", "Exact value",
                          value = config_defaults()$market_passthrough, min = 0, max = 1, step = 0.01))
              ),
              helpText("Firm's ability to pass carbon tax costs onto the consumer (0 = none, 1 = full)."),
              hr(),
              h5("Carbon Price Model", class = "font-heading fw-600"),
              radioButtons("carbon_price_model", NULL,
                           choices = c("No Carbon Tax" = "no_carbon_tax",
                                     "Apply Carbon Tax" = "carbon_tax"),
                           selected = config_defaults()$carbon_price_model,
                           inline = TRUE),
              helpText("Select whether to include carbon pricing effects in the analysis.
                       When enabled, the model applies scenario-specific carbon prices to company emissions."),
              hr(),
              actionButton("reset_defaults", "Reset All to Defaults",
                           icon = icon("undo"),
                           class = "btn-default mt-8")
            )
          )
        )
      ),

      # ============================================
      # Run Tab
      # ============================================
      tabItem(tabName = "run",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 3: Run the TRISK stress test"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p("Verify that all data is uploaded and parameters are configured, then click
                  'Run TRISK Analysis'. The model will:"),
                tags$ol(
                  tags$li("Match your portfolio companies to the assets database by company_id (or fuzzy-match by company_name)."),
                  tags$li("For each matched company-sector-technology, compute production trajectories under baseline and shock scenarios."),
                  tags$li("Calculate Net Present Value (NPV) under both scenarios using a DCF model."),
                  tags$li("Derive Probability of Default (PD) changes from NPV shifts using the Merton structural model."),
                  tags$li("Compute Expected Loss = -(Exposure at Default x PD) for both scenarios.")
                )
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Pre-Run Checklist",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("run_checklist")
          ),

          box(
            title = "Run Configuration Summary",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            verbatimTextOutput("config_summary")
          )
        ),

        fluidRow(
          box(
            width = 12,
            div(class = "run-action-wrap",
              actionButton("run_analysis", "Run TRISK Analysis",
                          class = "btn-primary btn-lg btn-run",
                          icon = icon("play-circle")),
              br(), br(),
              helpText("This may take a few minutes depending on portfolio size.")
            ),
            hr(),
            h4("Analysis Log"),
            verbatimTextOutput("analysis_log"),
            uiOutput("run_id_display")
          )
        )
      ),

      # ============================================
      # Results Tab - Portfolio Results
      # ============================================
      tabItem(tabName = "results",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 4: Review your portfolio results"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p("The value boxes below show portfolio-level headline metrics. The tabs contain
                  detailed breakdowns:"),
                tags$ul(
                  tags$li(strong("Summary:"), " Sector/technology breakdown with average NPV change and PD."),
                  tags$li(strong("Exposures PD:"), " Per-company PD baseline vs shock, PD change, Expected Loss change, with color-coded columns."),
                  tags$li(strong("Exposures NPV:"), " Per-company NPV baseline vs shock and percentage value change.")
                ),
                p("Positive PD/EL changes (red) indicate increased risk under the transition scenario.
                  Negative changes (green) indicate decreased risk.")
              )
            )
          )
        ),
        uiOutput("value_cards_row"),
        # Run History panel (collapsible)
        uiOutput("run_history_panel"),
        fluidRow(
          tabBox(
            width = 12,
            title = "Portfolio Results",

            tabPanel("Summary",
              uiOutput("summary_header"),
              tags$p(class = "fs-12 fg-secondary mb-8", tags$i(class = "fa fa-info-circle mr-6"), "Portfolio-level summary of TRISK stress test results. Shows average NPV change and PD shock by sector and technology, exposure-weighted across your loan book."),
              DTOutput("results_summary_table")
            ),

            tabPanel("Credit Risk Analysis",
              uiOutput("pd_analysis_header"),
              tags$div(class = "fs-12 fg-secondary mb-8",
                tags$span("Bar chart: "),
                tags$span("Red", style = "color:#F53D3F;font-weight:600;"),
                tags$span(" = PD increase (deterioration) | "),
                tags$span("Green", style = "color:#3D8B5E;font-weight:600;"),
                tags$span(" = PD decrease (improvement)"),
                tags$span(" \u00a0\u00b7\u00a0 ", style = "color:#AAA;"),
                tags$span("Sectors: "),
                tags$span("\u25cf", style = "color:#8B6914;"), tags$span(" Oil&Gas "),
                tags$span("\u25cf", style = "color:#1A1A1A;"), tags$span(" Coal "),
                tags$span("\u25cf", style = "color:#4A7FA5;"), tags$span(" Power "),
                tags$span("\u25cf", style = "color:#C0393B;"), tags$span(" Automotive "),
                tags$span("\u25cf", style = "color:#888888;"), tags$span(" Steel")
              ),
              fluidRow(
                box(width = 12, plotlyOutput("plot_pd_sector_enhanced", height = "420px"))
              ),
              DTOutput("pd_table")
            ),

            tabPanel("Risk Concentration",
              uiOutput("concentration_ui")
            ),

            tabPanel("Attribution",
              uiOutput("attribution_ui")
            ),

            tabPanel("Horizon Analysis",
              uiOutput("horizon_analysis_ui")
            ),

            # --- SCENARIO COMPARISON TAB (temporarily disabled) ---
            # Requires multi-scenario pair system. Re-enable when multi-pair is restored.
            # tabPanel("Scenario Comparison",
            #   uiOutput("scenario_comparison_ui")
            # ),
            # --- END SCENARIO COMPARISON TAB ---

            # --- NPV Analysis tab removed — NPV is intermediate; key data in Summary table ---
            # tabPanel("Exposure NPV Analysis",
            #   uiOutput("npv_analysis_header"),
            #   DTOutput("npv_table")
            # )
            NULL  # trailing NULL for clean comma handling
          )
        )
      ),

      # ============================================
      # Scenario Sensitivity Tab
      # ============================================
      tabItem(tabName = "sensitivity",
        uiOutput("sensitivity_content_ui")
      ),

      # ============================================
      # Integration Tab - Integrating Results
      # ============================================
      tabItem(tabName = "integration",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 6: Risk Integration — combine TRISK results with your internal estimates"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p(tags$b("Optional step."), " Use this page to combine TRISK climate stress test
                  outputs with your institution's internally calculated PD and Expected Loss.
                  Skip this step if you only need the raw TRISK results from Step 4."),
                tags$table(class = "table table-bordered table-condensed def-table integration-guidance-table",
                  tags$thead(
                    tags$tr(
                      tags$th("Method"), tags$th("Use When"), tags$th("Formula")
                    )
                  ),
                  tags$tbody(
                    tags$tr(
                      tags$td(tags$b("Absolute Change")),
                      tags$td("You want to add the raw TRISK PD/EL shift to your own estimates"),
                      tags$td(tags$code("Internal + (Shock - Baseline)"))
                    ),
                    tags$tr(
                      tags$td(tags$b("Relative Change")),
                      tags$td("You prefer a proportional adjustment based on TRISK multipliers"),
                      tags$td(tags$code("Internal * (Shock / Baseline)"))
                    ),
                    tags$tr(
                      tags$td(tags$b("Z-Score (Basel IRB)")),
                      tags$td("Basel IRB-aligned institutions using the Vasicek one-factor model (PD only)"),
                      tags$td(HTML(paste0(tags$code(HTML("&Phi;(&Phi;<sup>-1</sup>(Int) + &Phi;<sup>-1</sup>(Shock) - &Phi;<sup>-1</sup>(Base))")))))
                    )
                  )
                ),
                p(class = "upload-hint",
                  "Upload a CSV with ", tags$code("company_id"), " + ",
                  tags$code("internal_pd"), " (or ", tags$code("internal_el"),
                  ") columns, or edit values directly in the tables below.")
              )
            )
          )
        ),

        # Method Decision Guide
        fluidRow(
          box(
            width = 12,
            status = "info",
            solidHeader = FALSE,
            div(class = "card-surface",
              h5(icon("route"), " Which integration method should I choose?",
                 class = "font-heading mt-0 mb-8"),
              fluidRow(
                column(4,
                  tags$div(class = "method-guide-card",
                    h6(tags$strong("Absolute Change"), class = "fg-brand-coral mt-0"),
                    tags$p("Your internal PDs are ", tags$em("point-in-time"), " estimates and you want to overlay the raw climate stress impact as an additive adjustment.", class = "fs-12"),
                    tags$p(tags$code("Adjusted = Internal + Shock_delta"), class = "fs-11 fg-secondary")
                  )
                ),
                column(4,
                  tags$div(class = "method-guide-card",
                    h6(tags$strong("Relative Change"), class = "fg-brand-coral mt-0"),
                    tags$p("Your internal PDs are ", tags$em("through-the-cycle"), " and you prefer a proportional scaling that respects your existing PD level.", class = "fs-12"),
                    tags$p(tags$code("Adjusted = Internal \u00d7 (1 + Change%)"), class = "fs-11 fg-secondary")
                  )
                ),
                column(4,
                  tags$div(class = "method-guide-card",
                    h6(tags$strong("Z-Score (Basel IRB)"), class = "fg-brand-coral mt-0"),
                    tags$p("You use ", tags$em("Basel III/IV IRB models"), " with Vasicek framework. Combines PDs in normal quantile space, preserving tail-risk behavior.", class = "fs-12"),
                    tags$p(tags$code(HTML("Adjusted = &Phi;(&Phi;\u207B\u00B9(Int) + z\u0394)")), class = "fs-11 fg-secondary"),
                    tags$small("Recommended for regulatory capital calculations.", class = "fg-secondary")
                  )
                )
              )
            )
          )
        ),

        # --- PD Integration ---
        fluidRow(
          box(
            title = "PD Integration",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4,
                selectInput("pd_integration_method", "Integration Method:",
                  choices = c(
                    "Absolute Change" = "absolute",
                    "Relative Change" = "relative",
                    "Z-Score (Basel IRB)" = "zscore"
                  ),
                  selected = "absolute"
                )
              ),
              column(8,
                uiOutput("pd_method_description")
              )
            ),
            hr(),
            # Internal PD upload & explanation
            fluidRow(
              column(8,
                tags$div(class = "d-flex align-center gap-8 mb-6",
                  h5(icon("info-circle"), "Internal PD Values", class = "m-0"),
                  tags$button(id = "toggle_pd_info",
                    class = "btn btn-xs btn-help-toggle info-toggle-btn",
                    `data-target` = ".pd-info-box",
                    icon("question-circle"), " Help")
                ),
                div(class = "alert alert-info pd-info-box info-box-hidden",
                  tags$b("Internal_PD"), " represents ", tags$b("your institution's own PD estimates"),
                  " for each counterparty. This is ", tags$em("conceptually separate"), " from ",
                  tags$b("pd_baseline"), ", which is the TRISK model's baseline PD derived from the ",
                  "climate scenario analysis.", tags$br(), tags$br(),
                  "By default, Internal_PD is pre-filled with pd_baseline values as a starting point. ",
                  "To use your own internal PDs, either:", tags$br(),
                  tags$b("(a)"), " Upload a CSV file (with ", tags$code("company_id"),
                  " and ", tags$code("internal_pd"), " columns), or", tags$br(),
                  tags$b("(b)"), " Edit cells directly in the table below."
                )
              ),
              column(4,
                fileInput("upload_internal_pd", "Upload Internal PDs (CSV)",
                          accept = c(".csv", "text/csv"),
                          placeholder = "Optional: company_id, internal_pd"),
                uiOutput("internal_pd_upload_status")
              )
            ),
            DTOutput("pd_integration_table"),
            hr(),
            actionButton("apply_pd_integration", "Calculate TRISK-Adjusted PD",
                        class = "btn-trisk btn-lg", icon = icon("calculator")),
            br(), br(),
            # Portfolio-Level PD Aggregates — prominent ValueBoxes ABOVE the detail table
            uiOutput("pd_integration_portfolio_summary"),
            DTOutput("pd_integration_results"),
            # PD Sector Breakdown
            uiOutput("pd_sector_breakdown"),
            # PD Adjustment Bar Chart
            plotlyOutput("pd_adjustment_chart", height = "350px"),
            br()
          )
        ),

        # --- EL Integration ---
        fluidRow(
          box(
            title = "Expected Loss Integration",
            status = "warning",
            solidHeader = TRUE,
            width = 12,
            fluidRow(
              column(4,
                selectInput("el_integration_method", "Integration Method:",
                  choices = c(
                    "Absolute Change" = "absolute",
                    "Relative Change" = "relative"
                  ),
                  selected = "absolute"
                )
              ),
              column(8,
                uiOutput("el_method_description")
              )
            ),
            hr(),
            # Internal EL upload & explanation
            fluidRow(
              column(8,
                tags$div(class = "d-flex align-center gap-8 mb-6",
                  h5(icon("info-circle"), "Internal EL Values", class = "m-0"),
                  tags$button(id = "toggle_el_info",
                    class = "btn btn-xs btn-help-toggle info-toggle-btn",
                    `data-target` = ".el-info-box",
                    icon("question-circle"), " Help")
                ),
                div(class = "alert alert-info el-info-box info-box-hidden",
                  tags$b("Internal_EL"), " represents ", tags$b("your institution's own Expected Loss estimates"),
                  " for each counterparty. This is ", tags$em("conceptually separate"), " from ",
                  tags$b("expected_loss_baseline"), ", which is the TRISK model's EL derived from ",
                  "the climate scenario analysis.", tags$br(), tags$br(),
                  "By default, Internal_EL is pre-filled with expected_loss_baseline values. ",
                  "To use your own internal ELs, either:", tags$br(),
                  tags$b("(a)"), " Upload a CSV file (with ", tags$code("company_id"),
                  " and ", tags$code("internal_el"), " columns), or", tags$br(),
                  tags$b("(b)"), " Edit cells directly in the table below."
                )
              ),
              column(4,
                fileInput("upload_internal_el", "Upload Internal ELs (CSV)",
                          accept = c(".csv", "text/csv"),
                          placeholder = "Optional: company_id, internal_el"),
                uiOutput("internal_el_upload_status")
              )
            ),
            DTOutput("el_integration_table"),
            hr(),
            actionButton("apply_el_integration", "Calculate TRISK-Adjusted EL",
                        class = "btn-trisk btn-lg", icon = icon("calculator")),
            br(), br(),
            # Portfolio-Level EL Aggregates — prominent ValueBoxes ABOVE the detail table
            uiOutput("el_integration_portfolio_summary"),
            # Sector/Technology EL Breakdown
            uiOutput("el_sector_breakdown"),
            # EL Adjustment Bar Chart
            plotlyOutput("el_adjustment_chart", height = "350px"),
            br(),
            DTOutput("el_integration_results")
          )
        )
      ),

      # ============================================
      # Download Tab
      # ============================================
      tabItem(tabName = "download",
        fluidRow(
          box(
            width = 12,
            div(class = "guidance-wrapper",
              tags$button(class = "guidance-toggle-btn",
                tags$span("Step 7: Download your results"),
                tags$span(class = "chevron", HTML("&#9660;"))
              ),
              div(class = "guidance-content",
                p("Export the analysis results in your preferred format. The Excel file includes
                  multiple sheets: Summary, Full Results, and (if computed) PD Integration
                  and EL Integration results.")
              )
            )
          )
        ),
        fluidRow(
          box(
            title = "Download Results",
            status = "success",
            solidHeader = TRUE,
            width = 6,
            h4("Export Analysis Results"),
            br(),
            downloadButton("download_excel", "Download All Results (Excel)",
                          class = "btn-primary btn-lg"),
            br(), br(),
            downloadButton("download_csv", "Download Results (CSV)",
                          class = "btn-info"),
            br(), br(),
            downloadButton("download_json", "Download Results (JSON)",
                          class = "btn-info"),
            hr(),
            downloadButton("download_report_html", "Download HTML Report",
                          class = "btn-success"),
            br(), br(),
            helpText("Excel format includes multiple sheets with detailed breakdowns. ",
                     "The HTML report provides a self-contained summary suitable for sharing.")
          ),

          box(
            title = "Analysis Metadata",
            status = "info",
            solidHeader = TRUE,
            width = 6,
            uiOutput("run_id_step6"),
            verbatimTextOutput("metadata_summary"),
            hr(),
            downloadButton("download_config", "Download Configuration",
                          class = "btn-default")
          )
        )
      ),

      # ============================================
      # Documentation Tab with Definitions & Glossary
      # ============================================
      tabItem(tabName = "docs",
        fluidRow(
          box(
            title = "Documentation & Definitions",
            status = "primary",
            solidHeader = TRUE,
            width = 12,

            # ============================================
            # Methodology Overview (from 1in1000 TRISK presentation)
            # ============================================
            h3("TRISK Methodology", class = "font-heading"),
            p("TRISK is an asset-level, bottom-up climate transition risk stress test. It estimates how a late and sudden shift in climate policy would affect company profits, equity values, and default probabilities across your loan portfolio.", class = "fg-secondary"),

            # Three-layer overview
            h4("The Three-Layer Framework", class = "font-heading mt-16"),
            p("The model operates in three sequential layers. Each layer transforms the output of the previous one, building from physical production data to financial risk metrics."),

            fluidRow(
              column(4,
                div(class = "metric-card metric-info",
                  tags$small("Layer 1", class = "card-subtitle"),
                  h4("Scenarios & Trajectories", class = "section-title"),
                  p("Forward-looking production plans (5-year horizon) are aligned with climate scenarios to construct baseline and target pathways for each company.", class = "fs-12 fg-secondary")
                )
              ),
              column(4,
                div(class = "metric-card metric-warning",
                  tags$small("Layer 2", class = "card-subtitle"),
                  h4("Economic Impact", class = "section-title"),
                  p("A shock scenario forces misaligned companies to decarbonize. Net profits are calculated from production volume, price, carbon tax, and net profit margin.", class = "fs-12 fg-secondary")
                )
              ),
              column(4,
                div(class = "metric-card metric-danger",
                  tags$small("Layer 3", class = "card-subtitle"),
                  h4("Financial Risk", class = "section-title"),
                  p("Discounted cash flow model derives NPV change (market risk). An adjusted Merton structural model converts equity loss into PD change (credit risk).", class = "fs-12 fg-secondary")
                )
              )
            ),

            hr(),

            # Layer 1 detail
            h4("Layer 1: Scenarios & Company Trajectories", class = "font-heading mt-16"),
            p("TRISK uses the PACTA alignment approach to construct company-level scenario pathways. For each company-technology combination:"),
            tags$ul(
              tags$li(tags$strong("Baseline scenario (business-as-usual):"), " Production is extended from the last year of planned data using a business-as-usual trajectory (e.g., NGFS Current Policies). This represents what happens if current climate policies remain unchanged."),
              tags$li(tags$strong("Target scenario (transition goal):"), " Production is adjusted from the first year of planned data to align with an ambitious climate pathway (e.g., NGFS Net Zero 2050). This represents the end-state goal that companies must eventually reach."),
              tags$li(tags$strong("Shock scenario (model-computed):"), " At the shock year, the company is forced to migrate from the baseline to the target trajectory. This is not a user-selected scenario \u2014 it is computed by the model as a sudden policy-driven adjustment. Misaligned firms must also compensate for excess historical production above the target pathway.")
            ),
            p("This forward-looking approach is essential: a company's risk position depends on its strategic readiness for a climate transition, not just its current production mix.", class = "fg-secondary fs-12"),

            hr(),

            # Layer 2 detail
            h4("Layer 2: Real Economic Impact", class = "font-heading mt-16"),
            p("The baseline and shock production trajectories drive the calculation of net profits. The profit equation has four channels:"),
            tags$div(class = "card-surface mt-8 mb-16",
              tags$code("Net Profit = Production Volume × (Price − Carbon Tax × Emission Factor) × Net Profit Margin", class = "fs-13")
            ),
            tags$ul(
              tags$li(tags$strong("Production volume"), " — the dominant driver. Shock-scenario production drops sharply for misaligned high-carbon firms."),
              tags$li(tags$strong("Price"), " — commodity price projections from scenario providers (more subdued impact)."),
              tags$li(tags$strong("Carbon tax"), " — applied as an emission cost per unit of production for high-carbon technologies, only after the shock year."),
              tags$li(tags$strong("Net profit margin"), " — converts revenue impact to bottom-line profit impact.")
            ),
            p("For increasing (green) technologies, a proximity-to-target adjustment accounts for the capital costs of building out new capacity.", class = "fg-secondary fs-12"),

            hr(),

            # Layer 3 detail
            h4("Layer 3: Financial Risk", class = "font-heading mt-16"),
            h5("Market Risk (NPV)", class = "font-heading"),
            p("A discounted cash flow model calculates the net present value of each company under both baseline and shock scenarios. The relative change in valuation represents the market risk: how much equity value a company loses due to the climate transition shock."),

            h5("Credit Risk (PD)", class = "font-heading mt-8"),
            p("An adjusted Merton structural credit risk model derives the Probability of Default under each scenario. The model uses the company's balance sheet (market value of assets, liability structure, equity volatility) and the DCF-derived equity change to estimate PD shifts."),
            p(tags$em("Important:"), " The PD changes incorporate the discounted shock effect across the full stress test horizon. They represent the potential range of default risk given a specific transition pathway — not the true probability of a firm defaulting today.", class = "fg-secondary fs-12"),

            hr(),

            # Key design principles
            h4("Design Principles", class = "font-heading mt-16"),
            tags$table(class = "def-table",
              tags$tr(tags$td("Bottom-up"), tags$td("Starts with granular asset-level data and individual company production plans, then aggregates to portfolio.")),
              tags$tr(tags$td("Forward-looking"), tags$td("Uses 5-year production plans, not historical emissions. Distinguishes companies planning for transition from those that are not.")),
              tags$tr(tags$td("Data agnostic"), tags$td("Accepts data from multiple providers (Asset Impact, Asset Resolution, custom data). Not locked to a single vendor.")),
              tags$tr(tags$td("Multi-scenario"), tags$td("Supports NGFS, IEA, IPR, Oxford-INET, Mission Possible, JRC-GECO scenarios. Different scenarios yield different risk profiles.")),
              tags$tr(tags$td("Open source"), tags$td("All model code and methodology published openly. No black box — every calculation is auditable."))
            ),

            hr(),

            h3("Input Data Definitions", class = "font-heading"),

            h4("Portfolio Data"),
            tags$table(class = "def-table",
              tags$tr(tags$td("company_id"), tags$td("Unique company identifier, used to match across datasets.")),
              tags$tr(tags$td("company_name"), tags$td("Company name, used for fuzzy matching when company_id is unavailable.")),
              tags$tr(tags$td("country_iso2"), tags$td("ISO 3166-1 alpha-2 country code for the company's primary jurisdiction.")),
              tags$tr(tags$td("exposure_value_usd"), tags$td("Total loan exposure to the company, in US dollars.")),
              tags$tr(tags$td("term"), tags$td("Loan maturity term in years.")),
              tags$tr(tags$td("loss_given_default"), tags$td("Expected loss rate given default (0 to 1). LGD = 1 - recovery rate."))
            ),

            hr(),
            h4("Assets Data"),
            tags$table(class = "def-table",
              tags$tr(tags$td("company_id"), tags$td("Unique company identifier.")),
              tags$tr(tags$td("company_name"), tags$td("Company name.")),
              tags$tr(tags$td("asset_id"), tags$td("Unique identifier for a physical production asset.")),
              tags$tr(tags$td("country_iso2"), tags$td("Country where the asset is located.")),
              tags$tr(tags$td("asset_name"), tags$td("Name of the production asset.")),
              tags$tr(tags$td("sector"), tags$td("Industry sector (e.g. Power, Automotive, Coal, Oil&Gas, Steel, Cement).")),
              tags$tr(tags$td("technology"), tags$td("Production technology (e.g. CoalCap, GasCap, RenewablesCap, OilCap, Electric, ICE).")),
              tags$tr(tags$td("production_year"), tags$td("Year of the production observation.")),
              tags$tr(tags$td("emission_factor"), tags$td("CO2 emission factor per unit of production.")),
              tags$tr(tags$td("capacity"), tags$td("Installed production capacity.")),
              tags$tr(tags$td("capacity_factor"), tags$td("Utilization rate of the capacity (0 to 1).")),
              tags$tr(tags$td("production_unit"), tags$td("Unit of measurement for production (e.g. MW, tonnes, barrels)."))
            ),

            hr(),
            h4("Financial Features Data"),
            tags$table(class = "def-table",
              tags$tr(tags$td("company_id"), tags$td("Unique company identifier.")),
              tags$tr(tags$td("pd"), tags$td("Probability of Default, the company's current through-the-cycle PD.")),
              tags$tr(tags$td("net_profit_margin"), tags$td("Net profit as a proportion of revenue.")),
              tags$tr(tags$td("debt_equity_ratio"), tags$td("Total debt divided by total equity.")),
              tags$tr(tags$td("volatility"), tags$td("Asset value volatility, used in the Merton structural model."))
            ),

            hr(),
            h4("Scenarios Data"),
            tags$table(class = "def-table",
              tags$tr(tags$td("scenario_geography"), tags$td("Regional scope (e.g. Global, US, EU).")),
              tags$tr(tags$td("scenario"), tags$td("Scenario name (e.g. NGFS2023GCAM_CP, NGFS2023GCAM_NZ2050).")),
              tags$tr(tags$td("scenario_pathway"), tags$td("Source pathway or model (e.g. GCAM, REMIND, MESSAGEix).")),
              tags$tr(tags$td("scenario_type"), tags$td("'baseline' or 'target', used to filter scenario dropdowns.")),
              tags$tr(tags$td("sector"), tags$td("Industry sector.")),
              tags$tr(tags$td("technology"), tags$td("Production technology.")),
              tags$tr(tags$td("scenario_year"), tags$td("Year of the scenario data point.")),
              tags$tr(tags$td("scenario_price"), tags$td("Projected commodity price under this scenario.")),
              tags$tr(tags$td("price_unit"), tags$td("Currency unit for scenario_price.")),
              tags$tr(tags$td("pathway_unit"), tags$td("Unit for the production pathway (e.g. GW, Mt).")),
              tags$tr(tags$td("technology_type"), tags$td("Classification of technology (e.g. 'green', 'brown')."))
            ),

            hr(),
            h4("Carbon Price Data"),
            tags$table(class = "def-table",
              tags$tr(tags$td("year"), tags$td("Year of the carbon price observation.")),
              tags$tr(tags$td("model"), tags$td("Integrated Assessment Model source.")),
              tags$tr(tags$td("scenario"), tags$td("Scenario name.")),
              tags$tr(tags$td("scenario_geography"), tags$td("Regional scope.")),
              tags$tr(tags$td("carbon_tax"), tags$td("Carbon price in USD per tonne CO2."))
            ),

            hr(),

            h3("Model Parameters", class = "font-heading"),
            tags$table(class = "def-table",
              tags$tr(tags$td("baseline_scenario"), tags$td("The business-as-usual scenario representing current climate policies (e.g., NGFS Current Policies).")),
              tags$tr(tags$td("target_scenario"), tags$td("The climate transition goal scenario (e.g., Net Zero 2050). The model computes a shock trajectory from baseline to target at the shock year.")),
              tags$tr(tags$td("scenario_geography"), tags$td("Regional scope for scenario filtering.")),
              tags$tr(tags$td("shock_year"), tags$td("Year when the climate policy shock is applied in the model.")),
              tags$tr(tags$td("risk_free_rate"), tags$td("Risk-free interest rate used in DCF discounting.")),
              tags$tr(tags$td("discount_rate"), tags$td("Discount rate of dividends per year in the DCF model.")),
              tags$tr(tags$td("growth_rate"), tags$td("Terminal growth rate of profits beyond the final year in the DCF.")),
              tags$tr(tags$td("market_passthrough"), tags$td("Firm's ability to pass carbon tax costs to consumers (0 = none, 1 = full).")),
              tags$tr(tags$td("carbon_price_model"), tags$td("Whether to apply scenario-based carbon tax ('carbon_tax' or 'no_carbon_tax').")),
              tags$tr(tags$td("div_netprofit_prop_coef"), tags$td("Coefficient determining how strongly future dividends propagate to company value."))
            ),

            hr(),

            h3("Output Definitions", class = "font-heading"),

            h4("NPV Results"),
            tags$table(class = "def-table",
              tags$tr(tags$td("net_present_value_baseline"), tags$td("Discounted sum of future cash flows under the baseline scenario.")),
              tags$tr(tags$td("net_present_value_shock"), tags$td("Discounted sum of future cash flows under the shock (target) scenario.")),
              tags$tr(tags$td("net_present_value_difference"), tags$td("NPV_shock - NPV_baseline. Negative = value loss from climate transition.")),
              tags$tr(tags$td("crispy_perc_value_change"), tags$td("Percentage NPV change: (NPV_shock - NPV_baseline) / NPV_baseline.")),
              tags$tr(tags$td("crispy_value_loss"), tags$td("Dollar impact: crispy_perc_value_change x exposure_value_usd."))
            ),

            hr(),
            h4("PD Results"),
            tags$table(class = "def-table",
              tags$tr(tags$td("pd_baseline"), tags$td("Probability of Default under the baseline (business-as-usual) scenario.")),
              tags$tr(tags$td("pd_shock"), tags$td("Probability of Default under the shock (target climate) scenario.")),
              tags$tr(tags$td("PD_Change"), tags$td("pd_shock - pd_baseline. Positive = increased default risk.")),
              tags$tr(tags$td("PD_Change_Pct"), tags$td("Percentage change in PD: (pd_shock - pd_baseline) / pd_baseline x 100."))
            ),

            hr(),
            h4("Expected Loss Results"),
            tags$table(class = "def-table",
              tags$tr(tags$td("exposure_at_default (EAD)"), tags$td("exposure_value_usd x loss_given_default.")),
              tags$tr(tags$td("expected_loss_baseline"), tags$td("-(EAD x pd_baseline). Expected loss under baseline scenario.")),
              tags$tr(tags$td("expected_loss_shock"), tags$td("-(EAD x pd_shock). Expected loss under shock scenario.")),
              tags$tr(tags$td("expected_loss_difference"), tags$td("-(EAD x (pd_shock - pd_baseline)). Change in expected loss.")),
              tags$tr(tags$td("EL_Change"), tags$td("expected_loss_shock - expected_loss_baseline.")),
              tags$tr(tags$td("EL_Change_Pct"), tags$td("Percentage change in EL: (EL_shock - EL_baseline) / EL_baseline x 100."))
            ),

            hr(),
            h4("Integration Methods"),
            tags$table(class = "def-table",
              tags$tr(tags$td("Absolute Change"), tags$td("Adjusted = Internal + (Shock - Baseline). Adds raw change to internal estimate.")),
              tags$tr(tags$td("Relative Change"), tags$td("Adjusted = Internal x (1 + Change%). Scales internal estimate by relative shift.")),
              tags$tr(tags$td("Z-Score (Basel IRB)"), tags$td(HTML("Adjusted_PD = &Phi;(&Phi;<sup>-1</sup>(Internal_PD) + &Phi;<sup>-1</sup>(PD_shock) - &Phi;<sup>-1</sup>(PD_baseline)). Combines PDs in normal quantile space.")))
            ),

            hr(),
            h3("External Resources", class = "font-heading"),
            tags$ul(
              tags$li(tags$a(href = "https://theia-finance-labs.github.io/trisk.model/",
                            "trisk.model Documentation", target = "_blank")),
              tags$li(tags$a(href = "https://theia-finance-labs.github.io/trisk.analysis/",
                            "trisk.analysis Documentation", target = "_blank")),
              tags$li(tags$a(href = "https://github.com/Theia-Finance-Labs",
                            "GitHub Organization", target = "_blank")),
              tags$li(tags$a(href = "https://1in1000.com",
                            "1in1000 Website", target = "_blank"))
            )
          )
        )
      ),

      # ============================================
      # About Tab - 1in1000 content
      # ============================================
      tabItem(tabName = "about",
        fluidRow(
          box(
            title = "About 1in1000",
            status = "danger",
            solidHeader = TRUE,
            width = 12,

            fluidRow(
              column(8,
                h3("Climate Risk Research, Advisory & Software",
                   class = "font-heading"),
                p("1in1000 is a climate risk research initiative, advisory practice, and
                  software development team dedicated to open-source, transparent, and
                  easy-to-use climate financial risk tools."),
                p("We operate under Theia Finance Labs, an independent non-profit think-tank
                  and research incubator pushing the boundaries of financial risk analysis,
                  resilience, and sustainability."),
                p("Through a joint research initiative with the University of Oxford Sustainable
                  Finance Group, we advance the frontiers of climate risk modelling while
                  delivering hands-on climate stress testing and capacity building with banks,
                  supervisors, and governments around the world.")
              ),
              column(4, class = "text-center pt-20",
                tags$img(src = "logo-black.png", height = "80px", class = "about-logo"),
                br(), br(),
                tags$a(href = "https://1in1000.com", "www.1in1000.com", target = "_blank",
                       class = "fg-brand-red fw-600")
              )
            ),

            hr(),

            h4("Why 1in1000?", class = "font-heading"),
            p("Climate-related financial risks pose a systemic challenge to the global economy.
              Three key factors motivate our work:"),
            fluidRow(
              column(4,
                div(class = "about-card",
                  h5(strong("Perceived as Low Probability"), class = "fg-brand-coral"),
                  p("Climate risks are perceived as a low probability tail risk, yet their
                    potential impact on financial stability is enormous.", class = "fs-13")
                )
              ),
              column(4,
                div(class = "about-card",
                  h5(strong("Uncertain but Inevitable"), class = "fg-brand-coral"),
                  p("While the timing and severity are uncertain, the transition away from
                    fossil fuels is an inevitable structural shift.", class = "fs-13")
                )
              ),
              column(4,
                div(class = "about-card",
                  h5(strong("Capacity Gap"), class = "fg-brand-coral"),
                  p("Many financial institutions, especially in emerging markets, lack the
                    tools and capacity to assess these risks.", class = "fs-13")
                )
              )
            ),

            hr(),
            h4("Our Mission", class = "font-heading"),
            p(tags$em("Transparent climate analytical tools, tailored to any institution, anywhere."),
              class = "mission-statement"),
            tags$ul(
              tags$li(strong("Transparent:"), " All models, code, and data are open-source under GPL/LGPL licenses."),
              tags$li(strong("Tailored:"), " Tools designed to work with any portfolio structure, any scenario, any geography."),
              tags$li(strong("Any institution, anywhere:"), " From central banks to commercial lenders, in developed and emerging markets alike.")
            ),

            hr(),
            h4("What We Do", class = "font-heading"),
            fluidRow(
              column(4,
                h5(icon("file-alt"), " Research"),
                tags$ul(class = "fs-13",
                  tags$li("Publications on models and scenarios"),
                  tags$li("Frameworks for transition, physical, nature, and social risk"),
                  tags$li("Projects on every continent")
                )
              ),
              column(4,
                h5(icon("briefcase"), " Application"),
                tags$ul(class = "fs-13",
                  tags$li("Stress testing exercises with banks in climate-sensitive regions"),
                  tags$li("Technical analyses for central banks and supervisors")
                )
              ),
              column(4,
                h5(icon("desktop"), " Software"),
                tags$ul(class = "fs-13",
                  tags$li("Online and desktop portfolio analysis applications"),
                  tags$li("End-to-end pipeline with scenario repository and asset-level data")
                )
              )
            ),

            hr(),
            h4("Partners", class = "font-heading"),
            p("University of Oxford, Theia Finance Labs, World Bank, International Finance Corporation,
              Inevitable Policy Response, KAPSARC, CINEA (European Union)."),

            hr(),
            h4("Contact", class = "font-heading"),
            p(tags$a(href = "mailto:1in1000@theiafinance.org", "1in1000@theiafinance.org"),
              " | ",
              tags$a(href = "https://theiafinance.org/", "Theia Finance Labs", target = "_blank")),

            h5("Business Details", class = "font-heading mt-16"),
            fluidRow(
              column(6,
                div(class = "contact-card",
                  h5(strong("Theia Finance Labs"), class = "fg-brand-coral mt-0"),
                  p("2 Degrees Investing e.V.", class = "mb-4"),
                  p(tags$em("Neue Sch\u00F6nhauserstra\u00DFe 3-5, 10178 Berlin, Germany"),
                    class = "fs-13")
                )
              ),
              column(6,
                div(class = "contact-card",
                  h5(strong("Alternative Pathways Lab, s.r.o."), class = "fg-brand-coral mt-0"),
                  p(tags$em("Na Petynce 142/136, 16900, Praha, Czech Republic"),
                    class = "fs-13")
                )
              )
            ),

            hr(),
            h4("Funding", class = "font-heading"),
            p("Co-funded by the European Union. Views and opinions expressed are however those of
              the author(s) only and do not necessarily reflect those of the European Union or CINEA."),
            p("Scientific Transition Risk Exercises for Stress tests & Scenario Analysis has received
              funding from the European Union's Life programme under Grant No. LIFE21-GIC-DE-Stress."),

            hr(),
            h4("License & Version", class = "font-heading"),
            p("trisk.model: GPL-3.0 | trisk.analysis: LGPL-3.0"),
            verbatimTextOutput("version_info"),
            br(),
            p(tags$em("A research initiative incubated by and housed at Theia Finance Labs."),
              class = "text-center fg-muted")
          )
        )
      )
    )
  )
)
