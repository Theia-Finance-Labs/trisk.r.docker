# mod_download.R
# Export handlers (Excel, CSV, JSON, Config) and metadata display

setup_download <- function(input, output, session, rv) {

  # ============================================
  # Downloads
  # ============================================

  output$download_excel <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".xlsx")
    },
    content = function(file) {
      req(rv$results)

      df <- rv$results
      grp_cols <- intersect(c("sector", "technology"), names(df))

      summary_df <- if (length(grp_cols) > 0) {
        tryCatch({
          df %>%
            group_by(across(all_of(grp_cols))) %>%
            summarise(n_rows = n(), .groups = "drop")
        }, error = function(e) df)
      } else {
        df
      }

      sheets <- list(
        "Summary" = summary_df,
        "Full Results" = sanitize_export(df)
      )

      # Add multi-horizon results if available
      if (!is.null(rv$results_by_year) && length(rv$results_by_year) > 1) {
        sheets[["Multi-Horizon"]] <- sanitize_export(bind_rows(rv$results_by_year, .id = "shock_year"))
      }

      # Add multi-scenario comparison if available
      if (!is.null(rv$results_by_scenario) && length(rv$results_by_scenario) > 1) {
        all_scen_rows <- list()
        for (scen_code in names(rv$results_by_scenario)) {
          for (yr_key in names(rv$results_by_scenario[[scen_code]])) {
            df_s <- rv$results_by_scenario[[scen_code]][[yr_key]]
            df_s$target_scenario <- scen_code
            df_s$shock_year <- as.integer(yr_key)
            all_scen_rows[[paste0(scen_code, "_", yr_key)]] <- df_s
          }
        }
        sheets[["Scenario Comparison"]] <- sanitize_export(bind_rows(all_scen_rows))
      }

      # Add integration results if available
      if (!is.null(rv$pd_integration_result)) {
        sheets[["PD Integration"]] <- sanitize_export(rv$pd_integration_result)
      }
      if (!is.null(rv$el_integration_result)) {
        sheets[["EL Integration"]] <- sanitize_export(rv$el_integration_result)
      }

      writexl::write_xlsx(sheets, file)
    }
  )

  output$download_csv <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".csv")
    },
    content = function(file) {
      req(rv$results)
      write_csv(sanitize_export(rv$results), file)
    }
  )

  output$download_json <- downloadHandler(
    filename = function() {
      paste0("trisk_results_", rv$run_id, ".json")
    },
    content = function(file) {
      req(rv$results)
      jsonlite::write_json(sanitize_export(rv$results), file, pretty = TRUE)
    }
  )

  output$download_config <- downloadHandler(
    filename = function() {
      paste0("trisk_config_", rv$run_id, ".json")
    },
    content = function(file) {
      config <- list(
        run_id = rv$run_id,
        timestamp = as.character(Sys.time()),
        parameters = list(
          baseline_scenarios = paste(input$baseline_scenario, collapse = ", "),
          target_scenarios = paste(input$target_scenarios, collapse = ", "),
          scenario_geography = input$scenario_geography,
          shock_years = paste(input$shock_years, collapse = ", "),
          risk_free_rate = input$risk_free_rate,
          discount_rate = input$discount_rate,
          growth_rate = input$growth_rate,
          market_passthrough = input$market_passthrough,
          carbon_price_model = input$carbon_price_model
        ),
        data_summary = list(
          portfolio_rows = nrow(rv$portfolio),
          assets_rows = nrow(rv$assets),
          financial_rows = nrow(rv$financial)
        ),
        versions = list(
          trisk_model = as.character(packageVersion("trisk.model")),
          trisk_analysis = as.character(packageVersion("trisk.analysis"))
        )
      )
      jsonlite::write_json(config, file, pretty = TRUE, auto_unbox = TRUE)
    }
  )

  # ============================================
  # Metadata
  # ============================================

  output$metadata_summary <- renderText({
    if (is.null(rv$run_id)) {
      "No analysis run yet."
    } else {
      paste(
        "Run ID:", rv$run_id,
        "\nTimestamp:", as.character(Sys.time()),
        "\nBaseline(s):", paste(input$baseline_scenario, collapse = ", "),
        "\nTarget(s):", paste(input$target_scenarios, collapse = ", "),
        "\nGeography:", input$scenario_geography,
        "\nResults rows:", if (!is.null(rv$results)) as.character(nrow(rv$results)) else "N/A"
      )
    }
  })

  output$version_info <- renderText({
    # Only expose package versions — no R version, OS, env vars, or build metadata
    paste(
      "trisk.model:", as.character(packageVersion("trisk.model")),
      "\ntrisk.analysis:", as.character(packageVersion("trisk.analysis"))
    )
  })

}
