# mod_download.R
# Auto-extracted from server.R during modularization (2026-03-25)

setup_download <- function(input, output, session, rv) {

  # ============================================

  output$metadata_summary <- renderText({
    if (is.null(rv$run_id)) {
      "No analysis run yet."
    } else {
      # Build pairs summary for metadata
      pairs_str <- paste(sapply(rv$scenario_groups, function(key) {
        entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
        if (key == "core") {
          bl <- input$baseline_scenario; tgt <- input$target_scenario
        } else {
          bl <- input[[paste0("baseline_scenario_", key)]]
          tgt <- input[[paste0("target_scenario_", key)]]
        }
        paste0("  ", entry$label, ": ", bl %||% "(none)", " -> ", tgt %||% "(none)")
      }), collapse = "\n")
      paste(
        "Run ID:", rv$run_id,
        "\nTimestamp:", as.character(Sys.time()),
        "\nScenario Pairs:\n", pairs_str,
        "\nGeography:", input$scenario_geography,
        "\nResults rows:", if (!is.null(rv$results)) as.character(nrow(rv$results)) else "N/A"
      )
    }
  })

  output$version_info <- renderText({
    paste(
      "trisk.model:", as.character(packageVersion("trisk.model")),
      "\ntrisk.analysis:", as.character(packageVersion("trisk.analysis"))
    )
  })

}
