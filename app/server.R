# server.R
# Server orchestrator for TRISK Shiny application
# All logic lives in app/modules/mod_*.R — this file wires them together.

server <- function(input, output, session) {

  # ============================================
  # Reactive values to store data
  # ============================================
  rv <- reactiveValues(
    portfolio = NULL,
    assets = NULL,
    financial = NULL,
    scenarios = scenarios_data_preloaded,
    carbon = carbon_data_internal,
    results = NULL,
    run_id = NULL,
    analysis_log = character(),
    # Integration: internal PD/EL values entered by user
    internal_pd = NULL,
    internal_el = NULL,
    pd_integration_result = NULL,
    el_integration_result = NULL,
    # Multi-horizon results: named list keyed by shock_year (e.g. list("2030" = df, "2035" = df))
    results_by_year = NULL,
    # Multi-scenario results: named list keyed by scenario code
    # Each entry is itself a named list of year results (like results_by_year)
    results_by_scenario = NULL,
    # Run history: list of up to 5 previous runs
    # Each entry: list(results, config, run_id, timestamp, n_companies)
    run_history = list(),
    # Currently selected comparison run (index into run_history)
    compare_run_idx = NULL
  )

  # ============================================
  # Logging helper
  # ============================================
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    rv$analysis_log <- c(rv$analysis_log, paste(timestamp, msg))
  }

  # ============================================
  # Wire up modules — order matches UI tab flow
  # ============================================
  setup_upload(input, output, session, rv, log_message)
  setup_config(input, output, session, rv)
  setup_run(input, output, session, rv, log_message)
  setup_results_summary(input, output, session, rv)
  setup_results_horizon(input, output, session, rv)
  setup_results_scenarios(input, output, session, rv)
  setup_results_attribution(input, output, session, rv)
  setup_results_concentration(input, output, session, rv)
  setup_integration(input, output, session, rv)
  setup_download(input, output, session, rv)
}
