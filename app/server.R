# server.R
# Server logic for TRISK Shiny application
# Modules are in app/modules/mod_*.R, sourced by global.R

server <- function(input, output, session) {

  # ============================================
  # Reactive values (shared state across all modules)
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
    # Multi-horizon results: named list keyed by shock_year
    results_by_year = NULL,
    # Multi-scenario results: named list keyed by scenario code
    results_by_scenario = NULL,
    # Run history: list of up to 5 previous runs
    run_history = list(),
    compare_run_idx = NULL,
    # Scenario pairs
    pair_ids = integer(0),
    pair_counter = 0L,
    baseline_choices = NULL,
    target_choices = NULL,
    baseline_default = NULL,
    target_default = NULL,
    # Sector-aware scenario detection
    detected_sectors = NULL,
    scenario_groups = "core",
    # Scenario sensitivity analysis
    sensitivity_config = NULL,
    sensitivity_results = NULL,
    sensitivity_summary = NULL
  )

  # ============================================
  # Logging helper
  # ============================================
  log_message <- function(msg) {
    timestamp <- format(Sys.time(), "[%H:%M:%S]")
    rv$analysis_log <- c(rv$analysis_log, paste(timestamp, msg))
    if (length(rv$analysis_log) > 500) {
      rv$analysis_log <- tail(rv$analysis_log, 500)
    }
  }

  # Session idle timeout handler (JS fires this after 30 min inactivity)
  observeEvent(input$session_idle_timeout, {
    log_message("Session closed due to inactivity timeout")
    session$close()
  })

  # Session cleanup
  session$onSessionEnded(function() {
    gc()
  })

  # ============================================
  # Wire modules (order follows data flow)
  # ============================================

  # Phase 1: Data ingestion
  setup_upload(input, output, session, rv, log_message)
  setup_config(input, output, session, rv)

  # Phase 2: Analysis execution
  setup_run(input, output, session, rv, log_message)

  # Phase 3: Results visualization (read-only on rv, order doesn't matter)
  setup_results_summary(input, output, session, rv)
  setup_results_attribution(input, output, session, rv)
  setup_results_concentration(input, output, session, rv)
  setup_results_horizon(input, output, session, rv)
  setup_results_scenarios(input, output, session, rv)

  # Phase 4: Integration & export
  setup_integration(input, output, session, rv)
  setup_download(input, output, session, rv)

  # Phase 5: Sensitivity analysis
  setup_sensitivity(input, output, session, rv)
}
