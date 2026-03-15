# global.R
# Load packages and pre-configured data for TRISK Shiny app
# Visual style follows 1in1000.com design language

# Core packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinyjs)

# TRISK packages
library(trisk.model)
library(trisk.analysis)

# Data handling
library(DT)
library(ggplot2)
library(plotly)
library(readr)
library(writexl)
library(dplyr)
library(tidyr)
library(rmarkdown)
library(knitr)

# ============================================
# 1in1000 Color Palette (from 1in1000.com)
# ============================================

# Primary brand colors
BRAND_RED       <- "#F53D3F"   # Primary brand red (hsl 359 90% 60%)
BRAND_CORAL     <- "#E84B4D"   # Coral accent (hsl 4 85% 60%)
BRAND_CORAL_LT  <- "#EF7173"   # Coral light (hsl 4 85% 70%)
BRAND_CORAL_DK  <- "#C0393B"   # Coral dark (hsl 4 85% 50%)

# Background / UI (matching CSS tokens in www/trisk.css)
BG_PINK         <- "#F5F0F2"   # Light warm page background
BG_CARD         <- "#FFFFFF"   # Pure white cards
BG_SIDEBAR      <- "#FAFAFA"   # Neutral near-white sidebar
FG_TEXT          <- "#1A1A1A"  # Near-black text
FG_MUTED         <- "#555555"  # Medium gray text
BORDER_PINK      <- "#E5E0E2"  # Light warm border
SECONDARY_PINK   <- "#F0EAED"  # Sidebar active highlight

# Status / data colors (matching CSS tokens in www/trisk.css)
STATUS_GREEN     <- "#3D8B5E"  # Darker green for better contrast
STATUS_RED       <- "#C44245"  # Muted red for negative
STATUS_BLUE      <- "#4A7FA5"  # Muted blue for neutral

# Legacy TRISK colors (kept for chart compatibility)
TRISK_HEX_RED   <- "#F53D3F"
TRISK_HEX_GREEN <- "#5D9324"
TRISK_HEX_GREY  <- "#BAB6B5"

# Fixed sector colour palette — used across all sector-coloured plots
SECTOR_COLORS <- c(
  "Oil&Gas"    = "#8B4513",   # Brown
  "Coal"       = "#1A1A1A",   # Black (slightly off for visibility on dark bg)
  "Power"      = "#2E86C1",   # Blue
  "Automotive" = "#E74C3C",   # Red
  "Steel"      = "#F1C40F"    # Yellow
)

#' TRISK ggplot2 theme — font families are browser-rendered via ggplotly()
trisk_plot_theme <- function() {
  theme_minimal(base_size = 12, base_family = "Inter") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold",
                                family = "Space Grotesk", size = 14),
      plot.subtitle = element_text(hjust = 0.5, color = FG_MUTED, size = 11),
      legend.title = element_blank(),
      legend.text = element_text(size = 10),
      axis.title = element_text(family = "Space Grotesk", size = 11, color = FG_MUTED),
      axis.text = element_text(size = 10, color = FG_TEXT),
      axis.line = element_line(colour = TRISK_HEX_GREY, linewidth = 0.5),
      panel.grid.major = element_line(colour = "#EDEDED"),
      panel.grid.minor = element_blank(),
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "transparent", color = NA)
    )
}

#' Get Plotly theme colors based on current theme (light/dark)
plotly_theme_colors <- function(input) {
  if (!is.null(input$current_theme) && input$current_theme == "dark") {
    list(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#242438",
      font_color = "#E8E6F0",
      gridcolor = "#3A3A52",
      zerolinecolor = "#4A4A62"
    )
  } else {
    list(
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor = "#FFFFFF",
      font_color = FG_TEXT,
      gridcolor = "#EDEDED",
      zerolinecolor = "#CCCCCC"
    )
  }
}

#' Standard Plotly hoverlabel styling
PLOTLY_HOVERLABEL <- list(
  bgcolor = "#1A1A1A",
  font = list(family = "Inter, sans-serif", size = 12, color = "#FFFFFF"),
  bordercolor = "transparent"
)

#' Standard Plotly config (mode bar buttons)
PLOTLY_CONFIG <- list(
  displayModeBar = TRUE,
  modeBarButtonsToRemove = c("lasso2d", "select2d", "autoScale2d"),
  displaylogo = FALSE
)

# ============================================
# Column definitions (for DT table header tooltips)
# ============================================

COLUMN_DEFS <- list(
  company_id = "Unique company identifier",
  company_name = "Company display name",
  sector = "NACE/ICB sector classification",
  technology = "Production technology type",
  country_iso2 = "ISO 3166-1 alpha-2 country code",
  exposure_value_usd = "Loan exposure amount in USD",
  term = "Loan maturity in years",
  loss_given_default = "LGD: expected loss severity (0-1)",
  pd_baseline = "Probability of default under baseline scenario",
  pd_shock = "Probability of default under shocked scenario",
  PD_Change = "Absolute PD change (shock minus baseline)",
  PD_Change_Pct = "Relative PD change as percentage",
  EL_Baseline = "Expected loss under baseline (EAD x PD_baseline)",
  EL_Shock = "Expected loss under shock (EAD x PD_shock)",
  EL_Change = "Change in expected loss (shock minus baseline)",
  EL_Change_Pct = "Relative change in expected loss (%)",
  crispy_perc_value_change = "NPV percentage change under shock scenario",
  net_present_value_baseline = "Net present value under baseline",
  net_present_value_shock = "Net present value under shock"
)

#' Build DT headerCallback JS for column tooltips
make_header_tooltips <- function(col_names) {
  tips <- sapply(col_names, function(cn) {
    if (cn %in% names(COLUMN_DEFS)) COLUMN_DEFS[[cn]] else cn
  })
  DT::JS(paste0(
    "function(thead, data, start, end, display) {",
    "  var tips = ", jsonlite::toJSON(unname(tips)), ";",
    "  $(thead).find('th').each(function(i) {",
    "    $(this).attr('title', tips[i]);",
    "    $(this).css('cursor', 'help');",
    "  });",
    "}"
  ))
}

# ============================================
# Configuration
# ============================================

# Cap upload size to 200 MB (Shiny default is 5 MB).
# Scenario files can be 50-60 MB; allow headroom for larger datasets.
options(shiny.maxRequestSize = 200 * 1024^2)  # 200 MB

# Sanitize error messages shown to users (CWE-209: Information Exposure Through an Error Message)
# When TRUE, Shiny replaces raw R stack traces / e$message with a generic
# "An error has occurred" in the browser. Server-side logs still contain the full trace.
options(shiny.sanitize.errors = TRUE)

MAX_UPLOAD_ROWS <- 500000L  # Row count limit for uploaded CSVs

SCENARIOS_PATH <- "/opt/trisk/data/scenarios/scenarios.csv"
INPUT_DIR <- "/data/input"
OUTPUT_DIR <- "/data/output"

#' Default model parameter values (single source of truth)
#' Used by UI for initial values and by Reset button
config_defaults <- function() {
  list(
    risk_free_rate     = 0.02,
    discount_rate      = 0.07,
    growth_rate        = 0.03,
    market_passthrough = 0,
    carbon_price_model = "no_carbon_tax",
    shock_years        = c(2025, 2030, 2035)
  )
}

# ============================================
# Sector-aware scenario defaults
# ============================================

#' Maps portfolio sectors to their recommended default scenario pairs.
#' Each entry defines which sectors the pair covers, plus the default
#' baseline and target scenario codes.
#' "core" is always shown; others appear when their sectors are detected.
SECTOR_SCENARIO_DEFAULTS <- list(
  core = list(
    label      = "Core Energy (NGFS)",
    sectors    = c("Oil&Gas", "Coal", "Power"),
    baseline   = "NGFS2024GCAM_CP",
    target     = "NGFS2024GCAM_NZ2050",
    always_show = TRUE,
    tooltip    = NULL
  ),
  automotive = list(
    label      = "Automotive (IPR)",
    sectors    = c("Automotive"),
    baseline   = "IPR2023Automotive_baseline",
    target     = "IPR2023Automotive_FPS",
    always_show = FALSE,
    tooltip    = "Default scenario for Automotive sector \u2014 not covered by NGFS scenarios"
  ),
  steel = list(
    label      = "Steel (Mission Possible)",
    sectors    = c("Steel"),
    baseline   = "mission_possible_Steel_baseline",
    target     = "mission_possible_Steel_NZ",
    always_show = FALSE,
    tooltip    = "Default scenario for Steel sector \u2014 not covered by NGFS scenarios"
  )
)

#' Given a character vector of portfolio sector names, return which keys
#' of SECTOR_SCENARIO_DEFAULTS should be active (always includes "core").
detect_scenario_groups <- function(portfolio_sectors) {
  groups <- character()
  for (key in names(SECTOR_SCENARIO_DEFAULTS)) {
    entry <- SECTOR_SCENARIO_DEFAULTS[[key]]
    if (entry$always_show || any(entry$sectors %in% portfolio_sectors)) {
      groups <- c(groups, key)
    }
  }
  groups
}

# ============================================
# Load pre-downloaded scenarios
# ============================================

if (file.exists(SCENARIOS_PATH)) {
  scenarios_data_preloaded <- tryCatch({
    tibble::as_tibble(data.table::fread(SCENARIOS_PATH))
  }, error = function(e) {
    message("Warning: Could not load pre-bundled scenarios: ", e$message)
    NULL
  })

  if (!is.null(scenarios_data_preloaded)) {
    message("Loaded pre-bundled scenarios data")
    message(paste("  - Scenarios:", length(unique(scenarios_data_preloaded$scenario))))
    message(paste("  - Geographies:", length(unique(scenarios_data_preloaded$scenario_geography))))
  }
} else {
  # Fall back to test scenarios from trisk.model package
  scenarios_data_preloaded <- tryCatch({
    path <- system.file("testdata", "scenarios_testdata.csv", package = "trisk.model", mustWork = FALSE)
    if (nzchar(path) && file.exists(path)) {
      df <- read.csv(path)
      message("Loaded fallback scenarios from trisk.model testdata")
      message(paste("  - Scenarios:", length(unique(df$scenario))))
      df
    } else {
      message("No pre-bundled scenarios found - user will need to upload")
      NULL
    }
  }, error = function(e) {
    message("No pre-bundled scenarios found - user will need to upload")
    NULL
  })
}

# ============================================
# Load internal test data for carbon prices
# ============================================

carbon_data_internal <- tryCatch({
  read.csv(
    system.file("testdata", "ngfs_carbon_price_testdata.csv",
                package = "trisk.model", mustWork = TRUE)
  )
}, error = function(e) {
  message("Warning: Could not load internal carbon price data")
  NULL
})

# ============================================
# Load all mock/demo data from trisk packages
# ============================================

load_mock_data <- function(type) {
  tryCatch({
    switch(type,
      "assets" = {
        read.csv(system.file("testdata", "assets_testdata.csv",
                             package = "trisk.model", mustWork = TRUE))
      },
      "financial" = {
        read.csv(system.file("testdata", "financial_features_testdata.csv",
                             package = "trisk.model", mustWork = TRUE))
      },
      "scenarios" = {
        read.csv(system.file("testdata", "scenarios_testdata.csv",
                             package = "trisk.model", mustWork = TRUE))
      },
      "carbon" = {
        read.csv(system.file("testdata", "ngfs_carbon_price_testdata.csv",
                             package = "trisk.model", mustWork = TRUE))
      },
      "portfolio" = {
        for (f in c("portfolio_ids_testdata.csv", "portfolio_names_testdata.csv", "simple_portfolio.csv")) {
          path <- system.file("testdata", f, package = "trisk.analysis", mustWork = FALSE)
          if (nzchar(path) && file.exists(path)) {
            message(paste("Loaded portfolio mock data from:", f))
            return(read.csv(path))
          }
        }
        stop("No portfolio test data found in trisk.analysis package")
      },
      stop(paste("Unknown mock data type:", type))
    )
  }, error = function(e) {
    message(paste("Warning: Could not load mock", type, "data:", e$message))
    NULL
  })
}

# ============================================
# Helper functions
# ============================================

#' Validate internal PD CSV upload
#' @param df data.frame from uploaded CSV
#' @return NULL if valid, or error string
validate_internal_pd_csv <- function(df) {
  if (!"company_id" %in% names(df)) return("Missing required column: company_id")
  if (!"internal_pd" %in% names(df)) return("Missing required column: internal_pd")
  if (!is.numeric(df$internal_pd)) return("Column 'internal_pd' must be numeric")
  if (any(is.na(df$internal_pd))) return(paste0(sum(is.na(df$internal_pd)), " NA values found in internal_pd"))
  if (any(df$internal_pd < 0 | df$internal_pd > 1, na.rm = TRUE))
    return("Column 'internal_pd' must contain values in [0, 1]")
  NULL
}

#' Validate internal EL CSV upload
#' @param df data.frame from uploaded CSV
#' @return NULL if valid, or error string
validate_internal_el_csv <- function(df) {
  if (!"company_id" %in% names(df)) return("Missing required column: company_id")
  if (!"internal_el" %in% names(df)) return("Missing required column: internal_el")
  if (!is.numeric(df$internal_el)) return("Column 'internal_el' must be numeric")
  if (any(is.na(df$internal_el))) return(paste0(sum(is.na(df$internal_el)), " NA values found in internal_el"))
  NULL
}

generate_run_id <- function() {
  paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_", paste0(sample(c(0:9, letters[1:6]), 8, replace = TRUE), collapse = ""))
}

#' Validate portfolio data structure
#' Matches check_portfolio() from trisk.analysis
# Column allowlists: only these columns survive upload.
# Any extra columns (PII, internal IDs, etc.) are silently dropped.
PORTFOLIO_COLUMNS <- c("company_id", "company_name", "country_iso2", "sector",
                       "exposure_value_usd", "term", "loss_given_default", "technology")
ASSETS_COLUMNS    <- c("asset_id", "asset_name", "company_id", "company_name",
                       "country_iso2", "sector", "technology", "production_year",
                       "capacity", "capacity_factor", "production_unit", "emission_factor")
FINANCIAL_COLUMNS <- c("company_id", "pd", "net_profit_margin", "debt_equity_ratio",
                       "volatility")
SCENARIOS_COLUMNS <- c("scenario", "scenario_type", "scenario_geography", "sector",
                       "technology", "scenario_year", "direction", "fair_share_perc",
                       "value", "units")

validate_portfolio <- function(df) {
  required_cols <- c("company_id", "company_name", "country_iso2",
                     "exposure_value_usd", "term", "loss_given_default")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    return(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }
  return(NULL)
}

#' Validate assets data structure
validate_assets <- function(df) {
  required_cols <- c("company_id", "sector", "technology", "production_year", "capacity", "capacity_factor")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    return(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }
  return(NULL)
}

#' Validate financial data structure
validate_financial <- function(df) {
  required_cols <- c("company_id", "pd", "net_profit_margin", "debt_equity_ratio", "volatility")
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    return(paste("Missing required columns:", paste(missing, collapse = ", ")))
  }
  return(NULL)
}

#' Strip columns not in the allowlist for a given dataset type.
#' Prevents PII or extraneous columns from entering reactive state.
strip_columns <- function(df, type) {
  allowed <- switch(type,
    "portfolio"  = PORTFOLIO_COLUMNS,
    "assets"     = ASSETS_COLUMNS,
    "financial"  = FINANCIAL_COLUMNS,
    "scenarios"  = SCENARIOS_COLUMNS,
    names(df)  # fallback: keep all
  )
  keep <- intersect(allowed, names(df))
  dropped <- setdiff(names(df), allowed)
  if (length(dropped) > 0) {
    message(sprintf("[security] Stripped %d non-model columns from %s upload: %s",
                    length(dropped), type, paste(dropped, collapse = ", ")))
  }
  df <- df[, keep, drop = FALSE]

  # Formula injection protection: strip leading =, +, -, @ from string columns
  # to prevent CSV injection when data is later exported to Excel
  char_cols <- names(df)[sapply(df, is.character)]
  for (col in char_cols) {
    df[[col]] <- sub("^[=+@-]+", "", df[[col]])
  }

  df
}

# Server-side file type validation (defense-in-depth beyond client-side accept= hint)
validate_file_type <- function(filepath, original_name) {
  ext <- tolower(tools::file_ext(original_name))
  if (!ext %in% c("csv")) {
    return("Only .csv files are accepted.")
  }
  raw_bytes <- readBin(filepath, "raw", n = 4)
  if (length(raw_bytes) >= 4) {
    if (identical(raw_bytes[1:4], as.raw(c(0x25, 0x50, 0x44, 0x46)))) return("PDF files are not accepted. Please upload a CSV.")
    if (identical(raw_bytes[1:2], as.raw(c(0x50, 0x4B)))) return("ZIP/Office files are not accepted. Please upload a CSV.")
    if (identical(raw_bytes[1:2], as.raw(c(0xD0, 0xCF)))) return("Binary Office files are not accepted. Please upload a CSV.")
  }
  NULL
}

# Structured audit logging — writes JSON to stderr (captured by Docker log driver)
# Use for security-relevant events: uploads, analysis runs, exports
audit_log <- function(event, details = list()) {
  entry <- list(
    timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    event = event,
    session = if (exists("session")) session$token else "startup"
  )
  entry <- c(entry, details)
  message(jsonlite::toJSON(entry, auto_unbox = TRUE))
}

#' Audit a dataset: compute QA statistics and identify issues
#' @param df data.frame to audit
#' @param type one of "portfolio", "assets", "financial"
#' @return list with stats and issues data.frame
audit_dataset <- function(df, type) {
  if (is.null(df)) return(NULL)

  # Schema validation using existing functions
  schema_error <- switch(type,
    "portfolio" = validate_portfolio(df),
    "assets"    = validate_assets(df),
    "financial" = validate_financial(df),
    NULL
  )
  schema_valid <- is.null(schema_error)

  # Basic stats
  n_rows <- nrow(df)
  n_cols <- ncol(df)
  n_distinct_id <- if ("company_id" %in% names(df)) length(unique(df$company_id)) else NA
  n_duplicate_id <- if ("company_id" %in% names(df)) sum(duplicated(df$company_id)) else NA

  # Null rates per column
  null_rates <- sapply(df, function(col) round(sum(is.na(col)) / length(col) * 100, 1))
  high_null_cols <- names(null_rates[null_rates > 0])

  # Build issues data.frame
  issues <- data.frame(row = integer(), column = character(),
                       issue_type = character(), detail = character(),
                       stringsAsFactors = FALSE)

  # Check for NA values
  for (col_name in names(df)) {
    na_rows <- which(is.na(df[[col_name]]))
    if (length(na_rows) > 0) {
      issues <- rbind(issues, data.frame(
        row = na_rows, column = col_name,
        issue_type = "missing_value",
        detail = paste0("NA in column '", col_name, "'"),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Check for duplicate company_id (skip for assets — multiple rows per company is normal)
  if ("company_id" %in% names(df) && type %in% c("portfolio", "financial")) {
    dup_ids <- df$company_id[duplicated(df$company_id)]
    if (length(dup_ids) > 0) {
      dup_rows <- which(df$company_id %in% unique(dup_ids))
      issues <- rbind(issues, data.frame(
        row = dup_rows, column = "company_id",
        issue_type = "duplicate_id",
        detail = paste0("Duplicate company_id: ", df$company_id[dup_rows]),
        stringsAsFactors = FALSE
      ))
    }
  }

  # Portfolio-specific: check ISO-2 codes
  if (type == "portfolio" && "country_iso2" %in% names(df)) {
    invalid_iso <- which(!grepl("^[A-Z]{2}$", as.character(df$country_iso2)) | is.na(df$country_iso2))
    if (length(invalid_iso) > 0) {
      issues <- rbind(issues, data.frame(
        row = invalid_iso, column = "country_iso2",
        issue_type = "invalid_iso2",
        detail = paste0("Invalid ISO-2 code: '", df$country_iso2[invalid_iso], "'"),
        stringsAsFactors = FALSE
      ))
    }
  }

  list(
    schema_valid   = schema_valid,
    schema_error   = schema_error,
    n_rows         = n_rows,
    n_cols         = n_cols,
    n_distinct_id  = n_distinct_id,
    n_duplicate_id = n_duplicate_id,
    null_rates     = null_rates,
    high_null_cols = high_null_cols,
    issues         = issues
  )
}

#' Generate a template CSV for a given dataset type
#' @param type one of "portfolio", "assets", "financial"
#' @return 1-row data.frame with correct headers and example values
dataset_template <- function(type) {
  switch(type,
    "portfolio" = data.frame(
      company_id = "COMP001",
      company_name = "Example Corp",
      country_iso2 = "US",
      exposure_value_usd = 1000000,
      term = 5,
      loss_given_default = 0.45,
      stringsAsFactors = FALSE
    ),
    "assets" = data.frame(
      company_id = "COMP001",
      sector = "Power",
      technology = "CoalCap",
      production_year = 2025,
      capacity = 500,
      capacity_factor = 0.85,
      stringsAsFactors = FALSE
    ),
    "financial" = data.frame(
      company_id = "COMP001",
      pd = 0.02,
      net_profit_margin = 0.08,
      debt_equity_ratio = 1.5,
      volatility = 0.25,
      stringsAsFactors = FALSE
    ),
    data.frame()
  )
}

#' Smart rounding: 2 decimals for |x|>10, 5 decimals for |x|<=10
smart_round <- function(x) {
  ifelse(abs(x) > 10, round(x, 2), round(x, 5))
}

#' Display-friendly rounding (always 2 decimals) for KPI boxes
#' Returns clean "3.47" instead of "3.47265"
display_round <- function(x) {
  round(x, 2)
}

#' Format large numbers for display (always returns character, never scientific notation)
format_number <- function(x) {
  dplyr::case_when(
    is.na(x)       ~ "N/A",
    abs(x) >= 1e9  ~ paste0(format(round(x / 1e9, 2), scientific = FALSE), "B"),
    abs(x) >= 1e6  ~ paste0(format(round(x / 1e6, 2), scientific = FALSE), "M"),
    abs(x) >= 1e3  ~ paste0(format(round(x / 1e3, 2), scientific = FALSE), "K"),
    TRUE           ~ format(round(x, 2), scientific = FALSE)
  )
}

# ============================================
# Scenario labeling & categorization
# ============================================

#' Parse a scenario code into a human-readable label
#' e.g. "NGFS2023_GCAM_NZ2050" -> "Net Zero 2050 (NGFS 2023, GCAM)"
scenario_label <- function(code) {
  # Pathway code -> readable name mapping
  pathway_labels <- c(
    "NZ2050"  = "Net Zero 2050",
    "B2DS"    = "Below 2\u00B0C",
    "LD"      = "Low Demand",
    "DT"      = "Delayed Transition",
    "FW"      = "Fragmented World",
    "NDC"     = "Nationally Determined Contributions",
    "CP"      = "Current Policies",
    "FPS"     = "Forecast Policy Scenario",
    "RPS"     = "Required Policy Scenario",
    "NZ"      = "Net Zero",
    "baseline"= "Baseline",
    "CurPol"  = "Current Policies",
    "NDC-LTS" = "NDC + Long-Term Strategies",
    "1.5C"    = "1.5\u00B0C Pathway",
    "1.5C-Unif" = "1.5\u00B0C Uniform"
  )

  # Try NGFS pattern: NGFS{year}[_]{model}[_]{pathway}
  m <- regmatches(code, regexec("^NGFS(\\d{4})[_]?(GCAM|MESSAGE|REMIND)[_]?(.+)$", code))[[1]]
  if (length(m) == 4) {
    year <- m[2]; model <- m[3]; pathway_code <- m[4]
    pathway_name <- if (pathway_code %in% names(pathway_labels)) pathway_labels[pathway_code] else pathway_code
    return(paste0(pathway_name, " (NGFS ", year, ", ", model, ")"))
  }

  # Try GECO pattern: GECO{year}_{pathway}
  m <- regmatches(code, regexec("^GECO(\\d{4})[_](.+)$", code))[[1]]
  if (length(m) == 3) {
    year <- m[2]; pathway_code <- m[3]
    pathway_name <- if (pathway_code %in% names(pathway_labels)) pathway_labels[pathway_code] else pathway_code
    return(paste0(pathway_name, " (GECO ", year, ")"))
  }

  # Try IPR pattern: IPR{year}[Automotive]_{pathway}
  m <- regmatches(code, regexec("^IPR(\\d{4})(Automotive)?[_](.+)$", code))[[1]]
  if (length(m) == 4) {
    year <- m[2]; auto <- m[3]; pathway_code <- m[4]
    pathway_name <- if (pathway_code %in% names(pathway_labels)) pathway_labels[pathway_code] else pathway_code
    suffix <- if (nzchar(auto)) paste0(" Automotive") else ""
    return(paste0(pathway_name, " (IPR ", year, suffix, ")"))
  }

  # Try mission_possible pattern
  m <- regmatches(code, regexec("^mission_possible[_](.+)[_](baseline|NZ)$", code))[[1]]
  if (length(m) == 3) {
    sector <- m[2]; pathway_code <- m[3]
    pathway_name <- if (pathway_code %in% names(pathway_labels)) pathway_labels[pathway_code] else pathway_code
    return(paste0(pathway_name, " (Mission Possible, ", sector, ")"))
  }

  # Fallback: return original code
  code
}

#' Classify a scenario code into an NGFS-style category
scenario_category <- function(code) {
  # Extract pathway code (last segment after _ or model name)
  pathway <- sub("^.*[_]", "", code)
  # Also handle no-underscore variants
  if (grepl("(GCAM|MESSAGE|REMIND)", code)) {
    pathway <- sub("^.*(GCAM|MESSAGE|REMIND)[_]?", "", code)
  } else if (grepl("^GECO", code)) {
    pathway <- sub("^GECO\\d{4}[_]", "", code)
  } else if (grepl("^IPR", code)) {
    pathway <- sub("^IPR\\d{4}(Automotive)?[_]", "", code)
  } else if (grepl("^mission_possible", code)) {
    pathway <- sub("^mission_possible.*[_]", "", code)
  }

  # Orderly: ambitious, early, smooth transition
  orderly <- c("NZ2050", "B2DS", "LD", "FPS", "RPS", "NZ", "1.5C", "1.5C-Unif")
  # Disorderly: delayed or uncoordinated action
  disorderly <- c("DT", "NDC-LTS")
  # Too Little, Too Late: fragmented or insufficient pledges/baseline
  too_little <- c("FW", "NDC", "baseline")
  # Hot House World: no new action beyond current policies
  hot_house <- c("CP", "CurPol")

  if (pathway %in% orderly) return("\U0001F7E2 Orderly")
  if (pathway %in% disorderly) return("\U0001F7E1 Disorderly")
  if (pathway %in% too_little) return("\U0001F7E0 Too Little, Too Late")
  if (pathway %in% hot_house) return("\U0001F534 Hot House World")
  return("\u2753 Other")
}

#' Build grouped choices list for selectInput: list("Category" = c(label=code, ...))
build_scenario_choices <- function(scenario_codes) {
  labels <- sapply(scenario_codes, scenario_label, USE.NAMES = FALSE)
  categories <- sapply(scenario_codes, scenario_category, USE.NAMES = FALSE)

  # Named vector: human label -> code (for selectInput)
  named_choices <- setNames(scenario_codes, labels)

  # Group by category
  groups <- split(named_choices, categories[match(named_choices, scenario_codes)])

  # Sort within each group by year descending (newest first)
  sort_by_year_desc <- function(codes) {
    years <- suppressWarnings(as.integer(gsub(".*?(\\d{4}).*", "\\1", codes)))
    years[is.na(years)] <- 0L
    codes[order(-years, names(codes))]
  }

  # Assemble groups in logical order
  group_order <- c("\U0001F7E2 Orderly", "\U0001F7E1 Disorderly",
                   "\U0001F7E0 Too Little, Too Late", "\U0001F534 Hot House World", "\u2753 Other")
  ordered_groups <- list()
  for (g in group_order) {
    if (g %in% names(groups)) ordered_groups[[g]] <- sort_by_year_desc(groups[[g]])
  }
  ordered_groups
}

#' Extract the "family" of a scenario code for baseline matching.
#' Returns a string like "NGFS2024GCAM", "GECO2023", "mission_possible_Steel", etc.
#' Scenarios within the same family share a baseline.
scenario_family <- function(code) {
  # NGFS pattern: NGFS{year}[_]{model}[_]{pathway} -> family = "NGFS{year}{model}"
  m <- regmatches(code, regexec("^(NGFS\\d{4})[_]?(GCAM|MESSAGE|REMIND)[_]?(.+)$", code))[[1]]
  if (length(m) == 4) return(paste0(m[2], m[3]))

  # GECO pattern: GECO{year}_{pathway} -> family = "GECO{year}"
  m <- regmatches(code, regexec("^(GECO\\d{4})[_](.+)$", code))[[1]]
  if (length(m) == 3) return(m[2])

  # IPR pattern: IPR{year}[Automotive]_{pathway} -> family = "IPR{year}[Automotive]"
  m <- regmatches(code, regexec("^(IPR\\d{4})(Automotive)?[_](.+)$", code))[[1]]
  if (length(m) == 4) return(paste0(m[2], m[3]))

  # mission_possible pattern: mission_possible_{sector}_{pathway} -> family = "mission_possible_{sector}"
  m <- regmatches(code, regexec("^(mission_possible[_].+)[_](baseline|NZ)$", code))[[1]]
  if (length(m) == 3) return(m[2])

  # Fallback: treat code itself as its own family
  code
}

#' Determine the correct baseline scenario for a given target scenario.
#'
#' Rules:
#'   1. GECO targets use the GECO baseline from the same family (e.g. GECO2023_CP)
#'   2. Mission Possible targets use the mission_possible_{sector}_baseline from the same family
#'   3. All other targets (NGFS, IPR, custom) use the default baseline
#'      (NGFS GCAM 2024 Current Policies, or fallback)
#'
#' @param target_code The target scenario code
#' @param available_baselines Character vector of all available baseline scenario codes
#' @param default_baseline The default baseline to use when no family match is found
#' @return The baseline scenario code to use for this target
baseline_for_scenario <- function(target_code, available_baselines, default_baseline) {
  family <- scenario_family(target_code)
  parsed <- parse_scenario_code(target_code)

  # For NGFS targets, match by IAM within same vintage
  if (!is.null(parsed) && parsed$family == "NGFS" && !is.null(parsed$iam)) {
    # Look for baseline with same vintage + IAM (e.g., NGFS2024REMIND_CP)
    iam_pattern <- paste0("NGFS", parsed$vintage, "[_]?", parsed$iam, "[_]?CP$")
    iam_match <- grep(iam_pattern, available_baselines, value = TRUE)
    if (length(iam_match) > 0) return(iam_match[1])
    # Fallback to any NGFS baseline with same vintage
    vintage_pattern <- paste0("NGFS", parsed$vintage, ".*CP$")
    vintage_match <- grep(vintage_pattern, available_baselines, value = TRUE)
    if (length(vintage_match) > 0) return(vintage_match[1])
  }

  # For GECO targets, look for a GECO baseline in the same family
  if (grepl("^GECO", target_code)) {
    family_baselines <- available_baselines[sapply(available_baselines, function(b) {
      scenario_family(b) == family
    })]
    # Prefer CP/CurPol within the family
    cp_match <- grep("(CP|CurPol)$", family_baselines, value = TRUE)
    if (length(cp_match) > 0) return(cp_match[1])
    if (length(family_baselines) > 0) return(family_baselines[1])
  }

  # For IPR targets, look for the IPR _baseline variant with same vintage
  if (grepl("^IPR", target_code)) {
    if (!is.null(parsed)) {
      ipr_pattern <- paste0("IPR", parsed$vintage, ".*baseline$")
      ipr_match <- grep(ipr_pattern, available_baselines, value = TRUE)
      if (length(ipr_match) > 0) return(ipr_match[1])
    }
    ipr_baselines <- grep("^IPR.*baseline$", available_baselines, value = TRUE)
    if (length(ipr_baselines) > 0) return(ipr_baselines[1])
  }

  # For Mission Possible targets, look for the _baseline variant
  if (grepl("^mission_possible", target_code)) {
    mp_baseline <- paste0(family, "_baseline")
    if (mp_baseline %in% available_baselines) return(mp_baseline)
    family_baselines <- available_baselines[sapply(available_baselines, function(b) {
      scenario_family(b) == family
    })]
    if (length(family_baselines) > 0) return(family_baselines[1])
  }

  # Default: use the user's selected baseline
  default_baseline
}

#' Build a mapping of target scenario -> baseline scenario for all selected targets.
#' Returns a named character vector: names = target codes, values = baseline codes.
build_baseline_map <- function(target_scenarios, available_baselines, default_baseline) {
  mapping <- sapply(target_scenarios, function(tgt) {
    baseline_for_scenario(tgt, available_baselines, default_baseline)
  }, USE.NAMES = TRUE)
  mapping
}

# ============================================
# Scenario sensitivity helpers
# ============================================

#' Parse a scenario code into structured components.
#' @return list(family, vintage, iam, pathway, family_prefix) or NULL if unparseable
parse_scenario_code <- function(code) {
  # NGFS pattern: NGFS{year}[_]{model}[_]{pathway}
  m <- regmatches(code, regexec("^NGFS(\\d{4})[_]?(GCAM|MESSAGE|REMIND)[_]?(.+)$", code))[[1]]
  if (length(m) == 4) {
    return(list(family = "NGFS", vintage = m[2], iam = m[3], pathway = m[4],
                family_prefix = paste0("NGFS", m[2])))
  }
  # GECO pattern: GECO{year}_{pathway}
  m <- regmatches(code, regexec("^GECO(\\d{4})[_](.+)$", code))[[1]]
  if (length(m) == 3) {
    return(list(family = "GECO", vintage = m[2], iam = NA_character_, pathway = m[3],
                family_prefix = paste0("GECO", m[2])))
  }
  # IPR pattern: IPR{year}[Automotive]_{pathway}
  m <- regmatches(code, regexec("^IPR(\\d{4})(Automotive)?[_](.+)$", code))[[1]]
  if (length(m) == 4) {
    suffix <- if (nzchar(m[3])) m[3] else ""
    return(list(family = "IPR", vintage = m[2], iam = NA_character_, pathway = m[4],
                family_prefix = paste0("IPR", m[2], suffix)))
  }
  # mission_possible pattern
  m <- regmatches(code, regexec("^mission_possible[_](.+)[_](baseline|NZ)$", code))[[1]]
  if (length(m) == 3) {
    return(list(family = "MissionPossible", vintage = NA_character_, iam = NA_character_,
                pathway = m[3], family_prefix = paste0("mission_possible_", m[2])))
  }
  NULL
}

#' Map pathway codes to human-readable policy types.
#' Cross-provider: pathways with equivalent climate ambition are grouped together.
POLICY_TYPE_MAP <- list(
  "Net Zero"            = c("NZ2050", "NZ"),
  "Below 2\u00B0C"      = c("B2DS", "1.5C", "1.5C-Unif"),
  "Current Policies"    = c("CP", "CurPol", "baseline"),
  "Low Demand"          = c("LD"),
  "Delayed Transition"  = c("DT"),
  "Fragmented World"    = c("FW"),
  "NDC"                 = c("NDC", "NDC-LTS"),
  "Forecast Policy"     = c("FPS"),
  "Required Policy"     = c("RPS")
)

#' Get the policy type label for a pathway code.
pathway_to_policy_type <- function(pathway_code) {
  for (pt in names(POLICY_TYPE_MAP)) {
    if (pathway_code %in% POLICY_TYPE_MAP[[pt]]) return(pt)
  }
  pathway_code
}

#' Build sensitivity scenario groups from all available targets.
#' Groups by (policy_type, vintage) with cross-provider support.
#' @param primary_target The user's selected target scenario code
#' @param all_targets Character vector of all available target scenario codes
#' @return list of groups, each with policy_type, vintage, scenarios, etc.
build_sensitivity_groups <- function(primary_target, all_targets) {
  primary_parsed <- parse_scenario_code(primary_target)
  if (is.null(primary_parsed)) return(NULL)

  primary_policy <- pathway_to_policy_type(primary_parsed$pathway)

  # Parse all targets and classify
  parsed_list <- lapply(all_targets, function(code) {
    p <- parse_scenario_code(code)
    if (is.null(p)) return(NULL)
    p$code <- code
    p$policy_type <- pathway_to_policy_type(p$pathway)
    p
  })
  parsed_list <- Filter(Negate(is.null), parsed_list)

  # Group by (policy_type, vintage)
  group_keys <- unique(sapply(parsed_list, function(p) {
    paste0(p$policy_type, "||", ifelse(is.na(p$vintage), "none", p$vintage))
  }))

  groups <- list()
  for (gk in sort(group_keys)) {
    parts <- strsplit(gk, "\\|\\|")[[1]]
    pt <- parts[1]
    vint <- if (parts[2] == "none") NA_character_ else parts[2]

    members <- Filter(function(p) {
      p$policy_type == pt && identical(
        ifelse(is.na(p$vintage), "none", p$vintage),
        ifelse(is.na(vint), "none", vint)
      )
    }, parsed_list)

    codes <- sapply(members, function(m) m$code)
    families <- unique(sapply(members, function(m) m$family))
    iams <- unique(na.omit(sapply(members, function(m) m$iam)))

    label <- if (!is.na(vint)) paste0(pt, " (", vint, ")") else pt
    if (length(families) > 1) {
      label <- paste0(label, " \u2014 ", paste(families, collapse = ", "))
    }

    groups[[gk]] <- list(
      key         = gk,
      policy_type = pt,
      vintage     = vint,
      label       = label,
      scenarios   = codes,
      families    = families,
      iams        = iams,
      n_scenarios = length(codes),
      is_primary  = primary_target %in% codes
    )
  }

  list(
    groups         = groups,
    primary        = primary_target,
    primary_policy = primary_policy,
    primary_vintage = primary_parsed$vintage
  )
}

#' Extract a result data.frame from raw run_trisk_on_portfolio output.
extract_result_df <- function(raw) {
  if (is.data.frame(raw)) return(raw)
  if (is.list(raw)) {
    if ("portfolio_results_tech_detail" %in% names(raw)) return(raw$portfolio_results_tech_detail)
    if ("portfolio_results" %in% names(raw)) return(raw$portfolio_results)
    df_idx <- which(sapply(raw, is.data.frame))
    if (length(df_idx) > 0) return(raw[[df_idx[1]]])
  }
  stop("Could not extract a data.frame from analysis results")
}

#' Calculate expected loss: EL = -EAD * PD (follows trisk.analysis convention)
#' EAD = exposure_value_usd * loss_given_default
calculate_expected_loss <- function(exposure, pd, lgd) {
  -(exposure * lgd) * pd
}

#' Get unique scenario names from scenario data, optionally filtered by type
get_scenarios_by_type <- function(scenarios_df, type = NULL) {
  if (is.null(scenarios_df)) return(character())
  if (!is.null(type) && "scenario_type" %in% names(scenarios_df)) {
    unique(scenarios_df$scenario[scenarios_df$scenario_type == type])
  } else {
    unique(scenarios_df$scenario)
  }
}

#' Compute analysis metrics on results (mirrors trisk.analysis::compute_analysis_metrics)
#' This is needed because run_trisk_on_portfolio() does NOT compute these columns
compute_el_columns <- function(df) {
  if (all(c("exposure_value_usd", "loss_given_default", "pd_baseline", "pd_shock") %in% names(df))) {
    df$exposure_at_default <- df$exposure_value_usd * df$loss_given_default
    df$expected_loss_baseline <- -(df$exposure_at_default * df$pd_baseline)
    df$expected_loss_shock <- -(df$exposure_at_default * df$pd_shock)
    df$expected_loss_difference <- -(df$exposure_at_default * (df$pd_shock - df$pd_baseline))
  }
  if (all(c("net_present_value_baseline", "net_present_value_shock") %in% names(df))) {
    df$net_present_value_difference <- df$net_present_value_shock - df$net_present_value_baseline
    if (!"crispy_perc_value_change" %in% names(df)) {
      df$crispy_perc_value_change <- ifelse(
        df$net_present_value_baseline != 0,
        df$net_present_value_difference / df$net_present_value_baseline,
        NA_real_
      )
    }
    if (!"crispy_value_loss" %in% names(df) && "exposure_value_usd" %in% names(df) &&
        "crispy_perc_value_change" %in% names(df)) {
      df$crispy_value_loss <- df$crispy_perc_value_change * df$exposure_value_usd
    }
  }
  return(df)
}

# ============================================
# Export column allowlist
# ============================================
# Only these columns are included in CSV/JSON/Excel downloads.
# Keeps internal columns (raw upload fields, intermediate calcs) out of exports.
# Add columns here to make them available in exports.
EXPORT_COLUMNS <- c(
  # Identifiers
  "company_id", "company_name", "company_label",
  "sector", "technology", "country_iso2",
  # Scenario / run context
  "target_scenario", "baseline_scenario", "shock_year",
  "scenario_geography",
  # Exposure
  "exposure_value_usd", "loss_given_default", "exposure_at_default", "weight",
  # PD metrics
  "pd_baseline", "pd_shock",
  # NPV metrics
  "net_present_value_baseline", "net_present_value_shock",
  "net_present_value_difference", "crispy_perc_value_change", "crispy_value_loss",
  # EL metrics
  "expected_loss_baseline", "expected_loss_shock", "expected_loss_difference",
  # Attribution
  "pd_change", "pd_contribution", "el_change", "npv_change_pct"
)

#' Filter a data.frame to only the allowed export columns.
#' Columns not in the allowlist are silently dropped.
sanitize_export <- function(df) {
  keep <- intersect(EXPORT_COLUMNS, names(df))
  df[, keep, drop = FALSE]
}

# Module files — explicit list prevents code injection via rogue files
source(file.path("modules", "mod_upload.R"))
source(file.path("modules", "mod_config.R"))
source(file.path("modules", "mod_run.R"))
source(file.path("modules", "mod_download.R"))
source(file.path("modules", "mod_integration.R"))
source(file.path("modules", "mod_results_summary.R"))
source(file.path("modules", "mod_results_concentration.R"))
source(file.path("modules", "mod_results_attribution.R"))
source(file.path("modules", "mod_results_horizon.R"))
source(file.path("modules", "mod_results_scenarios.R"))
source(file.path("modules", "mod_sensitivity.R"))

message("\n========================================")
message("TRISK Shiny App Initialized (1in1000)")
message("========================================\n")
