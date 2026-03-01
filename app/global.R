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

#' TRISK ggplot2 theme using 1in1000 fonts
trisk_plot_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      legend.title = element_blank(),
      axis.line = element_line(colour = TRISK_HEX_GREY, linewidth = 0.5),
      panel.grid.major = element_line(colour = "#EDEDED"),
      panel.grid.minor = element_blank()
    )
}

# ============================================
# Configuration
# ============================================

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
# Load pre-downloaded scenarios
# ============================================

if (file.exists(SCENARIOS_PATH)) {
  scenarios_data_preloaded <- tryCatch({
    read_csv(SCENARIOS_PATH, show_col_types = FALSE)
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

#' Generate a unique run ID from timestamp + config hash (base R only)
#' @param config_list named list of analysis parameters
#' @return character string like "20260301_143022_a7f3b"
generate_run_id <- function(config_list) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  config_str <- paste(sort(unlist(config_list)), collapse = "")
  hash_int <- abs(sum(utf8ToInt(config_str)))
  hash_hex <- substring(sprintf("%x", hash_int), 1, 5)
  paste0(ts, "_", hash_hex)
}

#' Validate portfolio data structure
#' Matches check_portfolio() from trisk.analysis
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

  # Check for duplicate company_id
  if ("company_id" %in% names(df)) {
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

#' Format large numbers for display (always returns character)
format_number <- function(x) {
  as.character(
    ifelse(abs(x) >= 1e6,
           paste0(round(x / 1e6, 2), "M"),
           ifelse(abs(x) >= 1e3,
                  paste0(round(x / 1e3, 2), "K"),
                  as.character(round(x, 2))))
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

  # Named vector: label -> code (for selectInput)
  named_choices <- setNames(scenario_codes, paste0(labels, "  [", scenario_codes, "]"))

  # Group by category
  groups <- split(named_choices, categories[match(named_choices, scenario_codes)])

  # Sort groups in logical order
  group_order <- c("\U0001F7E2 Orderly", "\U0001F7E1 Disorderly",
                   "\U0001F7E0 Too Little, Too Late", "\U0001F534 Hot House World", "\u2753 Other")
  ordered_groups <- list()
  for (g in group_order) {
    if (g %in% names(groups)) ordered_groups[[g]] <- sort(groups[[g]])
  }
  ordered_groups
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

message("\n========================================")
message("TRISK Shiny App Initialized (1in1000)")
message("========================================\n")
