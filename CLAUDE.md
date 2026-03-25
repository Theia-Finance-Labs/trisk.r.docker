# CLAUDE.md - TRISK Desktop

## Why

TRISK Desktop is APL's climate transition risk stress testing tool for bank deployment. It enables financial institutions to assess portfolio-level impacts of climate transition scenarios on NPV, PD, and Expected Loss. The goal is enterprise-grade software that satisfies bank infosec, model governance, and regulatory audit requirements (ECB, PRA, Basel).

## What

- **Shiny web application** for interactive stress test analysis (upload portfolio, configure scenarios, run model, visualize results, export)
- **CLI container** for batch/pipeline processing
- **Docker-first deployment** hardened for bank workstations (air-gapped, localhost-only, read-only filesystem)
- Uses `trisk.model` (stress test engine) and `trisk.analysis` (portfolio analytics) R packages from Theia Finance Labs

## How

### Architecture
- `app/app.R` - entry point, sources global.R, ui.R, server.R
- `app/global.R` - packages, constants, validation functions, helpers, audit logging
- `app/ui.R` - shinydashboard UI definition
- `app/server.R` - server logic (currently monolithic, modules exist but are not wired up yet)
- `app/modules/mod_*.R` - modular server functions (target architecture for next refactor)
- `app/www/` - CSS, JS, fonts (all vendored locally for air-gap)
- `tests/testthat/` - unit tests for validation, column stripping, scenario logic, helpers

### Security layers
- **Auth**: Caddy reverse proxy with HTTP Basic Auth + TLS (Caddyfile)
- **Upload**: column allowlists via `strip_columns()`, row count limits, file size cap (100MB)
- **Export**: `sanitize_export()` column allowlist + `sanitize_formula_injection()` for CWE-1236
- **Errors**: `shiny.sanitize.errors = TRUE` + generic showNotification messages (no e$message to browser)
- **Audit**: persistent JSON-lines logging to `/data/output/audit/` via `audit_log()`
- **Container**: read-only fs, cap_drop ALL, no-new-privileges, seccomp profile, resource limits

### Conventions
- R: tidyverse style, `dplyr` for data manipulation, `ggplot2`/`plotly` for charts
- Docker: multi-stage build, pinned base image (SHA256), CRAN snapshot frozen, Git packages pinned to commit SHAs
- Data flow: CSV upload -> validation -> strip_columns -> reactiveValues -> analysis -> sanitize_export -> sanitize_formula_injection -> download
- Run IDs: `YYYYMMDD_HHMMSS_<8-char-rlang-hash>` for collision resistance

### Remaining tech debt
- **server.R monolith**: 5,300+ lines duplicating all module code. Modules define setup_*() functions that are never called. Next priority is wiring modules and slimming server.R.
- **shinytest2**: No integration tests for the full upload-configure-run-download workflow yet.
- **RBAC**: Only HTTP Basic Auth exists. No role separation (analyst vs viewer).
