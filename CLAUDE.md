# CLAUDE.md — TRISK Desktop

## Why

TRISK Desktop is a client-facing climate transition risk stress testing tool designed to meet the standard a senior risk manager at a bank would expect from a McKinsey delivery. It enables financial institutions to assess the impact of climate transition scenarios on their loan portfolios and produce results suitable for ICAAP reports, board presentations, and regulatory submissions.

**Primary objective:** Design an app that could pass off as a McKinsey-grade tool, targeted at users in the banking industry — specifically risk teams preparing climate stress test results for senior management and ICAAP/ILAAP reporting.

**Design principles:**
- Clarity over cleverness — every chart must be explainable to a CRO in 15 seconds
- Workflow, not dashboard — the numbered sidebar guides users step-by-step
- No clutter — each feature must earn its place by directly supporting the banking use case
- Professional polish — 1in1000.com design language (Inter + Space Grotesk fonts, pink/coral palette)

## What

An R/Shiny web application (Dockerized) that:

1. **Accepts portfolio data** — loan book with exposures, company IDs, and optionally internal PD/EL estimates
2. **Runs the TRISK model** — computes NPV, PD, and Expected Loss under baseline vs. transition shock scenarios (NGFS, IPR, GECO, Mission Possible)
3. **Presents results** — portfolio-level KPIs, sector breakdowns, concentration risk, attribution, multi-horizon analysis
4. **Enables integration** — combines TRISK outputs with the bank's internal PD/EL using absolute, relative, or Z-score (Basel IRB) methods
5. **Supports sensitivity analysis** — tests multiple scenario families (Net Zero, Below 2°C, Current Policies, etc.) and shows distribution of outcomes
6. **Exports everything** — Excel, CSV, JSON for audit trail and regulatory submission

**Key users:** Credit risk analysts, portfolio managers, CROs, ICAAP authors at banks and financial institutions.

## How

### Architecture
- **Frontend:** `shinydashboard` with custom CSS (1in1000 palette), `plotly` interactive charts via `ggplotly()`
- **Backend:** R with `trisk.model` package for stress test computations, `trisk.analysis` for data preparation
- **Deployment:** Docker Compose (`docker-compose.dev.yml`), Caddy reverse proxy, port 3838
- **Module pattern:** `setup_*(input, output, session, rv)` functions in `app/modules/`, sourced in `global.R`, called in `server.R`

### Key Files
| File | Purpose |
|------|---------|
| `app/ui.R` | Dashboard UI definition — all tabs, sidebar, layout |
| `app/server.R` | Main server logic — data handling, model execution, all render outputs (~6300 lines) |
| `app/global.R` | Shared constants, helpers, config defaults, scenario utilities (~900 lines) |
| `app/modules/mod_sensitivity.R` | Scenario Sensitivity Analysis module |
| `app/modules/mod_integration.R` | PD/EL Integration module |
| `app/www/trisk.css` | Custom styling (1in1000 design language) |
| `app/www/trisk.js` | Client-side JS (theme toggle, guidance panels, info toggles) |
| `docker-compose.dev.yml` | Dev Docker configuration |
| `Dockerfile` | Container build (R + system deps + Shiny) |

### Conventions
- Use `trisk_plot_theme()` for all ggplot2 charts, `plotly_theme_colors(input)` for dark mode support
- Use `PLOTLY_CONFIG` and `PLOTLY_HOVERLABEL` for consistent plotly behavior
- Use `SECTOR_COLORS` for consistent sector color mapping
- Use `format_number()` for display (handles M/B/K suffixes)
- Use `smart_round()` for PD values (5 decimal places for small numbers)
- PD changes should be expressed in basis points (bps) where appropriate
- All reactive values stored in `rv$*` (single reactiveValues object)
- Never delete files — comment out or refactor

### Testing
- Build and test via Docker: `docker compose -f docker-compose.dev.yml up --build -d`
- Use preview tools (`mcp__Claude_Preview__*`) for UI testing
- Demo data available via "Load Demo Data" button (trisk.model package data)
