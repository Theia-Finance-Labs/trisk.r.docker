# TRISK Installation Requirements

## Context

Installation requirements for deploying the TRISK Climate Transition Risk Stress Testing app on a bank analyst's laptop.

---

## Option A: Docker (Recommended)

The fastest and most reproducible path. All dependencies, data, and security hardening are baked into the image.

**Requirements:**
- **Docker Desktop** ≥ 24.0 (or Docker Engine on Linux)
- **docker compose** v2 (ships with Docker Desktop)
- **4 GB RAM** allocated to Docker
- **~2 GB disk** for the built image

**Install & run:**
```bash
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker
docker compose up --build
# → open http://localhost:3838
```

Everything else (R, packages, scenarios, fonts, security policy) is handled by the Dockerfile. No further setup needed.

> If the analyst's IT policy blocks Docker, use Option B below.

---

## Option B: Bare-Metal R (No Docker)

### 1. R Version

**R 4.4.1** (exact version the app is built and tested against)

---

### 2. System Libraries

**None required.** On Windows (and macOS), CRAN and Posit PPM ship pre-compiled binary packages that bundle all native dependencies. No compiler, no Homebrew, no apt-get.

---

### 3. R Packages

#### 3a. Package Manager — pak 0.8.0

```r
install.packages("pak", repos = "https://packagemanager.posit.co/cran/2025-02-01")
```

#### 3b. CRAN Packages (locked to 2025-02-01 snapshot)

All packages must come from the Posit Public Package Manager snapshot to guarantee version compatibility:

```r
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-02-01"))

pak::pak(c(
  "httpuv", "htmltools", "shiny", "shinydashboard", "shinyWidgets",
  "shinyjs", "DT", "ggplot2", "plotly", "readr", "writexl",
  "jsonlite", "dplyr", "tidyr"
), ask = FALSE, upgrade = FALSE)
```

**Full list (14 direct + transitive dependencies):**

| Package | Purpose |
|---------|---------|
| shiny | Web application framework |
| shinydashboard | Dashboard layout (sidebar + boxes) |
| shinyWidgets | Enhanced UI widgets (pickerInput, etc.) |
| shinyjs | JavaScript interop from R |
| httpuv | HTTP/WebSocket server backend |
| htmltools | HTML generation utilities |
| DT | Interactive DataTables |
| ggplot2 | Static charts |
| plotly | Interactive charts |
| readr | CSV reading (typed, fast) |
| writexl | Excel export (.xlsx) |
| jsonlite | JSON serialization |
| dplyr | Data manipulation |
| tidyr | Data reshaping (pivot_wider) |

#### 3c. GitHub Packages (pinned to exact commit SHAs)

These are the core TRISK model packages. They are **not** on CRAN.

```r
options(repos = c(CRAN = "https://packagemanager.posit.co/cran/2025-02-01"))

pak::pak(c(
  "Theia-Finance-Labs/trisk.model@b716c9d3573bee2a730973995b9da6fc9b9703c5",
  "Theia-Finance-Labs/trisk.analysis@307e52aa035b3ebe8bb0ddb967268e38600a1763"
), ask = FALSE, upgrade = FALSE)
```

> **Note:** GitHub access is required at install time only. The app runs fully offline after installation.

#### 3d. CLI Script (optional)

If the analyst will use the batch CLI (`scripts/run_analysis.R`), also install:

```r
pak::pak("optparse", ask = FALSE, upgrade = FALSE)
```

---

### 4. Scenario Data

The full NGFS/GECO/IPR scenario dataset must be downloaded once:

```r
scenarios_url <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs/scenarios.csv"
scenarios_path <- file.path("data", "scenarios", "scenarios.csv")
dir.create(dirname(scenarios_path), recursive = TRUE, showWarnings = FALSE)
download.file(scenarios_url, scenarios_path, mode = "wb")

# Verify integrity
expected_sha256 <- "b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56"
actual_sha256 <- digest::digest(scenarios_path, algo = "sha256", file = TRUE)
stopifnot(actual_sha256 == expected_sha256)
```

> **Fallback:** If the file is missing, the app falls back to a small test dataset bundled inside the `trisk.model` package. This is enough to demo but not for real analysis.

---

### 5. Directory Layout

```
trisk.r.docker/                     ← project root
├── app/
│   ├── app.R                       ← entry point
│   ├── global.R                    ← config, helpers, package loading
│   ├── ui.R                        ← dashboard UI
│   ├── server.R                    ← server orchestrator
│   ├── modules/
│   │   ├── mod_upload.R
│   │   ├── mod_config.R
│   │   ├── mod_run.R
│   │   ├── mod_results_summary.R
│   │   ├── mod_results_horizon.R
│   │   ├── mod_results_scenarios.R
│   │   ├── mod_results_attribution.R
│   │   ├── mod_results_concentration.R
│   │   ├── mod_integration.R
│   │   └── mod_download.R
│   └── www/
│       ├── fonts.css               ← self-hosted font declarations
│       ├── fonts/                  ← vendored Inter + Space Grotesk (woff2)
│       ├── trisk.css               ← app theme
│       ├── trisk.js                ← UI interactions
│       ├── logo.png, logo-black.png, logo-color.png
│       └── dot-pattern.svg
├── scripts/
│   ├── run_analysis.R              ← batch CLI
│   └── run_tests.R                 ← test runner
└── data/
    ├── scenarios/
    │   └── scenarios.csv           ← pre-downloaded (see §4)
    ├── input/                      ← analyst puts CSVs here
    └── output/                     ← results written here
```

---

### 6. Configuration (global.R paths)

Three paths in `app/global.R` are hardcoded for Docker. For bare-metal, edit them to match the local layout:

```r
# global.R lines 72-74 — change to:
SCENARIOS_PATH <- "data/scenarios/scenarios.csv"   # was /opt/trisk/data/scenarios/scenarios.csv
INPUT_DIR      <- "data/input"                      # was /data/input
OUTPUT_DIR     <- "data/output"                     # was /data/output
```

No environment variables are strictly required. Optional:
- `SHINY_LOG_LEVEL=WARN` — controls Shiny log verbosity
- `TMPDIR` — R temp directory (defaults to system temp)

---

### 7. Launch

```bash
cd trisk.r.docker
Rscript -e "shiny::runApp('app', host='127.0.0.1', port=3838)"
```

Then open **http://localhost:3838** in the browser.

---

### 8. Network Requirements

| When | What | Why |
|------|------|-----|
| Install time | HTTPS to `packagemanager.posit.co` | CRAN packages |
| Install time | HTTPS to `github.com` | trisk.model + trisk.analysis |
| Install time | HTTPS to `storage.googleapis.com` | scenarios.csv download |
| **Runtime** | **None** | **App runs fully offline** |

---

### 9. Verification Checklist

After installation, verify everything works:

```r
# 1. All packages load
suppressPackageStartupMessages({
  library(trisk.model)
  library(trisk.analysis)
  library(shiny)
  library(shinydashboard)
  library(shinyWidgets)
  library(shinyjs)
  library(DT)
  library(ggplot2)
  library(plotly)
  library(readr)
  library(writexl)
  library(dplyr)
  library(tidyr)
})
cat("All packages OK\n")

# 2. TRISK model versions
cat("trisk.model:", as.character(packageVersion("trisk.model")), "\n")
cat("trisk.analysis:", as.character(packageVersion("trisk.analysis")), "\n")

# 3. Scenario data present
cat("Scenarios:", file.exists("data/scenarios/scenarios.csv"), "\n")

# 4. App starts
shiny::runApp("app", host = "127.0.0.1", port = 3838, launch.browser = TRUE)
```

---

### 10. Security Notes for Bank IT

- **No outbound network at runtime** — all fonts, data, and assets are vendored locally
- **Localhost-only binding** (`127.0.0.1:3838`) — not exposed on LAN
- **Upload cap**: 50 MB max file size (`shiny.maxRequestSize`)
- **Export allowlist**: Only approved columns are included in CSV/Excel/JSON downloads
- **No credentials stored**: No authentication layer, no database, no API keys
- **Content Security Policy**: CSP meta tag blocks inline script injection
- **Read-only app directory**: The app only writes to `data/output/` and system temp
