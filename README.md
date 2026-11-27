# TRISK Desktop

**Climate transition risk stress testing for banks — locally deployed, fully offline, audit-ready.**

TRISK Desktop allows bank risk teams to run climate transition stress tests on their loan portfolios inside a controlled local environment. Sensitive portfolio data stays on bank infrastructure. Results are transparent, reproducible, and export-ready for ICAAP reports, board presentations, and regulatory submissions.

Built on the [TRISK model](https://theia-finance-labs.github.io/trisk.model/) by [Theia Finance Labs](https://github.com/Theia-Finance-Labs) and the [1in1000 Initiative](https://1in1000.com).

## Documentation

Full user guide and reference: **[theia-finance-labs.github.io/trisk.r.docker](https://theia-finance-labs.github.io/trisk.r.docker/)**

- [Getting Started](https://theia-finance-labs.github.io/trisk.r.docker/getting-started/) - install and run in 10 minutes
- [Data Preparation](https://theia-finance-labs.github.io/trisk.r.docker/data-preparation/) - CSV format guide
- [User Guide](https://theia-finance-labs.github.io/trisk.r.docker/workflow/01-upload/) - step-by-step walkthrough
- [Security Architecture](https://theia-finance-labs.github.io/trisk.r.docker/security/) - for bank IT teams

---

## What it does

| Step | What happens |
|------|-------------|
| **1. Upload** | Load your portfolio CSV, asset-level data, and optionally your internal PD/EL estimates |
| **2. Configure** | Choose scenario family (NGFS, IPR, GECO), baseline vs. shock, geography, and time horizon |
| **3. Run** | Execute the TRISK stress test engine on your portfolio |
| **4. Review results** | Interactive charts: sector breakdown, concentration risk, attribution waterfall, horizon analysis |
| **5. Scenario analysis** | Compare outcomes across multiple scenarios (Net Zero, Below 2C, Current Policies, etc.) |
| **6. Risk integration** | Combine TRISK outputs with your bank's internal PD/EL using absolute, relative, or Z-score methods |
| **7. Export** | Download results as Excel, CSV, or JSON for audit trail and regulatory submission |

The app includes a built-in demo dataset so you can explore the full workflow immediately after installation.

---

## Prerequisites

You need **one thing**: [Docker Desktop](https://www.docker.com/products/docker-desktop/).

| Requirement | Details |
|------------|---------|
| **Docker Desktop** | [Download for Windows/Mac](https://www.docker.com/products/docker-desktop/) or [Docker Engine for Linux](https://docs.docker.com/engine/install/) |
| **RAM** | 4 GB minimum allocated to Docker (8 GB recommended for large portfolios) |
| **Disk** | ~3 GB for the Docker image |
| **Network** | Required only during initial build. Fully offline at runtime. |
| **Browser** | Any modern browser (Chrome, Edge, Firefox, Safari) |

> **No R, RStudio, or programming knowledge required.** Everything runs inside the Docker container.

---

## Installation

### Step 1: Install Docker Desktop

Download and install Docker Desktop from [docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop/).

After installation, open Docker Desktop and wait for the whale icon in your system tray to show "Docker Desktop is running."

**Windows users:** You may need to enable WSL 2 during installation. Docker Desktop will prompt you if this is needed.

### Step 2: Get the TRISK Desktop files

**Option A — Clone with Git** (if Git is installed):

```bash
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker
```

**Option B — Download ZIP** (no Git required):

1. Go to the repository page
2. Click the green **Code** button, then **Download ZIP**
3. Extract the ZIP to a folder (e.g., `C:\trisk` on Windows or `~/trisk` on Mac/Linux)
4. Open a terminal and navigate to that folder

### Step 3: Build and start

**Option A — Using Docker Desktop (recommended for most users):**

1. Open **Docker Desktop**
2. Make sure it's running (whale icon is steady in the system tray / menu bar)
3. Open a terminal window:
    - **Windows:** press `Win + R`, type `cmd`, press Enter
    - **Mac:** open **Terminal** from Applications > Utilities
4. Navigate to the folder where you extracted TRISK:
    ```bash
    cd path/to/trisk.r.docker
    ```
5. Run this single command to build and start:
    ```bash
    docker compose -f docker-compose.dev.yml up --build -d
    ```

The first build takes ~10 minutes (downloads R, installs packages). Subsequent starts take seconds.

**Option B — Using the Docker Desktop GUI after first build:**

After the first build completes, you can manage the container entirely from Docker Desktop:

1. Open **Docker Desktop** and go to the **Containers** tab
2. Find `trisk.r.docker` (or `trisk-app-dev`)
3. Click the **▶ Play** button to start, **⏹ Stop** to shut down
4. Click the container name to see logs and confirm it's running

### Step 4: Verify the container is running

In Docker Desktop, click the container to see its logs. Wait until you see:

```
Listening on http://0.0.0.0:3838
```

Or check from the terminal:

```bash
docker compose -f docker-compose.dev.yml logs -f
```

### Step 5: Open the application

Open your browser and go to:

> **http://localhost:3838**

You should see the TRISK Desktop welcome screen.

!!! tip "Not loading?"
    - Make sure Docker Desktop shows the container as **Running** (green icon)
    - Try refreshing the page after 30 seconds — the app takes a moment to initialize on first start
    - Check [Troubleshooting](#troubleshooting) if issues persist

---

## Quick start: running the demo

1. Open http://localhost:3838
2. Navigate to **1. Upload Data** in the sidebar
3. Click **Load Demo Data** — this loads a synthetic portfolio with matching asset and financial data
4. Go to **2. Configure Analysis** — the defaults are already sensible
5. Click **3. Run Analysis** and wait for the model to complete (~30-60 seconds)
6. Explore **4. Portfolio Results** — sector breakdown, concentration, attribution charts
7. Try **5. Scenario Analysis** — compare multiple scenario families
8. Try **6. Risk Integration** — combine TRISK outputs with internal PD/EL
9. Go to **7. Download** to export results as Excel/CSV/JSON

---

## Running with your own data

### Preparing your data

You need at minimum **one file** (portfolio CSV). For full analysis, prepare **three files**:

#### 1. Portfolio CSV (required)

Your loan book with exposures to climate-sensitive sectors.

| Column | Type | Description | Example |
|--------|------|-------------|---------|
| `company_id` | integer/string | Unique company identifier | `374444` |
| `company_name` | string | Company name (used for fuzzy matching if company_id unavailable) | `ACME Energy Ltd.` |
| `sector` | string | Sector classification | `Power`, `Oil&Gas`, `Coal` |
| `technology` | string | Technology within sector | `CoalCap`, `RenewablesCap`, `OilCap`, `GasCap` |
| `country_iso2` | string | Two-letter country code | `DE`, `US`, `MN` |
| `exposure_value_usd` | numeric | Loan exposure in USD | `5000000` |
| `term` | integer | Loan term in years (1-5) | `3` |
| `loss_given_default` | numeric | LGD ratio (0 to 1) | `0.45` |

**Sector/technology values supported:** Power (CoalCap, OilCap, GasCap, NuclearCap, HydroCap, RenewablesCap), Oil&Gas (Oil, Gas), Coal (Coal), Automotive (Electric, FuelCell, Hybrid, ICE), Steel (Integrated facility, Electric arc furnace), Cement, Aviation.

#### 2. Assets CSV (recommended)

Company-level production data linking companies to physical assets.

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Must match portfolio |
| `company_name` | string | Company name |
| `asset_id` | string | Unique asset identifier |
| `country_iso2` | string | Asset location |
| `sector` | string | Production sector |
| `technology` | string | Technology type |
| `production_year` | integer | Year of production data |
| `capacity` | numeric | Asset capacity |
| `capacity_factor` | numeric | Utilization factor (0-1) |
| `emission_factor` | numeric | Emissions per unit produced |
| `production_unit` | string | Unit of production (MW, GJ, etc.) |

> **Bundled data available:** A comprehensive asset-level dataset is included in `data/input/assets_data.csv` covering thousands of companies globally. In most cases you do not need to provide your own assets file.

#### 3. Financial Features CSV (optional)

Internal financial metrics for PD integration.

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Must match portfolio |
| `pd` | numeric | Current probability of default |
| `net_profit_margin` | numeric | Net profit margin ratio |
| `debt_equity_ratio` | numeric | Debt to equity ratio |
| `volatility` | numeric | Asset value volatility |

### Uploading your data

1. Place your CSV files in the `data/input/` directory, **or**
2. Upload them directly through the web interface on the **Upload Data** tab

### Scenarios

Climate transition scenarios are **pre-loaded** from public sources:

| Provider | Scenarios included |
|----------|-------------------|
| **NGFS** | Net Zero 2050, Below 2C, Delayed Transition, Current Policies, NDCs |
| **IPR** | Forecast Policy Scenario (FPS), Required Policy Scenario (RPS) |
| **GECO** | IEA-based scenarios |
| **Mission Possible** | Sector-specific deep decarbonization pathways |

You can also upload custom scenario files matching the expected format.

---

## Stopping and restarting

```bash
# Stop the application
docker compose -f docker-compose.dev.yml down

# Start again (no rebuild needed)
docker compose -f docker-compose.dev.yml up -d

# Rebuild after an update
docker compose -f docker-compose.dev.yml up --build -d
```

---

## Output and exports

All exports land in the `data/output/` directory and are available through the **Download** tab:

| Format | Contents |
|--------|----------|
| **Excel (.xlsx)** | Full results workbook with company-level PD, EL, and scenario detail |
| **CSV** | Individual result tables for pipeline integration |
| **JSON** | Machine-readable results and run configuration metadata |

Each analysis run is tagged with a unique Run ID (`YYYYMMDD_HHMMSS_<hash>`) for audit traceability.

---

## Security architecture

TRISK Desktop is designed for deployment on bank workstations and controlled internal environments.

### Data containment

- All data processing happens **locally inside the Docker container**
- **No data leaves your machine** at runtime — no telemetry, no cloud calls, no analytics
- Input data is mounted **read-only** — the application cannot modify your source files
- Network access is **not required** after the initial Docker image build
- The application binds to **localhost only** (`127.0.0.1`) — not accessible from other machines on the network

### Container hardening

| Control | Implementation |
|---------|---------------|
| Non-root execution | Application runs as unprivileged `trisk` user |
| Read-only filesystem | Root filesystem is mounted read-only; only `/tmp` and `/data/output` are writable |
| Capability drop | All Linux capabilities dropped (`cap_drop: ALL`) |
| Privilege escalation | Blocked (`no-new-privileges: true`) |
| Resource limits | Memory capped at 4 GB, CPU capped at 2 cores, PID limit 256 |
| Log rotation | Capped at 10 MB x 3 files to prevent disk fill |
| Output restrictions | Output directory mounted with `noexec,nosuid` flags |

### Upload and export controls

- Uploaded files are validated against strict column allowlists — extra columns containing PII or sensitive data are automatically stripped
- Export outputs pass through a separate allowlist and formula injection sanitization (CWE-1236)
- Upload size capped at 100 MB
- User-facing errors are sanitized — no stack traces or internal paths exposed

### Supply chain

- Base Docker image pinned by SHA-256 digest
- All R packages locked to a date-stamped CRAN snapshot (Posit Package Manager)
- Git-sourced packages pinned to exact commit SHAs
- Scenario data vendored in the build context with SHA-256 checksum verification
- All fonts vendored locally — no runtime requests to Google Fonts or CDNs

### For bank IT teams

See the [Bank IT Security FAQ](docs/bank_it_security_faq.md) for answers to common questions about ports, permissions, scanning, logging, and air-gapped deployment.

---

## Production deployment (with TLS and authentication)

For production use with HTTPS and user authentication:

```bash
# 1. Generate a password hash
docker run --rm caddy:2-alpine caddy hash-password --plaintext 'YourSecurePassword'

# 2. Create .env file
cp .env.example .env
# Edit .env and set TRISK_PASS_HASH to the hash from step 1

# 3. Place your TLS certificate
mkdir -p tls
cp your-cert.pem tls/cert.pem
cp your-key.pem tls/key.pem

# 4. Start with the production compose file
docker compose up --build -d
```

Access via: **https://localhost** (login with the configured username/password).

The production deployment adds:
- Caddy reverse proxy with TLS encryption
- HTTP Basic Authentication
- Security headers (HSTS, CSP, X-Frame-Options, etc.)
- Internal-only Docker network (Shiny app not directly accessible)

---

## Configuration reference

### Environment variables

| Variable | Description | Default |
|----------|-------------|---------|
| `SHINY_LOG_LEVEL` | Log verbosity: `TRACE`, `DEBUG`, `INFO`, `WARN`, `ERROR` | `WARN` |
| `TRISK_DEBUG_LOG` | Enable verbose analysis logging in the UI | `false` |
| `TRISK_USER` | HTTP Basic Auth username (production only) | `analyst` |
| `TRISK_PASS_HASH` | Bcrypt hash of the auth password (production only) | — |
| `BASE_IMAGE` | Override base Docker image (for bank artifactory) | `rocker/r-ver:4.4.1@sha256:...` |

### Resource tuning

For large portfolios (500+ companies), increase Docker memory:

1. **Docker Desktop:** Settings > Resources > Memory > set to 8 GB or more
2. **docker-compose.yml:** Adjust `mem_limit` from `4g` to `8g`

---

## Troubleshooting

### Docker Desktop is not running

**Symptom:** `Cannot connect to the Docker daemon`

**Fix:** Open Docker Desktop and wait for it to fully start (whale icon steady in system tray).

### Build fails with network error

**Symptom:** `Could not resolve host` during `docker compose build`

**Fix:** Ensure internet access is available during build. After build completes, the app runs fully offline.

### Container starts but app doesn't load

```bash
# Check container status
docker compose -f docker-compose.dev.yml ps

# View logs
docker compose -f docker-compose.dev.yml logs trisk
```

Look for R package loading errors or port conflicts.

### Port 3838 already in use

```bash
# Find what's using the port
lsof -i :3838        # Mac/Linux
netstat -ano | findstr :3838   # Windows

# Or use a different port
# Edit docker-compose.dev.yml: change "127.0.0.1:3838:3838" to "127.0.0.1:4000:3838"
```

### Out of memory

**Symptom:** Container killed or R session crashes during analysis

**Fix:**
1. Docker Desktop > Settings > Resources > increase memory to 8 GB+
2. For very large portfolios, consider splitting into sector-level batches

### Upload validation errors

The app validates uploaded data on load. Common issues:
- Missing required columns — check the column names match exactly (case-sensitive)
- Wrong data types — ensure numeric columns don't contain text
- Empty file or wrong delimiter — use comma-separated CSV, UTF-8 encoding

---

## File structure

```
trisk.r.docker/
  app/
    app.R              # Entry point
    global.R           # Shared config, helpers, validation
    ui.R               # Dashboard UI definition
    server.R           # Server logic
    modules/           # Modular server functions
    www/               # CSS, JS, fonts (all vendored locally)
  data/
    input/             # Place your CSV files here
    output/            # Analysis results appear here
    scenarios/         # Pre-loaded scenario data
  docker-compose.yml       # Production (TLS + auth)
  docker-compose.dev.yml   # Development (direct access)
  Dockerfile               # Application container build
  Dockerfile.cli           # CLI container for batch processing
  Caddyfile                # Reverse proxy configuration
  .env.example             # Environment variable template
```

---

## License

| Component | License |
|-----------|---------|
| trisk.model | GPL-3.0 |
| trisk.analysis | LGPL-3.0 |
| TRISK Desktop (this application) | MIT |

## Links

- [TRISK Model Documentation](https://theia-finance-labs.github.io/trisk.model/)
- [TRISK Analysis Documentation](https://theia-finance-labs.github.io/trisk.analysis/)
- [Theia Finance Labs](https://github.com/Theia-Finance-Labs)
- [1in1000 Initiative](https://1in1000.com)

## Funding

Co-funded by the European Union. Scientific Transition Risk Exercises for Stress Tests & Scenario Analysis has received funding from the European Union's Life programme under Grant No. LIFE21-GIC-DE-Stress.
