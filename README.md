# TRISK Docker

Docker container for running TRISK (Transition Risk Stress Testing) locally at financial institutions.

## Overview

TRISK helps financial institutions assess the impact of climate transition scenarios on their loan portfolios. This Docker container provides a complete, self-contained environment that:

- ✅ Runs entirely locally - no data leaves your machine
- ✅ Includes all dependencies pre-installed
- ✅ Provides a web-based interface (no R knowledge required)
- ✅ Works offline after initial setup
- ✅ Transparent, open-source code

## Quick Start

### Prerequisites

- Docker Desktop installed ([Windows/Mac](https://www.docker.com/products/docker-desktop)) or Docker Engine ([Linux](https://docs.docker.com/engine/install/))
- At least 4GB RAM available for Docker
- Your data files in CSV format

### Option 1: Using Docker Compose (Recommended)

```bash
# Clone this repository
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker

# 1. Scenario data (57 MB, SHA256-verified) — required before the first build
bash scripts/download_scenarios.sh

# 2. Login password for the web UI
cp .env.example .env
docker run --rm caddy:2.10.2-alpine caddy hash-password --plaintext 'choose-a-password'
#    Paste the output into .env inside SINGLE quotes: TRISK_PASS_HASH='$2a$14$...'

# 3. TLS certificate (self-signed for a single workstation; use your bank-issued cert/key otherwise)
openssl req -x509 -newkey rsa:2048 -nodes -days 365 -subj "/CN=localhost" \
  -keyout tls/key.pem -out tls/cert.pem
chmod 644 tls/key.pem   # Caddy runs with all capabilities dropped; it cannot read a 0600 key

# 4. Build and start (Caddy TLS proxy + app)
docker compose up -d --build

# View logs
docker compose logs -f
```

On Windows, run these commands in Git Bash (it ships `bash` and `openssl`).

### Option 2: Using Docker directly

```bash
# Scenario data is required before the first build
bash scripts/download_scenarios.sh

# Build the image
docker build -t trisk:latest .

# Run the container (no TLS or login — localhost only)
docker run -d \
  --name trisk-app \
  -p 127.0.0.1:3838:3838 \
  -v $(pwd)/data/input:/data/input:ro \
  -v $(pwd)/data/output:/data/output \
  trisk:latest
```

### Access the Application

- Docker Compose (Option 1): **https://localhost/** — accept the self-signed certificate warning and log in as `analyst` (or your `TRISK_USER`) with the password you hashed.
- Docker directly (Option 2): **http://localhost:3838/**

## Data Requirements

### Portfolio CSV

Your loan book with exposures. Required columns:

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer/string | Unique company identifier (or use `company_name` for fuzzy matching) |
| `sector` | string | Sector: Oil&Gas, Coal, Power, etc. |
| `technology` | string | Technology: Gas, Coal, RenewablesCap, CoalCap, OilCap, etc. |
| `country_iso2` | string | Two-letter country code (e.g., DE, US, FR) |
| `exposure_value_usd` | numeric | Loan exposure amount in USD |
| `term` | integer | Loan term in years (1-5) |
| `loss_given_default` | numeric | LGD ratio (0-1) |

### Assets CSV

Company-level production data. Required columns:

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Must match portfolio |
| `company_name` | string | Company name |
| `asset_id` | integer | Unique asset identifier |
| `country_iso2` | string | Asset location |
| `sector` | string | Production sector |
| `technology` | string | Technology type |
| `production_year` | integer | Year of production data |
| `capacity` | numeric | Asset capacity |
| `capacity_factor` | numeric | Utilization (0-1) |
| `emission_factor` | numeric | Emissions per unit produced |
| `production_unit` | string | Unit of production (e.g., GJ, MWh) |

### Financial Features CSV

Company financial metrics. Required columns:

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Must match portfolio and assets |
| `pd` | numeric | Current probability of default |
| `net_profit_margin` | numeric | Net profit margin ratio |
| `debt_equity_ratio` | numeric | Debt to equity ratio |
| `volatility` | numeric | Asset value volatility |

### Scenarios Data

Climate transition scenarios are **pre-loaded** in the container from public NGFS data. You can also upload custom scenarios if needed.

## Output Files

The analysis produces:

- **NPV Results**: Net present value under baseline and shock scenarios per company/technology
- **PD Results**: Probability of default under baseline and shock scenarios
- **Company Trajectories**: Detailed production and financial projections

Export formats: Excel (.xlsx), CSV, JSON

## Command-Line Interface (Advanced)

For automated pipelines, use the CLI version:

```bash
# Build CLI image
docker build -f Dockerfile.cli -t trisk-cli:latest .

# Run analysis
docker run --rm \
  -v $(pwd)/data/input:/data/input:ro \
  -v $(pwd)/data/output:/data/output \
  trisk-cli:latest \
  --portfolio=/data/input/portfolio.csv \
  --assets=/data/input/assets.csv \
  --financial=/data/input/financial.csv \
  --output=/data/output
```

See all options:
```bash
docker run --rm trisk-cli:latest --help
```

## Configuration

### Resource Limits

Adjust memory limits in `docker-compose.yml` based on portfolio size:

```yaml
deploy:
  resources:
    limits:
      memory: 8G  # Increase for large portfolios
```

### Environment Variables

| Variable | Description | Default |
|----------|-------------|---------|
| `SHINY_LOG_LEVEL` | Logging verbosity | TRACE |

## Security

- All data processing happens locally inside the container
- No external network calls after initial setup
- Input data is mounted read-only
- Container runs as non-root user (trisk)

## Troubleshooting

### Container won't start

```bash
# Check logs
docker logs trisk-app
docker logs trisk-proxy   # Caddy: missing TRISK_PASS_HASH, unreadable tls/key.pem, ...

# Check if port is in use
lsof -i :443
```

### Out of memory

1. Increase Docker memory in Docker Desktop settings
2. Or adjust limits in docker-compose.yml

### Permission denied on data folder

```bash
chmod -R 755 ./data
```

### Shiny app shows error

1. Verify all required CSV columns are present
2. Check data types match expected formats
3. View detailed logs: `docker logs trisk-app`

## Development

### Rebuild after changes

```bash
docker compose build --no-cache
docker compose up -d
```

### Access container shell

```bash
docker exec -it trisk-app /bin/bash
```

### Run R interactively

```bash
docker exec -it trisk-app R
```

## License

- trisk.model: GPL-3.0
- trisk.analysis: LGPL-3.0
- This Docker configuration: MIT

## Links

- [trisk.model Documentation](https://theia-finance-labs.github.io/trisk.model/)
- [trisk.analysis Documentation](https://theia-finance-labs.github.io/trisk.analysis/)
- [Theia Finance Labs](https://github.com/Theia-Finance-Labs)
- [1in1000 Initiative](https://1in1000.com)

## Funding

Co-funded by the European Union. Scientific Transition Risk Exercises for Stress tests & Scenario Analysis has received funding from the European Union's Life programme under Grant No. LIFE21-GIC-DE-Stress.
