# Data Preparation

TRISK Desktop accepts four CSV input files. Only the **portfolio** file is strictly required - the app includes bundled asset-level and scenario data that covers most use cases.

## Overview

| Dataset | Required? | Rows (typical) | Purpose |
|---------|-----------|-----------------|---------|
| Portfolio | Yes | 10 - 10,000 | Your loan book with company exposures |
| Assets | Recommended | 100,000+ | Company production data (bundled dataset included) |
| Financial Features | Optional | 1,000+ | Company PD, margins, volatility for Merton model |
| Scenarios | Optional | 100,000+ | Climate transition pathways (114 scenarios pre-loaded) |

## Portfolio CSV

Your loan book with exposures to climate-sensitive sectors.

| Column | Type | Required | Description | Example |
|--------|------|----------|-------------|---------|
| `company_id` | integer | Yes | Unique company identifier | `374444` |
| `company_name` | string | Yes | Company name (used for fuzzy matching) | `ACME Energy Ltd.` |
| `sector` | string | No | Sector classification (enriched from assets if missing) | `Power` |
| `technology` | string | No | Technology (enriched from assets if missing) | `CoalCap` |
| `country_iso2` | string | Yes | ISO 3166-1 alpha-2 country code | `DE` |
| `exposure_value_usd` | numeric | Yes | Loan exposure in USD | `5000000` |
| `term` | integer | Yes | Loan term in years (1-5) | `3` |
| `loss_given_default` | numeric | Yes | LGD ratio (0 to 1) | `0.45` |

!!! tip "Sector and technology columns"
    If your portfolio only has `company_id` and exposure data, the app will automatically enrich it with sector and technology information from the assets database. You do not need to manually classify companies.

### Supported sectors and technologies

| Sector | Technologies |
|--------|-------------|
| Power | CoalCap, OilCap, GasCap, NuclearCap, HydroCap, RenewablesCap |
| Oil&Gas | Oil, Gas |
| Coal | Coal |
| Automotive | Electric, FuelCell, Hybrid, ICE |
| Steel | Integrated facility, Electric arc furnace |

## Assets CSV

Company-level production data linking portfolio companies to physical assets. A comprehensive global dataset is **bundled with the app** covering thousands of companies. Upload your own only if you have proprietary asset data.

| Column | Type | Required | Description |
|--------|------|----------|-------------|
| `company_id` | integer | Yes | Must match portfolio |
| `company_name` | string | Yes | Company name |
| `asset_id` | string | Yes | Unique asset identifier |
| `country_iso2` | string | Yes | Asset location (ISO alpha-2) |
| `sector` | string | Yes | Production sector |
| `technology` | string | Yes | Technology type |
| `production_year` | integer | Yes | Year of production data |
| `capacity` | numeric | Yes | Asset capacity |
| `capacity_factor` | numeric | Yes | Utilization factor (0-1) |
| `emission_factor` | numeric | Yes | Emissions per unit produced |
| `production_unit` | string | Yes | Unit of production (MW, GJ, tonnes) |

## Financial Features CSV

Company financial metrics used in the Merton structural model for PD estimation.

| Column | Type | Required | Description |
|--------|------|----------|-------------|
| `company_id` | integer | Yes | Must match portfolio and assets |
| `pd` | numeric | Yes | Current probability of default |
| `net_profit_margin` | numeric | Yes | Net profit margin ratio |
| `debt_equity_ratio` | numeric | Yes | Debt to equity ratio |
| `volatility` | numeric | Yes | Asset value volatility |

## Scenarios CSV

Climate transition scenarios are **pre-loaded** with 114 scenarios from NGFS, IPR, GECO, and Mission Possible. Upload custom scenarios only if you have proprietary pathway data.

See the [Scenario Library](reference/scenarios.md) reference for the full list of bundled scenarios.

## Data quality tips

!!! warning "Column names are case-sensitive"
    The column `company_id` is not the same as `Company_ID`. Use exact lowercase names as shown above.

- **company_id must match** across portfolio, assets, and financial features files
- **No missing values** in required columns - the app will flag rows with NAs
- **Country codes** must be ISO 3166-1 alpha-2 (two uppercase letters: DE, US, FR, MN)
- **Numeric columns** must contain numbers only - no currency symbols, commas, or text
- **File format:** UTF-8 encoded CSV with comma delimiters
- **Upload limit:** 100 MB per file, 500,000 rows maximum

!!! info "Column allowlisting"
    For security, the app automatically strips any columns not in the expected list. Extra columns (including potentially sensitive ones) are removed on upload and never stored.

## Download templates

The app provides downloadable CSV templates with the correct column headers. On the Upload Data tab, click **Download Template CSV** under each data panel to get a blank template.
