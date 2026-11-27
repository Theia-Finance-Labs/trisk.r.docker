# Data Dictionary

Complete field definitions for all TRISK Desktop inputs and outputs.

## Input fields

### Portfolio

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer/string | Unique company identifier |
| `company_name` | string | Company name (used for display and fuzzy matching) |
| `sector` | string | Sector classification (Power, Oil&Gas, Coal, Automotive, Steel) |
| `technology` | string | Technology within sector (CoalCap, GasCap, RenewablesCap, etc.) |
| `country_iso2` | string | ISO 3166-1 alpha-2 country code |
| `exposure_value_usd` | numeric | Loan exposure amount in USD |
| `term` | integer | Loan maturity in years (1-5) |
| `loss_given_default` | numeric | Loss Given Default ratio (0 to 1) |

### Assets

| Column | Type | Description |
|--------|------|-------------|
| `asset_id` | string | Unique asset identifier |
| `asset_name` | string | Asset name or description |
| `company_id` | integer | Links to portfolio `company_id` |
| `company_name` | string | Company that owns/operates the asset |
| `country_iso2` | string | Asset location country |
| `sector` | string | Production sector |
| `technology` | string | Technology type |
| `production_year` | integer | Year of the production data point |
| `capacity` | numeric | Installed capacity |
| `capacity_factor` | numeric | Utilization factor (0 to 1) |
| `emission_factor` | numeric | Emissions per unit of production |
| `production_unit` | string | Unit of measurement (MW, GJ, tonnes) |

### Financial Features

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Links to portfolio and assets |
| `pd` | numeric | Current probability of default |
| `net_profit_margin` | numeric | Net profit margin ratio |
| `debt_equity_ratio` | numeric | Debt to equity ratio |
| `volatility` | numeric | Asset value volatility (Merton model input) |

### Scenarios

| Column | Type | Description |
|--------|------|-------------|
| `scenario` | string | Scenario code (e.g., `NGFS2024GCAM_CP`) |
| `scenario_type` | string | `baseline` or `target` |
| `scenario_geography` | string | Regional scope (Global, EU, US, etc.) |
| `sector` | string | Sector covered |
| `technology` | string | Technology covered |
| `scenario_year` | integer | Year in the projection (2022-2100) |
| `scenario_price` | numeric | Commodity/energy price projection |
| `price_unit` | string | Price unit (USD per unit) |
| `scenario_pathway` | numeric | Production/capacity pathway value |
| `pathway_unit` | string | Pathway unit (GW, Mt, etc.) |
| `technology_type` | string | `carbontech` or `greentech` |
| `scenario_provider` | string | Source (NGFS2024, IPR2021, etc.) |
| `scenario_capacity_factor` | numeric | Expected capacity factor in scenario |

## Output fields

### PD Results (company-level)

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | integer | Company identifier |
| `company_name` | string | Company name |
| `sector` | string | Sector |
| `technology` | string | Technology |
| `pd_baseline` | numeric | Probability of Default under baseline scenario |
| `pd_shock` | numeric | Probability of Default under shock scenario |
| `pd_change` | numeric | `pd_shock - pd_baseline` (positive = higher risk) |
| `pd_change_pct` | numeric | Percentage change in PD |
| `exposure_value_usd` | numeric | Loan exposure |
| `loss_given_default` | numeric | LGD ratio |
| `exposure_at_default` | numeric | `exposure_value_usd * loss_given_default` |
| `expected_loss_baseline` | numeric | `-(EAD * pd_baseline)` |
| `expected_loss_shock` | numeric | `-(EAD * pd_shock)` |
| `expected_loss_difference` | numeric | `-(EAD * (pd_shock - pd_baseline))` |

### NPV Results (company-level)

| Column | Type | Description |
|--------|------|-------------|
| `net_present_value_baseline` | numeric | NPV of future cash flows under baseline |
| `net_present_value_shock` | numeric | NPV under shock scenario |
| `net_present_value_difference` | numeric | `shock - baseline` (negative = value destruction) |
| `net_present_value_change` | numeric | Percentage change in NPV |

### Run Metadata

| Field | Description |
|-------|-------------|
| `run_id` | Unique identifier: `YYYYMMDD_HHMMSS_<8-char-hash>` |
| `baseline_scenario` | Selected baseline scenario code |
| `target_scenario` | Selected target scenario code |
| `scenario_geography` | Selected geography |
| `shock_years` | Array of shock years tested |
| `risk_free_rate` | DCF risk-free rate used |
| `discount_rate` | DCF discount rate used |
| `growth_rate` | DCF terminal growth rate used |
| `market_passthrough` | Carbon cost passthrough rate used |
| `carbon_price_model` | `no_carbon_tax` or `carbon_tax` |
| `model_version` | `trisk.model` package version |
| `data_checksums` | SHA-256 hashes of input data files |
