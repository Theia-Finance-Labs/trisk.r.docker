# Step 1: Upload Data

The Upload Data tab is where you load your portfolio and supporting data into TRISK Desktop.

## Quick start with demo data

The fastest way to try the app is the **Load Demo Data** button at the top of the page. This loads a synthetic portfolio (4 companies across Oil&Gas, Coal, and Power) with matching asset, financial, and scenario data from the `trisk.model` and `trisk.analysis` R packages.

Click **Load Demo Data from trisk.model** and you will see green checkmarks confirming each dataset loaded successfully:

- Portfolio: 4 rows
- Assets: 42 rows
- Financial: 5 rows
- Scenarios: 1,422 rows
- Carbon prices: 1,376 rows

## Uploading your own data

Below the demo data section, four upload panels accept CSV files:

**Portfolio Data** (required)

:   Your loan book. Click **Browse** to select a CSV file. The app validates column names immediately after upload and reports any missing required columns.

**Assets Data** (recommended)

:   Company-level production and asset data. A comprehensive global dataset is bundled with the app. Upload your own only if you have proprietary asset-level data.

**Financial Features** (optional)

:   Company PD, profit margins, debt ratios, and volatility. Used by the Merton structural model to compute PD changes. If not provided, the app uses the bundled financial dataset.

**Scenarios** (optional)

:   114 scenarios from NGFS, IPR, GECO, and Mission Possible are pre-loaded. Upload only if you have custom scenario pathways.

## Validation

After upload, the app runs validation checks:

- Required columns present (case-sensitive match)
- Numeric columns contain valid numbers
- No entirely empty files
- Row count within limits (max 500,000)
- File size within limits (max 100 MB)

The **Data Quality Overview** panel at the bottom shows validation status for each dataset. If issues are found, download the **Error Report CSV** for details.

!!! tip "Column stripping"
    Extra columns beyond the expected set are automatically removed on upload. This is a security feature that prevents accidental retention of PII or sensitive data.

## Alternative: mount data via Docker

Instead of uploading through the browser, you can place CSV files in the `data/input/` directory before starting the container. The app detects files at this path and makes them available for loading.

```bash
# Place your files
cp my_portfolio.csv data/input/
cp my_assets.csv data/input/

# Start the container (files are mounted read-only)
docker compose -f docker-compose.dev.yml up -d
```

## Next step

Once data is loaded, proceed to [Step 2: Configure Analysis](02-configure.md).
