# Step 3: Run Analysis

The Run Analysis tab executes the TRISK stress test on your portfolio.

## Pre-run checklist

Before the Run button activates, the app verifies:

- Portfolio data uploaded
- Assets data uploaded
- Financial data uploaded
- Scenarios data available
- Carbon price data available
- Baseline and target scenarios selected
- Portfolio sectors detected
- At least one valid scenario pair

All items must show green checkmarks. If any show red, go back to the Upload or Configure tabs to resolve.

## Configuration summary

The right panel displays the exact parameters that will be used:

```
Core Energy (NGFS): Net Zero 2050
Geography: Global
Shock Year(s): 2025, 2030, 2035
Risk-Free Rate: 0.02
Discount Rate: 0.07
Growth Rate: 0.03
Market Passthrough: 0
Carbon Price Model: no_carbon_tax
```

Verify this matches your intent before proceeding.

## Running the model

Click the **Run TRISK Analysis** button. The model executes these steps:

1. **Match** portfolio companies to the assets database by `company_id`
2. **Compute** production trajectories under baseline and shock scenarios for each company-sector-technology
3. **Derive** PD changes using the Merton structural model - comparing company NPV under both scenarios
4. **Calculate** Expected Loss = -(Exposure at Default x PD) for both scenarios

The Analysis Log at the bottom shows real-time progress. A typical 100-company portfolio takes 30-60 seconds. Larger portfolios (500+ companies) may take several minutes.

## Run ID

Each run generates a unique identifier in the format:

```
YYYYMMDD_HHMMSS_<8-char-hash>
```

For example: `20260325_231552_eb1600eb`

This ID is attached to all results and exports, enabling full traceability and reproducibility. The hash component is derived from the run configuration, so identical parameters produce the same hash prefix.

## What if the run fails?

Common causes:

- **No matching companies** - portfolio company_ids don't appear in the assets database. Check that IDs match across files.
- **Scenario not available for geography** - the selected scenario may not cover your chosen geography. Try "Global" or check the [Scenario Library](../reference/scenarios.md).
- **Out of memory** - for large portfolios, increase Docker memory allocation (Settings > Resources > Memory).

The Analysis Log will show warning messages for any sector/year combinations that failed, while successful combinations still produce results.

## Next step

After the run completes, the app automatically navigates to [Step 4: Portfolio Results](04-results.md).
