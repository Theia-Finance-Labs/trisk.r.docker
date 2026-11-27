# Step 7: Download & Export

The Download tab provides all export options and run metadata for audit documentation.

## Export formats

**Download All Results (Excel)**

:   Multi-sheet XLSX workbook containing:

    - **Summary** - portfolio-level KPIs by sector and technology
    - **Full Results** - company-level baseline PD, shocked PD, PD change, Expected Loss, NPV
    - **PD Integration** - adjusted PD values (if Step 6 was used)
    - **EL Integration** - adjusted EL values (if Step 6 was used)

**Download Results (CSV)**

:   Flat CSV with all company-level results in a single file. Best for pipeline integration or further analysis in R, Python, or Excel.

**Download Results (JSON)**

:   Machine-readable structured output. Includes results array and metadata. Best for API integration or archival.

**Download HTML Report**

:   Self-contained HTML summary with embedded charts. Can be opened in any browser and shared by email without requiring TRISK Desktop.

## Analysis metadata

The right panel shows run metadata:

- **Run ID** - unique identifier for this analysis run (format: `YYYYMMDD_HHMMSS_<hash>`)
- **Timestamp** - exact date and time the analysis was executed
- **Scenario Pairs** - which baseline/target combinations were used
- **Geography** - scenario geography scope
- **Results rows** - number of company-level result rows

## Download Configuration

Click **Download Configuration** to save a JSON file with the complete run configuration:

```json
{
  "run_id": "20260325_231552_eb1600eb",
  "baseline_scenario": "NGFS2023GCAM_CP",
  "target_scenario": "NGFS2023GCAM_NZ2050",
  "scenario_geography": "Global",
  "shock_years": [2025, 2030, 2035],
  "risk_free_rate": 0.02,
  "discount_rate": 0.07,
  "growth_rate": 0.03,
  "market_passthrough": 0,
  "carbon_price_model": "no_carbon_tax",
  "model_version": "2.6.1",
  "data_checksums": { ... }
}
```

This file enables full reproducibility - anyone with the same input data and configuration can replicate the exact results.

## Security notes

- All exports pass through a **column allowlist** - only expected result columns are included, never raw uploaded data or internal fields
- Character columns are sanitized against **formula injection** (CWE-1236) - cells starting with `=`, `+`, `@`, or tab characters are prefixed with a single quote to prevent Excel formula execution
- Exports are written to the `data/output/` directory inside the container

## Using exports for regulatory reporting

For ICAAP and regulatory submissions:

1. Download the **Excel** file for the main results workbook
2. Download the **Configuration** JSON as audit evidence of parameters used
3. The **Run ID** ties all outputs to a specific analysis execution
4. If integration was used, the Excel file includes both raw TRISK and adjusted columns clearly labeled
