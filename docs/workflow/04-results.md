# Step 4: Portfolio Results

The Portfolio Results tab is the main output screen. It shows headline metrics, detailed breakdowns, and interactive charts across five sub-tabs.

## Headline value boxes

Five KPI cards appear at the top of the page:

| Metric | What it shows |
|--------|-------------|
| **Companies** | Number of portfolio companies with results |
| **Total Exposure** | Sum of all `exposure_value_usd` in your portfolio |
| **Max PD Shock** | Highest single-company PD shock value |
| **PD Change** | Portfolio-weighted average PD change (basis points) |
| **Total EL** | Total Expected Loss under the shock scenario |

## Summary tab

A table showing sector/technology-level aggregates:

- Number of companies per sector-technology combination
- Average shocked PD
- Average PD change
- Total exposure
- Total Expected Loss

Use this tab for the portfolio overview that goes into ICAAP summary tables and board presentations.

## Credit Risk Analysis tab

Detailed company-level credit risk view:

**PD Change by Sector bar chart**

:   Interactive plotly chart showing PD shock magnitude for each company, colored by sector. Red bars indicate PD increase (higher default risk), green bars indicate PD decrease (lower risk).

**Company-level PD table**

:   Full results table with sortable columns: company name, sector, technology, baseline PD, shocked PD, PD change, percentage change, exposure, Expected Loss.

**PD Distribution histogram**

:   Distribution of PD changes across all portfolio companies. Shows the spread of transition risk impact - how many companies see large PD increases versus moderate or negligible changes.

## Risk Concentration tab

Measures how concentrated transition risk is across your portfolio:

**Concentration metrics**

:   HHI Index (Herfindahl-Hirschman), Top-5 and Top-10 concentration ratios, maximum single-name weight, number of sectors represented.

**Concentration Heatmap**

:   Visual matrix showing exposure concentration by sector and PD impact level. Darker cells indicate higher concentration of risk in specific sector-impact combinations.

**Risk-Return Scatter**

:   Each bubble represents one company. The x-axis shows PD change, y-axis shows exposure weight in the portfolio, and bubble size represents the absolute EL change. Companies in the upper-right quadrant have both high exposure weight and large PD increases - the highest risk contributors.

## Attribution tab

Decomposes the portfolio-level PD change into sector and company contributions:

**PD Waterfall chart**

:   Starts at baseline portfolio PD, shows each sector's contribution to the total change, and lands at the final shocked PD. This answers: "Which sectors are driving the portfolio risk increase?"

**Sector attribution table**

:   Quantitative breakdown of each sector's exposure-weighted contribution to portfolio PD change.

## Horizon Analysis tab

!!! note "Requires 2+ shock years"
    This tab only activates when you selected multiple shock years in the Configure step (e.g., 2025, 2030, 2035).

Shows how risk evolves across different shock year horizons:

**Year-over-Year Metrics table**

:   For each shock year: average PD, maximum PD, total EL, EL change from previous year, number of affected companies, and PD delta between consecutive years. Later shock years typically show larger impacts as companies have longer to accumulate misalignment.

**Horizon charts**

:   Line charts showing PD and EL evolution across shock years at sector and portfolio level.

## Exporting from results

Each sub-tab includes an **Export CSV** button that downloads the specific table shown. For a full export of all results, use the [Download tab](07-download.md).

## Run history

If you run the analysis multiple times (e.g., with different scenarios or parameters), the run history panel lets you compare up to 5 previous runs side by side.

## Next step

To compare results across multiple scenario families, proceed to [Step 5: Scenario Analysis](05-scenarios.md). To integrate with your internal PD/EL, skip to [Step 6: Risk Integration](06-integration.md).
