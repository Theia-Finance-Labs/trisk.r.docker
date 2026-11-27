# Scenario Library

TRISK Desktop includes 114 pre-loaded climate transition scenarios from four major providers.

## Scenario providers

### NGFS (Network for Greening the Financial System)

The primary scenario source for banking climate stress tests. NGFS scenarios are organized by policy ambition:

| Category | Scenarios | Description |
|----------|-----------|-------------|
| Orderly (Net Zero) | NGFS2024GCAM_NZ2050, NGFS2023GCAM_NZ2050 | Early, coordinated policy action achieves net-zero by 2050 |
| Orderly (Below 2C) | NGFS2024GCAM_B2C | Gradual policy tightening limits warming to below 2C |
| Disorderly (Delayed) | NGFS2024GCAM_DT | Late policy action leads to disorderly transition after 2030 |
| Hot House World | NGFS2024GCAM_CP | Current Policies - minimal additional action (used as baseline) |

**Sectors covered:** Oil&Gas, Coal, Power

**Geographies:** Global, EU, US, Developing Economies, and regional groupings

**Models:** GCAM, REMIND-MAgPIE, MESSAGEix-GLOBIOM

### IPR (Inevitable Policy Response)

Scenarios focused on sectors with distinct transition pathways not well covered by NGFS:

| Scenario | Type | Sector |
|----------|------|--------|
| IPR2023Automotive_baseline | Baseline | Automotive |
| IPR2023Automotive_FPS | Target (Forecast Policy) | Automotive |
| IPR2021_FPS | Target | Power, Oil&Gas |
| IPR2021_RPS | Target (Required Policy) | Power, Oil&Gas |

**Primary use:** Automotive sector analysis where NGFS scenarios have limited coverage.

### Mission Possible Partnership

Deep decarbonization pathways for hard-to-abate industrial sectors:

| Scenario | Type | Sector |
|----------|------|--------|
| mission_possible_Steel_baseline | Baseline | Steel |
| mission_possible_Steel_NZ | Target (Net Zero) | Steel |

**Primary use:** Steel sector analysis.

### GECO (Global Energy and Climate Outlook)

EU Joint Research Centre scenarios providing additional European policy perspectives.

## How scenarios are matched

The app automatically selects appropriate scenarios based on portfolio composition:

| Portfolio sectors | Default scenario pair |
|-------------------|----------------------|
| Oil&Gas, Coal, Power | NGFS GCAM Current Policies (baseline) vs. Net Zero 2050 (target) |
| Automotive | IPR Automotive baseline vs. Forecast Policy Scenario |
| Steel | Mission Possible baseline vs. Net Zero |

If your portfolio spans multiple sector groups, the app creates separate scenario pairs for each group and runs them independently.

## Scenario structure

Each scenario provides year-by-year projections for:

- **Production/capacity pathways** - how much each technology produces under the scenario
- **Energy/commodity prices** - price projections under different policy assumptions
- **Carbon prices** - the implicit or explicit carbon cost in each scenario

The model uses these projections to compute how company revenues, costs, and ultimately NPV change when transitioning from the baseline to the target scenario.

## Custom scenarios

You can upload your own scenario file with the same column structure as the bundled data. See the [Data Dictionary](data-dictionary.md) for the required schema.

Custom scenarios appear in the baseline and target dropdown menus alongside the bundled scenarios.

## Scenario geographies

Available geographies depend on the scenario provider:

| Geography | NGFS | IPR | Mission Possible |
|-----------|------|-----|-----------------|
| Global | Yes | Yes | Yes |
| EU / Europe | Yes | Limited | No |
| US / North America | Yes | Limited | No |
| Developing Economies | Yes | No | No |
| Country-specific | Some | No | No |

When a selected geography is not available for a specific scenario, the app falls back to **Global**.
