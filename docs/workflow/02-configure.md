# Step 2: Configure Analysis

The Configure Analysis tab is where you select climate scenarios, set model parameters, and define the shock timeline.

## Scenario selection

The Scenarios panel shows two dropdown menus:

**Baseline (Business-as-Usual)**

:   The reference scenario representing the current policy trajectory. Default: **Current Policies** from the NGFS GCAM model.

**Target (Transition Goal)**

:   The climate transition shock scenario. Default: **Net Zero 2050** from the NGFS GCAM model.

The model computes the difference between these two scenarios to measure the transition risk impact. At the shock year, companies are forced from the baseline trajectory onto the target pathway.

!!! info "Sector-aware scenario defaults"
    The app automatically detects which sectors are in your portfolio and selects appropriate scenarios:

    - **Oil&Gas, Coal, Power** - NGFS scenarios (default)
    - **Automotive** - IPR scenarios
    - **Steel** - Mission Possible scenarios

    If your portfolio spans multiple sectors, the app creates separate scenario pairs for each sector group.

### Scenario geography

Select the regional scope for scenario pathways. Options include Global, EU, US, Developing Economies, and others depending on the loaded scenario data. Default: **Global**.

## Shock scenario year

Select one or more shock years between **2025 and 2040**. The shock year determines *when* companies are forced from the baseline trajectory onto the target pathway.

- A single shock year produces a single-point stress test
- **Multiple shock years** (e.g., 2025, 2030, 2035) enable the **Horizon Analysis** tab in the results, showing how risk evolves over time

## Model parameters

The Model Parameters panel controls the Discounted Cash Flow (DCF) valuation:

| Parameter | Default | Range | What it controls |
|-----------|---------|-------|-----------------|
| Risk-Free Rate | 0.02 | 0 - 0.10 | Interest rate used in DCF discounting |
| Discount Rate | 0.07 | 0 - 0.20 | Discount rate of dividends per year |
| Growth Rate | 0.03 | 0 - 0.10 | Terminal growth rate beyond the final year |
| Market Passthrough | 0.00 | 0 - 1.00 | Ability to pass carbon costs to consumers (0 = none, 1 = full) |

### Carbon price model

Choose whether to include carbon pricing effects:

- **No Carbon Tax** (default) - carbon prices are not applied to company emissions
- **Apply Carbon Tax** - scenario-specific carbon prices are applied, reducing profits for high-emission companies based on the market passthrough rate

### Reset to defaults

Click **Reset All to Defaults** to restore all parameters to their original values.

## Configuration summary

The Run Analysis tab (next step) displays a summary of your selected configuration, so you can verify before executing.

## Next step

Once configured, proceed to [Step 3: Run Analysis](03-run.md).
