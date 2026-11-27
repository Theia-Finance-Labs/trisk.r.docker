# Step 5: Scenario Analysis

The Scenario Analysis tab runs a sensitivity analysis across multiple climate scenario families to compare how different transition pathways affect your portfolio.

## How it works

Instead of testing a single baseline-target pair (as in Step 3), this tab runs your portfolio against **multiple target scenarios** while keeping the same baseline. This answers: "How sensitive is my portfolio to the severity of the transition?"

## Configuration

**Select Policy Groups to Analyze**

:   The app automatically detects which scenario families are available based on your loaded scenario data and groups them by ambition level (Net Zero, Below 2C, Delayed Transition, Current Policies). Check the boxes for the groups you want to compare.

**Your scenario** is marked with a star icon - this is the scenario pair you already ran in Step 3.

The analysis uses the same shock years, geography, and model parameters from your Step 2 configuration.

## Running the sensitivity analysis

Click **Run Sensitivity Analysis**. The app runs the TRISK model once per selected scenario family, reusing the same portfolio, assets, and financial data. This takes longer than a single run - roughly proportional to the number of selected scenarios.

## Results

After completion, the tab displays:

**Scenario Comparison table**

:   Side-by-side metrics (average PD, max PD, total EL, exposure at risk) for each scenario family. Highlights which transition pathway produces the largest portfolio impact.

**Distribution comparison charts**

:   PD distributions overlaid across scenario families, showing how the spread of company-level impacts varies by transition ambition.

**Sector-level sensitivity**

:   How each sector responds differently to mild versus aggressive transition scenarios. Some sectors (e.g., Coal) may show large PD changes across all scenarios, while others (e.g., Power with renewables) may benefit under aggressive transition pathways.

!!! tip "Interpreting scenario families"
    - **Net Zero 2050**: Aggressive, orderly transition - largest PD impact on carbon-intensive companies
    - **Below 2C**: Moderate transition ambition
    - **Delayed Transition**: Late, disorderly action - shock concentrated in later years
    - **Current Policies**: Minimal additional policy action - lowest transition risk, but highest physical risk (not modeled here)

## When to use this tab

- Regulatory submissions requiring multi-scenario analysis (ECB, PRA guidelines)
- Board presentations showing the range of possible outcomes
- Risk appetite discussions comparing mild versus severe transition impacts
- ICAAP stress test documentation with scenario sensitivity evidence

## Next step

To combine TRISK outputs with your internal PD/EL estimates, proceed to [Step 6: Risk Integration](06-integration.md). To export results directly, skip to [Step 7: Download](07-download.md).
