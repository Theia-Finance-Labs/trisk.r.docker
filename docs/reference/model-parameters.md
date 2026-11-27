# Model Parameters

TRISK Desktop uses a Discounted Cash Flow (DCF) model combined with a Merton structural credit risk model to translate climate scenario impacts into financial risk metrics.

## DCF Parameters

These parameters control the valuation model that converts production trajectory changes into NPV impacts:

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| **Risk-Free Rate** | 0.02 | 0 - 0.10 | The risk-free interest rate used for discounting future cash flows. Higher values reduce the present value of distant impacts, lowering sensitivity to long-horizon scenarios. |
| **Discount Rate** | 0.07 | 0 - 0.20 | Rate at which future dividends are discounted. A higher discount rate reduces the present value impact of the transition shock. |
| **Growth Rate** | 0.03 | 0 - 0.10 | Terminal growth rate of profits beyond the final scenario year. Used in the Gordon Growth Model for terminal value. Must be less than the discount rate. |

## Carbon Pricing Parameters

| Parameter | Default | Range | Description |
|-----------|---------|-------|-------------|
| **Market Passthrough** | 0.00 | 0 - 1.00 | The fraction of carbon tax costs that the company can pass through to consumers. 0 = company absorbs all costs, 1 = consumers absorb all costs. |
| **Carbon Price Model** | No Carbon Tax | — | Whether to apply scenario-specific carbon prices to company emissions. When enabled, high-emission companies face additional cost pressure from carbon taxation. |

## How the model works

### Step 1: Production trajectories

For each company-sector-technology combination, the model constructs two production trajectories:

- **Baseline trajectory** - production levels following the business-as-usual scenario (e.g., Current Policies)
- **Shock trajectory** - production levels following the target scenario (e.g., Net Zero 2050), applied from the shock year onward

### Step 2: Net Present Value

The DCF model computes the NPV of future cash flows under each trajectory:

```
NPV = Sum(t=1..T) [ Net_Profit(t) / (1 + discount_rate)^t ] + Terminal_Value
```

Where Terminal Value uses the Gordon Growth Model:

```
Terminal_Value = Net_Profit(T) * (1 + growth_rate) / (discount_rate - growth_rate)
```

The difference `NPV_shock - NPV_baseline` measures the economic impact of the transition on each company.

### Step 3: Probability of Default

The Merton structural model translates NPV changes into PD changes. A company's distance-to-default decreases when its asset value (proxied by NPV) drops. The model uses the company's financial characteristics (debt-equity ratio, volatility, profit margin) from the financial features dataset.

### Step 4: Expected Loss

Expected Loss combines the PD change with exposure data:

```
EL = -(Exposure_at_Default * PD)
```

Where:

```
Exposure_at_Default = exposure_value_usd * loss_given_default
```

## Sensitivity to parameters

| If you increase... | Effect on results |
|--------------------|-------------------|
| Risk-Free Rate | Lower NPV impact, lower PD sensitivity to distant shocks |
| Discount Rate | Lower NPV impact overall, particularly for long-horizon scenarios |
| Growth Rate | Higher terminal values, amplifies long-run differences between scenarios |
| Market Passthrough | Reduces carbon tax impact on company profits (costs passed to consumers) |
| Shock Year (later) | More accumulated misalignment, typically larger PD impacts |

## Default values

The defaults are calibrated for a balanced analysis of European/global portfolios. For specific regional or institutional contexts, adjust based on your internal DCF assumptions.

To restore all defaults, click **Reset All to Defaults** on the Configure Analysis tab.
