# Step 6: Risk Integration

The Risk Integration tab is an **optional step** where you combine TRISK climate stress test outputs with your bank's internally calculated PD and Expected Loss estimates.

Skip this step if you only need the raw TRISK results from Step 4.

## Why integrate?

TRISK produces PD changes *relative to its own baseline scenario*. Your bank likely has its own PD estimates from internal rating models. Integration combines both:

- TRISK tells you **how much** PD changes due to climate transition
- Your internal model tells you **what the current PD is** for each borrower
- Integration produces an **adjusted PD** that incorporates both

## PD Integration

### Choose a method

Three methods are available, each suited to a different institutional context:

**Absolute Change**

:   Best when your internal PDs are *point-in-time* estimates. Adds the raw TRISK PD delta to your internal PD.

    ```
    Adjusted PD = Internal PD + (PD_shock - PD_baseline)
    ```

**Relative Change**

:   Best when your internal PDs are *through-the-cycle* estimates. Applies a proportional scaling that respects your existing PD level.

    ```
    Adjusted PD = Internal PD * (1 + PD_change%)
    ```

**Z-Score (Basel IRB)**

:   For Basel III/IV IRB-aligned institutions using the Vasicek one-factor model. Combines PDs in normal quantile space, preserving tail-risk behavior.

    ```
    Adjusted PD = Phi(Phi_inv(Internal) + Phi_inv(Shock) - Phi_inv(Baseline))
    ```

    Recommended for regulatory capital calculations.

### Providing your internal PD values

Two options:

1. **Upload CSV** - prepare a file with `company_id` and `internal_pd` columns (values between 0 and 1)
2. **Edit in table** - the app shows a pre-filled table where you can type or paste values directly

### Integration results

After selecting a method and providing internal PDs, the app displays:

- **Company-level adjusted PD table** - baseline, TRISK shock, internal, and adjusted PD side by side
- **Portfolio aggregates** - exposure-weighted average adjusted PD, sector breakdowns
- **Adjustment bar chart** - visual comparison of internal versus adjusted PD per company

## EL Integration

The Expected Loss section works similarly, with two methods:

**Absolute Change**

:   `Adjusted EL = Internal EL + (EL_shock - EL_baseline)`

**Relative Change**

:   `Adjusted EL = Internal EL * (EL_shock / EL_baseline)`

Upload a CSV with `company_id` and `internal_el` columns, or edit values directly.

## Export

Integration results are included in the Excel and CSV exports on the Download tab, alongside the raw TRISK results. The exported file clearly labels which columns are TRISK outputs and which are integrated values.

## Next step

Proceed to [Step 7: Download & Export](07-download.md) to export all results.
