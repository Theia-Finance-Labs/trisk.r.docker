# PD Analysis Overhaul — Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** Change PD change from absolute (pp) to relative (%), add plot legends, clean up density plot, and add PD Change vs Exposure scatter plot.

**Architecture:** Four independent changes to the Exposure PD Analysis tab in a Shiny/R app. All PD change calculations shift from `(shock - baseline) * 100` to `(shock - baseline) / baseline * 100` with a zero-baseline guard. A new scatter plot replaces the bubble overlay removed from the density plot.

**Tech Stack:** R, Shiny, ggplot2, plotly (via ggplotly), shinydashboard

---

### Task 1: Change `vb_pd_change` Value Box to Relative %

**Files:**
- Modify: `app/server.R:1992-2011` (the `vb_pd_change` renderValueBox block)

**Step 1: Update the value box formula and display**

Replace the absolute formula with relative percentage change. Change the label and display format.

In `app/server.R`, replace lines 1992-2011 with:

```r
  output$vb_pd_change <- renderValueBox({
    if (is.null(rv$results)) {
      return(valueBox("---", "PD Change, Exposure-Weighted (%)", icon = icon("balance-scale"), color = "blue"))
    }
    val <- NA_real_
    df <- rv$results
    if (all(c("pd_baseline", "pd_shock") %in% names(df))) {
      exp_col <- if ("exposure_value_usd" %in% names(df)) df$exposure_value_usd else rep(1, nrow(df))
      total_exp <- sum(exp_col, na.rm = TRUE)
      if (total_exp > 0) {
        weighted_baseline <- sum(df$pd_baseline * exp_col, na.rm = TRUE) / total_exp
        weighted_change   <- sum((df$pd_shock - df$pd_baseline) * exp_col, na.rm = TRUE) / total_exp
        val <- if (weighted_baseline != 0) weighted_change / weighted_baseline * 100 else 0
      }
    }
    display_val <- if (!is.na(val)) paste0(smart_round(val), "%") else "N/A"
    valueBox(tags$span(display_val,
                       title = paste0("Full precision: ", smart_round(val), "%"),
                       style = "cursor: help;"),
             "PD Change, Exposure-Weighted (%)",
             icon = if (!is.na(val) && val > 0) icon("arrow-up") else icon("arrow-down"),
             color = if (!is.na(val) && val > 0.01) "red" else if (!is.na(val) && val < -0.01) "green" else "blue")
  })
```

**Step 2: Rebuild Docker and verify value box renders**

Run: `docker compose -f docker-compose.dev.yml up --build -d`
Expected: Container starts, no R errors in `docker logs trisk-app-dev`

**Step 3: Commit**

```bash
git add app/server.R
git commit -m "refactor: change vb_pd_change value box from absolute pp to relative %"
```

---

### Task 2: Update Summary Cards — Drop pp Card, Keep Only % Card

**Files:**
- Modify: `app/server.R:2462-2513` (the summary cards fluidRow)

**Step 1: Remove the "PD Change (pp)" card and keep only "PD Change (%)"**

In `app/server.R`, in the `fluidRow(...)` block starting at line 2496, delete the `make_card("PD Change (pp)", ...)` call (lines 2504-2506). The existing "PD Change (%)" card at lines 2507-2510 already uses the relative formula and stays as-is.

The resulting fluidRow should have 5 cards: Companies Analyzed, Total Exposure, Max PD Shock, PD Change (%), Total EL Change.

Also delete the now-unused `pd_change_pp` calculation block (lines 2462-2471) and the `pd_pp_class` variable since nothing references them anymore.

**Step 2: Rebuild and verify**

Run: `docker compose -f docker-compose.dev.yml up --build -d`
Load demo data → run analysis → check that Summary cards show 5 items with "PD Change (%)" displaying a percentage value.

**Step 3: Commit**

```bash
git add app/server.R
git commit -m "refactor: remove absolute PD Change (pp) summary card, keep only relative %"
```

---

### Task 3: Update Bar Chart to Use Relative % with Expanded Legend

**Files:**
- Modify: `app/server.R:2602-2665` (the `plot_pd_sector_enhanced` renderPlotly block)

**Step 1: Change the PD change formula from absolute to relative**

Replace line 2609:
```r
    df <- df %>% mutate(pd_change_pp = (pd_shock - pd_baseline) * 100)
```
With:
```r
    df <- df %>% mutate(
      pd_change_pct = ifelse(pd_baseline != 0,
                             (pd_shock - pd_baseline) / pd_baseline * 100,
                             0)
    )
```

Then replace ALL references to `pd_change_pp` with `pd_change_pct` in this block (sector_stats, year_sector_means, and the ggplot).

**Step 2: Update the axis label and expand the subtitle legend**

Replace the `labs(...)` call with:
```r
      labs(
        title = "PD Change by Sector",
        subtitle = paste0(
          "Bars = mean across shock years | ",
          "Whiskers = 95% CI (\u00b11.96 SE) | ",
          "Ticks (\u2501) = per-year avg"
        ),
        x = "", y = "Avg PD Change (%)"
      )
```

**Step 3: Rebuild and verify the bar chart**

Run: `docker compose -f docker-compose.dev.yml up --build -d`
Load demo → run → go to Exposure PD Analysis tab → verify bar chart shows % values, expanded subtitle is readable.

**Step 4: Commit**

```bash
git add app/server.R
git commit -m "refactor: bar chart uses relative PD change %, add expanded legend subtitle"
```

---

### Task 4: Clean Density Plot — Remove Scatter, Label Avg Lines

**Files:**
- Modify: `app/server.R:2667-2765` (the `plot_pd_distribution` renderPlotly block)

**Step 1: Rewrite the density plot without scatter overlay**

Replace the entire `output$plot_pd_distribution <- renderPlotly({...})` block with:

```r
  # Plot 2: PD Distribution — clean density curve with labeled averages
  output$plot_pd_distribution <- renderPlotly({
    req(rv$results_by_year)
    df <- all_years_data()
    req(nrow(df) > 0, "sector" %in% names(df), "pd_baseline" %in% names(df), "pd_shock" %in% names(df))

    # Compute relative PD change (%)
    df <- df %>% mutate(
      pd_change_pct = ifelse(pd_baseline != 0,
                             (pd_shock - pd_baseline) / pd_baseline * 100,
                             0)
    )

    # Overall average
    overall_mean <- mean(df$pd_change_pct, na.rm = TRUE)

    # Per-year averages
    year_means <- df %>%
      group_by(shock_year) %>%
      summarise(year_mean = mean(pd_change_pct, na.rm = TRUE), .groups = "drop")

    # Get density peak for label positioning
    dens <- density(df$pd_change_pct, na.rm = TRUE, n = 512)
    y_top <- max(dens$y) * 0.95

    # Build clean density plot
    p <- ggplot(df, aes(x = pd_change_pct)) +
      # Density curve — light green
      geom_density(fill = "#C5E1A5", color = TRISK_HEX_GREEN, alpha = 0.3, linewidth = 0.5) +
      # Overall average — dashed black
      geom_vline(xintercept = overall_mean, linetype = "dashed",
                 color = "black", linewidth = 0.7, alpha = 0.5) +
      # Per-year averages — dotted black
      geom_vline(data = year_means, aes(xintercept = year_mean),
                 linetype = "dotted", color = "black", linewidth = 0.5, alpha = 0.4) +
      labs(
        title = "Distribution of PD Changes",
        subtitle = paste0(
          "Dashed = overall avg (", round(overall_mean, 1), "%) | ",
          "Dotted = per-year avg: ",
          paste0(year_means$shock_year, " (", round(year_means$year_mean, 1), "%)",
                 collapse = ", ")
        ),
        x = "PD Change (%)", y = "Density"
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 8, color = "gray50"),
        legend.position = "none"
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })
```

**Step 2: Rebuild and verify the density plot**

Run: `docker compose -f docker-compose.dev.yml up --build -d`
Load demo → run → Exposure PD Analysis tab → verify:
- No scatter bubbles on the density plot
- Light green density curve present
- Dashed overall average line visible
- Dotted per-year lines visible
- Subtitle lists each year with its average value

**Step 3: Commit**

```bash
git add app/server.R
git commit -m "refactor: clean density plot - remove scatter, label avg lines, use relative %"
```

---

### Task 5: Add New PD Change vs Exposure Scatter Plot

**Files:**
- Modify: `app/ui.R:461-465` (add new plotlyOutput row)
- Modify: `app/server.R` (add new renderPlotly block after the density plot, ~line 2765)

**Step 1: Add the UI output for the new scatter plot**

In `app/ui.R`, after the existing `fluidRow` with the two side-by-side plots (after line 464), add a new full-width row:

Replace lines 461-465:
```r
              fluidRow(
                box(width = 6, plotlyOutput("plot_pd_sector_enhanced", height = "420px")),
                box(width = 6, plotlyOutput("plot_pd_distribution", height = "420px"))
              ),
              DTOutput("pd_table")
```
With:
```r
              fluidRow(
                box(width = 6, plotlyOutput("plot_pd_sector_enhanced", height = "420px")),
                box(width = 6, plotlyOutput("plot_pd_distribution", height = "420px"))
              ),
              fluidRow(
                box(width = 12, plotlyOutput("plot_pd_vs_exposure", height = "380px"))
              ),
              DTOutput("pd_table")
```

**Step 2: Add the server-side renderPlotly for the scatter plot**

In `app/server.R`, after the closing `})` of `plot_pd_distribution` (after the current line 2765), add:

```r
  # Plot 3: PD Change (%) vs Exposure — risk concentration scatter
  output$plot_pd_vs_exposure <- renderPlotly({
    req(rv$results_by_year)
    df <- all_years_data()
    req(nrow(df) > 0, "sector" %in% names(df),
        "pd_baseline" %in% names(df), "pd_shock" %in% names(df),
        "exposure_value_usd" %in% names(df))

    # Compute relative PD change (%)
    df <- df %>% mutate(
      pd_change_pct = ifelse(pd_baseline != 0,
                             (pd_shock - pd_baseline) / pd_baseline * 100,
                             0)
    )

    # Ensure company_name column exists for tooltip
    if (!"company_name" %in% names(df)) df$company_name <- NA_character_

    # Sector colours
    sectors_present <- sort(unique(df$sector))
    sector_colors <- SECTOR_COLORS[sectors_present]
    missing <- setdiff(sectors_present, names(SECTOR_COLORS))
    if (length(missing) > 0) sector_colors[missing] <- "gray50"

    p <- ggplot(df, aes(
      x = pd_change_pct,
      y = exposure_value_usd,
      color = sector,
      text = paste0(
        ifelse(!is.na(company_name), company_name, paste0("Company ", company_id)),
        "\nSector: ", sector,
        "\nPD Change: ", round(pd_change_pct, 1), "%",
        "\nExposure: $", format(round(exposure_value_usd), big.mark = ","),
        "\nShock Year: ", shock_year
      )
    )) +
      geom_point(alpha = 0.7, size = 3.5, shape = 16) +
      scale_color_manual(values = sector_colors, name = "Sector") +
      scale_y_continuous(labels = function(x) {
        ifelse(abs(x) >= 1e6, paste0(round(x / 1e6, 1), "M"),
        ifelse(abs(x) >= 1e3, paste0(round(x / 1e3, 0), "K"),
               as.character(round(x))))
      }) +
      labs(
        title = "PD Change vs Exposure",
        subtitle = "Top-right = high exposure + large PD increase (highest risk)",
        x = "PD Change (%)", y = "Exposure (USD)"
      ) +
      trisk_plot_theme() +
      theme(
        plot.subtitle = element_text(size = 9, color = "gray50"),
        legend.position = "bottom",
        legend.text = element_text(size = 9)
      )

    tc <- plotly_theme_colors(input)
    ggplotly(p, tooltip = "text") %>%
      layout(
        font = list(family = "Inter, sans-serif", size = 12, color = tc$font_color),
        paper_bgcolor = tc$paper_bgcolor,
        plot_bgcolor = tc$plot_bgcolor,
        hoverlabel = PLOTLY_HOVERLABEL,
        legend = list(orientation = "h", y = -0.15, x = 0.5, xanchor = "center")
      ) %>%
      config(displayModeBar = PLOTLY_CONFIG$displayModeBar,
             modeBarButtonsToRemove = PLOTLY_CONFIG$modeBarButtonsToRemove,
             displaylogo = PLOTLY_CONFIG$displaylogo)
  })
```

**Step 3: Rebuild and verify all three plots**

Run: `docker compose -f docker-compose.dev.yml up --build -d`
Load demo → run → Exposure PD Analysis tab → verify:
- Bar chart (top-left) with relative % values
- Density plot (top-right) clean, no scatter
- New scatter plot (full-width below) with sector-colored dots, PD change % on x-axis, exposure on y-axis

**Step 4: Commit**

```bash
git add app/server.R app/ui.R
git commit -m "feat: add PD Change vs Exposure scatter plot, full-width below existing plots"
```

---

### Task 6: Final Verification and Cleanup

**Step 1: Full end-to-end test**

Rebuild: `docker compose -f docker-compose.dev.yml up --build -d`
1. Load demo data
2. Visit Configure tab (to initialize dropdowns)
3. Run analysis with 3 shock years (2025, 2030, 2035)
4. Navigate to Portfolio Results → Summary tab → verify 5 summary cards (no pp card)
5. Navigate to Exposure PD Analysis → verify:
   - Bar chart: % axis, expanded subtitle legend
   - Density: clean, labeled average lines, subtitle lists per-year values
   - Scatter: sector-colored dots, % x-axis, exposure y-axis
6. Check Docker logs for errors: `docker logs trisk-app-dev 2>&1 | tail -20`

**Step 2: Commit all remaining changes**

```bash
git add -A
git commit -m "chore: PD analysis overhaul - final cleanup"
```
