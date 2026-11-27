# PD Analysis Overhaul — Design Document

**Date**: 2026-03-14
**Status**: Approved

## Overview

Redesign the Exposure PD Analysis tab to use relative percentage PD change, add descriptive legends, remove scatter from the density plot, and introduce a new PD Change vs Exposure scatter plot.

## 1. PD Change Formula Change

**From**: `(pd_shock - pd_baseline) * 100` (absolute, percentage points)
**To**: `(pd_shock - pd_baseline) / pd_baseline * 100` (relative, percentage change)

### Scope

- `vb_pd_change` value box — display "XX.X%" instead of "X.XX pp"
- Summary card row — merge "PD Change (pp)" into existing "PD Change (%)", drop the pp card
- `plot_pd_sector_enhanced` — x-axis label and data in relative %
- `plot_pd_distribution` — x-axis label and data in relative %
- New `plot_pd_vs_exposure` — built with relative % from the start

### Edge Cases

- `pd_baseline = 0` → return 0 (no meaningful relative change)
- Sign preserved: `pd_shock > pd_baseline` → positive (deterioration); `pd_shock < pd_baseline` → negative (improvement)

## 2. Plot Legends

### Bar chart (`plot_pd_sector_enhanced`)

Expanded subtitle:
- Gray whiskers = 95% confidence interval (±1.96 SE across companies × shock years)
- Black tick marks (━) = average PD change per individual shock year

### Density plot (`plot_pd_distribution`)

Label the vertical lines directly:
- Dashed black line: overall portfolio average, labeled with value
- Dotted black lines: per-shock-year averages, labeled with year identifier

## 3. Clean Density Plot

Remove all `geom_point` bubble overlay, bubble sizing, jitter, and density_y interpolation code. Retain:
- `geom_density()` with light green fill and TRISK_HEX_GREEN line
- `geom_vline()` for overall average (dashed, labeled)
- `geom_vline()` for per-year averages (dotted, labeled)
- Remove sector color legend (no scatter points to color)

## 4. New Scatter — PD Change vs Exposure

New `plot_pd_vs_exposure` in a full-width row below the two side-by-side plots:
- X-axis: PD Change (%) — relative percentage change
- Y-axis: Exposure (USD) — formatted with M/K suffixes
- Points: one per company×year, colored by sector (SECTOR_COLORS)
- Tooltips: company name, sector, PD change %, exposure, shock year
- Top-right corner = highest risk concentration

## 5. Layout

```
┌──────────────────────┬──────────────────────┐
│  PD Change by Sector │  Distribution of     │
│  (bar + CI + legend) │  PD Changes (density)│
└──────────────────────┴──────────────────────┘
┌─────────────────────────────────────────────┐
│  PD Change (%) vs Exposure ($)              │
│  (scatter, sector-colored, full width)      │
└─────────────────────────────────────────────┘
┌─────────────────────────────────────────────┐
│  Data Table                                 │
└─────────────────────────────────────────────┘
```

## Files to Modify

| File | Changes |
|------|---------|
| `app/server.R` | PD formula in value box, enhanced bar chart, density plot; new scatter plot |
| `app/ui.R` | Add `plotlyOutput("plot_pd_vs_exposure")` in full-width row; update summary card |
| `app/global.R` | Optional: add helper function for relative PD change |
