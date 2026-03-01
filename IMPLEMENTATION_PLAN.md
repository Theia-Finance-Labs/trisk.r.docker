# Implementation Plan: Website-to-App Visual Alignment

**For:** Senior R Shiny / Docker Developer
**Date:** 2026-03-01
**Decisions locked in:** Single red (#F53D3F), website green (#6AAF95), cool borders (#C8CEDA), full server.R cleanup
**Companion doc:** `DESIGN_ADJUSTMENT_PLAN.md` (color reference tables)

---

## Overview

7 sequential steps. Each step is self-contained and produces a testable checkpoint.
Total scope: ~6-8 hours. Steps 1-4 are the critical path (~4 hours). Steps 5-7 are polish.

---

## STEP 1: Unify the Color Token Foundation

**Files:** `www/trisk.css` (`:root` block), `global.R` (color constants)
**Risk:** Low — token-only changes, no layout impact
**Time:** ~45 min

### 1.1 — CSS Design Tokens (`www/trisk.css` `:root` block)

Replace every value listed below. No other lines in `:root` change.

```
FIND                              → REPLACE WITH
──────────────────────────────────────────────────────
--brand-coral: #E84B4D            → --brand-coral: #F53D3F
--brand-coral-lt: #EF7173         → --brand-coral-lt: #EF6C82
--brand-coral-dk: #C0393B         → --brand-coral-dk: #D93235
--brand-red: #F53D3F              → (no change, already correct)

--bg-page: #F5F0F2                → --bg-page: #F6EBEF
--bg-card: #FFFFFF                → --bg-card: #F5F5F5
--bg-header: #1A1A1A              → --bg-header: #141D2E

--border-light: #E5E0E2           → --border-light: #C8CEDA
--border-medium: #D0C8CC          → --border-medium: #B0B8C8

--status-success: #3D8B5E         → --status-success: #6AAF95
--status-warning: #C68A1A         → --status-warning: #FF4D00
--status-danger: #C44245          → --status-danger: #F53D3F

--vb-success: #276749             → --vb-success: #3B9B78
--vb-warning: #975A16             → --vb-warning: #CC3E00
--vb-danger: #9B2C2C              → --vb-danger: #C62828

--shadow-sm: ... rgba(0,0,0,0.06) → --shadow-sm: 0 1px 2px rgba(0,0,0,0.04)
--shadow-md: ... rgba(0,0,0,0.08) → --shadow-md: 0 2px 6px rgba(0,0,0,0.05)
--shadow-lg: ... rgba(0,0,0,0.10) → --shadow-lg: 0 4px 12px rgba(0,0,0,0.06)

--radius-sm: 6px                  → --radius-sm: 2px
--radius-md: 8px                  → --radius-md: 2px
--radius-lg: 12px                 → --radius-lg: 3px
```

### 1.2 — R Color Constants (`global.R`)

Replace the color constant block. Keep the variable names identical so no downstream R code breaks.

```r
# ============================================
# 1in1000 Color Palette (aligned with 1in1000.com website 2026-03)
# ============================================

# Primary brand — unified to single red
BRAND_RED       <- "#F53D3F"   # Primary brand red (hsl 359 90% 60%)
BRAND_CORAL     <- "#F53D3F"   # Unified — was #E84B4D, now matches BRAND_RED
BRAND_CORAL_LT  <- "#EF6C82"   # Salmon accent (hsl 350 80% 68%)
BRAND_CORAL_DK  <- "#D93235"   # Dark coral for hover states

# Background / UI
BG_PINK         <- "#F6EBEF"   # Warm pink page background (hsl 334 38% 93%)
BG_CARD         <- "#F5F5F5"   # Off-white card surface (hsl 0 0% 96%)
BG_SIDEBAR      <- "#FAFAFA"   # Neutral near-white sidebar
FG_TEXT          <- "#1A1A1A"   # Near-black text (unchanged)
FG_MUTED         <- "#555555"  # Medium gray text (unchanged)
BORDER_PINK      <- "#C8CEDA"  # Cool gray border (hsl 220 20% 82%)
SECONDARY_PINK   <- "#F0EAED"  # Sidebar active highlight (unchanged)

# Status / data colors
STATUS_GREEN     <- "#6AAF95"  # Sage green from website (hsl 158 30% 55%)
STATUS_RED       <- "#F53D3F"  # Unified brand red
STATUS_BLUE      <- "#4A7FA5"  # Muted blue (unchanged)

# Legacy TRISK colors (updated for consistency)
TRISK_HEX_RED   <- "#F53D3F"
TRISK_HEX_GREEN <- "#6AAF95"  # Was #5D9324 — aligned to website sage
TRISK_HEX_GREY  <- "#BAB6B5"
```

### 1.3 — Verification Checkpoint

After this step:
- `docker compose build && docker compose up` (or `R -e "shiny::runApp('app')"` locally)
- All buttons, box headers, and sidebar accents should reflect the new coral `#F53D3F`
- Page background should be slightly more pink
- Card backgrounds should be slightly off-white (subtle)
- Borders should appear cooler/more gray

**Known issue at this point:** `server.R` inline colors will still show old values in rendered HTML. That's expected — we fix those in Steps 5-6.

---

## STEP 2: Button Styling Overhaul

**File:** `www/trisk.css` (button rule blocks)
**Risk:** Low — CSS-only
**Time:** ~30 min

### 2.1 — Primary / TRISK Buttons (`.btn-primary`, `.btn-trisk`)

Locate the existing `.btn-primary` rules and replace:

```css
.btn-primary,
.btn-trisk {
  background-color: var(--brand-coral);          /* #F53D3F */
  border: 1px solid var(--brand-coral-dk);       /* #D93235 */
  color: var(--fg-on-dark);
  font-family: var(--font-heading);
  font-weight: var(--fw-medium);
  border-radius: var(--radius-md);               /* now 2px */
  letter-spacing: 0.01em;
  transition: all var(--transition-fast);
}

.btn-primary:hover, .btn-primary:focus,
.btn-trisk:hover, .btn-trisk:focus {
  background-color: var(--brand-coral-dk);       /* #D93235 */
  border-color: #B82A2C;
  color: var(--fg-on-dark);
  box-shadow: var(--shadow-sm);
}

.btn-primary:active,
.btn-trisk:active {
  background-color: #B82A2C;
  transform: scale(0.98);                        /* press effect from website */
}
```

### 2.2 — Success / Green Buttons (`.btn-success`)

```css
.btn-success {
  background-color: var(--status-success);       /* #6AAF95 */
  border: 1px solid rgba(59, 155, 120, 0.3);    /* translucent border from website */
  color: var(--fg-on-dark);
  font-family: var(--font-heading);
  font-weight: var(--fw-medium);
  border-radius: var(--radius-md);
  transition: all var(--transition-fast);
}

.btn-success:hover, .btn-success:focus {
  background-color: #3B9B78;                     /* --accent-contrast from website */
  border-color: rgba(59, 155, 120, 0.5);
  color: var(--fg-on-dark);
  box-shadow: var(--shadow-sm);
}

.btn-success:active {
  background-color: #2BAB76;
  transform: scale(0.98);
}
```

### 2.3 — Secondary / Info Buttons (`.btn-info`)

```css
.btn-info {
  background-color: var(--bg-card);              /* #F5F5F5 */
  border: 1px solid var(--border-medium);        /* #B0B8C8 */
  color: var(--fg-primary);
  font-family: var(--font-heading);
  font-weight: var(--fw-medium);
  border-radius: var(--radius-md);
  transition: all var(--transition-fast);
}

.btn-info:hover, .btn-info:focus {
  background-color: var(--bg-page);              /* #F6EBEF */
  border-color: var(--fg-muted);                 /* #888888 */
  color: var(--fg-primary);
  box-shadow: var(--shadow-sm);
}
```

### 2.4 — Default Buttons (`.btn-default`)

```css
.btn-default {
  font-family: var(--font-heading);
  font-weight: var(--fw-medium);
  border-radius: var(--radius-md);               /* 2px */
  transition: all var(--transition-fast);
}
```

### 2.5 — Verification Checkpoint

- Every button on every tab should now render with the new colors and near-sharp corners
- Green "Run Analysis" button = sage `#6AAF95`
- Red primary buttons = bright coral `#F53D3F`
- Hover states should darken smoothly with the press effect

---

## STEP 3: Box / Card / Panel Refinement

**File:** `www/trisk.css` (box rules)
**Risk:** Medium — visual change across every tab
**Time:** ~45 min

### 3.1 — Standard Box

Locate `.box` rules and update:

```css
.box {
  border-top: 1px solid var(--border-light);     /* was 3px — remove the thick accent strip */
  border-right: 1px solid var(--border-light);
  border-bottom: 1px solid var(--border-light);
  border-left: 1px solid var(--border-light);
  background: var(--bg-card);                    /* #F5F5F5 */
  border-radius: var(--radius-lg);               /* 3px */
  box-shadow: var(--shadow-sm);
}
```

### 3.2 — Box Variants (Danger, Success, Primary)

```css
/* Danger box — coral left accent instead of top */
.box.box-danger {
  border-left: 3px solid var(--brand-coral);     /* #F53D3F left accent */
  border-top: 1px solid var(--border-light);
}

.box.box-danger > .box-header.with-border {
  border-bottom-color: var(--border-light);
}

/* Solid danger header — update colors */
.box.box-danger > .box-header.with-border[style*="background"] {
  background: var(--brand-coral) !important;     /* ensure override */
}

/* Success box — keep dark editorial style */
.box.box-success {
  border-color: var(--fg-primary);
}

/* Primary / Info boxes — neutral */
.box.box-primary,
.box.box-info {
  border-top-color: var(--border-medium);        /* #B0B8C8 */
}
```

### 3.3 — Value Boxes / Small Boxes

The value boxes already reference `--vb-*` tokens which we updated in Step 1. Verify the border-radius applies:

```css
.small-box {
  border-radius: var(--radius-md);               /* 2px — was 8px */
}
```

### 3.4 — Tab Navigation

```css
/* Active tab uses new coral */
.nav-tabs-custom > .nav-tabs > li.active > a {
  border-bottom: 2px solid var(--brand-coral);   /* #F53D3F */
}

/* Hover tab */
.nav-tabs-custom > .nav-tabs > li > a:hover {
  border-bottom: 2px solid var(--brand-coral-lt); /* #EF6C82 */
}
```

### 3.5 — Sidebar Active Indicator

```css
.sidebar-menu > li.active > a {
  border-left: 3px solid var(--brand-coral);     /* #F53D3F — picks up new unified red */
}
```

### 3.6 — Guidance Boxes

```css
.guidance-toggle-btn {
  border-left: 4px solid var(--brand-coral);     /* picks up #F53D3F */
  border-radius: 0 var(--radius-md) var(--radius-md) 0;  /* 0 2px 2px 0 */
}

.guidance-content {
  border-left: 4px solid var(--brand-coral);
  border-radius: 0 0 var(--radius-md) 0;        /* 0 0 2px 0 */
}
```

### 3.7 — Verification Checkpoint

- Boxes should have subtle, uniform borders with no thick colored top strip
- Danger boxes should have a left coral accent line
- All corners should be near-sharp (3px boxes, 2px buttons)
- Tabs should highlight with the new `#F53D3F` coral

---

## STEP 4: Typography — Add Monospace Voice

**Files:** `ui.R` (font import), `www/trisk.css` (new token + selective rules)
**Risk:** Low — additive, nothing existing changes
**Time:** ~45 min

### 4.1 — Add JetBrains Mono to Google Fonts Import (`ui.R`)

Find the existing `tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/...")` and replace:

```r
tags$link(rel = "stylesheet",
  href = "https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&family=Space+Grotesk:wght@300;400;500;600;700&family=Inter:wght@300;400;500;600&display=swap"),
```

### 4.2 — Add Monospace Token to CSS (`:root` block)

Add one new line to the `:root` block after `--font-heading`:

```css
--font-mono: 'JetBrains Mono', 'Courier New', monospace;
```

### 4.3 — Apply Monospace to Specific UI Elements

Add new CSS rules (append to existing sections, don't replace):

```css
/* ---- Monospace UI voice (aligned with 1in1000.com nav typography) ---- */

/* Tab labels */
.nav-tabs-custom > .nav-tabs > li > a {
  font-family: var(--font-mono);
  font-size: var(--fs-xs);                       /* 11px */
  text-transform: uppercase;
  letter-spacing: 1.5px;
}

/* Status badges / pills */
.status-pill,
.label {
  font-family: var(--font-mono);
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
  border-radius: 0px;                            /* square badges, per website */
}

/* Sidebar menu items — subtle mono touch */
.sidebar-menu > li > a {
  font-family: var(--font-mono);
  font-size: 12px;
  letter-spacing: 0.5px;
}

/* Definition table first column (metric names) */
.def-table td:first-child {
  font-family: var(--font-mono);
  color: var(--brand-coral-dk);                  /* #D93235 */
}

/* Export button text */
.btn-export {
  font-family: var(--font-mono);
  font-size: var(--fs-sm);
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
```

### 4.4 — Verification Checkpoint

- Tab labels should render in JetBrains Mono, uppercase, with letter-spacing
- Sidebar menu items should be monospace
- Status badges should be square (0px radius) and monospace
- Main body text (paragraphs, descriptions) should still be Inter
- Headings should still be Space Grotesk
- No text should appear broken or clipped

---

## STEP 5: Server.R Inline Color Cleanup — Brand & Status Colors

**File:** `server.R`
**Risk:** Medium — touches rendered HTML output
**Time:** ~1.5 hours
**Strategy:** Replace hardcoded hex values with R constants. Do NOT touch chart gradient palettes yet (Step 6).

### 5.1 — Add New R Constants for Inline Use (`global.R`)

Add these derived constants below the existing block to cover server.R needs:

```r
# Derived UI colors for server.R inline styles (avoid hardcoding hex)
FG_SECONDARY     <- "#666666"  # Neutral gray (replacing inconsistent #666)
FG_TERTIARY      <- "#999999"  # Light gray (replacing #999)
BG_HIGHLIGHT     <- "#F0E6EA"  # Section highlight background
BG_COMPARE       <- "#F8F4F6"  # Comparison panel background
BG_COMPARE_ALT   <- "#FBF5F2"  # Light warm comparison row
BG_ALERT_WARM    <- "#FFFDE7"  # Warm yellow alert background
BORDER_HIGHLIGHT <- "#DDD0D4"  # Highlight border
COLOR_NEUTRAL    <- "#5A8EAE"  # Neutral blue for 'different' indicators
COLOR_SUCCESS_INLINE <- "#6AAF95"  # Inline success (aligned to STATUS_GREEN)
COLOR_DANGER_INLINE  <- "#F53D3F"  # Inline danger (aligned to unified red)
WARNING_AMBER    <- "#FF4D00"  # Warning indicator (aligned to website orange)
```

### 5.2 — Search-and-Replace Map for `server.R`

Execute these replacements. The left side is what to search for in `style = "..."` strings. The right side is the R constant to use via `paste0()` or `sprintf()`.

```
HARDCODED HEX         → R CONSTANT           LOCATIONS (approx)
──────────────────────────────────────────────────────────────
"#C44245"             → STATUS_RED            Lines 302, 314, 1226, 4901
"#6B9F3B"             → STATUS_GREEN          Lines 739, 1226, 4903
"#666"                → FG_SECONDARY          Lines 1069-1088, 1114, 1226,
                                              1271, 1311, 1766, 1822,
                                              1834, 2832-2876, 4905 (~25 hits)
"#999"                → FG_TERTIARY           Lines 1772, 2189, 2793,
                                              3398, 4993-5050 (~8 hits)
"#F0E6EA"             → BG_HIGHLIGHT          Lines 1280, 1781, 1786,
                                              1822, 1834, 4935 (~6 hits)
"#DDD0D4"             → BORDER_HIGHLIGHT      Lines 1062, 1064, 1260,
                                              4935 (~4 hits)
"#F8F4F6"             → BG_COMPARE            Line 1260
"#FBF5F2"             → BG_COMPARE_ALT        Lines 1064
"#FFFDE7"             → BG_ALERT_WARM         Line 1062
"#F53D3F"             → BRAND_RED             Line 1062, plotly refs
"#5A8EAE"             → COLOR_NEUTRAL         Lines 1312, 3682
"#D4A017"             → WARNING_AMBER         Lines 3409-3412 (HHI/GINI/CR5)
"#EF7173"             → BRAND_CORAL_LT        Plotly colorscale refs
"#1A1A1A"             → FG_TEXT               Lines 4925
"#FAFAFA"             → BG_SIDEBAR            Line 4099
"#FEFEFE"             → BG_CARD               Lines 4993-5050 (plot bg)
```

### 5.3 — Pattern for Inline Style Replacement

Before (hardcoded):
```r
tags$span("Error", style = "color: #C44245;")
```

After (tokenized):
```r
tags$span("Error", style = paste0("color: ", STATUS_RED, ";"))
```

Before (conditional):
```r
color <- if (abs(d) < 0.001) "#666" else if (is_worse) "#C44245" else "#6B9F3B"
```

After:
```r
color <- if (abs(d) < 0.001) FG_SECONDARY else if (is_worse) STATUS_RED else STATUS_GREEN
```

### 5.4 — Warning Color Update in Threshold Logic

Lines ~3409-3412 use `"#D4A017"` for amber warnings in financial metrics:

```r
# Before
hhi_color <- if (cd$hhi < 1500) STATUS_GREEN else if (cd$hhi < 2500) "#D4A017" else BRAND_CORAL

# After
hhi_color <- if (cd$hhi < 1500) STATUS_GREEN else if (cd$hhi < 2500) WARNING_AMBER else BRAND_CORAL
```

Apply the same pattern to `gini_color` and `cr5_color`.

### 5.5 — Verification Checkpoint

- Run the app, navigate through all tabs
- Error messages should be coral `#F53D3F` (not the old muted `#C44245`)
- Success indicators should be sage green `#6AAF95`
- Run comparison panels should still render correctly
- Financial metric thresholds (HHI, GINI, CR5) should show orange warnings
- Check the browser DevTools console for any R rendering errors

---

## STEP 6: Server.R Chart Palette Alignment

**File:** `server.R` (plotly colorscales and chart palettes)
**Risk:** Medium-High — changes data visualization appearance
**Time:** ~1 hour
**Strategy:** Update palettes to use the new green/red anchors while preserving gradient logic.

### 6.1 — PD Change Gradient (Lines ~1547-1589)

The current green-to-red gradient uses forest greens and deep reds. Reanchor to the new palette:

```r
# Before (forest green anchors)
pd_colors <- c("#2D6A2E", "#3D8B3E", "#5DAA5E", "#7DC07E", "#A0D8A0",
               "#7AADCA",
               "#E0A0A0", "#D48080", "#C45555", "#A83535", "#8B2020")

# After (sage green anchors, coral red anchors)
pd_colors <- c("#2B7A5E", "#3B9B78", "#6AAF95", "#8CC4AD", "#B0D9C8",
               "#7AADCA",
               "#F0A0A0", "#E88080", "#F53D3F", "#D93235", "#B82A2C")
```

### 6.2 — EL Change Gradient (reverse of PD)

```r
# After (red-to-green, matching new palette)
el_colors <- c("#B82A2C", "#D93235", "#F53D3F", "#E88080", "#7AADCA",
               "#8CC4AD", "#6AAF95", "#3B9B78", "#2B7A5E")
```

### 6.3 — Risk Palette (Lines ~3737-3740)

```r
# Before
palette <- c(BRAND_CORAL, BRAND_CORAL_LT, STATUS_BLUE, "#5A8EAE", "#7BADC4",
             STATUS_GREEN, "#8BB85F", "#A8C97B", "#D4A017", "#E8C547")

# After (uses R constants + website-aligned intermediates)
palette <- c(BRAND_CORAL, BRAND_CORAL_LT, STATUS_BLUE, COLOR_NEUTRAL, "#7BADC4",
             STATUS_GREEN, "#8BB85F", "#A8C97B", WARNING_AMBER, "#E8C547")
```

### 6.4 — PD/EL Detail Palettes (Lines ~4443, 4764)

```r
# Before
clrs_pd <- c("#1B4332", "#2D6A2E", "#5DAA5E", "#7AADCA", "#C45555", "#8B2020", "#5C0A0A")

# After
clrs_pd <- c("#1B4D3A", "#2B7A5E", "#6AAF95", "#7AADCA", "#F53D3F", "#D93235", "#B82A2C")
```

Apply the same to `clrs_adj`.

### 6.5 — Heatmap Colorscales (Lines ~3675-3682)

```r
# Before
list(0, STATUS_GREEN), list(0.5, "#FFFFFF"), list(1, BRAND_CORAL)

# After — no code change needed, the R constants now hold the new values
# STATUS_GREEN is now #6AAF95, BRAND_CORAL is now #F53D3F
# The heatmap will automatically use the new colors
```

### 6.6 — Plot Layout Colors (Lines ~4993-5050)

```r
# Before
plot_bgcolor = "#FEFEFE"
paper_bgcolor = "#FEFEFE"
zerolinecolor = "#999"

# After — align to card background
plot_bgcolor = BG_CARD       # now #F5F5F5
paper_bgcolor = BG_CARD      # now #F5F5F5
zerolinecolor = FG_TERTIARY  # #999999
```

### 6.7 — Verification Checkpoint

- Open the Results tab with actual analysis data loaded
- PD/EL heatmaps should show sage-green-to-coral gradients (no more forest green)
- Plotly chart backgrounds should be slightly off-white (matches card surfaces)
- Concentration metrics (HHI, GINI) should use orange for warnings
- Compare: the overall chart palette should feel harmonious with the new button/box colors

---

## STEP 7: Header & Sidebar Final Polish

**Files:** `www/trisk.css`, `ui.R`
**Risk:** Low
**Time:** ~30 min

### 7.1 — Dark Header Navy Tint

Already handled by `--bg-header: #141D2E` in Step 1. Verify the header renders correctly:

```css
/* Should already be picking up the token — verify these rules exist */
.skin-red .main-header .logo {
  background: var(--bg-header);                  /* #141D2E */
}

.skin-red .main-header .navbar {
  background: var(--bg-header);
}
```

### 7.2 — Sidebar Menu Monospace (Already Done in Step 4)

Verify sidebar items render in JetBrains Mono. If the text clips due to monospace being wider than Inter, adjust the sidebar width:

```r
# In ui.R — may need to increase from 280 to 300 if text clips
dashboardSidebar(width = 300, ...)
```

And update the corresponding CSS:

```css
.skin-red .main-header .logo {
  width: 300px;                                  /* was 320px — adjust if sidebar changes */
}

.skin-red .main-header .navbar {
  margin-left: 300px;
}
```

### 7.3 — Export Button on Dark Header

```css
.btn-export {
  background: rgba(255, 255, 255, 0.15);
  border: 1px solid rgba(255, 255, 255, 0.3);
  color: var(--fg-on-dark);
  border-radius: var(--radius-sm);               /* 2px */
  font-family: var(--font-mono);
  font-size: var(--fs-sm);
  letter-spacing: 0.5px;
  text-transform: uppercase;
  padding: var(--sp-2) var(--sp-4);
  transition: all var(--transition-fast);
}

.btn-export:hover {
  background: rgba(255, 255, 255, 0.25);
}
```

### 7.4 — Final Verification Checkpoint

Full walkthrough of all tabs:

| Tab | What to check |
|---|---|
| Welcome | Box styling, heading fonts, button color |
| Upload | File input borders, status messages, validation colors |
| Configure | Dropdowns, slider styling, guidance boxes |
| Run | Green run button (#6AAF95), progress indicators |
| Results | Charts, heatmaps, value boxes, comparison panels |
| Integration | Metric cards, threshold colors |
| Download | Export buttons |
| Docs | Definition table mono styling |
| About | General layout |

---

## Docker-Specific Considerations

### Font Loading in Containerized Environment

The Google Fonts import (`fonts.googleapis.com`) requires outbound internet from the container. If deploying behind a corporate firewall:

**Option A (recommended):** Keep the CDN import — works for any internet-connected deployment.

**Option B (airgapped):** Download the font files and serve them locally:

```bash
# In Dockerfile, add:
RUN mkdir -p /srv/shiny-server/www/fonts
# Copy pre-downloaded .woff2 files for JetBrains Mono, Space Grotesk, Inter
COPY fonts/ /srv/shiny-server/www/fonts/
```

Then replace the Google Fonts `tags$link()` with a local `@font-face` declaration in `trisk.css`.

### CSS Cache Busting

After updating `trisk.css`, browsers may cache the old version. Add a cache-buster query param:

```r
# In ui.R
tags$link(rel = "stylesheet", href = "trisk.css?v=2026-03-01"),
```

### Docker Rebuild

After all changes:

```bash
docker compose build --no-cache
docker compose up -d
```

The `--no-cache` ensures the CSS and R files are freshly copied into the image.

---

## File Change Summary

| File | Lines Changed (est.) | Type of Change |
|---|---|---|
| `www/trisk.css` | ~80-100 lines | Token values, button rules, box rules, new mono rules |
| `global.R` | ~30 lines | Color constant values + new derived constants |
| `ui.R` | ~2 lines | Google Fonts URL + cache-buster |
| `server.R` | ~80-100 lines | Hex-to-constant replacements + palette updates |

**Total: ~200-230 lines across 4 files.**

---

## Rollback Strategy

If the changes don't land well visually:

1. **Git revert** — all changes are in 4 files, easy to revert as a single commit
2. **Token-only rollback** — if only some colors feel wrong, revert individual `:root` values. The token architecture means a single line change propagates everywhere
3. **Selective rollback** — border radius and typography (Steps 3-4) are independent of color changes (Steps 1-2). They can be reverted separately without affecting the color alignment

---

## QA Checklist (Post-Implementation)

- [ ] All buttons render coral `#F53D3F` or sage `#6AAF95` (no old `#E84B4D` or `#3D8B5E`)
- [ ] No broken badge text (check monospace doesn't clip sidebar items)
- [ ] Tab underlines are coral, not old coral
- [ ] Box corners are 3px (not 12px)
- [ ] Card backgrounds are `#F5F5F5` (not pure white)
- [ ] Browser DevTools: no 404s for JetBrains Mono font
- [ ] Plotly charts load without JS console errors
- [ ] Heatmap gradients show sage-green-to-coral (not forest-green-to-muted-red)
- [ ] HHI/GINI/CR5 warnings render in orange `#FF4D00`
- [ ] Export buttons on dark header are visible and hoverable
- [ ] Guidance boxes have coral left accent
- [ ] `server.R` has zero remaining hardcoded `#C44245`, `#6B9F3B`, `#D4A017`
- [ ] Docker image builds cleanly with `--no-cache`
