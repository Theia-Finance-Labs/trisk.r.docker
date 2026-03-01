# Design Adjustment Plan: Aligning Docker App with 1in1000.com

**Prepared for:** Senior R Shiny / Docker Developer
**Date:** 2026-03-01
**Scope:** Visual consistency between `www.1in1000.com` and the TRISK R Shiny Docker app

---

## Executive Summary

The Docker app already has a solid design token system in `trisk.css`, but diverges from the website in four critical areas: **button colors** (green/red shades), **border radius** (rounded vs. sharp), **typography** (missing the monospace UI voice), and **card surface treatment**. This plan provides exact token-level changes a developer can implement without restructuring the app's layout or component logic.

---

## 1. BUTTON COLORS — Critical Priority

This is the single most visible inconsistency. The website's green and red are distinctly different hues from the app's current palette.

### 1A. Green Buttons (CTA / Success)

| Property | Docker App (Current) | Website (Target) | Notes |
|---|---|---|---|
| Primary green | `#3D8B5E` (forest) | **`#6AAF95`** (sage) | Website `--accent: hsl(158, 30%, 55%)` |
| Green hover | `#2F7A4E` | **`#3B9B78`** | Website `--accent-contrast: hsl(158, 45%, 42%)` |
| Green border | `#2F7A4E` | **`rgba(59, 155, 120, 0.3)`** | Translucent border, not solid |
| Green dark | `#266B42` | **`#2BAB76`** | Website `--lime: hsl(158, 60%, 42%)` |

**CSS changes in `trisk.css`:**

```css
/* Replace these tokens */
--status-success:   #6AAF95;   /* was #3D8B5E */
--vb-success:       #3B9B78;   /* was #276749 */

/* .btn-success overrides */
.btn-success {
  background-color: #6AAF95;
  border-color: rgba(59, 155, 120, 0.3);
}
.btn-success:hover {
  background-color: #3B9B78;
  border-color: rgba(59, 155, 120, 0.5);
}
```

**R constants in `global.R`:**

```r
STATUS_GREEN    <- "#6AAF95"   # was "#3D8B5E"
TRISK_HEX_GREEN <- "#6AAF95"  # was "#5D9324"
```

### 1B. Red / Coral Buttons (Primary CTA / Danger)

| Property | Docker App (Current) | Website (Target) | Notes |
|---|---|---|---|
| Primary red | `#E84B4D` | **`#F53D3F`** | Website `--primary: hsl(359, 90%, 60%)` — brighter, punchier |
| Red hover | `#C0393B` | **`#D93235`** | Darken the new primary by ~12% |
| Red light | `#EF7173` | **`#EF6C82`** | Website `--salmon: hsl(350, 80%, 68%)` — more pink shift |
| Red border accent | `#E84B4D` | **`#F53D3F`** | Align to new primary |

**CSS changes in `trisk.css`:**

```css
--brand-coral:      #F53D3F;   /* was #E84B4D */
--brand-coral-lt:   #EF6C82;   /* was #EF7173 — shifts slightly pink per website */
--brand-coral-dk:   #D93235;   /* was #C0393B */
--brand-red:        #F53D3F;   /* already correct, confirm consistency */
--border-accent:    #F53D3F;   /* was #E84B4D */
--status-danger:    #F53D3F;   /* was #C44245 — aligns to single brand red */

/* .btn-primary overrides */
.btn-primary, .btn-trisk {
  background-color: #F53D3F;
  border-color: #D93235;
}
.btn-primary:hover, .btn-trisk:hover {
  background-color: #D93235;
  border-color: #B82A2C;
}
```

**R constants in `global.R`:**

```r
BRAND_CORAL     <- "#F53D3F"   # was "#E84B4D"
BRAND_CORAL_LT  <- "#EF6C82"  # was "#EF7173"
BRAND_CORAL_DK  <- "#D93235"  # was "#C0393B"
BRAND_RED       <- "#F53D3F"   # consistent
STATUS_RED      <- "#F53D3F"   # was "#C44245"
TRISK_HEX_RED   <- "#F53D3F"  # already correct
```

---

## 2. BORDER RADIUS — High Priority

The website uses **sharp corners (`0px`)** across all interactive elements — a distinctive editorial/brutalist choice. The Docker app currently uses `8px` and `12px` radii, creating a softer, more generic look.

### Recommendation: Partial Alignment

Going fully to `0px` on a data-heavy Shiny dashboard may feel harsh. Recommended compromise:

| Element | Docker App (Current) | Target | Rationale |
|---|---|---|---|
| Buttons | `8px` | **`2px`** | Near-sharp, matches website spirit |
| Cards / Boxes | `12px` | **`3px`** | Subtle rounding, editorial feel |
| Inputs / Selects | `8px` | **`2px`** | Consistent with buttons |
| Badges / Pills | `12px` (fully round) | **`0px`** | Website badges are fully square |
| Tabs | — | **`0px`** | Sharp tab underlines match website |

**CSS changes:**

```css
--radius-sm:  2px;    /* was 6px */
--radius-md:  2px;    /* was 8px */
--radius-lg:  3px;    /* was 12px */
```

> **Developer note:** If stakeholder feedback prefers the current rounded look, this entire section can be skipped without breaking other changes. It is an aesthetic alignment, not a functional one.

---

## 3. TYPOGRAPHY — Medium Priority

### 3A. Introduce Monospace Voice for UI Labels

The website uses **JetBrains Mono** for navigation and UI chrome (bracket notation like `[ ABOUT ]`). The Docker app uses Space Grotesk for everything.

**Recommendation:** Add `JetBrains Mono` as a tertiary font for specific UI elements only — don't replace the existing fonts.

**Where to apply monospace:**

| Element | Current Font | Target |
|---|---|---|
| Tab labels | Space Grotesk | **JetBrains Mono, 11px, uppercase, 1.5px letter-spacing** |
| Status badges (LIVE, BETA equivalents) | Inter | **JetBrains Mono, 10px, uppercase** |
| Section header subtitles | Space Grotesk | **JetBrains Mono, 11px** |
| Footer / metadata text | Inter | **JetBrains Mono, 11px** |

**CSS additions:**

```css
/* Add to Google Fonts import */
@import url('https://fonts.googleapis.com/css2?family=JetBrains+Mono:wght@400;500&display=swap');

/* New utility token */
--ff-mono: 'JetBrains Mono', 'Courier New', monospace;

/* Apply selectively */
.nav-tabs-custom > .nav-tabs > li > a {
  font-family: var(--ff-mono);
  font-size: 11px;
  text-transform: uppercase;
  letter-spacing: 1.5px;
}

.status-pill, .label {
  font-family: var(--ff-mono);
  font-size: 10px;
  text-transform: uppercase;
  letter-spacing: 0.5px;
}
```

### 3B. Keep Existing Fonts for Content

- **Space Grotesk** for headings — keep as-is (close enough to website's Geist)
- **Inter** for body text — keep as-is (website also uses Inter as fallback)

No changes needed for the core font stack.

---

## 4. CARD / BOX SURFACE TREATMENT — Medium Priority

### 4A. Background Color Shift

| Surface | Docker App (Current) | Website (Target) |
|---|---|---|
| Page background | `#F5F0F2` | **`#F6EBEF`** | Website `--background-warm: hsl(334, 38%, 93%)` — slightly more pink |
| Card background | `#FFFFFF` | **`#F5F5F5`** | Website `--card: hsl(0, 0%, 96%)` — off-white, less contrast |
| Sidebar | `#FAFAFA` | No change needed — close enough |

**CSS changes:**

```css
--bg-page:    #F6EBEF;   /* was #F5F0F2 */
--bg-card:    #F5F5F5;   /* was #FFFFFF */
```

### 4B. Border Color Shift

| Border | Docker App (Current) | Website (Target) |
|---|---|---|
| Light border | `#E5E0E2` (warm) | **`#C8CEDA`** | Website `--border: hsl(220, 20%, 82%)` — cooler, more gray-blue |
| Medium border | `#D0C8CC` | **`#B0B8C8`** | Proportional shift to cooler tone |

**CSS changes:**

```css
--border-light:   #C8CEDA;  /* was #E5E0E2 */
--border-medium:  #B0B8C8;  /* was #D0C8CC */
```

### 4C. Box Shadow Refinement

Website uses minimal shadows. Current app shadows are already subtle but can be further reduced:

```css
--shadow-sm:  0 1px 2px rgba(0,0,0,0.04);   /* was 0.06 — lighter */
--shadow-md:  0 2px 6px rgba(0,0,0,0.05);   /* was 0.08 — lighter */
--shadow-lg:  0 4px 12px rgba(0,0,0,0.06);  /* was 0.10 — lighter */
```

### 4D. Box Top Border Accent

Current app boxes use a `3px` top accent border. Website cards don't have this — they use **uniform 1px borders** with no colored top strip.

**Recommendation:** Remove the colored top border from standard `.box` elements. Reserve accent borders for intentional highlights only.

```css
.box {
  border-top: 1px solid var(--border-light);  /* was 3px solid */
}

/* Keep accent borders only for danger/alert boxes */
.box.box-danger {
  border-left: 3px solid var(--brand-coral);  /* left border, not top — matches website pattern */
  border-top: 1px solid var(--border-light);
}
```

---

## 5. DARK HEADER — Low Priority (Already Close)

| Property | Docker App (Current) | Website (Target) |
|---|---|---|
| Header background | `#1A1A1A` | **`#141D2E`** | Website uses navy-tinted dark, not pure dark |
| Text on dark | `#FFFFFF` | No change |

```css
--bg-header: #141D2E;  /* was #1A1A1A */
```

Small shift — the navy tint adds subtle warmth that connects to the website's `--navy` token.

---

## 6. SEMANTIC STATUS COLORS — Low Priority

Align the value-box and status indicator palette to the website's system:

| Role | Docker App (Current) | Website (Target) |
|---|---|---|
| Success | `#3D8B5E` | **`#6AAF95`** | Sage green (covered in Section 1) |
| Danger | `#C44245` | **`#F53D3F`** | Brighter coral (covered in Section 1) |
| Warning | `#C68A1A` | **`#FF4D00`** | Website `--destructive/orange` — pure orange, not amber |
| Info | `#4A7FA5` | No change — website doesn't define an info blue |

```css
--status-warning: #FF4D00;  /* was #C68A1A — shifts from amber to orange */
```

> **Caution:** The orange `#FF4D00` is quite vivid. If used for large UI surfaces (value boxes), consider using it at reduced opacity or only for small indicators. Test with actual dashboard data before committing.

---

## 7. GUIDANCE BOX ACCENT — Low Priority

The app's guidance boxes use a coral left-border on cream background — this already matches the website's card accent pattern well. One small alignment:

```css
--bg-guidance: #FFF8F0;  /* keep — already matches website's warm cream */

.guidance-toggle-btn {
  border-radius: 0 2px 2px 0;  /* was 0 8px 8px 0 — sharper, per radius changes */
}
```

---

## Implementation Order (Recommended)

| Phase | Changes | Risk | Effort |
|---|---|---|---|
| **Phase 1** | Button colors (Section 1A + 1B) | Low — color-only, no layout impact | ~1 hour |
| **Phase 2** | R constants in `global.R` (Section 1A + 1B) | Low — ensures server-rendered colors match | ~30 min |
| **Phase 3** | Border radius reduction (Section 2) | Medium — visual change across entire app | ~30 min |
| **Phase 4** | Card surfaces + borders (Section 4) | Medium — subtle but pervasive | ~1 hour |
| **Phase 5** | Monospace typography (Section 3A) | Low — additive, doesn't break existing | ~45 min |
| **Phase 6** | Header + status colors (Sections 5, 6) | Low — minor tweaks | ~30 min |

**Total estimated scope:** ~4-5 hours for a developer familiar with the codebase.

---

## Files to Modify

| File | Changes |
|---|---|
| `www/trisk.css` | Token values, button styles, border radius, card surfaces, typography additions |
| `global.R` | R color constants (`BRAND_CORAL`, `STATUS_GREEN`, `TRISK_HEX_GREEN`, etc.) |
| `ui.R` | Add JetBrains Mono to `tags$link()` Google Fonts import |
| `server.R` | Audit inline `style=` attributes that hardcode old hex values (search for `#E84B4D`, `#3D8B5E`, `#C44245`) |

---

## Visual Reference: Color Comparison

```
                    DOCKER APP (CURRENT)     WEBSITE (TARGET)
                    ────────────────────     ────────────────
GREEN CTA btn       ██ #3D8B5E (forest)      ██ #6AAF95 (sage)
GREEN hover         ██ #2F7A4E              ██ #3B9B78
GREEN deep          ██ #276749              ██ #2BAB76

RED primary         ██ #E84B4D (muted coral) ██ #F53D3F (bright coral)
RED hover           ██ #C0393B              ██ #D93235
RED light           ██ #EF7173              ██ #EF6C82 (salmon)

PAGE bg             ██ #F5F0F2              ██ #F6EBEF
CARD bg             ██ #FFFFFF              ██ #F5F5F5
BORDER              ██ #E5E0E2 (warm)       ██ #C8CEDA (cool)
HEADER              ██ #1A1A1A (black)      ██ #141D2E (navy)
WARNING             ██ #C68A1A (amber)      ██ #FF4D00 (orange)
```

---

## Out of Scope

The following website elements are **not recommended** for the Docker app:

- **Bracket notation** `[ ABOUT ]` for navigation — too editorial for a data dashboard
- **Dot-grid background pattern** — the app already has its own SVG dot pattern; no change needed
- **Fixed side navigation** (HOME / ABOUT / CRISPY) — website-specific navigation pattern
- **Scroll progress indicator** — not useful in a tabbed Shiny app
- **Full 0px border-radius** — compromise at 2-3px is more appropriate for data-dense UI
