# Enterprise Hardening Plan — mystifying-black branch

**Date:** 2026-03-25
**Branch:** claude/mystifying-black (worktree: .claude/worktrees/mystifying-black/)
**Goal:** Fix all CRITICAL and HIGH issues found by 5 independent code review agents before merging to main and deploying to bank pilot.

## Consolidated Findings (deduplicated across 5 reviewers)

### CRITICAL (must fix before merge)

| ID | Issue | File(s) | Lines |
|----|-------|---------|-------|
| C1 | `rv$portfolio` mutated during analysis — corrupts state on re-runs | server.R | 1376-1398 |
| C2 | Observer accumulation — `observeEvent` created inside `observe()` without cleanup | server.R | 1735-1793 |
| C3 | `%||%` operator used but never defined (crash if rlang not re-exported) | server.R, mod_sensitivity.R | multiple |
| C4 | `MAX_UPLOAD_ROWS` defined but never enforced in any upload handler | server.R, mod_upload.R, mod_integration.R | multiple |
| C5 | Default password hash hardcoded in Caddyfile fallback | Caddyfile | 23 |
| C6 | `download_summary_csv` / `download_pd_csv` bypass `sanitize_export()` | server.R | 2074-2092 |
| C7 | Internal PD/EL uploads skip `validate_file_type()` and `strip_columns()` | server.R, mod_integration.R | 5338-5378, 5844-5893 |

### HIGH (should fix before merge)

| ID | Issue | File(s) | Lines |
|----|-------|---------|-------|
| H1 | `output$scenario_warnings` overwritten with `renderUI(NULL)` at runtime | server.R | 1580 |
| H2 | Scenarios upload skips `strip_columns()` | server.R, mod_upload.R | 215-233 |
| H3 | `rv$internal_pd/el` mutated inside `renderUI` — reactive loop risk | server.R | 5384-5386, 5889-5891 |
| H4 | `weighted.mean()` with zero-weight vectors produces NaN | server.R | 2265+ (multiple) |
| H5 | Unbounded `rv$analysis_log` growth | server.R | 55 |
| H6 | Attribution CSV export bypasses `sanitize_export()` | server.R, mod_results_attribution.R | 4350, 601 |
| H7 | Concentration CSV export bypasses `sanitize_export()` | server.R, mod_results_concentration.R | 5236, 1004 |
| H8 | Error report CSV not sanitized | server.R, mod_upload.R | 683, 503 |
| H9 | `output$has_data_issues <- reactive({})` — wrong Shiny output type | server.R | 645-660 |
| H10 | Audit log writes to stderr only — no persistent file | global.R | 467-475 |
| H11 | Seccomp profile uses default-allow (weaker than Docker default) | security/trisk-seccomp.json | 2-3 |
| H12 | N+1 loop in internal PD/EL matching — O(n^2) on bank portfolios | server.R | 5357-5367, 5862-5872 |
| H13 | `scenario_ranking_table` dead variable `earliest_yr` + wrong comment | server.R, mod_results_scenarios.R | 3306-3309 |
| H14 | Duplicate `output$` definitions (plot_pd_distribution, attr_el_waterfall) | server.R | 2245/2652, 4169/4223 |
| H15 | ~800 lines of dead code in `if(FALSE)` blocks | server.R | multiple |
| H16 | mod_upload.R diverges from server.R on mock data loading | mod_upload.R | 20-34 |
| H17 | mod_upload.R missing `audit_log()` calls | mod_upload.R | entire file |
| H18 | mod_run.R local `extract_result_df()` shadows global definition | mod_run.R | 322-331 |
| H19 | mod_sensitivity.R missing NULL guards on rv$results columns | mod_sensitivity.R | 28, 55, 57 |
| H20 | mod_results_horizon.R YoY table crashes if columns missing | mod_results_horizon.R | 378-380 |
| H21 | mod_config.R missing req() on scenario_geography column | mod_config.R | 29 |
| H22 | Session cleanup writes to rv after session ends (useless) | server.R | 65-84 |
| H23 | `.fg-secondary` CSS hardcodes color, breaks dark mode | trisk.css | 897 |
| H24 | 151 inline style= attributes in server.R break CSP/dark mode | server.R | throughout |
| H25 | `download_report_html` handler not wired in server.R | mod_download.R, server.R | 123, - |

## Execution Plan

### Phase 1: Critical fixes in server.R [STATUS: DONE]
- [x] C1: Use local `portfolio_for_run` variable, never write back to `rv$portfolio`
- [x] C2: Add observer tracking + destroy pattern for history buttons (reactiveVal + destroy loop)
- [x] C3: Define `%||%` at top of global.R
- [x] C4: Add `nrow > MAX_UPLOAD_ROWS` check to all 6 upload paths (portfolio, assets, financial, scenarios, internal PD, internal EL)
- [x] C5: Remove default hash from Caddyfile (now fails-safe if env vars missing)
- [x] C6: Wrap download_summary_csv and download_pd_csv with `sanitize_export()`
- [x] C7: Add `validate_file_type()` + row limit to internal PD/EL upload handlers

### Phase 2: High-priority fixes in server.R [STATUS: DONE]
- [x] H1: Removed `output$scenario_warnings <- renderUI(NULL)` overwrite at runtime
- [x] H2: Scenarios upload now gets `strip_columns("scenarios")` (done as part of C4)
- [x] H3: Moved rv$internal_pd/el reset from renderUI to separate observe() blocks
- [x] H4: Added `safe_weighted_mean()` helper to global.R (replacement of calls TODO)
- [x] H5: Capped `rv$analysis_log` at 500 entries in log_message()
- [x] H6-H8: Added `sanitize_export()` to attribution, concentration, error report CSV exports
- [x] H9: Fixed `output$has_data_issues` to use `renderText()` not `reactive()`
- [x] H10: Audit log now writes to persistent `/data/output/audit/trisk-audit.jsonl` + stderr fallback
- [x] H12: Replaced O(n^2) PD/EL matching loop with vectorized named-vector lookup
- [x] H13: Removed dead `earliest_yr` variable, fixed misleading comment
- [x] H14: Removed duplicate output$ definitions (plot_pd_distribution, attr_el_waterfall)
- [x] H15: Removed 528 lines of dead code in 6 if(FALSE) blocks
- [x] H22: Simplified session cleanup to just `gc()`

### Phase 3: High-priority fixes in modules [STATUS: DONE]
- [x] H16: Aligned mod_upload.R mock data loading with server.R (always overwrite scenarios/carbon)
- [x] H17: Added audit_log() calls to all mod_upload.R upload handlers + mock data load
- [x] H18: Removed local extract_result_df() from mod_run.R (uses global definition)
- [x] H19: Added req() guards to mod_sensitivity.R for target_scenario/baseline_scenario columns
- [x] H20: Added column existence guards to mod_results_horizon.R YoY table
- [x] H21: Added defensive fallback in mod_config.R when scenario_geography column missing
- [x] Added sanitize_export() to mod_results_attribution.R and mod_results_concentration.R CSV exports
- [x] Added MAX_UPLOAD_ROWS enforcement + strip_columns("scenarios") to mod_upload.R

### Phase 4: Infrastructure fixes [STATUS: DONE]
- [x] H11: Removed custom seccomp profile from docker-compose.yml (Docker default is better)
- [x] H23: Fixed .fg-secondary CSS to use var(--fg-secondary)
- [ ] H25: Wire download_report_html in server.R or remove button from ui.R — DEFERRED (module not wired yet)

### Phase 5: Cleanup [STATUS: MOSTLY DONE]
- [x] H4 follow-up: Replaced 23 weighted.mean() calls with safe_weighted_mean()
- [x] Merged main into mystifying-black (test suite, CHANGELOG, etc.)
- [x] Added sanitize_formula_injection() from main
- [x] Fixed regex bug in strip_columns (invalid character range [=+@-])
- [x] Fixed regex bug in sanitize_formula_injection
- [x] Improved generate_run_id() to use rlang::hash
- [x] Lowered upload limit to 100MB
- [ ] H24: Extract most-impactful inline styles to CSS classes — DEFERRED
- [ ] Run testthat suite in Docker (requires DT package, not available locally)
- [ ] Update CLAUDE.md with current architecture state

## Completed Steps

### 2026-03-25 Session 1
**Phase 1 (all 7 CRITICAL):**
- C1: server.R — replaced `rv$portfolio <- portfolio_enriched` with local `portfolio_for_run`, updated 3 downstream references
- C2: server.R — wrapped observer creation in reactiveVal + destroy pattern (compare_observers, dup_observers)
- C3: global.R — added `%||%` operator definition after library() calls
- C4: server.R — added row limit check after fread() in all 6 upload handlers; also applied strip_columns("scenarios") to fix H2
- C5: Caddyfile — removed hardcoded default password hash from basicauth block
- C6: server.R — wrapped download_summary_csv and download_pd_csv with sanitize_export()
- C7: server.R — added validate_file_type() and MAX_UPLOAD_ROWS check to internal PD/EL uploads

**Phase 2 (12 of 14 HIGH):**
- H1: Removed output$scenario_warnings runtime overwrite
- H3: Extracted rv$internal_pd/el reset into dedicated observe() blocks
- H4: Added safe_weighted_mean() to global.R
- H5: Added 500-entry cap to rv$analysis_log
- H6-H8: Added sanitize_export() to 3 more CSV export paths
- H9: Changed output$has_data_issues from reactive() to renderText()
- H10: audit_log() now writes to persistent JSONL file + hashes session token
- H12: Replaced O(n^2) loop with vectorized named-vector lookup for PD/EL matching
- H13: Removed dead earliest_yr variable
- H22: Simplified session$onSessionEnded to gc() only

**Phase 4 (2 of 3):**
- H11: Removed custom default-allow seccomp; Docker default is more restrictive
- H23: Fixed .fg-secondary CSS class to use var(--fg-secondary) for dark mode
