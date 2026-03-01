#!/usr/bin/env Rscript
# ============================================
# Design Alignment Validation Tests
# Validates that CSS tokens, R constants, and server.R
# inline colors are aligned with 1in1000.com website
# ============================================

cat("\n========================================\n")
cat("Design Alignment Validation\n")
cat("========================================\n\n")

pass <- 0
fail <- 0
warnings <- 0

check <- function(desc, condition) {
  if (condition) {
    cat(paste0("  PASS  ", desc, "\n"))
    pass <<- pass + 1
  } else {
    cat(paste0("  FAIL  ", desc, "\n"))
    fail <<- fail + 1
  }
}

warn <- function(desc) {
  cat(paste0("  WARN  ", desc, "\n"))
  warnings <<- warnings + 1
}

# ============================================
# 1. CSS Token Validation
# ============================================
cat("--- CSS Design Tokens (www/trisk.css) ---\n")

css <- readLines("app/www/trisk.css")
css_text <- paste(css, collapse = "\n")

# Brand colors — unified red
check("--brand-coral is #F53D3F", grepl("--brand-coral:\\s*#F53D3F", css_text))
check("--brand-coral-lt is #EF6C82", grepl("--brand-coral-lt:\\s*#EF6C82", css_text))
check("--brand-coral-dk is #D93235", grepl("--brand-coral-dk:\\s*#D93235", css_text))
check("--brand-red is #F53D3F", grepl("--brand-red:\\s*#F53D3F", css_text))

# Surfaces
check("--bg-page is #F6EBEF", grepl("--bg-page:\\s*#F6EBEF", css_text))
check("--bg-card is #F5F5F5", grepl("--bg-card:\\s*#F5F5F5", css_text))
check("--bg-header is #141D2E", grepl("--bg-header:\\s*#141D2E", css_text))

# Borders — cool gray
check("--border-light is #C8CEDA", grepl("--border-light:\\s*#C8CEDA", css_text))
check("--border-medium is #B0B8C8", grepl("--border-medium:\\s*#B0B8C8", css_text))

# Status — website-aligned
check("--status-success is #6AAF95", grepl("--status-success:\\s*#6AAF95", css_text))
check("--status-warning is #FF4D00", grepl("--status-warning:\\s*#FF4D00", css_text))
check("--status-danger is #F53D3F", grepl("--status-danger:\\s*#F53D3F", css_text))

# Value boxes
check("--vb-success is #3B9B78", grepl("--vb-success:\\s*#3B9B78", css_text))

# Radius — near-sharp
check("--radius-sm is 2px", grepl("--radius-sm:\\s*2px", css_text))
check("--radius-md is 2px", grepl("--radius-md:\\s*2px", css_text))
check("--radius-lg is 3px", grepl("--radius-lg:\\s*3px", css_text))

# Typography — monospace token
check("--font-mono token exists", grepl("--font-mono:", css_text))
check("JetBrains Mono in --font-mono", grepl("JetBrains Mono", css_text))

# OLD values must NOT be present in :root
# Extract just the :root block
root_match <- regmatches(css_text, regexpr(":root\\s*\\{[^}]+\\}", css_text))
if (length(root_match) > 0) {
  root_block <- root_match[1]
  check("No old #E84B4D in :root", !grepl("#E84B4D", root_block))
  check("No old #EF7173 in :root", !grepl("#EF7173", root_block))
  check("No old #C0393B in :root", !grepl("#C0393B", root_block))
  check("No old #E5E0E2 in :root", !grepl("#E5E0E2", root_block))
  check("No old #D0C8CC in :root", !grepl("#D0C8CC", root_block))
  check("No old #3D8B5E in :root", !grepl("#3D8B5E", root_block))
  check("No old #C44245 in :root", !grepl("#C44245", root_block))
  check("No old #C68A1A in :root", !grepl("#C68A1A", root_block))
  check("No old 12px radius in :root", !grepl("--radius-lg:\\s*12px", root_block))
} else {
  warn("Could not extract :root block from CSS")
}

# Button active state
check("btn-primary :active has scale(0.98)", grepl("\\.btn-primary:active[^}]*scale\\(0\\.98\\)", css_text))
check("btn-success has translucent border", grepl("rgba\\(59,\\s*155,\\s*120", css_text))

# Box styling — left accent not top
check("Box border-top is 1px (not 3px)", grepl("\\.box\\s*\\{[^}]*border-top:\\s*1px", css_text))

cat("\n")

# ============================================
# 2. R Constants Validation (global.R)
# ============================================
cat("--- R Color Constants (global.R) ---\n")

global_r <- readLines("app/global.R")
global_text <- paste(global_r, collapse = "\n")

check("BRAND_RED is #F53D3F", grepl('BRAND_RED\\s*<-\\s*"#F53D3F"', global_text))
check("BRAND_CORAL is #F53D3F (unified)", grepl('BRAND_CORAL\\s*<-\\s*"#F53D3F"', global_text))
check("BRAND_CORAL_LT is #EF6C82", grepl('BRAND_CORAL_LT\\s*<-\\s*"#EF6C82"', global_text))
check("BRAND_CORAL_DK is #D93235", grepl('BRAND_CORAL_DK\\s*<-\\s*"#D93235"', global_text))
check("STATUS_GREEN is #6AAF95", grepl('STATUS_GREEN\\s*<-\\s*"#6AAF95"', global_text))
check("STATUS_RED is #F53D3F", grepl('STATUS_RED\\s*<-\\s*"#F53D3F"', global_text))
check("TRISK_HEX_GREEN is #6AAF95", grepl('TRISK_HEX_GREEN\\s*<-\\s*"#6AAF95"', global_text))
check("BG_PINK is #F6EBEF", grepl('BG_PINK\\s*<-\\s*"#F6EBEF"', global_text))
check("BG_CARD is #F5F5F5", grepl('BG_CARD\\s*<-\\s*"#F5F5F5"', global_text))
check("BORDER_PINK is #C8CEDA", grepl('BORDER_PINK\\s*<-\\s*"#C8CEDA"', global_text))
check("WARNING_AMBER exists", grepl('WARNING_AMBER\\s*<-', global_text))
check("FG_SECONDARY exists", grepl('FG_SECONDARY\\s*<-', global_text))
check("FG_TERTIARY exists", grepl('FG_TERTIARY\\s*<-', global_text))
check("COLOR_SUCCESS_INLINE exists", grepl('COLOR_SUCCESS_INLINE\\s*<-', global_text))

# No old values
check("No old #E84B4D in BRAND_CORAL", !grepl('BRAND_CORAL\\s*<-\\s*"#E84B4D"', global_text))
check("No old #3D8B5E in STATUS_GREEN", !grepl('STATUS_GREEN\\s*<-\\s*"#3D8B5E"', global_text))
check("No old #5D9324 in TRISK_HEX_GREEN", !grepl('TRISK_HEX_GREEN\\s*<-\\s*"#5D9324"', global_text))
check("No old #C44245 in STATUS_RED", !grepl('STATUS_RED\\s*<-\\s*"#C44245"', global_text))

cat("\n")

# ============================================
# 3. Font Import Validation (ui.R)
# ============================================
cat("--- Font Import (ui.R) ---\n")

ui_r <- readLines("app/ui.R")
ui_text <- paste(ui_r, collapse = "\n")

check("JetBrains Mono in Google Fonts import", grepl("JetBrains\\+Mono", ui_text))
check("Space Grotesk still in import", grepl("Space\\+Grotesk", ui_text))
check("Inter still in import", grepl("Inter", ui_text))
check("CSS cache-buster present", grepl("trisk\\.css\\?v=", ui_text))

cat("\n")

# ============================================
# 4. Server.R Stale Color Audit
# ============================================
cat("--- Server.R Stale Color Audit ---\n")

server_r <- readLines("app/server.R")
server_text <- paste(server_r, collapse = "\n")

# These hardcoded hex values should be replaced with R constants
stale_colors <- c(
  "#C44245" = "STATUS_RED",
  "#6B9F3B" = "STATUS_GREEN/COLOR_SUCCESS_INLINE",
  "#D4A017" = "WARNING_AMBER"
)

for (hex in names(stale_colors)) {
  # Count occurrences in style= strings (inline HTML)
  pattern <- paste0('style\\s*=\\s*["\'][^"\']*', gsub("#", "\\\\#", hex))
  matches <- grep(hex, server_r, fixed = TRUE)
  if (length(matches) > 0) {
    warn(paste0("server.R still has ", length(matches), " occurrence(s) of ",
                hex, " (should be ", stale_colors[hex], ") at lines: ",
                paste(matches, collapse = ", ")))
  } else {
    check(paste0("No stale ", hex, " in server.R"), TRUE)
  }
}

# Check that R constants are used instead of hardcoded values
check("server.R uses STATUS_GREEN constant", grepl("STATUS_GREEN", server_text))
check("server.R uses STATUS_RED constant", grepl("STATUS_RED", server_text))
check("server.R uses BRAND_CORAL constant", grepl("BRAND_CORAL", server_text))
check("server.R uses WARNING_AMBER constant", grepl("WARNING_AMBER", server_text))
check("server.R uses FG_SECONDARY constant", grepl("FG_SECONDARY", server_text))
check("server.R uses BG_CARD for plot bg", grepl("BG_CARD", server_text))

cat("\n")

# ============================================
# 5. Cross-file Consistency
# ============================================
cat("--- Cross-file Consistency ---\n")

# Extract CSS token values and R constant values, check they match
# This validates the tokens are in sync between CSS and R
check("CSS --brand-coral matches R BRAND_CORAL (both #F53D3F)",
      grepl("--brand-coral:\\s*#F53D3F", css_text) && grepl('BRAND_CORAL\\s*<-\\s*"#F53D3F"', global_text))
check("CSS --status-success matches R STATUS_GREEN (both #6AAF95)",
      grepl("--status-success:\\s*#6AAF95", css_text) && grepl('STATUS_GREEN\\s*<-\\s*"#6AAF95"', global_text))
check("CSS --bg-page matches R BG_PINK (both #F6EBEF)",
      grepl("--bg-page:\\s*#F6EBEF", css_text) && grepl('BG_PINK\\s*<-\\s*"#F6EBEF"', global_text))
check("CSS --border-light matches R BORDER_PINK (both #C8CEDA)",
      grepl("--border-light:\\s*#C8CEDA", css_text) && grepl('BORDER_PINK\\s*<-\\s*"#C8CEDA"', global_text))

cat("\n")

# ============================================
# Summary
# ============================================
cat("========================================\n")
cat(sprintf("Results: %d passed, %d failed, %d warnings\n", pass, fail, warnings))
cat("========================================\n")

if (fail > 0) {
  cat("\nFAILED — fix the issues above before shipping.\n")
  quit(status = 1)
} else if (warnings > 0) {
  cat("\nPASSED with warnings — review warnings above.\n")
  quit(status = 0)
} else {
  cat("\nALL PASSED — design alignment is complete.\n")
  quit(status = 0)
}
