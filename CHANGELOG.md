# Changelog

All notable changes to TRISK Desktop will be documented in this file.

## [2.1.0] - 2026-03-25

### Security
- Added Caddy reverse proxy with HTTP Basic Auth and TLS termination
- Added Content Security Policy headers (via Caddy + meta tag)
- Added custom seccomp profile blocking ptrace/bpf/keyctl syscalls
- Added formula injection sanitization for Excel/CSV exports (CWE-1236)
- Sanitized all user-facing error messages (CWE-209) - no more raw `e$message` in browser
- Added `.env` and TLS cert patterns to `.gitignore`
- Added `.env.example` with documented configuration variables
- Added `SECURITY.md` with deployment security checklist
- Container: Caddy-pinned image with SHA256 digest, `read_only`, `cap_drop: ALL`

### Added
- Persistent structured audit logging (JSON lines to `/data/output/audit/`)
- Audit log captures: timestamp, session ID, user identity, action, parameters
- Logs forwarded to stderr for Docker log driver / SIEM integration
- Input data SHA256 checksums in config export for reproducibility
- Model version SHA and scenario dataset hash in config export
- `testthat` unit test suite for validation, column stripping, scenario logic, helpers
- `CHANGELOG.md` for release tracking
- `CLAUDE.md` project documentation
- Main `Dockerfile` with multi-stage build, healthcheck, non-root user
- Shiny startup script (`run-shiny.sh`) for configurable host/port

### Fixed
- `generate_run_id()` hash collision vulnerability (replaced `sum(utf8ToInt)` with `rlang::hash`)
- `log_message()` now forwards to `message()` for Docker log capture (was in-memory only)
- Removed R version, Shiny version, BUILD_DATE from browser-visible outputs
- Removed `R.version$version.string` from config JSON export (information disclosure)
- CI: GitHub Actions pinned to SHA digests (was major version tags)
- CI: Added Trivy container image CVE scanning
- CI: Test container binds to localhost only (was 0.0.0.0)

### Changed
- Docker Compose: Shiny no longer exposed directly; routed through Caddy proxy on port 443
- Docker Compose: Added seccomp profile, session idle timeout
- Upload size limit aligned with documentation

## [2.0.0] - 2026-03-15

### Added
- Multi-scenario analysis (run multiple target scenarios in one batch)
- Multi-horizon analysis (multiple shock years per scenario)
- Run history with comparison (up to 5 previous runs)
- PD/EL integration with internal bank estimates (absolute, relative, Z-score methods)
- Concentration analysis (Gini coefficient, HHI, top-N exposure)
- Attribution analysis (sector/technology contribution decomposition)
- Column allowlisting on upload and export (PII stripping)
- Error sanitization (`shiny.sanitize.errors = TRUE`)
- Docker hardening (read-only filesystem, cap_drop, resource limits, log rotation)
- Air-gapped deployment support (vendored fonts, bundled scenarios)
- Supply chain integrity (pinned base image, CRAN snapshot, Git commit SHAs, scenario SHA256)
