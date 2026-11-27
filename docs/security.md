# Security Architecture

TRISK Desktop is designed for deployment on bank workstations and controlled internal environments.

## Data containment

- All processing happens **locally inside the Docker container**
- **No data leaves your machine** at runtime - no telemetry, no cloud calls, no analytics
- Input data is mounted **read-only**
- Network access is **not required** after the initial image build
- The application binds to **localhost only** (`127.0.0.1`)

## Container hardening

| Control | Implementation |
|---------|---------------|
| Non-root execution | Application runs as unprivileged `trisk` user with nologin shell |
| Read-only filesystem | Root filesystem is read-only; only `/tmp` and `/data/output` are writable |
| Capability drop | All Linux capabilities dropped (`cap_drop: ALL`) |
| Privilege escalation | Blocked (`no-new-privileges: true`) |
| Resource limits | Memory 4 GB, CPU 2 cores, PID limit 256 |
| Log rotation | 10 MB x 3 files maximum |
| Output restrictions | Output volume mounted with `noexec,nosuid` flags |
| Network isolation | `internal: true` Docker network - no outbound internet |

## Application security

**Upload controls**

:   Uploaded files are validated against strict column allowlists. Extra columns - including potentially sensitive ones like employee names, account numbers, or internal IDs - are automatically stripped and never stored.

**Export controls**

:   Exported results pass through a separate column allowlist. Only expected output columns are included. Character columns are sanitized against formula injection (CWE-1236) - cells starting with `=`, `+`, `@`, or tab characters are prefixed with a single quote.

**Error handling**

:   User-facing errors are sanitized (`shiny.sanitize.errors = TRUE`). No stack traces, file paths, or internal state is exposed to the browser. Detailed errors are logged server-side only.

**Audit logging**

:   All uploads, analysis runs, and exports are logged to `/data/output/audit/trisk-audit.jsonl` in structured JSON-lines format with ISO-8601 timestamps and session identifiers.

## Supply chain security

- Base Docker image pinned by **SHA-256 digest** (not just tag)
- CRAN packages locked to a **Posit PPM date snapshot** (immutable)
- GitHub packages pinned to **exact commit SHAs**
- Scenario data verified against **SHA-256 checksum** at build time
- All fonts vendored locally - no runtime CDN requests

## Authentication (production deployment)

The production `docker-compose.yml` includes a Caddy reverse proxy with:

- **TLS encryption** using bank-provided certificates
- **HTTP Basic Authentication** with bcrypt-hashed passwords
- **Security headers**: HSTS, X-Frame-Options DENY, CSP, no-referrer, permissions policy
- For SSO/LDAP integration, replace the Basic Auth block in the Caddyfile with your proxy-level authentication

## Deployment checklist

1. Replace TLS certificates in `tls/` with bank-issued certs
2. Generate auth password hash: `docker run --rm caddy:2-alpine caddy hash-password`
3. Set `TRISK_PASS_HASH` in `.env`
4. Override `BASE_IMAGE` if using a bank artifactory
5. Run `trivy image` scan on the built image
6. Configure Docker log driver for SIEM forwarding if required
7. Review seccomp profile against bank security policy

## Known limitations

!!! warning "CSP allows unsafe-inline and unsafe-eval"
    The Content Security Policy permits `unsafe-inline` and `unsafe-eval` in script and style directives. This is a known requirement of the Shiny framework, which dynamically generates and injects JavaScript. Input validation and output sanitization provide defense-in-depth.

External links to documentation sites (theia-finance-labs.github.io, 1in1000.com) appear in the About and Documentation tabs. In air-gapped deployments, these links will not resolve but do not affect functionality.
