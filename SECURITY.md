# Security Controls

## Container Hardening
- Base image pinned by SHA256 digest
- Non-root user (`trisk`) with nologin shell
- `cap_drop: ALL`, `no-new-privileges: true`
- Read-only filesystem with `noexec,nosuid` tmpfs
- Custom seccomp profile (ptrace/bpf/keyctl blocked)
- Resource limits (memory, CPU, PIDs, file descriptors)

## Supply Chain
- CRAN packages locked to Posit PPM date snapshot
- GitHub packages pinned to exact commit SHAs
- Scenario data verified against SHA256 checksum at build
- All fonts vendored locally (no runtime CDN)

## Network
- `internal: true` Docker network (no outbound internet)
- Shiny bound to internal network only (proxy-facing)
- TLS termination at Caddy reverse proxy
- Localhost-only port binding on host

## Application
- Column allowlisting on upload (PII stripping)
- Column allowlisting on export (DLP control)
- Server-side file type validation
- Formula injection sanitization
- XSS protection via `htmlEscape()` on user data
- Error messages sanitized (no stack traces)
- Session idle timeout (30 minutes)
- Structured audit logging

## Authentication
- HTTP Basic Auth via Caddy reverse proxy
- Override with SSO/LDAP at proxy layer for production

## Deployment Checklist
1. Replace TLS certificates in `tls/` with bank-provided certs
2. Generate Caddy auth password hash and set in `.env`
3. Override `BASE_IMAGE` to use bank artifactory
4. Configure SIEM log forwarding (switch Docker log driver to syslog)
5. Run `trivy image` scan and remediate HIGH/CRITICAL CVEs
6. Review seccomp profile against bank security policy
