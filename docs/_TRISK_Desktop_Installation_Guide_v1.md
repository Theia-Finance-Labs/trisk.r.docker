# TRISK Desktop — Enterprise Installation Guide

**Version:** 1.0
**Classification:** Internal — For IT / Information Security / Risk Technology Teams
**Vendor:** 1in1000 (Theia Finance Labs)
**Application:** TRISK Desktop — Climate Transition Risk Stress Testing Tool
**Deployment model:** Self-hosted, containerized (Docker), air-gapped at runtime

---

## Table of Contents

1. [Executive Summary](#1-executive-summary)
2. [Pre-Installation: Internal Approvals](#2-pre-installation-internal-approvals)
3. [System Requirements](#3-system-requirements)
4. [Network and Firewall Requirements](#4-network-and-firewall-requirements)
5. [Security Architecture](#5-security-architecture)
6. [Pre-Installation Checklist](#6-pre-installation-checklist)
7. [Installation Procedure — Docker (Recommended)](#7-installation-procedure--docker-recommended)
8. [Installation Procedure — Bare-Metal R (Alternative)](#8-installation-procedure--bare-metal-r-alternative)
9. [TLS Certificate Configuration](#9-tls-certificate-configuration)
10. [Authentication Configuration](#10-authentication-configuration)
11. [Data Provisioning](#11-data-provisioning)
12. [Post-Installation Verification](#12-post-installation-verification)
13. [SIEM and Log Integration](#13-siem-and-log-integration)
14. [Vulnerability Scanning](#14-vulnerability-scanning)
15. [Backup and Recovery](#15-backup-and-recovery)
16. [Decommissioning](#16-decommissioning)
17. [Troubleshooting](#17-troubleshooting)
18. [Appendix A: Licensing](#appendix-a-licensing)
19. [Appendix B: Full Package Inventory](#appendix-b-full-package-inventory)
20. [Appendix C: Approval Templates](#appendix-c-approval-templates)
21. [Appendix D: Environment Variable Reference](#appendix-d-environment-variable-reference)

---

## 1. Executive Summary

TRISK Desktop is a self-hosted web application for climate transition risk stress testing, designed for ICAAP/ILAAP preparation, board reporting, and regulatory submissions. It runs as a containerized service on an analyst's workstation or a shared internal server.

**Key architecture facts for IT review:**

| Property | Value |
|----------|-------|
| Runtime network access | **None** — fully air-gapped after installation |
| Data residency | **100% on-premise** — no cloud, no telemetry, no callbacks |
| Authentication | HTTP Basic Auth (Caddy proxy) or institution SSO/LDAP |
| Encryption in transit | TLS 1.2+ via Caddy reverse proxy |
| Container privileges | Non-root, all capabilities dropped, custom seccomp profile |
| PII handling | Column allowlisting on upload strips unauthorized fields |
| External dependencies at runtime | None — all fonts, assets, and data vendored locally |
| Licensing | Open-source model packages (GPL-3.0 / LGPL-3.0), proprietary UI |

---

## 2. Pre-Installation: Internal Approvals

The following approvals should be obtained before proceeding with installation. This section maps to typical bank IT governance frameworks (ITIL, COBIT, ISO 27001).

### 2.1 Change Advisory Board (CAB) / Change Management

**Required artifacts:**

- [ ] Change Request (CR) — standard change for new software installation
- [ ] Risk assessment — classify as low risk (local-only, no network at runtime, no database)
- [ ] Rollback plan — see [Section 16: Decommissioning](#16-decommissioning)
- [ ] Testing evidence — UAT sign-off from requesting business unit (Risk team)

**Classification recommendation:** Standard change (pre-approved category) if Docker is already approved in your environment. Normal change if Docker requires first-time approval.

### 2.2 Information Security / CISO Office

**Required artifacts:**

- [ ] Data classification assessment — the application processes portfolio exposure data (likely "Confidential" or "Internal"). No PII is required by the application
- [ ] Vendor security questionnaire — completed by 1in1000 / Theia Finance Labs
- [ ] Container security review — review of Dockerfile, seccomp profile, and hardening controls (see [Section 5](#5-security-architecture))
- [ ] Network security review — confirmation that the application makes zero outbound connections at runtime
- [ ] Penetration test results — if required by policy for internal tools
- [ ] DLP assessment — export controls via column allowlisting are documented

### 2.3 Architecture Review Board (ARB)

**Required if deploying on shared infrastructure (e.g., internal VM, Kubernetes):**

- [ ] Architecture decision record (ADR) — single-container stateless application, Caddy reverse proxy, TLS termination
- [ ] Capacity planning — 4 GB RAM, 2 CPU cores, 2 GB disk per instance
- [ ] Integration assessment — standalone tool, no integration with core banking systems

### 2.4 Software Procurement / Vendor Management

- [ ] License review — open-source dependencies under GPL-3.0 and LGPL-3.0 (see [Appendix A](#appendix-a-licensing))
- [ ] Vendor due diligence on Theia Finance Labs / 1in1000
- [ ] Support agreement — if applicable (SLA, escalation contacts)
- [ ] Data processing agreement (DPA) — not required for self-hosted deployment (no data leaves the institution)

### 2.5 Desktop Engineering / Endpoint Management

**If deploying on analyst workstations:**

- [ ] Docker Desktop approval — confirm Docker is in the approved software catalog (or request addition)
- [ ] Local admin rights — may be needed for Docker Desktop installation (one-time)
- [ ] Disk space allocation — minimum 5 GB free
- [ ] Port reservation — TCP 443 (production) or TCP 3838 (development)

---

## 3. System Requirements

### 3.1 Hardware (Minimum)

| Component | Minimum | Recommended |
|-----------|---------|-------------|
| CPU | 2 cores | 4 cores |
| RAM | 8 GB total (4 GB for Docker) | 16 GB total |
| Disk | 5 GB free | 10 GB free |
| Display | 1280 x 800 | 1920 x 1080 |

### 3.2 Operating System

| OS | Supported Versions |
|----|--------------------|
| Windows | 10 Pro/Enterprise (21H2+), 11 Pro/Enterprise, Server 2019+ |
| macOS | 12 Monterey or later |
| Linux | Ubuntu 22.04+, RHEL 8+, Debian 11+ |

### 3.3 Software Prerequisites

**Docker deployment (recommended):**

| Software | Version | Notes |
|----------|---------|-------|
| Docker Desktop | 24.0+ | Or Docker Engine on Linux |
| docker compose | v2 | Bundled with Docker Desktop |
| Web browser | Chrome 90+, Firefox 90+, Edge 90+ | For accessing the application |

**Bare-metal deployment (alternative):**

| Software | Version | Notes |
|----------|---------|-------|
| R | 4.4.1 (exact) | Must match tested version |
| Web browser | Chrome 90+, Firefox 90+, Edge 90+ | For accessing the application |

### 3.4 Docker Resource Allocation

Configure Docker Desktop with at least:

- **Memory:** 4 GB (Settings > Resources > Memory)
- **CPU:** 2 cores
- **Disk image size:** 20 GB

---

## 4. Network and Firewall Requirements

### 4.1 Installation Phase (One-Time)

The following outbound HTTPS (port 443) connections are required **only during initial build**. After the image is built, these are never needed again.

| Destination | Purpose | Can Be Bypassed? |
|-------------|---------|-------------------|
| `packagemanager.posit.co` | R package downloads (CRAN mirror) | Yes — use internal R mirror or pre-built image |
| `github.com` | TRISK model packages (source) | Yes — vendor packages or use pre-built image |
| `storage.googleapis.com` | Scenario data download (CSV) | Yes — transfer file manually via secure channel |
| `ghcr.io` | Pre-built Docker image pull (optional) | Yes — build locally instead |

### 4.2 Runtime Phase

| Direction | Ports | Connections |
|-----------|-------|-------------|
| Inbound | TCP 443 (prod) or TCP 3838 (dev) on `127.0.0.1` only | Local browser only |
| Outbound | **None** | **Zero external connections** |

**For air-gapped environments:** The Docker image can be built on a staging machine with internet access, exported via `docker save`, transferred on approved media, and loaded via `docker load` on the target workstation. See Section 7.3.

### 4.3 DNS

No DNS resolution required at runtime. The application binds to `127.0.0.1` (localhost).

---

## 5. Security Architecture

### 5.1 Container Hardening

| Control | Implementation |
|---------|---------------|
| Non-root execution | Container runs as user `trisk` (nologin shell) |
| Capability drop | `cap_drop: ALL` — no Linux capabilities granted |
| Privilege escalation | `no-new-privileges: true` |
| Filesystem | Read-only root filesystem |
| Writable paths | `/tmp` (tmpfs, noexec, nosuid, 2 GB cap) and `/data/output` (noexec, nosuid) |
| Syscall filtering | Custom seccomp profile blocking ptrace, perf_event_open, keyctl, add_key, request_key, personality, userfaultfd, process_vm_readv/writev |
| Process limits | PID limit: 256 |
| File descriptor limits | 1024 soft, 4096 hard |
| Memory limits | 4 GB hard cap, 4 GB swap cap (prevents OOM-swap spiral) |
| CPU limits | 2.0 cores |
| Log rotation | 10 MB max, 3 files (prevents disk exhaustion) |

### 5.2 Network Isolation

```
[Analyst Browser] ──HTTPS (443)──> [Caddy Proxy] ──HTTP (3838)──> [TRISK App]
      │                                  │                              │
   localhost                        trisk-host                    trisk-internal
   only                            network                       network (no internet)
```

- The TRISK application container is on an `internal: true` Docker network — **it cannot reach the internet**
- Only the Caddy reverse proxy is exposed to the host, on `127.0.0.1:443`
- Caddy terminates TLS and forwards to TRISK on the internal network

### 5.3 Application Security

| Control | Implementation |
|---------|---------------|
| Input validation | Column allowlisting on CSV upload — unauthorized columns stripped |
| Output control | Column allowlisting on export — DLP-compatible |
| File type validation | Server-side MIME type checking |
| Formula injection | Sanitization on Excel export |
| XSS protection | `htmlEscape()` on all user-supplied data |
| CSP | Content-Security-Policy header blocking external scripts |
| Error handling | Sanitized error messages — no stack traces exposed |
| Session management | 30-minute idle timeout |
| Upload size | 50 MB maximum |
| Audit logging | Structured JSON logging of user actions |

### 5.4 Supply Chain Security

| Control | Implementation |
|---------|---------------|
| Base image | Pinned by SHA256 digest (not floating tag) |
| CRAN packages | Locked to Posit PPM snapshot `2025-02-01` |
| GitHub packages | Pinned to exact Git commit SHAs |
| Scenario data | SHA256 checksum verification at build time |
| Fonts | Vendored locally (no CDN at runtime) |

### 5.5 Security Headers (Caddy)

```
X-Frame-Options: DENY
X-Content-Type-Options: nosniff
Referrer-Policy: no-referrer
Strict-Transport-Security: max-age=63072000; includeSubDomains
Permissions-Policy: camera=(), microphone=(), geolocation=(), payment=()
Server: (removed)
```

---

## 6. Pre-Installation Checklist

Complete all items before beginning the installation procedure.

### 6.1 Governance

- [ ] Change request approved (Section 2.1)
- [ ] Information Security review complete (Section 2.2)
- [ ] Architecture review complete if shared deployment (Section 2.3)
- [ ] License review complete (Section 2.4)
- [ ] Docker Desktop approved and in software catalog (Section 2.5)

### 6.2 Infrastructure

- [ ] Target machine meets hardware requirements (Section 3.1)
- [ ] Operating system is on supported list (Section 3.2)
- [ ] Docker Desktop installed and running (Section 3.3)
- [ ] Docker memory allocation set to 4 GB minimum (Section 3.4)
- [ ] Sufficient disk space (5 GB+)

### 6.3 Network

- [ ] Outbound HTTPS to build-time dependencies confirmed (Section 4.1) **OR** air-gap transfer plan agreed
- [ ] Firewall rule for localhost TCP 443 or 3838 confirmed (Section 4.2)

### 6.4 Security

- [ ] TLS certificates obtained from institution PKI (Section 9)
- [ ] Authentication credentials prepared (Section 10)
- [ ] SIEM log forwarding configuration agreed (Section 13)
- [ ] Vulnerability scan plan agreed (Section 14)

### 6.5 Data

- [ ] Scenario data file obtained (Section 11.1)
- [ ] Sample portfolio CSV prepared for UAT (Section 11.2)
- [ ] Output directory location agreed with analyst

---

## 7. Installation Procedure — Docker (Recommended)

### 7.1 Standard Installation (Internet Access Available)

```bash
# Step 1: Obtain application source
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker

# Step 2: Download scenario data
bash scripts/download_scenarios.sh

# Step 3: Create environment file
cp .env.example .env

# Step 4: Generate authentication password hash
docker run --rm caddy:2-alpine caddy hash-password --plaintext 'YOUR_SECURE_PASSWORD'
# Copy the output hash into .env as TRISK_PASS_HASH

# Step 5: Place TLS certificates (see Section 9)
cp /path/to/institution-cert.pem tls/cert.pem
cp /path/to/institution-key.pem  tls/key.pem

# Step 6: Build and start
docker compose up --build -d

# Step 7: Verify
docker compose ps          # All services should show "Up (healthy)"
curl -k https://localhost   # Should return HTTP 401 (auth required)
```

### 7.2 Bank Artifactory Deployment (Internal Registry)

If your institution uses JFrog Artifactory, Sonatype Nexus, or similar:

```bash
# Option A: Override base image at build time
echo "BASE_IMAGE=artifactory.bank.com/approved/r-ver:4.4.1" >> .env
docker compose up --build -d

# Option B: Pull pre-scanned image from internal registry
docker pull artifactory.bank.com/approved/trisk:1.0.0
docker tag artifactory.bank.com/approved/trisk:1.0.0 theia-finance-labs/trisk:latest
docker compose up -d
```

### 7.3 Air-Gapped Installation (No Internet on Target)

**On the staging machine (with internet):**

```bash
# Build the image
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker
bash scripts/download_scenarios.sh
docker compose build

# Export the images
docker save theia-finance-labs/trisk:latest | gzip > trisk-app.tar.gz
docker save caddy:2-alpine | gzip > caddy-proxy.tar.gz

# Transfer trisk-app.tar.gz, caddy-proxy.tar.gz, and the full project
# directory to the target machine via approved media (USB, SFTP, etc.)
```

**On the target machine (air-gapped):**

```bash
# Load the images
docker load < trisk-app.tar.gz
docker load < caddy-proxy.tar.gz

# Configure environment
cd trisk.r.docker
cp .env.example .env
# Edit .env with authentication hash and settings

# Place TLS certificates
cp /path/to/cert.pem tls/cert.pem
cp /path/to/key.pem  tls/key.pem

# Start (no --build needed, images are pre-loaded)
docker compose up -d
```

### 7.4 Development Mode (For UAT / Testing)

For testing without TLS/auth (localhost only):

```bash
docker compose -f docker-compose.dev.yml up --build -d
# Access at http://localhost:3838 (no TLS, no auth)
```

**Do not use development mode in production.**

---

## 8. Installation Procedure — Bare-Metal R (Alternative)

Use this if Docker is not approved in your environment. See `INSTALL.md` in the repository root for the full step-by-step procedure, including:

1. R 4.4.1 installation
2. Package installation from Posit PPM snapshot
3. GitHub package installation (trisk.model, trisk.analysis)
4. Scenario data download
5. Path configuration in `global.R`
6. Launch command

**Note:** Bare-metal installations do not include the container hardening controls described in Section 5. Additional endpoint security measures should be applied per institutional policy.

---

## 9. TLS Certificate Configuration

### 9.1 Obtain Certificates

Request certificates from your institution's PKI team:

- **Certificate:** PEM-encoded X.509 certificate (`.pem` or `.crt`)
- **Private key:** PEM-encoded private key (`.pem` or `.key`)
- **Subject:** `CN=localhost` or the hostname where TRISK will be accessed
- **SAN:** Include `localhost` and `127.0.0.1` in Subject Alternative Names

### 9.2 Install Certificates

```bash
# Place files in the tls/ directory (gitignored)
cp institution-cert.pem tls/cert.pem
cp institution-key.pem  tls/key.pem

# Verify permissions (key must be readable by Caddy container)
chmod 644 tls/cert.pem
chmod 600 tls/key.pem
```

### 9.3 Self-Signed Certificates (Development Only)

For testing environments where institution certificates are not yet available:

```bash
openssl req -x509 -newkey rsa:4096 -keyout tls/key.pem -out tls/cert.pem \
  -sha256 -days 365 -nodes \
  -subj "/CN=localhost" \
  -addext "subjectAltName=DNS:localhost,IP:127.0.0.1"
```

---

## 10. Authentication Configuration

### 10.1 HTTP Basic Auth (Default)

```bash
# Generate bcrypt password hash
docker run --rm caddy:2-alpine caddy hash-password --plaintext 'SecurePassword123!'

# Add to .env
TRISK_USER=analyst
TRISK_PASS_HASH=$2a$14$xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
```

### 10.2 SSO / LDAP Integration (Institutional)

For enterprise SSO, replace the Caddy basic auth block in `Caddyfile` with your institution's authentication proxy (e.g., Keycloak, Azure AD, Okta). The Caddy reverse proxy layer supports any header-based or forward-auth mechanism.

Contact 1in1000 for SSO integration guidance specific to your IdP.

### 10.3 Multi-User Access

For shared server deployments, configure separate credentials per user or integrate with institutional LDAP. The application is stateless — multiple users can access simultaneously with independent sessions.

---

## 11. Data Provisioning

### 11.1 Scenario Data

The TRISK model requires a scenario dataset containing NGFS, GECO, IPR, and Mission Possible pathways.

**Source:** `https://storage.googleapis.com/crispy-public-data/trisk_inputs/scenarios.csv`
**SHA256:** `b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56`
**Size:** ~200 MB

For air-gapped environments, download on a connected machine and transfer via approved media.

**Verification:**

```bash
shasum -a 256 data/scenarios/scenarios.csv
# Must match: b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56
```

### 11.2 Portfolio Data (User-Supplied)

Analysts upload their portfolio via the application UI. The CSV must contain:

| Column | Type | Description |
|--------|------|-------------|
| `company_id` | String | Unique company identifier |
| `sector` | String | TRISK sector classification |
| `technology` | String | TRISK technology classification |
| `country_iso2` | String | ISO 3166-1 alpha-2 country code |
| `exposure_value_usd` | Numeric | Loan exposure in USD |
| `term` | Numeric | Loan maturity (years) |
| `loss_given_default` | Numeric | LGD estimate (0-1) |

A demo dataset is available in the application ("Load Demo Data" button) for UAT.

### 11.3 Directory Structure

```
data/
├── scenarios/
│   └── scenarios.csv        # Pre-downloaded (Section 11.1)
├── input/                   # Analyst CSV uploads (read-only mount)
└── output/                  # Results export directory (writable)
```

---

## 12. Post-Installation Verification

### 12.1 Container Health Check

```bash
# All services should show "Up (healthy)"
docker compose ps

# Expected output:
# trisk-proxy   caddy:2-alpine   Up      127.0.0.1:443->443/tcp
# trisk-app     trisk:latest     Up (healthy)    3838/tcp
```

### 12.2 Application Access

1. Open `https://localhost` in the browser (or `http://localhost:3838` in dev mode)
2. Enter credentials when prompted
3. The TRISK Desktop dashboard should load with the sidebar navigation

### 12.3 Functional Verification

1. Click **"Load Demo Data"** on the Upload Data tab
2. Navigate to **Configure Scenario** — verify scenario options appear
3. Click **Run Analysis** — verify the model executes (30-60 seconds)
4. Review **Portfolio Results** — verify charts and KPI cards render
5. Test **Export** — download results as Excel/CSV and verify file contents

### 12.4 Security Verification

```bash
# Verify no outbound connections
docker exec trisk-app wget -q --spider https://google.com 2>&1
# Should fail (no internet access from internal network)

# Verify non-root execution
docker exec trisk-app whoami
# Should return: trisk

# Verify read-only filesystem
docker exec trisk-app touch /test 2>&1
# Should fail: Read-only file system

# Verify seccomp profile
docker exec trisk-app cat /proc/self/status | grep Seccomp
# Should return: Seccomp:  2 (filter mode)
```

### 12.5 Sign-Off

| Test | Pass/Fail | Tester | Date |
|------|-----------|--------|------|
| Container health check | | | |
| Application access | | | |
| Demo data load | | | |
| Model execution | | | |
| Results rendering | | | |
| Export functionality | | | |
| No outbound network | | | |
| Non-root execution | | | |
| Read-only filesystem | | | |
| Seccomp active | | | |

---

## 13. SIEM and Log Integration

### 13.1 Docker Log Driver

By default, logs use the `json-file` driver with rotation. For SIEM integration, switch to syslog:

```yaml
# In docker-compose.yml, replace the logging block:
logging:
  driver: syslog
  options:
    syslog-address: "tcp://siem.bank.com:514"
    tag: "trisk-app"
    syslog-facility: "local0"
```

### 13.2 Audit Events

The application logs the following events in structured JSON format:

- Portfolio upload (file name, row count, columns detected)
- Analysis execution (scenario, parameters, duration)
- Results export (format, row count)
- Session start/end
- Errors (sanitized, no stack traces)

### 13.3 Caddy Access Logs

Caddy logs all HTTP requests including:

- Client IP (always `127.0.0.1` for localhost deployments)
- Request path and method
- Response status code
- Authentication username
- Timestamp

---

## 14. Vulnerability Scanning

### 14.1 Image Scanning

Scan the built image before deployment:

```bash
# Using Trivy (recommended)
trivy image theia-finance-labs/trisk:latest

# Using Grype
grype theia-finance-labs/trisk:latest
```

**Policy:** Remediate all HIGH and CRITICAL findings before production deployment. MEDIUM findings should be documented with risk acceptance.

### 14.2 Ongoing Scanning

Schedule periodic scans (monthly recommended) to detect newly disclosed CVEs in base image packages:

```bash
# Rebuild with latest base image patches
docker compose build --no-cache
trivy image theia-finance-labs/trisk:latest
```

### 14.3 SBOM Generation

Generate a Software Bill of Materials for compliance:

```bash
# Using Syft
syft theia-finance-labs/trisk:latest -o spdx-json > trisk-sbom.spdx.json

# Using Trivy
trivy image --format spdx-json theia-finance-labs/trisk:latest > trisk-sbom.spdx.json
```

---

## 15. Backup and Recovery

### 15.1 What to Back Up

| Component | Location | Frequency |
|-----------|----------|-----------|
| Configuration | `.env`, `Caddyfile`, `tls/` | After any change |
| Analysis results | `data/output/` | After each analysis run |
| Portfolio data | `data/input/` | Per analyst policy |

### 15.2 What Does NOT Need Backup

- Docker images (rebuildable from source)
- Scenario data (re-downloadable, checksummed)
- Application state (stateless — no database)

### 15.3 Recovery Procedure

1. Restore configuration files (`.env`, `Caddyfile`, `tls/`)
2. Rebuild Docker images: `docker compose up --build -d`
3. Restore `data/output/` from backup if needed

---

## 16. Decommissioning

To completely remove TRISK Desktop from a workstation:

```bash
# Stop and remove containers
docker compose down -v

# Remove Docker images
docker rmi theia-finance-labs/trisk:latest
docker rmi caddy:2-alpine

# Remove application directory
rm -rf trisk.r.docker/

# Remove Docker volumes (if any remain)
docker volume prune
```

**Data retention:** Ensure analysis results in `data/output/` are archived per institutional data retention policy before deletion.

---

## 17. Troubleshooting

### 17.1 Container Fails to Start

```bash
# Check container logs
docker compose logs trisk

# Common issues:
# - "port 443 already in use" → another service on port 443, change port in docker-compose.yml
# - "TRISK_PASS_HASH not set" → generate and add to .env (Section 10.1)
# - OOM killed → increase Docker memory allocation to 4 GB+
```

### 17.2 Application Loads but Shows Errors

```bash
# Check Shiny application logs
docker logs trisk-app 2>&1 | tail -50

# Enable debug logging
echo "TRISK_DEBUG_LOG=true" >> .env
docker compose up -d
```

### 17.3 TLS Certificate Errors

- Verify certificate files exist in `tls/` directory
- Verify certificate has `localhost` or `127.0.0.1` in SAN
- For self-signed certs, browser will show warning — add exception or use institution CA

### 17.4 Performance Issues

- Ensure Docker has 4 GB RAM allocated
- Large portfolios (>10,000 loans) may require 8 GB RAM
- Check `docker stats` for resource utilization

---

## Appendix A: Licensing

| Component | License | Implications |
|-----------|---------|-------------|
| trisk.model | GPL-3.0 | Open source. Modifications must be shared under GPL-3.0. Using as a library does not trigger copyleft for the calling application. |
| trisk.analysis | LGPL-3.0 | Open source. Can be used as a library without copyleft obligations. |
| R language | GPL-2.0 | Open source runtime |
| Shiny framework | GPL-3.0 | Open source |
| Caddy | Apache-2.0 | Permissive open source |
| TRISK Desktop UI | Proprietary | Licensed by Theia Finance Labs / 1in1000 |

All open-source components are permissible for internal institutional use without redistribution obligations (the institution is not distributing the software externally).

---

## Appendix B: Full Package Inventory

### R Packages (CRAN — locked to 2025-02-01 snapshot)

| Package | Version | Purpose |
|---------|---------|---------|
| shiny | latest-at-snapshot | Web application framework |
| shinydashboard | latest-at-snapshot | Dashboard layout |
| shinyWidgets | latest-at-snapshot | Enhanced UI widgets |
| shinyjs | latest-at-snapshot | JavaScript interop |
| httpuv | latest-at-snapshot | HTTP server backend |
| htmltools | latest-at-snapshot | HTML generation |
| DT | latest-at-snapshot | Interactive data tables |
| ggplot2 | latest-at-snapshot | Static charts |
| plotly | latest-at-snapshot | Interactive charts |
| readr | latest-at-snapshot | CSV parsing |
| writexl | latest-at-snapshot | Excel export |
| jsonlite | latest-at-snapshot | JSON serialization |
| dplyr | latest-at-snapshot | Data manipulation |
| tidyr | latest-at-snapshot | Data reshaping |

### R Packages (GitHub — pinned to commit SHA)

| Package | Commit SHA | Source |
|---------|-----------|--------|
| trisk.model | `b716c9d3573bee2a730973995b9da6fc9b9703c5` | Theia-Finance-Labs/trisk.model |
| trisk.analysis | `307e52aa035b3ebe8bb0ddb967268e38600a1763` | Theia-Finance-Labs/trisk.analysis |

### System Libraries (Runtime — Debian)

libcurl4, libssl3, libxml2, libfontconfig1, libfreetype6, libpng16-16, libtiff5, libjpeg8, libharfbuzz0b, libfribidi0, libpq5

---

## Appendix C: Approval Templates

### Template: Change Request Summary

```
Title: Install TRISK Desktop — Climate Transition Risk Stress Testing Tool
Requester: [Risk team / Credit Risk / ICAAP team]
Category: Standard Change — New Software Installation
Risk Level: Low
Impact: Single workstation / No integration with core systems
Description: Self-hosted, containerized application for climate transition
  risk stress testing. Runs locally with zero outbound network access.
  Used for ICAAP preparation, regulatory submissions, and board reporting.
Rollback: docker compose down -v && rm -rf trisk.r.docker/
Testing: UAT with demo data, functional verification checklist (Section 12)
```

### Template: InfoSec Review Summary

```
Application: TRISK Desktop v1.0
Vendor: 1in1000 (Theia Finance Labs)
Data classification: Confidential (portfolio exposure data)
Network: Air-gapped at runtime — zero outbound connections
Authentication: HTTP Basic Auth (Caddy) / SSO-compatible
Encryption: TLS 1.2+ via Caddy reverse proxy
Container: Non-root, all caps dropped, seccomp filtered, read-only fs
PII: Not required. Column allowlisting strips unauthorized fields on upload.
Audit: Structured JSON logging, SIEM-compatible
```

---

## Appendix D: Environment Variable Reference

| Variable | Required | Default | Description |
|----------|----------|---------|-------------|
| `TRISK_USER` | Yes (prod) | `analyst` | HTTP Basic Auth username |
| `TRISK_PASS_HASH` | Yes (prod) | — | Bcrypt hash for HTTP Basic Auth |
| `BASE_IMAGE` | No | `rocker/r-ver:4.4.1@sha256:...` | Override for bank artifactory |
| `SHINY_LOG_LEVEL` | No | `WARN` | Log verbosity: `WARN` or `TRACE` |
| `TRISK_DEBUG_LOG` | No | `false` | Enable debug logging: `true` or `false` |

---

*Document prepared by 1in1000 Consulting for institutional deployment of TRISK Desktop.*
*For support: contact your 1in1000 engagement lead.*
