# TRISK Climate Transition Risk Stress Testing Tool
# Docker container for local deployment at financial institutions
#
# Supply-chain policy:
#   - Base image pinned by sha256 digest (override BASE_IMAGE for bank artifactory)
#   - APT uses default signature verification (fail-closed, no insecure flags)
#   - CRAN packages locked to a Posit PPM date snapshot
#   - GitHub packages pinned to exact commit SHAs
#   - Scenario data vendored in build context, verified against SHA256 checksum
#   - Google Fonts vendored locally (no runtime network required)
#
# Runtime policy:
#   - Non-root user (trisk)
#   - Designed for read_only + tmpfs /tmp in compose
#   - No outbound network required at runtime

# ============================================================
# ARGs — single place to bump versions
# ============================================================
# Base image — override for bank artifactory (e.g. artifactory.bank.com/approved/r-ver:4.4.1)
ARG BASE_IMAGE=rocker/r-ver:4.4.1@sha256:f3ef082e63ca36547fcf0c05a0d74255ddda6ca7bd88f1dae5a44ce117fc3804
ARG PAK_VERSION=0.8.0
# Posit Public Package Manager snapshot (YYYY-MM-DD) — ensures CRAN is immutable
ARG CRAN_SNAPSHOT=2025-02-01
ARG TRISK_MODEL_SHA=b716c9d3573bee2a730973995b9da6fc9b9703c5
ARG TRISK_ANALYSIS_SHA=307e52aa035b3ebe8bb0ddb967268e38600a1763
ARG SCENARIOS_SHA256=b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56

# ============================================================
# Stage 1: builder — compile R packages (dev libs + compilers)
# ============================================================
FROM ${BASE_IMAGE} AS builder

ARG PAK_VERSION
ARG CRAN_SNAPSHOT
ARG TRISK_MODEL_SHA
ARG TRISK_ANALYSIS_SHA

# Lock CRAN to a point-in-time snapshot via Posit Public Package Manager
# Use __linux__/jammy path for pre-compiled binaries (avoids compiling from source)
ENV CRAN_REPO=https://packagemanager.posit.co/cran/__linux__/jammy/${CRAN_SNAPSHOT}

# APT runs with default signature verification (fail-closed).
# If apt-get update fails due to expired keys in the digest-pinned base image,
# update the BASE_IMAGE digest to a newer build of rocker/r-ver.

# System build dependencies (dev headers, compilers)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4-openssl-dev \
    libssl-dev \
    libxml2-dev \
    libfontconfig1-dev \
    libfreetype6-dev \
    libpng-dev \
    libtiff5-dev \
    libjpeg-dev \
    libharfbuzz-dev \
    libfribidi-dev \
    && rm -rf /var/lib/apt/lists/*

# Install pinned pak version
RUN R -e "install.packages('pak', repos = '${CRAN_REPO}', version = '${PAK_VERSION}')"

# Install TRISK core packages — pinned to exact commit SHAs, no upgrades
RUN R -e "\
  options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); \
  pak::pak(c( \
    'Theia-Finance-Labs/trisk.model@${TRISK_MODEL_SHA}', \
    'Theia-Finance-Labs/trisk.analysis@${TRISK_ANALYSIS_SHA}' \
  ), ask = FALSE, upgrade = FALSE)"

# Install Shiny UI dependencies — from the frozen CRAN snapshot
RUN R -e "\
  options(repos = c(CRAN = Sys.getenv('CRAN_REPO'))); \
  pak::pak(c( \
    'httpuv', 'htmltools', 'shiny', 'shinydashboard', 'shinyWidgets', \
    'shinyjs', 'DT', 'ggplot2', 'plotly', 'readr', 'data.table', 'writexl', \
    'jsonlite', 'dplyr', 'tidyr' \
  ), ask = FALSE, upgrade = FALSE)"

# Verify core packages load
RUN R -e "\
  suppressPackageStartupMessages({ \
    library(trisk.model); library(trisk.analysis); library(shiny) \
  }); \
  message('trisk.model  ', packageVersion('trisk.model')); \
  message('trisk.analysis ', packageVersion('trisk.analysis'))"

# ============================================================
# Stage 2: runtime — lean image, no compilers / dev headers
# ============================================================
FROM ${BASE_IMAGE} AS runtime

ARG SCENARIOS_SHA256

LABEL maintainer="Theia Finance Labs <info@theia-finance-labs.com>"
LABEL description="TRISK Climate Transition Risk Stress Testing Tool"
LABEL version="1.0.0"
LABEL org.opencontainers.image.source="https://github.com/Theia-Finance-Labs/trisk.docker"

# Runtime-only shared libraries (no -dev, no compilers)
RUN apt-get update && apt-get install -y --no-install-recommends \
    libcurl4 \
    libssl3 \
    libxml2 \
    libfontconfig1 \
    libfreetype6 \
    libpng16-16 \
    libtiff5 \
    libjpeg8 \
    libharfbuzz0b \
    libfribidi0 \
    libpq5 \
    curl \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Copy compiled R library tree from builder
COPY --from=builder /usr/local/lib/R /usr/local/lib/R

# Scenario data vendored in build context (no runtime network fetch).
# Run `bash scripts/download_scenarios.sh` before first build, or supply your own.
# Build FAILS on checksum mismatch — no silent fallback for secure builds.
COPY data/scenarios/scenarios.csv /opt/trisk/data/scenarios/scenarios.csv
RUN echo "${SCENARIOS_SHA256}  /opt/trisk/data/scenarios/scenarios.csv" | sha256sum -c -

# Copy Shiny application (path no longer implies shiny-server)
RUN mkdir -p /opt/trisk/app
COPY app/ /opt/trisk/app/

# Copy CLI and test scripts
RUN mkdir -p /opt/trisk/scripts
COPY scripts/run_analysis.R /opt/trisk/scripts/
COPY scripts/test_continuity.R /opt/trisk/scripts/
COPY scripts/test_portfolio_pd.R /opt/trisk/scripts/
COPY scripts/test_debug.R /opt/trisk/scripts/
COPY scripts/run_tests.R /opt/trisk/scripts/

# Startup script (Rscript avoids interactive-session banner in logs)
RUN echo '#!/bin/bash' > /usr/local/bin/run-shiny.sh && \
    echo "Rscript -e \"shiny::runApp('/opt/trisk/app', host='0.0.0.0', port=3838)\"" >> /usr/local/bin/run-shiny.sh && \
    chmod +x /usr/local/bin/run-shiny.sh

# Create non-root user for runtime
RUN groupadd -r trisk && useradd -r -g trisk -d /home/trisk -s /sbin/nologin trisk

# Data mount points
RUN mkdir -p /data/input /data/output && \
    chown -R trisk:trisk /data/output /opt/trisk/app

WORKDIR /opt/trisk/app

# Ensure R/Shiny write temp files to /tmp (mounted as tmpfs in compose)
ENV TMPDIR=/tmp

# Run as non-root
USER trisk

EXPOSE 3838

# Healthcheck: verify Shiny returns HTTP 200 on the root URL
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
    CMD curl -fsS http://localhost:3838/ -o /dev/null -w '%{http_code}' | grep -q 200 || exit 1

CMD ["/usr/local/bin/run-shiny.sh"]
