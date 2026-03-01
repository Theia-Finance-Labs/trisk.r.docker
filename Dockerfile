# TRISK Climate Transition Risk Stress Testing Tool
# Docker container for local deployment at financial institutions

# Use rocker/r-ver which has ARM64 support
FROM rocker/r-ver:4.4.1

LABEL maintainer="Theia Finance Labs <info@theia-finance-labs.com>"
LABEL description="TRISK Climate Transition Risk Stress Testing Tool"
LABEL version="1.0.0"
LABEL org.opencontainers.image.source="https://github.com/Theia-Finance-Labs/trisk.docker"

# System dependencies for R packages
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
    curl \
    pandoc \
    && rm -rf /var/lib/apt/lists/*

# Install pak for fast package installation
RUN R -e "install.packages('pak', repos = 'https://cloud.r-project.org')"

# Copy package installation script
COPY scripts/install_packages.R /tmp/install_packages.R

# Install TRISK packages and all dependencies
# This layer is cached unless install_packages.R changes
RUN Rscript /tmp/install_packages.R

# Pre-download scenario data (optional but recommended for offline use)
# If download fails, app still works with mock data from trisk.model package
COPY scripts/download_scenarios.R /tmp/download_scenarios.R
RUN mkdir -p /opt/trisk/data/scenarios && \
    Rscript /tmp/download_scenarios.R /opt/trisk/data/scenarios || \
    echo "WARNING: Scenario download failed. App will use test data from trisk.model package."

# Copy Shiny application
RUN mkdir -p /srv/shiny-server/trisk
COPY app/ /srv/shiny-server/trisk/

# Copy CLI and test scripts
RUN mkdir -p /opt/trisk/scripts
COPY scripts/run_analysis.R /opt/trisk/scripts/
COPY scripts/test_continuity.R /opt/trisk/scripts/
COPY scripts/test_portfolio_pd.R /opt/trisk/scripts/
COPY scripts/test_debug.R /opt/trisk/scripts/
COPY scripts/run_tests.R /opt/trisk/scripts/

# Create a simple startup script that runs Shiny directly
RUN printf '#!/bin/bash\nR -e "shiny::runApp(\"/srv/shiny-server/trisk\", host=\"0.0.0.0\", port=3838)"\n' > /usr/local/bin/run-shiny.sh && \
    chmod +x /usr/local/bin/run-shiny.sh

# Create data mount point for bank's private data
RUN mkdir -p /data/input /data/output

# Set working directory
WORKDIR /srv/shiny-server/trisk

# Expose Shiny port
EXPOSE 3838

# Health check
HEALTHCHECK --interval=30s --timeout=10s --start-period=120s --retries=3 \
    CMD curl -f http://localhost:3838/ || exit 1

# Run Shiny app directly (no Shiny Server needed)
CMD ["/usr/local/bin/run-shiny.sh"]
