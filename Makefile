# Makefile for TRISK Docker operations

.PHONY: build build-cli run stop logs shell clean help test

# Default target
help:
	@echo "TRISK Docker - Available commands:"
	@echo ""
	@echo "  make build      - Build the Shiny app Docker image"
	@echo "  make build-cli  - Build the CLI Docker image"
	@echo "  make run        - Start the container"
	@echo "  make stop       - Stop the container"
	@echo "  make logs       - View container logs"
	@echo "  make shell      - Open shell in container"
	@echo "  make clean      - Remove containers and images"
	@echo ""

# Build Shiny image
build:
	docker compose build trisk

# Build CLI image  
build-cli:
	docker build -f Dockerfile.cli -t theia-finance-labs/trisk-cli:latest .

# Run container (see README "Quick Start" for the one-time .env / TLS setup)
run:
	@grep -q "^TRISK_PASS_HASH=.\+" .env 2>/dev/null || { echo "Set TRISK_PASS_HASH in .env first (see .env.example)"; exit 1; }
	@test -f tls/cert.pem -a -f tls/key.pem || { echo "Missing tls/cert.pem or tls/key.pem (see README)"; exit 1; }
	docker compose up -d
	@echo ""
	@echo "TRISK is starting..."
	@echo "Open https://localhost/ in your browser"
	@echo ""

# Stop container
stop:
	docker compose down

# View logs
logs:
	docker compose logs -f

# Open shell in container
shell:
	docker exec -it trisk-app /bin/bash

# Run R console in container
r-console:
	docker exec -it trisk-app R

# Clean up
clean:
	docker compose down -v --rmi local
	docker image prune -f

# Run tests inside the container
test:
	docker compose exec trisk Rscript /opt/trisk/scripts/run_tests.R

# Rebuild from scratch (no cache)
rebuild:
	docker compose build --no-cache
	docker compose up -d
