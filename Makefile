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
	docker-compose build trisk

# Build CLI image  
build-cli:
	docker build -f Dockerfile.cli -t theia-finance-labs/trisk-cli:latest .

# Run container
run:
	docker-compose up -d
	@echo ""
	@echo "TRISK is starting..."
	@echo "Open http://localhost:3838/trisk/ in your browser"
	@echo ""

# Stop container
stop:
	docker-compose down

# View logs
logs:
	docker-compose logs -f

# Open shell in container
shell:
	docker exec -it trisk-app /bin/bash

# Run R console in container
r-console:
	docker exec -it trisk-app R

# Clean up
clean:
	docker-compose down -v --rmi local
	docker image prune -f

# Run tests inside the container
test:
	docker-compose exec trisk Rscript /opt/trisk/scripts/run_tests.R

# Rebuild from scratch (no cache)
rebuild:
	docker-compose build --no-cache
	docker-compose up -d
