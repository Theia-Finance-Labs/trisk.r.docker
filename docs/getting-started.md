# Getting Started

Get TRISK Desktop running and produce your first stress test results in under 10 minutes.

## Prerequisites

| Requirement | Details |
|------------|---------|
| **Docker Desktop** | [Download for Windows/Mac](https://www.docker.com/products/docker-desktop/) or [Docker Engine for Linux](https://docs.docker.com/engine/install/) |
| **RAM** | 4 GB minimum allocated to Docker (8 GB recommended) |
| **Disk** | ~3 GB for the Docker image |
| **Browser** | Chrome, Edge, Firefox, or Safari |

!!! note "No R or programming knowledge required"
    Everything runs inside the Docker container. You only need Docker and a web browser.

## Step 1: Install Docker Desktop

Download and install from [docker.com/products/docker-desktop](https://www.docker.com/products/docker-desktop/).

After installation, open Docker Desktop and wait for the whale icon in your system tray (Windows) or menu bar (Mac) to show **Docker Desktop is running**.

!!! warning "Windows users"
    You may need to enable WSL 2 during installation. Docker Desktop will prompt you if needed.

## Step 2: Get the TRISK Desktop files

**Option A — Download ZIP** (no Git required):

1. Go to [github.com/Theia-Finance-Labs/trisk.r.docker](https://github.com/Theia-Finance-Labs/trisk.r.docker)
2. Click the green **Code** button → **Download ZIP**
3. Extract the ZIP to a folder (e.g., `C:\trisk` on Windows or `~/trisk` on Mac)

**Option B — Clone with Git** (if Git is installed):

```bash
git clone https://github.com/Theia-Finance-Labs/trisk.r.docker.git
cd trisk.r.docker
```

## Step 3: Build and start

Open a terminal and navigate to the TRISK folder:

- **Windows:** press `Win + R`, type `cmd`, press Enter, then `cd C:\trisk\trisk.r.docker`
- **Mac:** open Terminal (Applications > Utilities), then `cd ~/trisk/trisk.r.docker`

Run this single command:

```bash
docker compose -f docker-compose.dev.yml up --build -d
```

The first build takes ~10 minutes (downloads R and installs packages). After that, starts take seconds.

## Step 4: Verify the container is running

**Using Docker Desktop** (easiest):

1. Open Docker Desktop → go to the **Containers** tab
2. You should see `trisk.r.docker` or `trisk-app-dev` with a **green** running status
3. Click the container name to see logs — wait until you see `Listening on http://0.0.0.0:3838`

**Or from the terminal:**

```bash
docker compose -f docker-compose.dev.yml logs -f
```

## Step 5: Open the application

Open your browser and go to:

> **[http://localhost:3838](http://localhost:3838)**

You should see the TRISK Desktop welcome screen.

!!! tip "Not loading?"
    - Make sure Docker Desktop shows the container as **Running** (green icon)
    - Try refreshing the page after 30 seconds — the app takes a moment to initialize on first start
    - Check [Troubleshooting](troubleshooting.md) if issues persist

## Run the demo

Once the app loads:

1. Click **1. Upload Data** in the sidebar
2. Click the green **Load Demo Data from trisk.model** button
3. Click **2. Configure Analysis** — default scenarios are already selected
4. Click **3. Run Analysis**, then click the **Run TRISK Analysis** button
5. Wait ~30 seconds for the model to complete
6. Explore your results in **4. Portfolio Results**

The demo loads a synthetic portfolio across Oil&Gas, Coal, and Power sectors with matching asset-level production data.

## Using your own data

To analyze your own portfolio:

1. Prepare your CSV files following the [Data Preparation](data-preparation.md) guide
2. Either upload through the browser interface, or place files in `data/input/` before starting the container
3. Follow the same workflow: Upload → Configure → Run → Review

## Starting and stopping

**Using Docker Desktop** (recommended):

1. Open Docker Desktop → **Containers** tab
2. Click **▶ Play** to start, **⏹ Stop** to shut down
3. No rebuild needed for restarts — your data and results are preserved

**Using the terminal:**

```bash
# Stop
docker compose -f docker-compose.dev.yml down

# Restart (no rebuild needed)
docker compose -f docker-compose.dev.yml up -d

# Rebuild after updating the code
docker compose -f docker-compose.dev.yml up --build -d
```

## Next steps

- [Data Preparation](data-preparation.md) — format your portfolio files
- [Step 1: Upload Data](workflow/01-upload.md) — detailed upload walkthrough
- [Security Architecture](security.md) — what your IT team needs to know
- [Troubleshooting](troubleshooting.md) — if something doesn't work
