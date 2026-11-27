# Troubleshooting

Common issues and solutions when running TRISK Desktop.

## Installation issues

### Docker Desktop is not running

**Symptom:** `Cannot connect to the Docker daemon`

**Fix:** Open Docker Desktop and wait for it to fully start. The whale icon in your system tray should be steady (not animating).

### Build fails with network error

**Symptom:** `Could not resolve host` during `docker compose build`

**Fix:** Internet access is required during the build step to download R packages. After build completes, the app runs fully offline.

### Build fails with disk space error

**Symptom:** `no space left on device`

**Fix:** The TRISK image requires ~3 GB. Free disk space or run `docker system prune` to remove unused Docker images and containers.

## Runtime issues

### App doesn't load in browser

**Check the container is running:**

```bash
docker compose -f docker-compose.dev.yml ps
```

**Check the logs:**

```bash
docker compose -f docker-compose.dev.yml logs trisk
```

Look for `Listening on http://0.0.0.0:3838` to confirm the app started.

### Port 3838 already in use

**Symptom:** Container starts but browser shows connection refused

=== "Mac/Linux"

    ```bash
    lsof -i :3838
    ```

=== "Windows"

    ```bash
    netstat -ano | findstr :3838
    ```

**Fix:** Stop the process using port 3838, or change the port in `docker-compose.dev.yml`:

```yaml
ports:
  - "127.0.0.1:4000:3838"  # Use port 4000 instead
```

Then access the app at `http://localhost:4000`.

### Out of memory

**Symptom:** Container killed unexpectedly, or R session crashes during analysis

**Fix:**

1. Open Docker Desktop > Settings > Resources > increase Memory to 8 GB+
2. For very large portfolios (1000+ companies), consider splitting into sector-level batches

## Data issues

### Upload validation errors

!!! tip "Column names are case-sensitive"
    `company_id` is not the same as `Company_ID` or `CompanyId`. Use exact lowercase names.

**Missing required columns** - check column names match the [Data Dictionary](reference/data-dictionary.md) exactly.

**Wrong data types** - ensure numeric columns contain only numbers (no currency symbols, commas, or text).

**Empty file** - verify the CSV is not empty and uses comma delimiters (not semicolons or tabs).

### No companies matched

**Symptom:** Analysis runs but produces 0 results

**Cause:** The `company_id` values in your portfolio don't match any entries in the assets database.

**Fix:**

- Verify your portfolio uses the same `company_id` values as the assets file
- If using the bundled assets database, check that your companies are covered
- Try the demo data first to confirm the app works, then troubleshoot your data

### Scenario not available

**Symptom:** Warning message about missing scenario for selected geography

**Fix:** Some scenarios are only available for "Global" geography. Switch to Global in the Configure tab, or check the [Scenario Library](reference/scenarios.md) for available geographies.

## Performance

### Analysis takes too long

Expected run times:

| Portfolio size | Approximate time |
|---------------|-----------------|
| 10 companies | ~10 seconds |
| 100 companies | ~30-60 seconds |
| 500 companies | ~2-5 minutes |
| 1000+ companies | 5-15 minutes |

Multiple shock years multiply the time proportionally.

**To speed up analysis:**

- Reduce the number of shock years (use 1 instead of 3)
- Increase Docker memory and CPU allocation
- Split large portfolios into sector-specific batches

## FAQ

**Can I run TRISK Desktop offline?**

:   Yes. After the initial Docker build (which requires internet), the app runs fully offline with no external network calls.

**What browsers are supported?**

:   Chrome 90+, Firefox 90+, Edge 90+, Safari 14+.

**Can I use my own scenario data?**

:   Yes. Upload a CSV with the expected columns on the Upload Data tab. See the [Data Dictionary](reference/data-dictionary.md) for the schema.

**How do I update to a new version?**

:   Pull the latest code and rebuild:

    ```bash
    git pull
    docker compose -f docker-compose.dev.yml up --build -d
    ```

**Where are results stored?**

:   Results are stored in the `data/output/` directory on your host machine (mounted into the container). Audit logs go to `data/output/audit/`.

**Can multiple users share one installation?**

:   The development configuration supports one user at a time. The production configuration with Caddy proxy supports multiple authenticated users, but Shiny sessions are single-threaded per connection.
