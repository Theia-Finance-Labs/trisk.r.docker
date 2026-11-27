# download_scenarios.R
# Pre-downloads public scenario data so banks don't need internet access after deployment.
# Standalone / dev alternative to scripts/download_scenarios.sh (which the docs use);
# the Docker build verifies the same SHA256 again when it copies the file in.
# Override the expected checksum with SCENARIOS_SHA256 if you supply other data.

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "/opt/trisk/data/scenarios"

message("========================================")
message("Downloading TRISK Scenario Data")
message("========================================")
message(paste("Output directory:", output_dir))

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

scenarios_url  <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs/scenarios.csv"
scenarios_file <- file.path(output_dir, "scenarios.csv")

message(paste("\nDownloading from:", scenarios_url))

# The file is ~60 MB; R's default 60 s timeout aborts it on slower links
options(timeout = max(600, getOption("timeout")))
download.file(
  url      = scenarios_url,
  destfile = scenarios_file,
  mode     = "wb",
  quiet    = FALSE
)

if (!file.exists(scenarios_file)) {
  stop("Download completed but file not found!")
}

file_size <- file.size(scenarios_file)
message(paste("\nSuccess! Downloaded:", scenarios_file))
message(paste("File size:", format(file_size, big.mark = ","), "bytes"))

# Quick structural validation
test_read <- read.csv(scenarios_file, nrows = 5)
message(paste("Columns found:", ncol(test_read)))
message(paste("Column names:", paste(names(test_read), collapse = ", ")))

# ── Checksum verification (same hash as download_scenarios.sh and the Dockerfiles) ──
expected_sha <- Sys.getenv("SCENARIOS_SHA256",
                           unset = "b181045bd27628e427d541ddffc860a63e3b3f8148ccaa01d708cfbbc29b4b56")
message("\nVerifying SHA256 checksum...")
# sha256sum (Linux, Git Bash) or shasum (macOS); certutil on a plain Windows console
actual_sha <- if (nzchar(Sys.which("sha256sum"))) {
  sub("\\s+.*$", "", system2("sha256sum", shQuote(scenarios_file), stdout = TRUE))
} else if (nzchar(Sys.which("shasum"))) {
  sub("\\s+.*$", "", system2("shasum", c("-a", "256", shQuote(scenarios_file)), stdout = TRUE))
} else if (nzchar(Sys.which("certutil"))) {
  gsub("\\s", "", system2("certutil", c("-hashfile", shQuote(scenarios_file), "SHA256"), stdout = TRUE)[2])
} else {
  unlink(scenarios_file)
  stop("No SHA256 tool found (sha256sum, shasum or certutil); cannot verify the download.")
}
if (!identical(actual_sha, expected_sha)) {
  unlink(scenarios_file)
  stop(paste0(
    "Checksum mismatch!\n",
    "  Expected: ", expected_sha, "\n",
    "  Actual:   ", actual_sha
  ))
}
message("Checksum OK.")

message("\n========================================")
message("Scenario download complete")
message("========================================\n")
