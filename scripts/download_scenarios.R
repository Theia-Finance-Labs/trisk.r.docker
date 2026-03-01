# download_scenarios.R
# Pre-downloads public scenario data so banks don't need internet access after deployment.
# This script is run during Docker build.
#
# When invoked from the Dockerfile, integrity is verified via sha256sum in a
# separate RUN step.  This script is kept for standalone / dev use and performs
# its own optional checksum check when SCENARIOS_SHA256 is set.

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

# ── Optional checksum verification ──
expected_sha <- Sys.getenv("SCENARIOS_SHA256", unset = "")
if (nzchar(expected_sha)) {
  message("\nVerifying SHA256 checksum...")
  actual_sha <- tools::md5sum(scenarios_file)  # R lacks native sha256; use system
  result <- system2("sha256sum", scenarios_file, stdout = TRUE)
  actual_sha <- sub("\\s+.*$", "", result)
  if (!identical(actual_sha, expected_sha)) {
    stop(paste0(
      "Checksum mismatch!\n",
      "  Expected: ", expected_sha, "\n",
      "  Actual:   ", actual_sha
    ))
  }
  message("Checksum OK.")
}

message("\n========================================")
message("Scenario download complete")
message("========================================\n")
