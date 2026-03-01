# download_scenarios.R
# Pre-downloads public scenario data so banks don't need internet access after deployment
# This script is run during Docker build

args <- commandArgs(trailingOnly = TRUE)
output_dir <- if (length(args) > 0) args[1] else "/opt/trisk/data/scenarios"

message("========================================")
message("Downloading TRISK Scenario Data")
message("========================================")
message(paste("Output directory:", output_dir))

# Ensure output directory exists
if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

# Download the public scenarios file from GCS
scenarios_url <- "https://storage.googleapis.com/crispy-public-data/trisk_inputs/scenarios.csv"
scenarios_file <- file.path(output_dir, "scenarios.csv")

message(paste("\nDownloading from:", scenarios_url))

tryCatch({
  download.file(
    url = scenarios_url, 
    destfile = scenarios_file, 
    mode = "wb",
    quiet = FALSE
  )
  
  # Verify download
  if (file.exists(scenarios_file)) {
    file_size <- file.size(scenarios_file)
    message(paste("\nSuccess! Downloaded:", scenarios_file))
    message(paste("File size:", format(file_size, big.mark = ","), "bytes"))
    
    # Quick validation - try to read first few lines
    test_read <- read.csv(scenarios_file, nrows = 5)
    message(paste("Columns found:", ncol(test_read)))
    message(paste("Column names:", paste(names(test_read), collapse = ", ")))
  } else {
    stop("Download completed but file not found!")
  }
  
}, error = function(e) {
  warning(paste("Failed to download scenarios:", e$message))
  warning("The container will still work, but users will need to upload scenarios manually.")
})

message("\n========================================")
message("Scenario download complete")
message("========================================\n")
