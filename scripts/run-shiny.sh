#!/usr/bin/env bash
set -euo pipefail
exec Rscript -e "shiny::runApp('/opt/trisk/app', host=Sys.getenv('SHINY_HOST','0.0.0.0'), port=as.integer(Sys.getenv('SHINY_PORT','3838')))"
