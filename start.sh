#!/bin/bash
set -e

# Selects what the container does at runtime.
#   MODE=app      -> run the Shiny app on port 3838 (default)
#   MODE=rstudio  -> run RStudio Server on port 8787 (rocker's /init)
MODE="${MODE:-app}"

if [ "$MODE" = "app" ]; then
  echo "Starting Shiny app on port 3838"
  exec R -e "shiny::runApp('/srv/shiny-server/shinyApp', host = '0.0.0.0', port = 3838)"
elif [ "$MODE" = "rstudio" ]; then
  echo "Starting RStudio Server on port 8787 (user: rstudio)"
  exec /init
else
  echo "Unknown MODE: '$MODE'. Use MODE=app or MODE=rstudio." >&2
  exit 1
fi
