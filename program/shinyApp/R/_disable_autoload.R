# Shiny must not source the files in R/ itself: global.R sources
# R/SourceAll.R once per app, in its order.
# Source: https://shiny.posit.co/r/reference/shiny/1.7.5/loadsupport.html