setwd("program")
renv::restore(lockfile = "renv.lock")
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinymanager)
library(shinyjs) 
library(DESeq2) 
library(grid)
library(pheatmap)

########
# Set Up security 
########
# credentials <- data.frame(
#   user = c("Clivia", "Lea"), # mandatory
#   password = c("Cii@31", "Lea"), # mandatory
#   #start = c("2019-04-15"), # optinal (all others)
#   #expire = c(NA, "2019-12-31"),
#   admin = c(FALSE, TRUE),
#   comment = "Log In to Run secret Shiny",
#   stringsAsFactors = FALSE
# )
source("fun_filterRNA.R")
source("fun_plotPCA.R")
source("fun_LFC.R")
source("fun_volcano.R")
source("fun_popupModal.R")
source("server_dev.R")
source("ui.R")

shinyApp(ui,server)