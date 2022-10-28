## Server 2.0

# eigentlich getestet auf 4.1.2
# setwd("program")
# if(!(renv::status()$synchronized)){
#  renv::restore(lockfile = "renv.lock")
# }

library(DT)
library(plotly)
library(shiny, lib.loc = .libPaths()[1])
library(shinyWidgets)
library(shinymanager)
library(shinyjs)
library(DESeq2)
library(grid)
library(pheatmap)
library(pathview)
library(clusterProfiler)
library(BiocManager)
library(shinyhelper)
library(dplyr)
library(shinycssloaders)
library(ggpubr)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(jsonlite)
library(rmarkdown)
library(tinytex)
library(testthat)
library(shinytest)
library(biomaRt)
library(zip)
library(cicerone)
library(shinyalert)
# library(svglite)

source("R/module_DownloadReport.R",local=T)

# source the uis for each panel here
source("R/data_selection/ui.R",local=T)
source("R/pre_processing/ui.R",local=T)
source("R/pca/ui.R",local=T)
source("R/volcano_plot/ui.R",local=T)
source("R/heatmap/ui.R",local=T)
source("R/single_gene_visualisation/ui.R",local=T)
source("R/enrichment_analysis/ui.R",local=T)


options(repos = BiocManager::repositories())
options(spinner.color = "#1c8a3b", spinner.color.background = "#ffffff", spinner.size = 2)
########
# Set Up security
########
credentials <- data.frame(
  user = c("Clivia", "Lea"), # mandatory
  password = c("Cii@31", "Lea"), # mandatory
  # start = c("2019-04-15"), # optinal (all others)
  # expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Log In to Run secret Shiny",
  stringsAsFactors = FALSE
)
# source("fun_filterRNA.R",local = T)
# source("fun_plotPCA.R")
# source("fun_LFC.R")
# source("fun_volcano.R")
# source("fun_popupModal.R")
# source("fun_entitieSelection.R")

# source("server_dev.R")
# source("ui.R")



ui <- shiny::fluidPage(
  tags$head(
    ##########
    # Styling Setting
    ##########
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      body {
        background-color: #f8f7fa;
      }
      body, label, input, button, select, h4, h3, h2 {
        color: #3b3b3b;
      }
      .tabbable > .nav > li > a {
         background-color: #d5d4d6;
         color: #3b3b3b
      }
      #sidebar1 {
        background-color: #cbedca;
      }
      #sidebar2 {
        background-color: #edceec;
      }
      #sidebar3 {
        background-color: #cecef2;
      }
      #sidebar4 {
        background-color: #f2e6c2;
      }
      #sidebar5 {
        background-color: #d4c2f2;
      }
  "))
  ),
  ##########
  use_cicerone(),
  useShinyalert(),
  shinyjs::useShinyjs(),
  ##########

  div(style = "display:inline-block; float:right", actionButton(inputId = "Quit_App", label = "Quit App", class = "btn-secondary")),
  div(style = "display:inline-block; float:right", actionButton(inputId = "guide", label = "Guide me!", class = "btn-secondary")),
  div(style = "display:inline-block; float:right", helpText(" ", align = "right") %>% helper(type = "markdown", content = "Inital_help", size = "l", colour = "red", style = "zoom: 600%;")),
  hidden(selectInput("element", label = "PrideMonth?", choices = c(0, 1), selected = ifelse(format(as.POSIXct(Sys.time()), "%m") == "06", 1, 0))),
  conditionalPanel(
    condition = "input.element == 0",
    div(id="TitleID_normal",titlePanel("ShinyOmics")),
  ),
  conditionalPanel(
    condition = "input.element == 1",
    div(
      id="TitleID_pride",
      h2(HTML('<span style="color:#E75A5A">S</span><span style="color:#E7AF5A">h</span><span style="color:#CBE75A">i</span><span style="color:#76E75A">n</span><span style="color:#5AE792">y</span><span style="color:#5AE7E7">O</span><span style="color:#5A92E7">m</span><span style="color:#765AE7">i</span><span style="color:#CB5AE7">c</span><span style="color:#E75AAF">s</span>'))
      ),
  ),
  splitLayout(
    cellWidths = c("75%", "10%", "15%"),
    DownloadReport_ui("DownloadTestModule"),
    helpText("Metabolon Help", align = "center") %>% helper(type = "markdown", content = "Metabolon_help", size = "l", colour = "blue", style = "position: relative;top: -18px;left: 15px;; zoom: 200%;"),
    NULL
  ),
  
  tabsetPanel(
    id = "tabsetPanel1",
    # textOutput('debug', container = pre),

    ################################################################################
    # Tab Selection w Upload
    ################################################################################
    data_selection_panel,
    pre_processing_panel,
    pca_panel,
    volcano_plot_panel,
    heatmap_panel,
    single_gene_visualisation_panel,
    enrichment_analysis_tab_panel,
  ),
  
  
  
  hidden(selectInput("element_02", label = "LeasBirthday", choices = c(0, 1), selected = ifelse(format(as.POSIXct(Sys.time()), "%d-%m") == "30-09", 1, 0))),
  conditionalPanel(
    condition = "input.element_02 == 0",
    div(id="foot_normal",absolutePanel("Brought to you by Lea Seep", bottom = 0, left = 10, fixed = TRUE))
  ),
  conditionalPanel(
    condition = "input.element_02 == 1",
    div(id="foot_birthday",absolutePanel("Brought to you by Lea Seep - it is her birthday today :)", bottom = 0, left = 10, fixed = TRUE,style = "background-color: #a9d96a;"))
  )
  
)

# Wrap your UI with secure_app
# ui <- secure_app(ui)
