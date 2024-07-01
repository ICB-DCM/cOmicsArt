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
library(msigdbr)
library(tidyr)
library(kableExtra)
library(readxl)
library(ggvenn)
library(ComplexUpset)
library(gridExtra)
# library(svglite)

source("R/C.R")
source("R/module_DownloadReport.R",local=T)
# source the uis for each panel here
source("R/data_selection/ui.R",local=T)
source("R/pre_processing/ui.R",local=T)
source("R/pca/ui.R",local=T)
source("R/heatmap/ui.R",local=T)
source("R/single_gene_visualisation/ui.R",local=T)
source("R/enrichment_analysis/ui.R",local=T)
source("R/sample_correlation/ui.R",local = T)
source("R/significance_analysis/ui.R",local=T)

options(repos = BiocManager::repositories())
options(spinner.color = "#1c8a3b", spinner.color.background = "#ffffff", spinner.size = 2)

ui <- shiny::fluidPage(
  # JS to reset input values
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  tags$head(
    ##########
    # Styling Setting
    ##########
    tags$style(HTML("
      body {
        background-color: #f8f7fa;
      }
      body, label, input, button, select, h4, h3, h2 {
        color: #3b3b3b;
      }
      .tabbable > .nav > li > a {
         background-color: #dedede;
         color: black
      }
      .tabbable > .nav > li[class=active] > a {
          background-color: #7a7e80; color:black
      }
      .custom-modal .modal-dialog {
        width: 90%;
        max-width: 90%;
      }
      #sidebar_data_selection {
          background-color: #70BF4F47;
      }
      #sidebar_pre_processing {
          background-color: #3897F147;
      }
      #sidebar_sampleCorrelation {
          background-color: #A208BA35;
      }
      #sidebar_pca {
          background-color: #FD8D3347;
      }
      #sidebar_significance_analysis {
          background-color: #FFD33547;
      }
      #sidebar_heatmap {
          background-color: #70BF4F47;
      }
      #sidebar_single_gene_visualisation {
          background-color: #3897F147;
      }
      #sidebar_enrichment_analysis {
          background-color: #A208BA35;
      }
      .tabbable > .nav > li > a[data-value='Data selection'] {
         background-color: #70BF4F47 !important; /* Lighter Green */
         color: black
      }
      .tabbable > .nav > li[class=active] > a[data-value='Data selection'] {
        background-color: #70BF4F !important; /* Strong Green */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='Pre-processing'] {
        background-color: #3897F147 !important; /* Lighter Blue */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Pre-processing'] {
        background-color: #3897F1 !important; /* Strong Blue */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='Sample Correlation'] {
        background-color: #A208BA35 !important; /* Lighter Purple */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Sample Correlation'] {
        background-color: #A208BA !important; /* Strong Purple */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='PCA'] {
        background-color: #FD8D3347 !important; /* Lighter Orange */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='PCA'] {
        background-color: #FD8D33 !important; /* Strong Orange */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='Significance Analysis'] {
        background-color: #FFD33547 !important; /* Lighter Yellow */
        color: black !important;
      }
      .tabbable > .nav > li > a[data-value='Heatmap'] {
        background-color: #70BF4F47 !important; /* Lighter Green */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Heatmap'] {
        background-color: #70BF4F !important; /* Strong Green */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='Single Gene Visualisations'] {
        background-color: #3897F147 !important; /* Lighter Blue */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Single Gene Visualisations'] {
        background-color: #3897F1 !important; /* Strong Blue */
        color: white !important;
      }
      .tabbable > .nav > li > a[data-value='Enrichment Analysis'] {
        background-color: #FD8D3347 !important; /* Lighter Orange */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Enrichment Analysis'] {
        background-color: #A208BA35 !important; /* Lighter Purple */
        color: black !important;
      }
      .tabbable > .nav > li[class=active] > a[data-value='Significance Analysis'] {
        background-color: #A208BA !important; /* Strong Purple */
        color: white !important;
      }
  "))
  ),
  ##########
  use_cicerone(),
  #useShinyalert(),
  shinyjs::useShinyjs(),
  ##########
  div(
    style = "display:inline-block; float:right",
    actionButton(
    inputId = "Quit_App",
    label = "Quit App",
    class = "btn-secondary"
    )
  ),
  hidden(selectInput(
    "element",
    label = "PrideMonth?",
    choices = c(0, 1),
    selected = ifelse(format(as.POSIXct(Sys.time()), "%m") == "07", 1, 0)
  )),
  conditionalPanel(
    condition = "input.element == 0",
    div(
      id = "TitleID_normal",
      column(width=1, tags$img(src = "Logo_cOmicsArt_clear.png", height="100%", width="100%")),
      h1(HTML('<span style="color:#EC0014">c</span><span style="color:#FD8D33">O</span><span style="color:#3897F1">m</span><span style="color:#FFD335">i</span><span style="color:#A208BA">c</span><span style="color:#EF0089">s</span><span style="color:#EC0014">A</span><span style="color:#FD8D33">r</span><span style="color:#3897F1">t</span>'))
    ),
  ),
  conditionalPanel(
    condition = "input.element == 1",
    div(
      id = "TitleID_pride",
      h2(HTML('<span style="color:#E75A5A">S</span><span style="color:#E7AF5A">h</span><span style="color:#CBE75A">i</span><span style="color:#76E75A">n</span><span style="color:#5AE792">y</span><span style="color:#5AE7E7">O</span><span style="color:#5A92E7">m</span><span style="color:#765AE7">i</span><span style="color:#CB5AE7">c</span><span style="color:#E75AAF">s</span>'))
      ),
  ),
  splitLayout(
    cellWidths = c("75%", "10%", "15%"),
    DownloadReport_ui("DownloadTestModule"),
    NULL
  ),

  tabsetPanel(
    id = "tabsetPanel1",
    ################################################################################
    # Tab Selection w Upload
    ################################################################################
    data_selection_panel,
    pre_processing_panel,
    sampleCorrelation_UI("sample_correlation"),
    pca_UI("PCA"),
    significance_analysis_UI("SignificanceAnalysis"),
    heatmap_UI("Heatmap"),
    single_gene_visualisation_UI("single_gene_visualisation"),
    enrichment_analysis_UI("EnrichmentAnalysis")
  ),
  hidden(selectInput(
    "element_02",
    label = "AuthorBirthdays",
    choices = c(0, 1, 2),
    selected = if(format(as.POSIXct(Sys.time()), "%d-%m") == "22-11"){
      1  # Lea's Birthday
    } else if (format(as.POSIXct(Sys.time()), "%d-%m") == "19-12"){
      2  # Paul's Birthday
    } else {
      0  # No Birthday
    }
  )),
  conditionalPanel(
    condition = "input.element_02 == 0",
    absolutePanel("Brought to you by Lea Seep & Paul Jonas Jost",
                         bottom = 0, left = 10, fixed = TRUE)
  ),
  conditionalPanel(
    condition = "input.element_02 == 1",
    div(
      id = "foot_birthday",
      absolutePanel(
        "It is Lea's birthday today :)",
        bottom = 0, left = 10, fixed = TRUE,style = "background-color: #a9d96a;"
      )
    )
  ),
  conditionalPanel(
    condition = "input.element_02 == 2",
    div(
      id = "foot_birthday_Paul",
      absolutePanel(
        "It is Paul's birthday today :)",
        bottom = 0, left = 10, fixed = TRUE,style = "background-color: #a9d96a;"
      )
    )
  ),
  # Pannel displaying the current session id
  absolutePanel(
    # text output needs to be defined in server
    textOutput("session_id"),
    bottom = 0, right = 10, fixed = TRUE
  )
)
