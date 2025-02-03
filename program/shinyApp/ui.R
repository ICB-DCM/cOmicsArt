## Server 2.0

# eigentlich getestet auf 4.1.2
# setwd("program")
# if(!(renv::status()$synchronized)){
#  renv::restore(lockfile = "renv.lock")
# }

library(DT)
library(plotly)
library(waiter)
library(shiny, lib.loc = .libPaths()[1])
library(shinyWidgets)
library(shinymanager)
library(shinyjs)
library(DESeq2)
library(grid)
library(ggplot2)
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
library(sva)
library(pcaPP) # requires gfortran. Not sure how to install on server
library(reshape2)
# library(svglite)
library(formattable)

source("R/C.R")
source("R/module_DownloadReport.R",local=T)
# source the uis for each panel here
source("R/help_tab/ui.R",local=T)
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
  # Loading Bars?
  # useWaitress(),
  useWaiter(),
  use_cicerone(),
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
      .shinyhelper-container {
        font-size: 24px;
        color: darkred !important;
      }
      .well button {
      background-color: white;
      color: black;
      border: 2px solid darkgrey;
      font-size: 15px;
      font-weight: bold;
      box-shadow: 3px 3px 5px rgba(255, 0, 0, 0.8);
      padding: 5px 5px;
      border-radius: 10px;
      }

      #shiny-disconnected-overlay {
        background-color: grey;
        opacity: 1;
        z-index: 99999 !important;
        color: white;
        font-size: 20px;
        text-align: center;
        display: flex;
        align-items: center;
        justify-content: center;
        pointer-events: auto;  /* Allow pointer events */
        padding: 20px;
        line-height: 1.5;
      }
      #shiny-disconnected-overlay a {
        color: #add8e6;  /* Light blue color for links */
        text-decoration: underline;
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
    ")),
    tags$script(HTML("
      $(document).on('shiny:disconnected', function(event) {
        function checkOverlay() {
          var overlay = $('#shiny-disconnected-overlay');
          if (overlay.length) {
            console.log('Overlay found, updating content');  // Debugging line
            overlay.html(
              '<div style=\"text-align: center; line-height: 1.5;\">' +
              'Connection lost.<br>You need to <a href=\"#\" onclick=\"location.reload();\" style=\"color: #add8e6;\">refresh the page</a> to start again.<br>' +
              'There can be multiple reasons, such as an unstable internet connection. If you reproduce this behavior, ' +
              'please report the steps/clicks you took!<br>This would help all of us—developers, contributors, and users ❤️<br>' +
              'Report best through <a href=\"https://github.com/ICB-DCM/cOmicsArt/issues/new/choose\" target=\"_blank\" style=\"color: #add8e6; margin: 0 5px;\">GitHub</a> ' +
              'or email to <a href=\"mailto:cOmicsArtist@outlook.de\" style=\"color: #add8e6; margin: 0 5px;\">cOmicsArtist@outlook.de</a>.' +
              '</div>'
            );
          } else {
            setTimeout(checkOverlay, 100);  // Retry after 100ms
          }
        }
        checkOverlay();
      });
    
    // Function to get a cookie value by name
    function getCookie(name) {
      const cname = name + '=';
      const decodedCookie = decodeURIComponent(document.cookie);
      const ca = decodedCookie.split(';');
      for(let i = 0; i < ca.length; i++) {
        let c = ca[i].trim();
        if (c.indexOf(cname) == 0) return c.substring(cname.length, c.length);
      }
      return '';
    }

    // Function to set a cookie
    function setCookie(name, value, days) {
      const d = new Date();
      d.setTime(d.getTime() + (days*24*60*60*1000));
      const expires = 'expires=' + d.toUTCString();
      document.cookie = name + '=' + value + ';' + expires + ';path=/';
    }

    // Function to delete a cookie
    function deleteCookie(name) {
      document.cookie = name + '=; expires=Thu, 01 Jan 1970 00:00:00 UTC; path=/;';
    }

    // Check if the 'hasBeenBefore' cookie is present
    function checkHasBeenBeforeCookie() {
      return getCookie('hasBeenBefore') === 'true';
    }
    
   // Listen for changes on the checkbox and set the cookie if checked
    document.addEventListener('click', function(event) {
      if (event.target && event.target.id === 'set_cookie_checkbox') {
        const isChecked = document.getElementById('set_cookie_checkbox').checked;
        if (isChecked) {
          setCookie('hasBeenBefore', 'true', 30);
        } else {
          deleteCookie('hasBeenBefore');
        }
      }
    });
    "))
  ),
  ##########
  use_cicerone(),
  #useShinyalert(),
  shinyjs::useShinyjs(),
  ##########
  div(
    style = "display: inline-block; float:right;",
    # Quit App Button
      actionButton(
        inputId = "Quit_App",
        label = "Quit App",
        class = "btn-secondary"
      )
  ),
  div(
    id = "TitleID_normal",
    column(
      width = 1,
      tags$img(src = "Logo_cOmicsArt_clear.png", height = "100%", width = "100%")
    ),
    h1(
      "cOmicsArt",
      style = "background: linear-gradient(to right, #EC0014 8%, #FD8D33 10%, #3897F1 12%, #FFD335 14%, #A208BA 16%, #EF0089 18%, #EC0014 20%);
      background-size: 100%;
      -webkit-background-clip: text; 
      -webkit-text-fill-color: transparent;
      font-weight: bold;"
    )
  ),
  div(
    id = "UsefulLinks",
    splitLayout(
      cellWidths = c("75%", "5%", "20%"),
      DownloadReport_ui("DownloadTestModule"),
      NULL,
      div(
        style = "display: inline-block; float:left;",
        actionLink(
          inputId = "set_cookie",
          label = "Delete 'Skip first help' cookie",
          style = "font-size: 0.9em; color: #555; text-decoration: underline;"
        )
      )
    ),
    splitLayout(
      cellWidths = c("75%", "10%", "15%"),
      tags$a(
        href = "https://icb-dcm.github.io/cOmicsArt/", 
        target = "_blank",
        tagList(
          icon("book"),  # Replace "book" with any other suitable icon
          span("Go To Documentation", style = "margin-left: 5px;")
        ),
        style = "font-size: 18px; color: black; text-decoration: underline;"
      ),
      NULL
    ),
    splitLayout(
      cellWidths = c("75%", "10%", "15%"),
      tags$a(
        href = "https://lea-orga.notion.site/12eab506afb581bf8ecfeeb2bb07c319", 
        target = "_blank",
        tagList(
          icon("comment-dots"),  # Replace "comment-dots" with another icon if desired
          span("Give Us Feedback!", style = "margin-left: 5px;")
        ),
        style = "font-size: 18px; color: black;text-decoration: underline;"
      ),
      NULL
    )
  ),

  tabsetPanel(
    id = "tabsetPanel1",
    ################################################################################
    # Tab Selection w Upload
    ################################################################################
    help_tab_panel,
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
