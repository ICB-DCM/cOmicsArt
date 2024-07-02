significance_analysis_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_significance_analysis",
    h5(" ") %>% helper(type = "markdown", content = "SigAna_Choices"),
    uiOutput(outputId = ns("type_of_comparison_ui")),
    uiOutput(outputId = ns("chooseComparisons_ui")),
    # UI to choose test method
    uiOutput(outputId = ns("chooseTest_ui")),
    uiOutput(outputId = ns("chooseSignificanceLevel_ui")),
    uiOutput(outputId = ns("chooseTestCorrection_ui")),
    # Button to start analysis
    actionButton(
      inputId = ns("significanceGo"),
      label = "Get Significance Analysis"
    ),
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh UI"
    )),
    hr(style = "border-top: 1px solid #858585;")
  )
}

# Main panel
significance_analysis_main_ui <- function(ns){
  mainPanel(
    id = "main_significance_analysis",
    htmlOutput(outputId = ns("significance_analysis_info"), container = pre),
    tabsetPanel(
      id = ns("significance_analysis_results"),
      tabPanel(
        title = "Result Visualization",
        plotOutput(outputId = ns("Significant_Plot_final")),
        # UI to select comparisons to visualize
        splitLayout(  # Only used for questionmark
          cellWidths = c("26%", "74%"),
          cellArgs = list(style = "padding: 5px"),
          h4("Visualization Choices") %>% helper(type = "markdown", content = "SigAna_Vis"),
          NULL
        ),
        uiOutput(outputId = ns("chooseComparisonsToVisualize_ui")),
        uiOutput(outputId = ns("chooseVisualization_ui")),
        # Choose what genes to look at (e.g. significant, upregulated, downregulated)
        uiOutput(outputId = ns("chooseGenesToLookAt_ui")),
        hr(style = "border-top: 1px solid #000000;"),
        # Choose intersections to highlight
        splitLayout(
          cellWidths = c("33%", "67%"),
          cellArgs = list(style = "padding: 5px"),
          uiOutput(
            outputId = ns("chooseIntersections_ui")
          ),
          NULL
        ),
        # Download and Report UI
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          actionButton(
            inputId = ns("only2Report_Sig"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            outputId = ns("getR_Code_Sig"),
            label = "Get underlying R code and data",
            icon = icon("code")
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            outputId = ns("SavePlot_Sig"),
            label = "Save plot",
            class = "btn-info"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            inputId = ns("file_ext_Sig"),
            label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"),
            selected = ".png"
          )
        ),
      ),
    ),
  )
}

# Complete UI
significance_analysis_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = "Significance Analysis",
    id = "significance_analysis",
    fluid = T,
    h4("Significance Analysis"),
    # sidebar
    significance_analysis_sidebar_ui(ns),
    # main
    significance_analysis_main_ui(ns)
  )
}