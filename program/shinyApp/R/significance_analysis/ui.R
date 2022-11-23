significance_analysis_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_significance_analysis",
    # UI to choose whether to use raw data or normalised data
    uiOutput(outputId = ns("type_of_data_ui")),
    # UI to choose type of comparison
    uiOutput(outputId = ns("type_of_comparison_ui")),
    # UI to choose comparisons
    uiOutput(outputId = ns("chooseComparisons_ui")),
    # UI to choose test method
    uiOutput(outputId = ns("chooseTest_ui")),
    # UI to choose significance level
    uiOutput(outputId = ns("chooseSignificanceLevel_ui")),
    # UI to choose test correction
    uiOutput(outputId = ns("chooseTestCorrection_ui")),
    # Button to start analysis
    actionButton(
      inputId = ns("significanceGo"),
      label = "Get significance analysis"
      ),
    hr(style = "border-top: 1px solid #858585;")
  )
}

# Main panel
significance_analysis_main_ui <- function(ns){
  mainPanel(
    id = "main_significance_analysis",
    tabsetPanel(
      id = ns("significance_analysis_results"),
      tabPanel(
        title = "Results"
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