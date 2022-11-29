significance_analysis_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_significance_analysis",
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
        title = "Result Visualization",
        # UI for visualization Plot
        plotOutput(outputId = ns("Significant_Plot_final")),
        # UI to select comparisons to visualize
        uiOutput(outputId = ns("chooseComparisonsToVisualize_ui")),
        # UI to choose visualization method
        uiOutput(outputId = ns("chooseVisualization_ui")),
        # UI to choose what genes to llok at (e.g. significant, upregulated, downregulated)
        uiOutput(outputId = ns("chooseGenesToLookAt_ui"))
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