significance_analysis_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_significance_analysis",
    h5(" ") %>% helper(type = "markdown", content = "SigAna_Choices"),
    uiOutput(outputId = ns("UseBatch_ui")),
    uiOutput(outputId = ns("type_of_comparison_ui")),
    uiOutput(outputId = ns("chooseComparisons_ui")),
    # UI to choose test method
    uiOutput(outputId = ns("chooseTest_ui")),
    sliderInput(
      inputId = ns("significance_level"),
      label = "Significance level",
      min = 0.005,
      max = 0.1,
      value = 0.05,
      step = 0.005
    ),
    selectInput(
      inputId = ns("test_correction"),
      label = "Multiple testing correction",
      choices = c(
        "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
        "Holm", "Hommel", "Hochberg", "FDR"
      ),
      selected = "Benjamini-Hochberg"
    ),
    # Button to start analysis
    actionButton(
      inputId = ns("significanceGo"),
      label = "Get Differential Analysis",
      icon = icon("fas fa-mouse-pointer")
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
    div(id = "Significance_div",
      tabsetPanel(
        id = ns("significance_analysis_results"),
        tabPanel(
          title = "Result Visualization",
          value = "Multiple_Comparisons_Visualizations",
          plotOutput(outputId = ns("Significant_Plot_final")),
          # UI to select comparisons to visualize
          splitLayout(  # Only used for questionmark
            cellWidths = c("26%", "74%"),
            cellArgs = list(style = "padding: 15px"),
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
            cellArgs = list(style = "padding: 15px"),
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
              choices = c(".png", ".tiff", ".svg", ".pdf"),
              selected = ".png"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
            cellArgs = list(style = "padding: 15px"),
            div(textAreaInput(
              inputId = ns("NotesSigAna"),
              label = "Notes:",
              placeholder = NOTES_PlACEHOLDER,
              width = "1000px"
            ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText(NOTES_HELP)),
            NULL
          )
        ),
      ),
    )
  )
}

# Complete UI
significance_analysis_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = tagList(tags$span("5. Differential Analysis")), 
    value = "Differential Analysis",
    id = "significance_analysis",
    fluid = T,
    h4("Differential Analysis"),
    # sidebar
    significance_analysis_sidebar_ui(ns),
    # main
    significance_analysis_main_ui(ns)
  )
}