single_gene_visualisation_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_single_gene_visualisation",
    uiOutput(outputId = ns("type_of_data_gene_ui")),
    uiOutput(outputId = ns("type_of_visualitsation_ui")),
    uiOutput(outputId = ns("Select_GeneAnno_ui")),
    uiOutput(outputId = ns("Select_Gene_ui")),
    helpText("Note: if you choose a group rather than a single entitie, the values will be summarized by taking the median"),

    actionButton(
      inputId = ns("singleGeneGo"), 
      label = "Get single gene visualisation"
      ),
    # hidden Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh"
    )),
    hr(style = "border-top: 1px solid #858585;"),
    uiOutput(outputId = ns("accross_condition_ui"))
  )
}

single_gene_visualisation_main_ui <- function(ns){
  mainPanel(
    id = "main_single_gene_visualisation",
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("50%", "50%"),
      plotOutput(outputId = ns("SingleGenePlot")),
      NULL
    ),
    uiOutput(outputId = ns("chooseComparisons_ui")),
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("70%", "30%"),
      NULL,
      actionButton(
        inputId = ns("only2Report_SingleEntities"),
        label = "Send only to Report",
        class = "btn-info"
      ),
    ),
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("getR_Code_SingleEntities"),
        label = "Get underlying R code and data",
        icon = icon("code")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("SavePlot_singleGene"),
        label = "Save plot",
        class = "btn-info"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      radioGroupButtons(
        inputId = ns("file_ext_singleGene"),
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"),
        selected = ".png"
      )
    ),
    textAreaInput(
      inputId = ns("NotesSingleEntities"),
      label = "Notes:",
      placeholder = NOTES_PlACEHOLDER,
      width = "1000px"
    ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
    helpText(NOTES_HELP)
  )
}


single_gene_visualisation_UI <- function(id){
  ns <- NS(id)
  single_gene_visualisation_panel <- tabPanel(
    title = "Single Gene Visualisations",
    id = "single_gene_visualisation",
    fluid = TRUE,
    h4("Single Gene Visualisations"),
    #########################################
    # Single Gene Visualisations
    #########################################
    single_gene_visualisation_sidebar <- single_gene_visualisation_sidebar_ui(ns),
    single_gene_visualisation_main <- single_gene_visualisation_main_ui(ns)
  )
}
