single_gene_visualisation_sidebar_ui<- function(ns){
  sidebarPanel(
    id = "sidebar_single_gene_visualisation",
    uiOutput(outputId = ns("type_of_data_gene_ui")) %>% helper(type = "markdown", content = "SingleGene_Options"),
    uiOutput(outputId = ns("type_of_visualitsation_ui")),
    uiOutput(outputId = ns("Select_GeneAnno_ui")),
    uiOutput(outputId = ns("Select_Gene_ui")),

    actionButton(
      inputId = ns("singleGeneGo"), 
      label = "Get Single Gene Visualisation",
      icon = icon("fas fa-mouse-pointer")
      ),
    # hidden Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh"
    )),
    hr(style = "border-top: 1px solid #858585;"),
    uiOutput(outputId = ns("accross_condition_ui")) %>% helper(type = "markdown", content = "SingleGene_Select")
  )
}

single_gene_visualisation_main_ui <- function(ns){
  mainPanel(
    id = "main_single_gene_visualisation",
    textOutput(outputId = ns("SingleGene_Info"), container = pre),
      div(id = "SingleGene_div",
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("50%", "50%"),
        plotOutput(outputId = ns("SingleGenePlot")),
        textOutput(outputId = ns("InfoText"))
      ),
      #h5(HTML("Note, that you only see boxplots if you have more than 3 samples per group")),
      uiOutput(outputId = ns("chooseComparisons_ui")),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = ns("only2Report_SingleEntities"),
          label = "Send only to Report",
          class = "btn-info"
        )
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
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
          choices = c(".png", ".tiff", ".pdf", ".svg"),
          selected = ".png"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
        cellArgs = list(style = "padding: 15px"),
        div(textAreaInput(
          inputId = ns("NotesSingleEntities"),
          label = "Notes:",
          placeholder = NOTES_PlACEHOLDER,
          width = "1000px"
        ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText(NOTES_HELP)),
        NULL
      ),
    )
  )
}


single_gene_visualisation_UI <- function(id){
  ns <- NS(id)
  single_gene_visualisation_panel <- tabPanel(
    title = tagList(tags$span("7. Single Entitie Visualisation")), 
    value = "Single Gene Visualisations",
    id = "single_gene_visualisation",
    fluid = TRUE,
    h4("Single Entitie Visualisations"),
    #########################################
    # Single Gene Visualisations
    #########################################
    single_gene_visualisation_sidebar_ui(ns),
    single_gene_visualisation_main_ui(ns)
  )
}
