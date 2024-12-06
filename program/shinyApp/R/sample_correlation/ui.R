sampleCorrelation_sidebar_panel <- function(ns){
  sidebarPanel(
    id = "sidebar_sampleCorrelation",
    h4("Sample Correlation") %>% helper(type = "markdown", content = "SampleCorr_Choices"),
    uiOutput(outputId = ns("UseBatch_ui")),
    selectInput(
      inputId = ns("corrMethod"),
      label = "Choose the correlation method",
      choices = c("pearson", "kendall", "spearman"),
      selected = "pearson"
    ),
    actionButton(
      inputId = ns("Do_SampleCorrelation"),
      label = "Get Sample Correlation",
      icon("fas fa-laptop-code")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(outputId = ns("SampleAnnotationChoice_ui")) %>% helper(type = "markdown", content = "SampleCorr_Color"),
    # hidden Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh"
    )),
  )
}


sampleCorrelation_main_panel <- function(ns){
  mainPanel(
  id = "main_sampleCorrelation",
  textOutput(outputId = ns("SampleCorr_Info"), container = pre),
  div(id = "div_sampleCorrelation_main_panel",
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("100%"),
      plotOutput(
        outputId = ns("SampleCorrelationPlot")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      actionButton(
        inputId = ns("only2Report_SampleCorrelation"),
        label = "Send only to Report",
        class = "btn-info"
      )
    ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("getR_SampleCorrelation"),
        label = "Get underlying R code and data",
        icon = icon(name = "code")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("SavePlot_SampleCorrelation"),
        label = "Save plot",
        class = "btn-info"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      radioGroupButtons(
        inputId = ns("file_ext_SampleCorrelation"),
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf", ".svg"),
        selected = ".png"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
      cellArgs = list(style = "padding: 15px"),
      div(textAreaInput(
        inputId = ns("NotesSampleCorrelation"),
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


sampleCorrelation_UI <- function(id){
  ns <- NS(id)
  
  tabPanel(
    title = "Sample Correlation",
    id = "sample_correlation",
    fluid = T,
    h4("Sample Correlation"),
    sampleCorrelation_sidebar_panel(ns),
    sampleCorrelation_main_panel(ns),
  )
}