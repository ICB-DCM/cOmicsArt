data_selection_sidebar_panel <- sidebarPanel(
  id = "sidebar_data_selection",
    tabsetPanel(
      tabPanel("File Input",
      br(),
        div(class = "omic_type",
            selectInput(
              inputId = "omic_type_file_input",
              label = "Omic Type that is uploaded",
              choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
              selected = "",
              width = "80%"
            )
        ),
        shiny::fileInput(
          inputId = "data_matrix1",
          label = HTML('Upload data matrix <br/><small>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv" download>Download example data (Transcriptomics, human)</a></small>'),
          accept = c(".csv", ".xlsx"),
          width = "80%"
        ) %>% helper(type = "markdown", content = "DataSelection_DataUploadFileInput"),
        shiny::fileInput(
          inputId = "data_sample_anno1",
          label = HTML('Upload sample annotation <br/><small>(rows must be samples)<br/><a href="airway-sample-sheet-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv", ".xlsx"),
          width = "80%"
        ),
        shiny::fileInput(
          inputId = "data_row_anno1",
          label = HTML('Upload entities annotation matrix <br/><small>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv", ".xlsx"),
          width = "80%"
        ),
        actionButton(
          inputId = "inspect_data",
          label = "Inspect data",
          icon = icon('eye'),
          width = "80%",
        ) %>% helper(type = "markdown", content = "DataSelection_UploadInspection"),
        br(), br(), br(),
        actionButton(
          inputId = "refresh_file_input",
          label = "Upload new data",
          width = "80%",
          icon = icon('paper-plane'),
          style = "color: #fffff; background-color: #90DBF4; border-color: #000000"
        ),
        hr(style = "border-top: 1px solid #858585;")
      ),
      tabPanel("Precompiled",
      br(),
        div(class = "omic_type",
            selectInput(
              inputId = "omic_type_precompiled",
              label = "Omic Type that is uploaded",
              choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
              selected = "",
              width = "80%"
            )
        ),
        shiny::fileInput(
          inputId = "data_preDone",
          label = HTML('Load precompiled data <br/><small>(saved in this procedure or type SummarizedExperiment)<br/> <a href="Transcriptomics_only_precompiled-LS.RDS" download> Download example data</a></small>'),
          accept = ".RDS",
          width = "80%"
        ) %>% helper(type = "markdown", content = "DataSelection_SummarizedExp"),
        br(), br(),
        actionButton(
          inputId = "refresh_precompiled",
          label = "Upload new data",
          width = "80%",
          icon = icon('paper-plane'),
          style = "color: #fffff; background-color: #90DBF4; border-color: #000000",
        ),
        hr(style = "border-top: 1px solid #858585;")
      ),
      tabPanel("Metadata",
      br(),
        div(class = "omic_type",
            selectInput(
              inputId = "omic_type_metadata",
              label = "Omic Type that is uploaded",
              choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
              selected = "",
              width = "80%"
            )
        ),
        shiny::fileInput(
          inputId = "data_matrix_metadata",
          label = HTML('Upload data matrix <br/><small>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv" download>Download example data (Transcriptomics, human)</a></small>'),
          accept = c(".csv", ".xlsx"),
          width = "80%"
        ) %>% helper(type = "markdown", content = "DataSelection_MetaData"),
        shiny::fileInput(
          inputId = "metadataInput",
          label = HTML("Upload your Meta Data Sheet <small>(currently replaces sample annotation)</small>"),
          accept = c(".xlsx"),
          buttonLabel = list(icon("folder"),"Simply upload your Metadata Sheet!"),
          width = "80%"
        ),
        shiny::fileInput(
          inputId = "data_row_anno_metadata",
          label = HTML('Upload entities annotation matrix <br/><small>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv", ".xlsx"),
          width = "80%"
        ),
        br(), br(),
        actionButton(
          inputId = "refresh_metadata",
          label = "Upload new data",
          width = "80%",
          icon = icon('paper-plane'),
          style = "color: #fffff; background-color: #90DBF4; border-color: #000000",
        ),
        hr(style = "border-top: 1px solid #858585;")
      ),
      tabPanel("Testdata",
      br(),
        div(class = "omic_type",
            selectInput(
              inputId = "omic_type_testdata",
              label = "Omic Type that is uploaded",
              choices = c("Transcriptomics"),
              selected = "",
              width = "80%"
            )
        ),
        br(),
        actionButton(
          inputId = "EasyTestForUser",
          label = "Start straight away with a test-dataset!",
          icon = icon('paper-plane'),
          style = "color: #fffff; background-color: #90DBF4; border-color: #000000"
        ),
        hr(style = "border-top: 1px solid #858585;")
      )
    )
  )


data_selection_main_panel <- mainPanel(
  id = "mainPanel_DataSelection",
  div(
    class = "AddGeneSymbols_ui",
    uiOutput("AddGeneSymbols_organism_ui"),
    uiOutput("AddGeneSymbols_ui")
  ),
  fluidRow(
    column(6, actionButton(
      "select_data", "Select Data",
      width = "100%",
      icon = icon('filter'),
      style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
    )),
    column(6,actionButton(
      "use_full_data", "Use Full Dataset",
      width = "100%",
      icon = icon('rocket'),
      style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
    ))
  ),
  conditionalPanel(
    condition = "input.select_data % 2 == 1",
    hr(style = "border-top: 1px solid #858585;"),
    fluidRow(
      column(5,
             div(class = "DataSelection",
                 h4("Row selection - biochemical entities"),
                 uiOutput("providedRowAnnotationTypes_ui"),
                 uiOutput("row_selection_ui"),
                 uiOutput("propensityChoiceUser_ui")
             )),
      column(6,
             div(class = "SampleSelection",
                 h4("Sample selection") %>% helper(type = "markdown", content = "DataSelection_RowSelection"),
                 uiOutput("providedSampleAnnotationTypes_ui"),
                 uiOutput("sample_selection_ui")
             ))
    ),
    hr(style = "border-top: 1px solid #858585;"),
    div(
      id = "SaveInputAsRDS",
      downloadButton(
        outputId = "SaveInputAsList",
        label = "Save file input to upload later"
      ) %>% helper(type = "markdown", content = "DataSelection_compilation_help")
    ),
    htmlOutput(outputId = "debug", container = pre),
    br(), br(), br(),
    hr(style = "border-top: 1px solid #858585;"),
    actionButton(
      inputId = "NextPanel",
      label = "Start the Journey",
      width = "100%",
      icon = icon('rocket'),
      style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
    ),
  ),
  # hidden button
  hidden(actionButton(
    inputId = "refresh1",
    label = "YOu should not be seeing this"
  ))
)


data_selection_panel <- tabPanel(
  title = "Data selection",
  id = "Data_selection",
  fluid = T,
  h4("Data Selection"),
  ################################################################################
  # Data Selection
  ################################################################################
  data_selection_sidebar_panel,
  data_selection_main_panel
)
