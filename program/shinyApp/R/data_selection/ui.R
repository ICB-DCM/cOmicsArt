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
      HTML('<span style="font-size: 20px; font-weight: bold;">Upload</span> <small><a href="https://github.com/ICB-DCM/cOmicsArt/blob/main/UploadHelpcOmicsArt.xlsx" target="_blank" download>Download an example Excel Workbook to prepare data upload</a></small>'),
      br(),br(),
        shiny::fileInput(
          inputId = "data_matrix1",
          label = HTML('1. Data matrix <small><a href="airway-read-counts-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        ) %>% helper(type = "markdown", content = "DataSelection_DataUploadFileInput"),
        shiny::fileInput(
          inputId = "data_sample_anno1",
          label = HTML('2. Sample annotation <small><a href="airway-sample-sheet-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        ),
        shiny::fileInput(
          inputId = "data_row_anno1",
          label = HTML('3. Entities annotation <small><a href="airway-entitie_description-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        ),
      actionLink(
        "clear_FileInput", 
        HTML("<span style='font-size: 16px; color: black;'>❌ Clear Data</span>"), 
        style = "cursor: pointer;"
      ),br(),br(),br(),
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
          style = "color: black; background-color: white; border-color: black;"
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
          style = "color: black; background-color: white; border-color: black;"
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
        ) %>% helper(type = "markdown", content = "DataSelection_MetaData",style = "font-size: 24px;"),
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
          style = "color: black; background-color: white; border-color: black;"
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
      uiOutput("testdata_help_text"),
        br(),
        actionButton(
          inputId = "EasyTestForUser",
          label = "Upload test data",
          style = "color: black; background-color: white; border-color: black;"
        ),
        hr(style = "border-top: 1px solid #858585;")
      )
    )
  )


data_selection_main_panel <- mainPanel(
  id = "mainPanel_DataSelection",
  div(id ="InfoBox_DataSelection",
      htmlOutput(outputId = "debug", container = pre)
      ),
  div(id = "mainPanel_other",
  # add link to toggle on the div geneAnno_toggle
  actionButton(
    inputId = "geneAnno_toggle_button",
    label = "(show/hide) Further entitie Annotation options",
    icon = icon('plus'),
    style = "color: #000000; background-color: transparent; border-color: transparent"
  ),
  div(
    id  = "geneAnno_toggle",
    style = "display: none;",
    class = "AddGeneSymbols_ui",
    uiOutput("AddGeneSymbols_organism_ui"),
    uiOutput("AddGeneSymbols_ui"),
    hr(style = "border-top: 1px solid #858585;")
  ),

  fluidRow(
    column(5,
           actionButton(
            "select_data", "[optional] Select Data",
            width = "100%",
            icon = icon('filter'),
            style = "color: #fffff; background-color: white; border-color: #000000"
            )
    )),
  br(),
  conditionalPanel(
    condition = "input.select_data % 2 == 0",
    fluidRow(column(5,
        actionButton(
          "use_full_data", "Go to Preprocessing",
          width = "100%",
          icon = icon("fas fa-mouse-pointer"),
          style = "
          background-color: white;
          color: black;
          border: 2px solid darkgrey;
          font-size: 15px;
          font-weight: bold;
          box-shadow: 3px 3px 5px rgba(255, 0, 0, 0.8);
          padding: 5px 5px;
          border-radius: 10px;"
        )
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
    div(
      id = "SaveInputAsRDS",
      hr(style = "border-top: 1px solid #858585;"),
      downloadButton(
        outputId = "SaveInputAsList",
        label = "Save file input to upload later"
      ) %>% helper(type = "markdown", content = "DataSelection_compilation_help")
    ),
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
