### Upload module UI
# Two pieces that sit in the "1. Data selection" tab: the sidebar with the four
# sources, and the top of the main panel (status box, gene annotation and
# "Save file input to upload later").

upload_sidebar_ui <- function(id){
  ns <- NS(id)
  sidebarPanel(
    id = ns("sidebar_data_selection"),
    tabsetPanel(tabPanel(
      "File Input",
      br(),
      div(
        class = "omic_type",
        selectInput(
          inputId = ns("omic_type_file_input"),
          label = "Omic Type that is uploaded",
          choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
          selected = "",
          width = "80%"
        )
      ),
      HTML('<span style="font-size: 20px; font-weight: bold;">Upload</span><br> <small><a href="https://github.com/ICB-DCM/cOmicsArt/blob/main/UploadHelpcOmicsArt.xlsx" target="_blank" download>Download an example Excel Workbook to prepare data upload</a></small>'),
      br(),br(),
      div(
        id=ns("fileInputPlaceholder"),
        shiny::fileInput(
          inputId = ns("data_matrix1"),
          label = HTML('1. Data matrix  <br><small><a href="airway-read-counts-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        ) %>% helper(type = "markdown", content = "DataSelection_DataUploadFileInput"),
        shiny::fileInput(
          inputId = ns("data_sample_anno1"),
          label = HTML('2. Sample annotation <br><small><a href="airway-sample-sheet-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        ),
        shiny::fileInput(
          inputId = ns("data_row_anno1"),
          label = HTML('3. Entities annotation <br><small><a href="airway-entitie_description-LS.csv" download>Download example data</a></small>'),
          accept = c(".csv"),
          width = "80%"
        )
      ),
      actionLink(
        ns("clear_FileInput"),
        HTML("<span style='font-size: 16px; color: black;'>❌ Clear Data</span>"),
        style = "cursor: pointer;"
      ),br(),br(),br(),
      actionButton(
        inputId = ns("inspect_data"),
        label = "Inspect data",
        icon = icon('eye'),
        width = "80%",
      ) %>% helper(type = "markdown", content = "DataSelection_UploadInspection"),
      br(), br(), br(),
      actionButton(
        inputId = ns("refresh_file_input"),
        label = "Upload new data",
        width = "80%",
        icon = icon('paper-plane'),
        style = "color: black; background-color: white; border-color: black;"
      ),
      hr(style = "border-top: 1px solid #858585;")
    ), tabPanel(
      "Precompiled", br(),
      div(
        class = "omic_type",
        selectInput(
          inputId = ns("omic_type_precompiled"),
          label = "Omic Type that is uploaded",
          choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
          selected = "",
          width = "80%"
        )
      ),
      shiny::fileInput(
        inputId = ns("data_preDone"),
        label = HTML('Load precompiled data <br/><small>(saved in this procedure or type SummarizedExperiment)<br/> <a href="Transcriptomics_only_precompiled-LS.RDS" download> Download example data</a></small>'),
        accept = ".RDS",
        width = "80%"
      ) %>% helper(type = "markdown", content = "DataSelection_SummarizedExp"),
      br(), br(),
      actionButton(
        inputId = ns("refresh_precompiled"),
        label = "Upload new data",
        width = "80%",
        icon = icon('paper-plane'),
        style = "color: black; background-color: white; border-color: black;"
      ),
      hr(style = "border-top: 1px solid #858585;")
    ), tabPanel(
      "Metadata", br(),
      div(
        class = "omic_type",
        selectInput(
          inputId = ns("omic_type_metadata"),
          label = "Omic Type that is uploaded",
          choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
          selected = "",
          width = "80%"
        )
      ),
      shiny::fileInput(
        inputId = ns("data_matrix_metadata"),
        label = HTML('Upload data matrix <br/><small>(rows entities, cols samples) <br/><a href="airway-read-counts-LS.csv" download>Download example data (Transcriptomics, human)</a></small>'),
        accept = c(".csv", ".xlsx"),
        width = "80%"
      ) %>% helper(type = "markdown", content = "DataSelection_MetaData",style = "font-size: 24px;"),
      shiny::fileInput(
        inputId = ns("metadataInput"),
        label = HTML("Upload your Meta Data Sheet <small>(currently replaces sample annotation)</small>"),
        accept = c(".xlsx"),
        buttonLabel = list(icon("folder"),"Simply upload your Metadata Sheet!"),
        width = "80%"
      ),
      shiny::fileInput(
        inputId = ns("data_row_anno_metadata"),
        label = HTML('Upload entities annotation matrix <br/><small>(rows must be entities)<br/><a href="airway-entitie_description-LS.csv" download>Download example data</a></small>'),
        accept = c(".csv", ".xlsx"),
        width = "80%"
      ),
      br(), br(),
      actionButton(
        inputId = ns("refresh_metadata"),
        label = "Upload new data",
        width = "80%",
        icon = icon('paper-plane'),
        style = "color: black; background-color: white; border-color: black;"
      ),
      hr(style = "border-top: 1px solid #858585;")
    ), tabPanel(
      "Testdata", br(),
      div(
        class = "omic_type",
        selectInput(
          inputId = ns("omic_type_testdata"),
          label = "Omic Type that is uploaded",
          choices = c("Transcriptomics","Lipidomics","Metabolomics"),
          selected = "",
          width = "80%"
        )
      ),
      # Conditional help text based on omic type (client-side rendering for performance)
      conditionalPanel(
        condition = "input.omic_type_testdata == 'Transcriptomics'",
        ns = ns,
        HTML(EXAMPLE_RNA_DESCRIPTION)
      ),
      conditionalPanel(
        condition = "input.omic_type_testdata == 'Metabolomics'",
        ns = ns,
        HTML(EXAMPLE_METABO_DESCRIPTION)
      ),
      conditionalPanel(
        condition = "input.omic_type_testdata == 'Lipidomics'",
        ns = ns,
        HTML(EXAMPLE_LIPID_DESCRIPTION)
      ),
      br(),
      actionButton(
        inputId = ns("EasyTestForUser"),
        label = "Upload test data",
        style = "color: black; background-color: white; border-color: black;"
      ),
      hr(style = "border-top: 1px solid #858585;")
    ))
  )
}

upload_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    div(id = ns("InfoBox_DataSelection"),
        conditionalPanel(
          condition = "output.debug == null || output.debug == ''",
          ns = ns,
          pre("Please choose your type of File Input then Proceed by clicking 'Upload new data'")  # Initial message
        ),
        htmlOutput(outputId = ns("debug"), container = pre)
        ),
    # shown after the first successful Upload
    hidden(div(id = ns("upload_done"),
      # add link to toggle on the div geneAnno_toggle
      actionButton(
        inputId = ns("geneAnno_toggle_button"),
        label = "(show/hide) Further entitie Annotation options",
        icon = icon('plus'),
        style = "color: #000000; background-color: transparent; border-color: transparent"
      ),
      div(
        id  = ns("geneAnno_toggle"),
        style = "display: none;",
        class = "AddGeneSymbols_ui",
        uiOutput(ns("AddGeneSymbols_organism_ui")),
        uiOutput(ns("AddGeneSymbols_ui")),
        hr(style = "border-top: 1px solid #858585;")
      ),
      div(
        id = ns("SaveInputAsRDS"),
        downloadButton(
          outputId = ns("SaveInputAsList"),
          label = "Save file input to upload later"
        ) %>% helper(type = "markdown", content = "DataSelection_compilation_help"),
        hr(style = "border-top: 1px solid #858585;")
      )
    ))
  )
}
