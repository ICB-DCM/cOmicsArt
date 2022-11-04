data_selection_sidebar_panel <- sidebarPanel(
  id = "sidebar_data_selection",
  div(class = "omicType",
      selectInput(
        inputId = "omicType", # RNAorLIPID
        label = "Omic Type that is uploaded",
        choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
        selected = ""
      )
  ),
  div(
    class = "AddGeneSymbols_ui",
    uiOutput("AddGeneSymbols_ui"),
    uiOutput("AddGeneSymbols_organism_ui")
  ),
  #uiOutput("AddGeneSymbols_organism_ui"),
  actionButton(
  inputId = "refresh1",
  label = "Do"
  ),
  div(class="LineToDistinguish",hr(style = "border-top: 1px solid #000000;")),
  div(class="DataSelection",
      h4("Row selection -  biochemical entities"),
      uiOutput("providedRowAnnotationTypes_ui"),
      uiOutput("row_selection_ui"),
      uiOutput("propensityChoiceUser_ui")),
      # Outlier Selection -> for fixed removal pre-processing needs to be redone!
  div(class="SampleSelection",
      h4("Sample selection"),
      uiOutput("providedSampleAnnotationTypes_ui"),
      uiOutput("sample_selection_ui")),
    uiOutput("NextPanel_ui")
)


data_selection_main_panel <- mainPanel(
  id="mainPanel_DataSelection",
  tabsetPanel(
    type = "pills",
    tabPanel(
  title="Upload section",
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("85%", "10%", "5%"),
    NULL,
    actionButton(
    inputId = "Reset", 
    label = "Reset"
    ),
    NULL
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
    uiOutput("data_matrix1_ui"),
    uiOutput("data_sample_anno1_ui")
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
    uiOutput("data_row_anno1_ui"),
    uiOutput("data_preDone_ui") %>% helper(type = "markdown", content = "SummarizedExp_help")
  ),
  hr(style = "border-top: 2px solid #cbedca;"),
  uiOutput("metadataInput_ui"),
  hr(style = "border-top: 2px solid #cbedca;"),
  downloadButton(
    "SaveInputAsList",
    label = "Save file input to upload later") %>% helper(type = "markdown", content = "compilation_help"),
  htmlOutput("debug", container = pre),
  HTML("<br>"),
  HTML("<br>")),
  tabPanel(
    title="Upload visual inspection",
    helpText("If you have uploaded your data, you might want to visually check the tables to confirm the correct data format. If you notice irregualarities you will need to correct the input data - this cannot be done in ShinyOmics, See the help on how your data is expected."),
    actionButton(
      inputId = "DoVisualDataInspection",
      label = "Upload data for visual inspection"
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      DT::dataTableOutput("DataMatrix_VI"),
      htmlOutput("DataMatrix_VI_Info", container = pre)
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      DT::dataTableOutput("SampleMatrix_VI"),
      htmlOutput("SampleMatrix_VI_Info", container = pre)
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      DT::dataTableOutput("EntitieMatrix_VI"),
      htmlOutput("EntitieMatrix_VI_Info", container = pre)
    ),
    htmlOutput("OverallChecks", container = pre)
  )
  )
)


data_selection_panel <- tabPanel(
  "Data selection",
  id= "Data_selection",
  fluid = T,
  h4("Data Selection"),
  ################################################################################
  # Data Selection
  ################################################################################
  data_selection_sidebar_panel,
  data_selection_main_panel
)