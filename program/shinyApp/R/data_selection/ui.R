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
    uiOutput(outputId = "AddGeneSymbols_ui"),
    uiOutput(outputId = "AddGeneSymbols_organism_ui")
  ),
  #uiOutput("AddGeneSymbols_organism_ui"),
  actionButton(
    inputId = "refresh1",
    label = "Do"
  ),
  div(
    class = "LineToDistinguish",
    hr(style = "border-top: 1px solid #000000;")
  ),
  div(
    class = "DataSelection",
    h4("Row selection - biochemical entities") %>% helper(type = "markdown", content = "DataSelection_RowSelection"),
    uiOutput(outputId = "providedRowAnnotationTypes_ui"),
    uiOutput(outputId = "row_selection_ui"),
    uiOutput(outputId = "propensityChoiceUser_ui")
  ),
  # Outlier Selection -> for fixed removal pre-processing needs to be redone!
  div(
    class = "SampleSelection",
    h4("Sample selection"),
    uiOutput(outputId = "providedSampleAnnotationTypes_ui"),
    uiOutput(outputId = "sample_selection_ui")),
    uiOutput(outputId = "NextPanel_ui")
)


data_selection_main_panel <- mainPanel(
  id = "mainPanel_DataSelection",
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "Upload section",
      br(),
      hr(style = "border-top: 2px solid #90DBF4;"),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("85%", "10%", "5%"),
        actionButton(
          inputId = "EasyTestForUser",
          label = "Start straight away with a test-dataset!",
          icon = icon('paper-plane'),
          style = "color: #fffff; background-color: #90DBF4; border-color: #000000"
        ),
        actionButton(
          inputId = "Reset",
          label = "Reset"
        ) %>% helper(type = "markdown", content = "DataSelection_Reset"),
        NULL
      ),
      hr(style = "border-top: 2px solid #90DBF4;"),
      a(id = "toggleAdvanced",
        "Data Upload via file input",
        style = "background-color: #90DBF4; color: black; padding: 7px 10px; "
        ) %>% helper(type = "markdown", content = "DataSelection_DataUploadFileInput"),
        shinyjs::hidden(
          div(
        id = "advanced",
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          uiOutput(outputId = "data_matrix1_ui") %>% helper(type = "markdown", content = "DataSelection_Matrix"),
          uiOutput(outputId = "data_sample_anno1_ui") %>% helper(type = "markdown", content = "DataSelection_SampleAnno")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          uiOutput(outputId = "data_row_anno1_ui") %>% helper(type = "markdown", content = "DataSelection_RowAnno"),
          uiOutput(
            outputId = "data_preDone_ui"
          ) %>% helper(type = "markdown", content = "DataSelection_SummarizedExp")
        )
        )
      ),
      hr(style = "border-top: 2px solid #90DBF4;"),
      uiOutput(outputId = "metadataInput_ui") %>% helper(type = "markdown", content = "DataSelection_MetaData"),
      hr(style = "border-top: 2px solid #90DBF4;"),
      downloadButton(
        outputId = "SaveInputAsList",
        label = "Save file input to upload later"
      ) %>% helper(type = "markdown", content = "DataSelection_compilation_help"),
      htmlOutput(outputId = "debug", container = pre),
      HTML(text = "<br>"),
      HTML(text = "<br>")),
      tabPanel(
        title = "Upload visual inspection",
        helpText("If you have uploaded your data, you might want to visually check the tables to confirm the correct data format. If you notice irregualarities you will need to correct the input data - this cannot be done in ShinyOmics, See the help on how your data is expected."),
        actionButton(
          inputId = "DoVisualDataInspection",
          label = "Upload data for visual inspection"
        ) %>% helper(type = "markdown", content = "DataSelection_UploadInspection"),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput("DataMatrix_VI"),
          htmlOutput(outputId = "DataMatrix_VI_Info", container = pre)
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput("SampleMatrix_VI"),
          htmlOutput(outputId = "SampleMatrix_VI_Info", container = pre)
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput("EntitieMatrix_VI"),
          htmlOutput(outputId = "EntitieMatrix_VI_Info", container = pre)
        ),
        htmlOutput(outputId = "OverallChecks", container = pre)
      )
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