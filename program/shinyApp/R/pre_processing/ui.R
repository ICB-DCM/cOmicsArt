pre_processing_sidebar_panel <- sidebarPanel(
  id = "sidebar_pre_processing",
  #########################################
  # Do Center & scaling + potential other pre-processing stuff
  #########################################
  # this could be enhanced with personalized procedures
  radioButtons(
    inputId = "PreProcessing_Procedure",
    label = "Pre-Processing Procedures",
    choices = list(
      "No pre-processing" = "none",
      "Omic-specific filtering of low abundance" = "filterOnly",
      "DESeq2 pre-processing (including variance stabilising transformation)" = "vst_DESeq",
      "centering to 0 and scaling" = "simpleCenterScaling",
      "scaling values to be within 0 and 1" = "Scaling_0_1",
      "log10" = "log10",
      "log2" = "log2",
      "Pareto scaling (mean-centered and scaled by the square root of the standard deviation)" = "pareto_scaling",
      "natural logarithm" = "ln"
    ),
    selected = "none"
  ) %>% helper(type = "markdown", content = "PreProcessing_Procedures"),
  uiOutput(outputId = "DESeq_formula_sub_ui"),
  uiOutput(outputId = "batch_effect_ui") %>% helper(type = "markdown", content = "PreProcessing_Batch"),
  actionButton(
    inputId = "Do_preprocessing",
    label = "Get Pre-Processing",
    icon(name = "fas fa-mouse-pointer")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  uiOutput("violin_plot_color_ui")
)


pre_processing_main_panel <- mainPanel(
  id = "mainpanel_pre_processing",
  # Statistics to the data
  htmlOutput(outputId = "Statisitcs_Data", container = pre),
  div(
    id = "data_summary",
    HTML(text = "<br>"),
    fluidRow(
      column(
        6,
        h4("Raw Data"),
        plotOutput("raw_violin_plot"),
        plotOutput("raw_kde_plot")
      ),
      column(
        6,
        h4("Pre-processed Data"),
        plotOutput("preprocessed_violin_plot"),
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      actionButton(
        inputId = "only2Report_Preprocess",
        label = "Send only to Report",
        class = "btn-info"
      )
    ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = "getR_Code_Preprocess",
        label = "Get underlying R code and data",
        icon = icon(name = "code")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = "SavePlot_Preprocess",
        label = "Save plot",
        class = "btn-info"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      radioGroupButtons(
        inputId = "file_ext_Preprocess",
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"),
        selected = ".png"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
      cellArgs = list(style = "padding: 15px"),
      div(textAreaInput(
        inputId = "NotesPreprocessedData",
        label = "Notes:",
        placeholder = NOTES_PlACEHOLDER,
        width = "1000px"
      ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
      helpText(NOTES_HELP)),
      NULL
    )
  )
  )



pre_processing_panel <- tabPanel(
  title = "Pre-processing",
  id = "pre_processing_panel",
  fluid = T,
  h4("Data Pre-processing"),
  pre_processing_sidebar_panel,
  pre_processing_main_panel
)