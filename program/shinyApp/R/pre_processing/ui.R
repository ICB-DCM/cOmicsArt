pre_processing_sidebar_panel <- sidebarPanel(
  id = "sidebar_pre_processing",
  #########################################
  # Do Center & scaling + potential other pre-processing stuff
  #########################################
  # this could be enhanced with personalized procedures
  # First dropdown: Choose Processing Type
  selectInput(
    inputId = "processing_type",
    label = "Choose Processing Type",
    choices = c("No pre-processing",
                "Filtering",
                "Omic-Specific",
                "Log-Based",
                "Miscellaneous"),
    selected = NULL,
    multiple = FALSE
  ) %>% helper(type = "markdown", content = "PreProcessing_Procedures"),

  # Second dropdown: Options based on Processing Type
  uiOutput(outputId = "dynamic_options_ui"),

  # Additional UI elements based on the selected option
  uiOutput(outputId = "additional_inputs_filter_ui"),


  uiOutput(outputId = "formula_sub_ui"),
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
        plotOutput("raw_violin_plot")
      ),
      column(
        6,
        h4("Pre-processed Data"),
        plotOutput("preprocessed_violin_plot"),
      )
    ),
    h4("Mean and Standard Deviation of preprocessed data"),
    plotOutput("mean_sd_plot"),
    fluidRow(column(4, ""), column(
      4,
      h5("Mean and SD Plot Download"),
      actionButton(
        inputId = "only2Report_mean_sd_plot",
        label = "Send only to Report",
        class = "btn-info"
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      downloadButton(
          outputId = "getR_Code_mean_sd_plot",
          label = "Get underlying R code and data",
          icon = icon(name = "code")
      ),
      downloadButton(
          outputId = "SavePlot_mean_sd_plot",
          label = "Save plot",
          class = "btn-info"
      ),
      radioGroupButtons(
        inputId = "file_type_mean_sd_plot",
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"),
        selected = ".png"
      )
    ), column(
      4,
      h5("Violin Plot Download"),
      actionButton(
        inputId = "only2Report_Preprocess",
        label = "Send only to Report",
        class = "btn-info"
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      downloadButton(
          outputId = "getR_Code_Preprocess",
          label = "Get underlying R code and data",
          icon = icon(name = "code")
      ),
      downloadButton(
          outputId = "SavePlot_Preprocess",
          label = "Save plot",
          class = "btn-info"
      ),
      radioGroupButtons(
        inputId = "file_type_Preprocess",
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"),
        selected = ".png"
      )
    )),
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
  title = tagList(tags$span("2. Pre-processing")), 
  value = "Pre-processing",
  id = "pre_processing_panel",
  fluid = T,
  h4("Data Pre-processing"),
  pre_processing_sidebar_panel,
  pre_processing_main_panel
)