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
      "Pareto scaling (mean-centered and scaled by the square root of the standard deviation)" = "pareto_scaling",
      "natural logarithm" = "ln"
    ),
    selected = "none"
  ) %>% helper(type = "markdown", content = "PreProcessing_Procedures"),
  uiOutput(outputId = "DESeq_formula_main_ui"),
  uiOutput(outputId = "DESeq_formula_sub_ui"),
  uiOutput(outputId = "batch_effect_ui"),
  actionButton(
    inputId = "Do_preprocessing",
    label = "Get Pre-Processing",
    icon(name = "fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  uiOutput("violin_plot_color_ui")
)


pre_processing_main_panel <- mainPanel(
  id = "mainpanel_pre_processing",
  # Statistics to the data
  div(
    id="data_summary",
    helpText("General statistics to the input data, stuff like dimensions"),
    htmlOutput(outputId = "Statisitcs_Data"),
    HTML(text = "<br>"),
    fluidRow(
      column(6,
             h4("Raw Data"),
             plotOutput("raw_violin_plot"),
             plotOutput("raw_kde_plot")
      ),
      column(6,
             h4("Pre-processed Data"),
             plotOutput("preprocessed_violin_plot"),
             plotOutput("preprocessed_kde_plot")
      )
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