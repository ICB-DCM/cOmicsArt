pre_processing_sidebar_panel <- sidebarPanel(
  id = "sidebar_pre_processing",
  #########################################
  # Do Center & scaling + potential other pre-processing stuff
  #########################################
  h5("Pre-Processing Procedures"),  # this could be enhanced with personalized procedures
  radioButtons(
    inputId = "PreProcessing_Procedure",
    label = "Pre-Processing Procedures",
    choices = c(
      "none", "vst_DESeq", "simpleCenterScaling", "Scaling_0_1",
      "log10", "pareto_scaling", "ln"
    ),
    selected = "none"
  ),
  uiOutput(outputId = "DESeq_formula_main_ui"),
  uiOutput(outputId = "DESeq_formula_sub_ui"),
  switchInput(
      inputId = "DESeq_show_advanced",
      label = "Advanced formula options for DESeq2",
      inline = T,
      size = "mini",
      value = F
    ),
  uiOutput(outputId = "DESeq_formula_advanced_ui"),
  actionButton(
    inputId = "Do_preprocessing",
    label = "Pre-Process",
    icon(name = "fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;")
)


pre_processing_main_panel <- mainPanel(
  id = "mainpanel_pre_processing",
  # Statistics to the data
  helpText("general statistics to the input data, stuff like dimensions"),
  # hidden(div(id = 'Spinner_Statisitcs_Data', plotOutput("Statisitcs_Data")%>% withSpinner(type=8))),
  htmlOutput(outputId = "Statisitcs_Data") %>% withSpinner(type = 8),
  HTML(text = "<br>"),
  HTML(text = "<br>"),
  splitLayout(
    cellWidths = c("25%", "25%", "25%"),
    # uiOutput(outputId = "NextPanel2_ui"),
    # uiOutput(outputId = "NextPanel3_ui"),
    # uiOutput(outputId = "NextPanel4_ui")
  )
)


pre_processing_panel <- tabPanel(
  title = "Pre-processing",
  id = "pre_processing_panel",
  fluid = T,
  h4("Data Pre-processing") %>% helper(type = "markdown", content = "PreProcessing_help"),
  pre_processing_sidebar_panel,
  pre_processing_main_panel
)