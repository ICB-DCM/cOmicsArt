pre_processing_sidebar_panel <- sidebarPanel(
  id = "sidebar_pre_processing",
  #########################################
  # Do Center & scaling + potential other pre-processing stuff
  #########################################
  h5("Pre-Processing Procedures"), # this could be enhanced with personalized procedures
  radioButtons(
    inputId = "PreProcessing_Procedure",
    label = "Pre-Processing Procedures",
    choices = c(
      "none", "vst_DESeq", "simpleCenterScaling", "Scaling_0_1",
      "log10", "pareto_scaling", "ln"
    ),
    selected = "none"
  ),
  uiOutput("DESeq_formula_ui"),
  actionButton(
    inputId = "Do_preprocessing",
    label = "Pre-Process",
    icon("fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;")
)


pre_processing_main_panel <- mainPanel(
  id = "mainpanel_pre_processing",
  # Statistics to the data
  helpText("general statistics to the input data, stuff like dimensions"),
  # hidden(div(id = 'Spinner_Statisitcs_Data', plotOutput("Statisitcs_Data")%>% withSpinner(type=8))),
  htmlOutput("Statisitcs_Data") %>% withSpinner(type = 8),
  HTML("<br>"),
  HTML("<br>"),
  splitLayout(
    cellWidths = c("25%", "25%", "25%"),
    uiOutput("NextPanel2_ui"),
    uiOutput("NextPanel3_ui"),
    uiOutput("NextPanel4_ui")
  )
)


pre_processing_panel <- tabPanel(
  "Pre-processing",
  id = "pre_processing_panel",
  fluid = T,
  h4("Data Pre-processing") %>% helper(type = "markdown", content = "PreProcessing_help"),
  pre_processing_sidebar_panel,
  pre_processing_main_panel
)