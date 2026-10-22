data_selection_sidebar_panel <- upload_sidebar_ui("upload")


data_selection_main_panel <- mainPanel(
  id = "mainPanel_DataSelection",
  upload_main_ui("upload"),
  div(id = "mainPanel_other",
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
          background-color: #70BF4F47;
          color: black;
          border: 2px solid darkgrey;
          font-size: 15px;
          font-weight: bold;
          box-shadow: 3px 3px 5px rgba(255, 0, 0, 0.2);
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
    hr(style = "border-top: 1px solid #858585;"),
    actionButton(
      inputId = "NextPanel",
      label = "Start the Journey",
      width = "100%",
      icon = icon('rocket'),
      style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
    ),
  )
  )
)


data_selection_panel <- tabPanel(
  title = tagList(tags$span("1. Data selection")), 
  value = "Data selection",
  id = "Data_selection",
  fluid = T,
  h4("Data Selection"),
  ################################################################################
  # Data Selection
  ################################################################################
  data_selection_sidebar_panel,
  data_selection_main_panel
)
