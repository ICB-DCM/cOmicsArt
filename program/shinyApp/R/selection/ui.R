### Selection module UI
# The Selection block of the "1. Data selection" tab, below the Upload's part
# of the main panel: the "Select data" toggle, the entity and sample choices,
# "Go to Preprocessing" and "Start the Journey". Hidden until the first Upload.

selection_ui <- function(id){
  ns <- NS(id)
  div(id = ns("mainPanel_other"),
  fluidRow(
    column(5,
           actionButton(
            ns("select_data"), "[optional] Select Data",
            width = "100%",
            icon = icon('filter'),
            style = "color: #fffff; background-color: white; border-color: #000000"
            )
    )),
  br(),
  conditionalPanel(
    condition = "input.select_data % 2 == 0",
    ns = ns,
    fluidRow(column(5,
        actionButton(
          ns("use_full_data"), "Go to Preprocessing",
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
    ns = ns,
    hr(style = "border-top: 1px solid #858585;"),
    fluidRow(
      column(5,
             div(class = "DataSelection",
                 h4("Row selection - biochemical entities"),
                 uiOutput(ns("providedRowAnnotationTypes_ui")),
                 uiOutput(ns("row_selection_ui"))
             )),
      column(6,
             div(class = "SampleSelection",
                 h4("Sample selection") %>% helper(type = "markdown", content = "DataSelection_RowSelection"),
                 uiOutput(ns("providedSampleAnnotationTypes_ui")),
                 uiOutput(ns("sample_selection_ui"))
             ))
    ),
    hr(style = "border-top: 1px solid #858585;"),
    actionButton(
      inputId = ns("NextPanel"),
      label = "Start the Journey",
      width = "100%",
      icon = icon('rocket'),
      style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
    ),
  )
  )
}
