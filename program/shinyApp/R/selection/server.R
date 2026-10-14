### Selection module server
# Which entities and samples of the Upload a Dataset keeps.
# Arguments:
#   raw: reactive, the Upload's raw SummarizedExperiment
#   upload_done: reactive, set after each successful Upload
# Returns reactives:
#   selected: the Selection for the current choices (resolve_selection())
#   inputs: the choices as made (row_type, row_selection, sample_type,
#     sample_selection), NULL for controls never shown
#   confirmed: changes on "Go to Preprocessing" / "Start the Journey"
selection_server <- function(id, raw, upload_done){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    output$providedRowAnnotationTypes_ui <- renderUI({
      req(upload_done())
      shinyWidgets::virtualSelectInput(
        inputId = ns("providedRowAnnotationTypes"),
        label = "Which annotation type do you want to select on?",
        choices = selection_annotation_columns(raw(), "entities"),
        multiple = F,
        search = T,
        showSelectedOptionsFirst = T
      )
    })
    output$row_selection_ui <- renderUI({
      req(upload_done(), input$providedRowAnnotationTypes)
      choices <- entity_choices(raw(), input$providedRowAnnotationTypes)
      if(choices$numeric){
        selectInput(
          inputId = ns("row_selection"),
          label = "Which entities to use? (Your input category is numeric, selection is currently only supported for categorical data!)",
          choices = choices$values,
          selected = "all",
          multiple = T
        )
      } else {
        shinyWidgets::virtualSelectInput(
          inputId = ns("row_selection"),
          label = "Which entities to use? (Will be the union if multiple selected)",
          choices = choices$values,
          selected = "all",
          multiple = T,
          search = T,
          showSelectedOptionsFirst = T
        )
      }
    })
    output$providedSampleAnnotationTypes_ui <- renderUI({
      req(upload_done())
      selectInput(
        inputId = ns("providedSampleAnnotationTypes"),
        label = "Which annotation type do you want to select on?",
        choices = selection_annotation_columns(raw(), "samples"),
        selected = selection_annotation_columns(raw(), "samples")[1],
        multiple = F
      )
    })
    output$sample_selection_ui <- renderUI({
      req(upload_done(), isTruthy(input$providedSampleAnnotationTypes))
      selectInput(
        inputId = ns("sample_selection"),
        label = "Which entities to use? (Will be the union if multiple selected)",
        choices = sample_choices(raw(), input$providedSampleAnnotationTypes),
        selected = "all",
        multiple = T
      )
    })

    inputs <- reactive({
      list(
        row_type = input$providedRowAnnotationTypes,
        row_selection = input$row_selection,
        sample_type = input$providedSampleAnnotationTypes,
        sample_selection = input$sample_selection
      )
    })
    selected <- reactive({
      req(upload_done())
      resolve_selection(raw(), inputs())
    })

    list(
      selected = selected,
      inputs = inputs,
      confirmed = reactive(c(input$NextPanel, input$use_full_data))
    )
  })
}
