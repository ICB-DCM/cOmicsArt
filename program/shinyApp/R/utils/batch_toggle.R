# The "Use batch corrected data?" picker of an analysis module: renders it
# into output$UseBatch_ui (only when the `dataset`'s run has a batch column;
# the module's UI has uiOutput(ns("UseBatch_ui"))) and returns a reactive
# with the stage of DATA_STAGES the module's next run uses, the picker's
# value: "batch_corrected" for Yes, else "processed".
batch_toggle <- function(input, output, ns, dataset){
  output$UseBatch_ui <- renderUI({
    req(dataset$has_batch())
    selectInput(
      inputId = ns("UseBatch"),
      label = "Use batch corrected data?",
      choices = c(No = "processed", Yes = "batch_corrected"),
      selected = "processed"
    )
  })
  # the picker keeps its value when a run without a batch column hides it
  reactive(if (dataset$has_batch()) input$UseBatch %||% "processed" else "processed")
}
