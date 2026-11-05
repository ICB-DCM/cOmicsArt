# The "Use batch corrected data?" picker of an analysis module: renders it
# into output$UseBatch_ui (only when the `dataset`'s run has a batch column;
# the module's UI has uiOutput(ns("UseBatch_ui"))) and returns a reactive
# with the stage of DATA_STAGES the module's next run uses: "batch_corrected"
# when picked, else "processed".
batch_toggle <- function(input, output, ns, dataset){
  output$UseBatch_ui <- renderUI({
    req(dataset$has_batch())
    selectInput(
      inputId = ns("UseBatch"),
      label = "Use batch corrected data?",
      choices = c("No", "Yes"),
      selected = "No"
    )
  })
  reactive(if (dataset$has_batch() && identical(input$UseBatch, "Yes")) "batch_corrected" else "processed")
}
