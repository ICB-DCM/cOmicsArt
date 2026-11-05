# What every analysis module needs around its own computation: one result
# per Dataset version, the loading screen, the plot download and the
# annotation pickers. (The R-code download is on the Dataset handle,
# dataset$rcode_download().)

# The result of a module on `dataset` (a Dataset handle). Returns
#   get(): the result, NULL before one and after clear(); what the module
#     returns to dataset_server()
#   set(payload): sets the result, a list, with `version` = the Dataset's
#     version it was computed on (the glossary's Stale)
#   update(fn): replaces the result with fn(result), e.g. to add a field;
#     stops with req() without a result
#   set_params(values): merges the named list `values` into the result's
#     params (a NULL value stays an entry), via update()
#   clear(): no result; runs `on_clear()`, the module's own resetting
# A new Dataset version (new Dataset, run or clear) clears it.
module_result <- function(dataset, on_clear = function() NULL){
  value <- reactiveVal(NULL)
  clear <- function(){
    value(NULL)
    on_clear()
  }
  observeEvent(dataset$version(), clear())
  update <- function(fn) value(fn(req(isolate(value()))))
  list(
    get = function() value(),
    set = function(payload){
      payload$version <- isolate(dataset$version())
      value(payload)
    },
    update = update,
    set_params = function(values){
      update(function(result){
        result$params[names(values)] <- values
        result
      })
    },
    clear = clear
  )
}

# Evaluates `expr` behind the loading screen, which is hidden again however
# `expr` ends; returns its value. `waiter` is for tests.
with_loading <- function(expr, color = "#3897F147",
                         waiter = Waiter$new(html = LOADING_SCREEN, color = color, hide_on_render = FALSE)){
  waiter$show()
  on.exit(waiter$hide())
  expr
}

# The "Save plot" download of a module's plot, named download_name(`module`,
# ext(), `item`) with `ext` the reactive file extension (".png", ...).
# `save(file, device)` writes the plot (device without the dot); then the
# module's "Send only to Report" button (`report_button`, a namespaced id)
# is clicked, so the plot also goes to the report.
plot_download <- function(module, item = NULL, ext, save, report_button){
  downloadHandler(
    filename = function() download_name(module, ext(), item),
    content = function(file){
      save(file, gsub("\\.", "", ext()))
      shinyjs::click(report_button)
    }
  )
}

# A picker (renderUI) over the annotation columns of the `dataset`'s
# processed data: `side` "samples" (colData) or "entities" (rowData), after
# the `extra_choices` (e.g. c("None" = "none")). `selected` is the first
# column when left out; NULL selects nothing. A new run resets the user's
# choice.
annotation_picker <- function(id, label, ns, dataset, side = c("samples", "entities"),
                              multiple = FALSE, selected, extra_choices = NULL){
  side <- match.arg(side)
  first_column <- missing(selected)
  renderUI({
    columns <- selection_annotation_columns(req(dataset$processed()), side)
    selectInput(
      ns(id), label = label, choices = c(extra_choices, columns), multiple = multiple,
      selected = if (first_column) columns[1] else selected
    )
  })
}
