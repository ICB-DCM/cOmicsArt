# What every analysis module needs around its own computation: one result
# per Dataset version, running behind the loading screen, the plot download,
# the report and the annotation pickers. (The R-code download is on the
# Dataset handle, dataset$rcode_download().)

# The result of a module on `dataset` (a Dataset handle). Returns
#   get(): the result, NULL before one and after clear(); what the module
#     returns to dataset_server()
#   run(): what the computation gave beyond the result, e.g. the fitted
#     model the plots draw from; NULL without a result
#   set(payload, run = NULL): sets the result, a list, with `version` = the
#     Dataset's version it was computed on (the glossary's Stale), and its run
#   update(fn): replaces the result with fn(result), e.g. to add a field;
#     stops with req() without a result
#   set_params(values): merges the named list `values` into the result's
#     params (a NULL value stays an entry), via update()
#   clear(): no result and no run; runs `on_clear()`, the module's own
#     resetting
# A new Dataset version (new Dataset, run or clear) clears it.
module_result <- function(dataset, on_clear = function() NULL){
  value <- reactiveVal(NULL)
  run_value <- reactiveVal(NULL)
  clear <- function(){
    value(NULL)
    run_value(NULL)
    on_clear()
  }
  observeEvent(dataset$version(), clear())
  update <- function(fn) value(fn(req(isolate(value()))))
  list(
    get = function() value(),
    run = function() run_value(),
    set = function(payload, run = NULL){
      payload$version <- isolate(dataset$version())
      run_value(run)
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

# Evaluates `expr` behind the loading screen (with_loading(), `...` are its
# arguments); an error shows in an error modal, after `error_prefix`, and
# gives NULL. For a module's computation: NULL is a failed run.
try_run <- function(expr, error_prefix = "", ...){
  with_loading(
    tryCatch(expr, error = function(e){
      error_modal(paste0(error_prefix, e$message))
      NULL
    }),
    ...
  )
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

# Sends a module's plot to the report, behind a "Saving to Report..." note.
# `save(file)` writes the png `file`, in the session's folder and named
# `stem` and the time. The report gets the section `title`: its Info, the
# `info` lines and the image; the `notes` (the module's notes input), if
# any; and the Publication Snippet `snippet`, if given.
plot_report <- function(session, title, stem, save, info, notes = NULL, snippet = NULL){
  note <- showNotification("Saving to Report...", duration = 0)
  on.exit(removeNotification(note))
  file <- paste0(getwd(), "/www/", session$token, "/", stem, Sys.time(), ".png")
  save(file)
  lines <- c(
    paste0("## ", title, " {.tabset .tabset-fade}"),
    "### Info",
    info,
    paste0("![", title, "](", file, ")")
  )
  if (isTruthy(notes) && !isEmpty(notes)) lines <- c(lines, add_notes_report(shiny::markdown(notes)))
  if (!is.null(snippet)) lines <- c(lines, "### Publication Snippet", snippet)
  for (line in lines) fun_LogIt(session, message = line)
  showNotification("Report Saved!", type = "message", duration = 1)
}
