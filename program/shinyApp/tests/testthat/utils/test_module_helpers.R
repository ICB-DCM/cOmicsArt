# The helpers the analysis modules share (R/utils/module_helpers.R)

# a module with nothing but a module_result(); its returns are the result's
# get and the result itself
result_module <- function(id, dataset, on_clear){
  moduleServer(id, function(input, output, session){
    result <- module_result(dataset, on_clear)
    list(get = result$get, result = result)
  })
}

test_that("module_result() stamps the Dataset version, merges params and clears on a new version", {
  ds <- changeable_dataset("Lipidomics")
  cleared <- 0
  testServer(result_module, args = list(dataset = ds$handle, on_clear = function() cleared <<- cleared + 1), {
    session$flushReact()
    result <- session$returned$result
    expect_null(session$returned$get())
    on_start <- cleared
    result$set(list(value = "x", params = list(a = 1)))
    expect_equal(session$returned$get()$version, 2)
    expect_equal(session$returned$get()$value, "x")
    # params are merged; a NULL value stays an entry
    result$set_params(list(b = 2, a = NULL))
    expect_equal(names(session$returned$get()$params), c("a", "b"))
    expect_null(session$returned$get()$params$a)
    expect_equal(session$returned$get()$params$b, 2)
    # a new Dataset version clears it and runs on_clear
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned$get())
    expect_equal(cleared, on_start + 1)
    # set() after it stamps the new version; clear() clears
    result$set(list(value = "y", params = list()))
    expect_equal(session$returned$get()$version, 3)
    result$clear()
    expect_null(session$returned$get())
    expect_equal(cleared, on_start + 2)
  })
})

test_that("update() replaces the result with a function of it, e.g. a new field", {
  ds <- changeable_dataset("Lipidomics")
  testServer(result_module, args = list(dataset = ds$handle, on_clear = function() NULL), {
    session$flushReact()
    result <- session$returned$result
    result$set(list(params = list(a = 1)))
    result$update(function(r){ r$gene_list <- c("g1", "g2"); r })
    expect_equal(session$returned$get()$gene_list, c("g1", "g2"))
    expect_equal(session$returned$get()$params, list(a = 1))
    expect_equal(session$returned$get()$version, 2)
  })
})

test_that("set_params() without a result does nothing", {
  ds <- changeable_dataset("Lipidomics")
  testServer(result_module, args = list(dataset = ds$handle, on_clear = function() NULL), {
    session$flushReact()
    expect_error(session$returned$result$set_params(list(a = 1)), class = "shiny.silent.error")
    expect_null(session$returned$get())
  })
})

test_that("set() keeps the run beside the result; clear() and a new version drop it", {
  ds <- changeable_dataset("Lipidomics")
  testServer(result_module, args = list(dataset = ds$handle, on_clear = function() NULL), {
    session$flushReact()
    result <- session$returned$result
    expect_null(result$run())
    result$set(list(params = list()), run = list(fit = "model"))
    expect_equal(result$run(), list(fit = "model"))
    expect_null(session$returned$get()$fit)
    result$clear()
    expect_null(result$run())
    result$set(list(params = list()), run = "again")
    ds$state$version <- 3
    session$flushReact()
    expect_null(result$run())
  })
})

# a stand-in for a Waiter that counts its calls
fake_waiter <- function(){
  calls <- new.env()
  calls$show <- 0
  calls$hide <- 0
  list(
    show = function() calls$show <- calls$show + 1,
    hide = function() calls$hide <- calls$hide + 1,
    calls = calls
  )
}

test_that("with_loading() shows the loading screen while `expr` runs and returns its value", {
  waiter <- fake_waiter()
  value <- with_loading({
    expect_equal(waiter$calls$show, 1)
    expect_equal(waiter$calls$hide, 0)
    42
  }, waiter = waiter)
  expect_equal(value, 42)
  expect_equal(waiter$calls$hide, 1)
})

test_that("with_loading() hides the loading screen also when `expr` fails", {
  waiter <- fake_waiter()
  expect_error(with_loading(stop("failed"), waiter = waiter), "failed")
  expect_equal(waiter$calls$hide, 1)
})

test_that("try_run() returns the value, or NULL with an error modal after the prefix", {
  shown <- NULL
  rlang::local_bindings(error_modal = function(error_message, ...) shown <<- error_message,
                        .env = environment(try_run))
  expect_equal(try_run(42, waiter = fake_waiter()), 42)
  expect_null(shown)
  waiter <- fake_waiter()
  expect_null(try_run(stop("boom"), "Error running k-means: ", waiter = waiter))
  expect_equal(shown, "Error running k-means: boom")
  expect_equal(waiter$calls$hide, 1)
})

# a module with nothing but a report button
report_module <- function(id, save, info, notes = NULL, snippet = NULL){
  moduleServer(id, function(input, output, session){
    observeEvent(input$report, plot_report(session, "My Plot", "my_plot_", save, info, notes, snippet))
  })
}

test_that("plot_report() writes the image and the section: Info, image, notes, snippet", {
  test_env <- environment()
  saved <- NULL
  testServer(report_module, args = list(
    save = function(file) saved <<- file, info = c("line 1", "line 2"),
    notes = "my notes", snippet = "the snippet"
  ), {
    report <- local_session_report(session, test_env)
    session$setInputs(report = 1)
    expect_match(saved, paste0("/www/", session$token, "/my_plot_.*\\.png$"))
    lines <- strsplit(report(), "\n")[[1]]
    lines <- lines[lines != ""]
    expect_equal(lines[1:5], c(
      "## My Plot {.tabset .tabset-fade}", "### Info", "line 1", "line 2",
      paste0("![My Plot](", saved, ")")
    ))
    expect_match(report(), "my notes", fixed = TRUE)
    expect_equal(tail(lines, 2), c("### Publication Snippet", "the snippet"))
  })
})

test_that("plot_report() leaves out the notes and the snippet without them", {
  test_env <- environment()
  testServer(report_module, args = list(save = function(file) NULL, info = "line"), {
    report <- local_session_report(session, test_env)
    session$setInputs(report = 1)
    expect_false(grepl("Publication Snippet", report(), fixed = TRUE))
    expect_match(report(), "^## My Plot")
  })
})

# a module with nothing but an annotation picker
picker_module <- function(id, dataset, ...){
  moduleServer(id, function(input, output, session){
    output$picker <- annotation_picker("choice", "Pick", session$ns, dataset, ...)
  })
}
picker_html <- function(dataset, ...){
  html <- NULL
  testServer(picker_module, args = list(dataset = dataset, ...), {
    session$flushReact()
    html <<- as.character(output$picker$html)
  })
  html
}

test_that("annotation_picker() offers the annotation columns, the first one selected", {
  lipid <- fake_dataset()
  samples <- picker_html(lipid, side = "samples")
  for (column in colnames(SummarizedExperiment::colData(lipid$processed()))) {
    expect_match(samples, sprintf('value="%s"', column), fixed = TRUE)
  }
  first <- colnames(SummarizedExperiment::colData(lipid$processed()))[1]
  expect_match(samples, sprintf('value="%s" selected', first), fixed = TRUE)
  entities <- picker_html(lipid, side = "entities")
  expect_match(entities, sprintf('value="%s"', colnames(SummarizedExperiment::rowData(lipid$processed()))[1]), fixed = TRUE)
})

test_that("annotation_picker() takes extra choices and another or no selection", {
  lipid <- fake_dataset()
  none <- picker_html(lipid, side = "samples", extra_choices = c("None" = "none"), selected = "none")
  expect_match(none, 'value="none" selected', fixed = TRUE)
  expect_match(none, ">None<", fixed = TRUE)
  nothing <- picker_html(lipid, side = "samples", multiple = TRUE, selected = NULL)
  expect_false(grepl("selected", nothing, fixed = TRUE))
})
