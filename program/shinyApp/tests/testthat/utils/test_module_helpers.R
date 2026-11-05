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

test_that("set_params() without a result does nothing", {
  ds <- changeable_dataset("Lipidomics")
  testServer(result_module, args = list(dataset = ds$handle, on_clear = function() NULL), {
    session$flushReact()
    expect_error(session$returned$result$set_params(list(a = 1)), class = "shiny.silent.error")
    expect_null(session$returned$get())
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
