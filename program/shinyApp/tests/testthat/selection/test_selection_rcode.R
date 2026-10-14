# The downloadable R code repeats the Selection the app made.
lipids <- readRDS(
  file.path(test_path("..", "..", ".."), "www", "Lipidomics_only_precompiled-LS.RDS")
)$Lipidomics_SumExp

# Run the generated select_data() step with the parameters the app stores.
run_selection_script <- function(parameters, data_orig){
  util <- tempfile(fileext = ".R")
  cat(prepare_function_for_util(select_data, "select_data"), file = util)
  env <- new.env()
  env$parameters <- parameters
  env$data_orig <- data_orig
  env$`%||%` <- `%||%`
  sys.source(util, envir = env)
  eval(parse(text = create_function_script(select_data_info, parameters)), envir = env)
  env
}

cases <- list(
  all = list(),
  samples = list(sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD")),
  entities = list(row_type = "CLASS", row_selection = c("TACG_sat", "PETA", "DACG")),
  both = list(
    sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD"),
    row_type = "CLASS", row_selection = c("TACG_sat", "PETA", "DACG")
  )
)
for (name in names(cases)) {
  test_that(paste("the R code selects the same data as the app:", name), {
    parameters <- selection_params(resolve_selection(lipids, cases[[name]]))
    app <- lipids[parameters$rows_selected, parameters$samples_selected]
    script <- run_selection_script(parameters, lipids)
    expect_equal(dim(script$data), dim(app))
    expect_equal(script$rows_selected, rownames(app))
    expect_equal(script$samples_selected, colnames(app))
  })
}

test_that("the stored Selection keeps the chosen values apart from the IDs", {
  parameters <- selection_params(resolve_selection(lipids, cases$samples))
  expect_equal(parameters$selected_samples, c("CD_CD_HFD", "HFD_HFD_CD"))
  expect_equal(parameters$sample_type, "diet")
  expect_length(parameters$samples_selected, 12)
  expect_equal(parameters$selected_rows, "all")
  expect_length(parameters$rows_selected, 660)
})
