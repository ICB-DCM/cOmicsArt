# The R-code download of every pipeline ships what its script needs. The
# R_code tests (test-rcode-*.R) run the scripts; these checks find the same
# gaps in seconds and without the app.

# select_data() and preprocessing() start every script (preprocessing_base_script())
BASE_INFOS <- list(select_data_info, preprocessing_info)
PIPELINES <- mget(grep("_PIPELINE$", ls(globalenv()), value = TRUE), envir = globalenv())

# The functions a script defines, by the name it defines them under.
shipped_functions <- function(infos) {
  unlist(lapply(infos, function(info) {
    c(setNames(list(info$foo), info$name), info$additional_foos)
  }), recursive = FALSE)
}

# The variables the steps in `infos` leave in the script: their outputs and,
# for a function inlined into the script (not in util.R), its locals.
step_outputs <- function(infos) {
  unlist(lapply(infos, function(info) c(
    info$output_name, unlist(info$output_mapping),
    if (!isTRUE(info$to_util)) codetools::findLocals(body(info$foo))
  )))
}

# Functions of the app (not of a package) that `f` calls.
app_functions_called <- function(f) {
  called <- codetools::findGlobals(f, merge = FALSE)$functions
  called[vapply(called, function(name) {
    fn <- get0(name, mode = "function")
    # package functions (and Bioconductor generics) live in a namespace
    !is.null(fn) && !is.null(environment(fn)) &&
      identical(topenv(environment(fn)), globalenv())
  }, logical(1))]
}

test_that("every app function a script calls is written to its util.R", {
  expect_gt(length(PIPELINES), 10)
  for (name in names(PIPELINES)) {
    shipped <- shipped_functions(c(BASE_INFOS, PIPELINES[[name]]))
    called <- base::unique(unlist(lapply(shipped, app_functions_called)))
    missing <- setdiff(called, names(shipped))
    expect(
      length(missing) == 0,
      paste0(name, " does not ship: ", paste(missing, collapse = ", "))
    )
  }
})

test_that("an inlined plot function assigns the plot the script shows", {
  for (name in names(PIPELINES)) {
    infos <- c(BASE_INFOS, PIPELINES[[name]])
    for (i in seq_along(infos)) {
      info <- infos[[i]]
      if (isTRUE(info$to_util) || is.null(info$plot_name)) next
      # the shown plot comes from the inlined body or an earlier step
      defined <- step_outputs(infos[seq_len(i)])
      missing <- setdiff(all.vars(str2lang(info$plot_name)), defined)
      expect(
        length(missing) == 0,
        paste0(name, ": ", info$name, " does not assign ", paste(missing, collapse = ", "))
      )
    }
  }
})

test_that("every script ends by showing its plot", {
  for (name in names(PIPELINES)) {
    last <- PIPELINES[[name]][[length(PIPELINES[[name]])]]
    expect(!is.null(last$plot_name), paste0(name, ": ", last$name, " has no plot_name"))
  }
  par <- list(SigAna = list(
    comparisons_to_visualize = "a:b", visualization_method = "UpSet",
    significance_level = 0.05, sig_to_look_at = "Significant"
  ))
  script <- create_function_script(
    plot_significant_results_info, par = par, par_mem = "SigAna",
    path_to_util = withr::local_tempfile()
  )
  expect_match(script, "\nsig_ana_plot\\s*$")
})

test_that("the script loads the packages of the Pre-processing helpers", {
  # deseq_processing() calls DESeq2's vst() without DESeq2::
  expect_match(create_library_code("plot_pca"), "library(DESeq2)", fixed = TRUE)
})
