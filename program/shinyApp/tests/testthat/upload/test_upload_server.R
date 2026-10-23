# tests for the Upload module in R/upload/server.R
app_dir <- normalizePath(test_path("..", "..", ".."))

# Run testServer on the Upload module from the app folder, with the session
# folder and report the app creates at session start. testServer evaluates
# `expr` inside the module, so test data are passed in as `values`.
test_upload_server <- function(expr, values = list()){
  expr <- substitute(expr)
  withr::local_dir(app_dir)
  eval(bquote(testServer(upload_server, {
    list2env(.(values), environment())
    session_dir <- file.path("www", session$token)
    dir.create(session_dir, showWarnings = FALSE)
    write("# Report", file.path(session_dir, "Report.md"))
    session$onSessionEnded(function() unlink(session_dir, recursive = TRUE))
    .(expr)
  })))
}

# write tables like the visual inspection does, return them as file inputs
write_file_inputs <- function(matrix, sample_table, annotation_rows){
  file_input <- function(table){
    path <- tempfile(fileext = ".csv")
    write.csv(table, path, row.names = TRUE)
    list(datapath = path, name = basename(path))
  }
  list(
    data_matrix1 = file_input(matrix),
    data_sample_anno1 = file_input(sample_table),
    data_row_anno1 = file_input(annotation_rows)
  )
}

test_that("the save-for-later filename names the Upload's omic type", {
  test_upload_server({
    session$setInputs(omic_type_testdata = "Lipidomics")
    session$setInputs(EasyTestForUser = 1)
    expect_match(basename(output$SaveInputAsList), "^cOmicsArt_Upload_Lipidomics_.*\\.RDS$")
  })
})

test_that("gene annotation works after 'Use the Data' from visual inspection", {
  counts <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  files <- write_file_inputs(
    counts,
    data.frame(group = c("a", "b"), row.names = c("s1", "s2")),
    data.frame(id = c("a", "b"), row.names = c("g1", "g2"))
  )
  test_upload_server(values = list(files = files), {
    # not Transcriptomics, so no Ensembl lookup
    do.call(session$setInputs, c(list(omic_type_file_input = "Lipidomics"), files))
    session$setInputs(DoVisualDataInspection = 1)
    session$setInputs(usingVIdata = 1)
    expect_equal(session$returned$done(), "DataUploadSuccesful")
    session$setInputs(AddGeneSymbols = 1)
    session$setInputs(do_annotation = 1)
    expect_equal(session$returned$annotated(), 1)
    expect_true(session$returned$added_gene_anno())
  })
})

test_that("column-name correction handles each click once after repeated inspections", {
  # names starting with a digit are not syntactic, so correction is offered
  counts <- matrix(1:4, nrow = 2, dimnames = list(c("1g", "2g"), c("1s", "2s")))
  files <- write_file_inputs(
    counts,
    data.frame(group = c("a", "b"), row.names = c("1s", "2s")),
    data.frame(id = c("a", "b"), row.names = c("1g", "2g"))
  )
  # count the corrections and the "Close" handler runs (it shows a notification)
  calls <- new.env()
  calls$correct <- 0
  calls$close <- 0
  module_env <- environment(upload_server)
  correct_upload_names <- module_env$correct_upload_names
  module_env$correct_upload_names <- function(...){
    calls$correct <- calls$correct + 1
    correct_upload_names(...)
  }
  module_env$showNotification <- function(...) calls$close <- calls$close + 1
  on.exit({
    module_env$correct_upload_names <- correct_upload_names
    rm("showNotification", envir = module_env)
  }, add = TRUE)

  test_upload_server(values = list(files = files), {
    do.call(session$setInputs, c(list(omic_type_file_input = "Lipidomics"), files))
    for (round in 1:2){
      session$setInputs(DoVisualDataInspection = round)
      session$setInputs(yes_correct_column_names = round)
      session$setInputs(close_modal_VI = round)
    }
    # the corrected matrix can be downloaded
    corrected <- read.csv(output$DownloadUpdatedMatrix, row.names = 1, check.names = FALSE)
    expect_equal(colnames(corrected), c("sample_1s", "sample_2s"))
  })
  expect_equal(calls$correct, 2)
  expect_equal(calls$close, 2)
})

test_that("gene annotation queries a fixed Ensembl release", {
  # www.ensembl.org and the mirrors make biomaRt 2.54 fail in getBM()
  for (organism in names(loadedVersion)){
    expect_match(
      loadedVersion[[organism]]$ensmbl@host, "^https://[a-z]{3}[0-9]{4}\\.archive\\.ensembl\\.org"
    )
  }
})

test_that("a failed Ensembl query keeps the Upload and says so", {
  counts <- matrix(1:4, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
  files <- write_file_inputs(
    counts,
    data.frame(group = c("a", "b"), row.names = c("s1", "s2")),
    data.frame(id = c("a", "b"), row.names = c("g1", "g2"))
  )
  module_env <- environment(upload_server)
  module_env$getBM <- function(...) stop("HTTP error code: 405")
  on.exit(rm("getBM", envir = module_env), add = TRUE)

  test_upload_server(values = list(files = files), {
    do.call(session$setInputs, c(list(omic_type_file_input = "Transcriptomics"), files))
    session$setInputs(refresh_file_input = 1)
    raw_before <- session$returned$raw()
    session$setInputs(AddGeneSymbols = 1)
    session$setInputs(
      AddGeneSymbols_organism = "Human genes (GRCh38.p14)",
      annotation_name = "ensembl_gene_id",
      annotation_colname = "id"
    )
    session$setInputs(do_annotation = 1)
    expect_match(output$debug, "Gene annotation failed")
    expect_identical(session$returned$raw(), raw_before)
    expect_null(session$returned$annotated())
    expect_false(session$returned$added_gene_anno())
  })
})
