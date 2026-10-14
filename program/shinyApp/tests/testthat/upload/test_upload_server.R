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

test_that("the save-for-later filename starts with the Upload's omic type", {
  test_upload_server({
    session$setInputs(omic_type_testdata = "Lipidomics")
    session$setInputs(EasyTestForUser = 1)
    expect_match(basename(output$SaveInputAsList), "^Lipidomics_only_precompiled ")
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
