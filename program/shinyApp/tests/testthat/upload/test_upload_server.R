# tests for the Upload module in R/upload/server.R
app_dir <- normalizePath(test_path("..", "..", ".."))

# Run testServer on the Upload module from the app folder, with the session
# folder and report the app creates at session start.
test_upload_server <- function(expr){
  expr <- substitute(expr)
  withr::local_dir(app_dir)
  eval(bquote(testServer(upload_server, {
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
