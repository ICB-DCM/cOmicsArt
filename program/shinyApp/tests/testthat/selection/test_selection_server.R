# tests for the Selection module in R/selection/server.R
lipids <- readRDS(
  file.path(test_path("..", "..", ".."), "www", "Lipidomics_only_precompiled-LS.RDS")
)$Lipidomics_SumExp

test_that("the module selects samples and entities by annotation", {
  testServer(selection_server, args = list(
    raw = reactive(lipids), upload_done = reactive("DataUploadSuccesful")
  ), {
    # before any choice: everything
    expect_equal(dim(session$returned$selected()$data), c(660, 28))
    session$setInputs(
      providedSampleAnnotationTypes = "diet",
      sample_selection = c("CD_CD_HFD", "HFD_HFD_CD"),
      providedRowAnnotationTypes = "CLASS",
      row_selection = c("TACG_sat", "PETA", "DACG")
    )
    selected <- session$returned$selected()
    expect_equal(dim(selected$data), c(297, 12))
    expect_length(selected$rows_selected, 297)
    expect_equal(selected$selected_samples, c("CD_CD_HFD", "HFD_HFD_CD"))
    expect_equal(session$returned$inputs()$sample_type, "diet")
  })
})

test_that("the choices follow the chosen annotation column and a new Upload", {
  raw <- reactiveVal(lipids)
  testServer(selection_server, args = list(
    raw = raw, upload_done = reactive("DataUploadSuccesful")
  ), {
    session$setInputs(providedSampleAnnotationTypes = "diet")
    expect_match(output$sample_selection_ui$html, "HFD_HFD_CD", fixed = TRUE)
    session$setInputs(providedRowAnnotationTypes = "CLASS")
    expect_match(output$row_selection_ui$html, "TACG_sat", fixed = TRUE)
    # a new Upload with other annotation
    other <- lipids[, 1:4]
    SummarizedExperiment::colData(other)$diet <- c("p", "p", "q", "q")
    raw(other)
    session$flushReact()
    expect_match(output$sample_selection_ui$html, "\"q\"", fixed = TRUE)
    expect_no_match(output$sample_selection_ui$html, "HFD_HFD_CD", fixed = TRUE)
  })
})

test_that("nothing is selected before an Upload", {
  testServer(selection_server, args = list(
    raw = reactive(NULL), upload_done = reactive(NULL)
  ), {
    expect_error(session$returned$selected(), class = "shiny.silent.error")
  })
})

test_that("'Go to Preprocessing' and 'Start the Journey' confirm the Selection", {
  testServer(selection_server, args = list(
    raw = reactive(lipids), upload_done = reactive("DataUploadSuccesful")
  ), {
    session$setInputs(use_full_data = 0, NextPanel = 0)
    before <- session$returned$confirmed()
    session$setInputs(use_full_data = 1)
    expect_false(identical(session$returned$confirmed(), before))
    after_go <- session$returned$confirmed()
    session$setInputs(NextPanel = 1)
    expect_false(identical(session$returned$confirmed(), after_go))
  })
})
