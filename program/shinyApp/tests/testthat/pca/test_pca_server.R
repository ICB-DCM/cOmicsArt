# tests for PCA on the Dataset handle (issue 32)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# the sidebar choices a user has before "Get PCA"
display_inputs <- function(handle){
  list(
    coloring_options = "diet",
    entitie_anno = colnames(rowData(handle$processed()))[1],
    PCA_anno_tooltip = "diet",
    x_axis_selection = "PC1", y_axis_selection = "PC2",
    Show_loadings = "FALSE", Plot_ellipses = "TRUE",
    topSlider = 10, bottomSlider = 10, filterValue = 0.3
  )
}
run_pca <- function(session, handle, ...){
  do.call(session$setInputs, modifyList(display_inputs(handle), list(...)))
  session$setInputs(Do_PCA = 1)
}

test_that("the pickers offer the processed data's annotation; the batch picker only with a batch column", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    session$flushReact()
    data <- ds$handle$processed()
    for (column in colnames(colData(data))) {
      expect_match(as.character(output$coloring_options_ui$html), sprintf('value="%s"', column), fixed = TRUE)
      expect_match(as.character(output$PCA_anno_tooltip_ui$html), sprintf('value="%s"', column), fixed = TRUE)
    }
    for (column in colnames(rowData(data))) {
      expect_match(as.character(output$entitie_anno_ui$html), sprintf('value="%s"', column), fixed = TRUE)
    }
    expect_match(as.character(output$nPCAs_to_look_at_ui$html), sprintf('data-max="%d"', ncol(data)), fixed = TRUE)
    expect_error(output$UseBatch_ui)  # req() fails: no batch column
    expect_error(output$SampleAnnotationTypes_pca_ui)  # selection switched off
    session$setInputs(data_selection_pca = TRUE)
    expect_match(as.character(output$SampleAnnotationTypes_pca_ui$html), 'value="diet"', fixed = TRUE)
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(pca_Server, args = list(dataset = tmm), {
    session$flushReact()
    expect_match(as.character(output$UseBatch_ui$html), "Use batch corrected data?", fixed = TRUE)
  })
})

test_that("a run returns the PCA of all samples with its parameters and the Dataset version", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    expect_null(session$returned())
    run_pca(session, ds$handle, scale_data = "TRUE")
    res <- session$returned()
    expected <- get_pca(ds$handle$processed(), TRUE, "diet", "all")
    expect_equal(res$pca$x, expected$pca$x)
    expect_setequal(rownames(res$pca$x), colnames(ds$handle$processed()))
    expect_equal(res$percentVar, expected$percentVar)
    expect_equal(res$version, 2)
    expect_equal(res$params$sample_selection, "all")
    expect_false(res$params$useBatch)
    expect_true(res$params$scale_data)
    expect_equal(res$params$color_by, "diet")
    expect_equal(res$params$x_axis, "PC1")
    expect_match(res$params$title, "PC1 vs PC2 -- preprocessing: log10", fixed = TRUE)
    # the plots are drawn
    expect_false(is.null(pca_reactives$PCA_plot))
    expect_false(is.null(pca_reactives$Scree_plot))
    expect_false(is.null(pca_reactives$Loadings_plot))
    expect_false(is.null(pca_reactives$LoadingsMatrix_plot))
  })
})

test_that("batch corrected data is used when chosen", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(pca_Server, args = list(dataset = tmm), {
    session$setInputs(UseBatch = "Yes", coloring_options = "condition", scale_data = "TRUE")
    session$setInputs(Do_PCA = 1)
    res <- session$returned()
    expect_true(res$params$useBatch)
    batch <- get_pca(tmm$processed(batch = TRUE), TRUE, "condition", "all")$pca$x
    expect_equal(res$pca$x, batch)
    expect_false(isTRUE(all.equal(res$pca$x, get_pca(tmm$processed(), TRUE, "condition", "all")$pca$x)))
  })
})

test_that("the in-module sample selection computes on those samples, and the plots use that data", {
  ds <- changeable_dataset("Lipidomics")
  diet <- isolate(colData(ds$handle$processed())$diet)
  chosen <- unique(diet)[1]
  testServer(pca_Server, args = list(dataset = ds$handle), {
    session$setInputs(data_selection_pca = TRUE, SampleAnnotationTypes_pca = "diet", sample_selection_pca = chosen)
    run_pca(session, ds$handle)
    res <- session$returned()
    samples <- colnames(ds$handle$processed())[diet == chosen]
    expect_setequal(rownames(res$pca$x), samples)
    expect_equal(res$params$sample_selection, chosen)
    expect_equal(res$params$sample_types, "diet")
    expect_setequal(colnames(pca_reactives$run$data), samples)
    expect_match(output$PCA_Info, "Due to sample selection", fixed = TRUE)

    # switched off again: all samples, whatever the pickers still hold
    session$setInputs(data_selection_pca = FALSE)
    session$setInputs(Do_PCA = 2)
    expect_setequal(rownames(session$returned()$pca$x), colnames(ds$handle$processed()))
    expect_equal(session$returned()$params$sample_selection, "all")
  })
})

test_that("a failed run leaves no result and does not stop the module", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    run_pca(session, ds$handle)
    expect_false(is.null(session$returned()))
    # no sample has this value: get_pca() fails
    session$setInputs(data_selection_pca = TRUE, SampleAnnotationTypes_pca = "diet", sample_selection_pca = "no such group")
    expect_no_error(session$setInputs(Do_PCA = 2))
    expect_null(session$returned())
    expect_null(pca_reactives$PCA_plot)
    # and it still runs afterwards
    session$setInputs(data_selection_pca = FALSE)
    session$setInputs(Do_PCA = 3)
    expect_false(is.null(session$returned()))
  })
})

test_that("a new Dataset version clears the result", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    run_pca(session, ds$handle)
    expect_false(is.null(session$returned()))
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
    expect_null(pca_reactives$PCA_plot)
  })
})

test_that("a display change updates the parameters, not the PCA", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    run_pca(session, ds$handle)
    before <- session$returned()
    session$setInputs(x_axis_selection = "PC2", y_axis_selection = "PC3", topSlider = 5, filterValue = 0.1, nPCAs_to_look_at = 3)
    after <- session$returned()
    expect_equal(after$params$x_axis, "PC2")
    expect_equal(after$params$y_axis, "PC3")
    expect_equal(after$params$n_top, 5)
    expect_equal(after$params$n_pcs, 3)
    expect_match(after$params$title, "PC2 vs PC3", fixed = TRUE)
    expect_identical(after$pca, before$pca)
    expect_equal(after$version, before$version)
  })
})

test_that("each R-code zip carries the PCA parameters under the templates' names", {
  ds <- changeable_dataset("Lipidomics")
  testServer(pca_Server, args = list(dataset = ds$handle), {
    run_pca(session, ds$handle)
    downloads <- list(
      output$getR_Code_PCA, output$getR_Code_Scree_Plot,
      output$getR_Code_Loadings, output$getR_Code_Loadings_matrix
    )
    for (zip_file in downloads) {
      par <- rcode_parameters(zip_file)
      expect_true(all(c(
        "sample_selection", "sample_types", "scale_data", "x_axis", "y_axis",
        "color_by", "title", "show_loadings", "plot_ellipses", "entitie_anno",
        "tooltip_var", "n_top", "n_bottom", "cutoff", "n_pcs"
      ) %in% names(par$PCA)))
      expect_equal(par$PCA$color_by, "diet")
      expect_equal(par$preprocessing$preprocessing_procedure, "log10")
    }
  })
})

test_that("dataset_server() puts the result into results()$pca; a new run clears it", {
  expect_collected("pca", function(session, handle){
    session$setInputs("pca-coloring_options" = "diet")
    session$setInputs("pca-Do_PCA" = 1)
  })
})
