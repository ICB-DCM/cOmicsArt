# tests for Heatmap on the Dataset handle (issue 33)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# Top K by fold change between the first two groups of `column`
top_k_inputs <- function(handle, column, k = 10){
  groups <- unique(isolate(colData(handle$processed()))[[column]])
  list(
    row_selection_options = "Top K", TopK = k, TopK_order = "LogFoldChange",
    sample_annotation_types_cmp_heatmap = column,
    Groups2Compare_ref_heatmap = groups[1], Groups2Compare_treat_heatmap = groups[2],
    psig_threhsold_heatmap = 0.05
  )
}
run_heatmap <- function(session, inputs, go = 1){
  do.call(session$setInputs, inputs)
  session$setInputs(Do_Heatmap = go)
}
expected_top_k <- function(data, column, k = 10){
  groups <- unique(colData(data)[[column]])
  rownames(entitieSelection(
    data, selection_type = "Top K", n_top_k = k, top_k_type = "LogFoldChange",
    compare_within = column, reference = groups[1], treatment = groups[2],
    significance_level = 0.05
  ))
}
# a rowData column of Ensembl IDs (Transcriptomics test data)
ensembl_column <- function(data){
  annotation <- rowData(data)
  names(which(vapply(annotation, function(x) all(grepl("^ENS", x)), logical(1))))[1]
}

test_that("the pickers offer the processed data's annotation; the batch picker only with a batch column", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    session$flushReact()
    data <- ds$handle$processed()
    for (column in colnames(colData(data))) {
      expect_match(as.character(output$anno_options_ui$html), sprintf('value="%s"', column), fixed = TRUE)
      expect_match(as.character(output$sample_annotation_types_cmp_heatmap_ui$html), sprintf('value="%s"', column), fixed = TRUE)
    }
    for (column in colnames(rowData(data))) {
      expect_match(as.character(output$row_label_options_ui$html), sprintf('value="%s"', column), fixed = TRUE)
      expect_match(as.character(output$row_anno_options_ui$html), sprintf('value="%s"', column), fixed = TRUE)
    }
    session$setInputs(sample_annotation_types_cmp_heatmap = "diet")
    expect_match(as.character(output$Groups2Compare_ref_heatmap_ui$html), unique(colData(data)$diet)[1], fixed = TRUE)
    expect_error(output$UseBatch_ui)  # req() fails: no batch column
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(heatmap_server, args = list(dataset = tmm), {
    session$flushReact()
    expect_match(as.character(output$UseBatch_ui$html), "Use batch corrected data?", fixed = TRUE)
  })
})

test_that("a Top K run returns the K entities with its parameters and the Dataset version", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    expect_null(session$returned())
    run_heatmap(session, top_k_inputs(ds$handle, "diet"))
    res <- session$returned()
    expect_equal(res$entities, expected_top_k(ds$handle$processed(), "diet"))
    expect_length(res$entities, 10)
    expect_null(res$gene_list)
    expect_equal(res$version, 2)
    expect_equal(res$params$selection_type, "Top K")
    expect_equal(res$params$n_top_k, 10)
    expect_equal(res$params$data_stage, "processed")
    expect_match(res$params$title, "top 10", fixed = TRUE)
    expect_true(res$params$cluster_rows)
    expect_length(res$params$row_labels, 10)
    expect_s4_class(heatmap_plot(), "Heatmap")
  })
})

test_that("batch corrected data is used for the selection, labels and annotations", {
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(heatmap_server, args = list(dataset = tmm), {
    run_heatmap(session, c(top_k_inputs(tmm, "condition"), list(UseBatch = "batch_corrected")))
    res <- session$returned()
    expect_equal(res$params$data_stage, "batch_corrected")
    expect_equal(res$entities, expected_top_k(tmm$data("batch_corrected"), "condition"))
    expect_identical(heatmap_reactives$run$data, tmm$data("batch_corrected"))
  })
})

test_that("more than 100 rows wait for 'Continue'; 'Cancel' shows nothing", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    run_heatmap(session, list(row_selection_options = "all"))
    expect_null(session$returned())
    session$setInputs(cancel_heatmap = 1)
    expect_match(output$Heatmap_Info, "user's choice", fixed = TRUE)
    session$setInputs(Do_Heatmap = 2)
    session$setInputs(continue_heatmap = 1)
    expect_equal(session$returned()$entities, rownames(ds$handle$processed()))
  })
})

test_that("fewer than two rows give the info text, no result", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    # Top K without a comparison: entitieSelection() gives NULL
    run_heatmap(session, list(row_selection_options = "Top K", TopK = 10, TopK_order = "LogFoldChange"))
    expect_null(session$returned())
    expect_match(output$Heatmap_Info, "fewer than two rows", fixed = TRUE)
  })
})

test_that("'Save genes' puts the Ensembl labels into the result; other labels are not saved", {
  tmm <- fake_dataset("Transcriptomics", "TMM")
  testServer(heatmap_server, args = list(dataset = tmm), {
    column <- ensembl_column(tmm$processed())
    run_heatmap(session, c(top_k_inputs(tmm, "condition"), list(row_label_options = column)))
    session$setInputs(SaveGeneList_Heatmap = 1)
    res <- session$returned()
    expect_equal(res$gene_list, rowData(tmm$processed())[res$entities, column])
    # a new run clears it
    session$setInputs(Do_Heatmap = 2)
    expect_null(session$returned()$gene_list)
  })
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    run_heatmap(session, top_k_inputs(ds$handle, "diet"))
    session$setInputs(SaveGeneList_Heatmap = 1)
    expect_null(session$returned()$gene_list)
  })
})

test_that("a new Dataset version clears the result", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    run_heatmap(session, top_k_inputs(ds$handle, "diet"))
    expect_false(is.null(session$returned()))
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
    expect_null(heatmap_reactives$run)
  })
})

test_that("a display change updates the parameters, not the selection", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    run_heatmap(session, top_k_inputs(ds$handle, "diet"))
    before <- session$returned()
    label <- colnames(rowData(ds$handle$processed()))[2]
    session$setInputs(cluster_rows = FALSE, rowWiseScaled = TRUE, anno_options = "diet", row_label_options = label)
    after <- session$returned()
    expect_false(after$params$cluster_rows)
    expect_true(after$params$scale_rows)
    expect_equal(after$params$col_annotations, "diet")
    expect_equal(after$params$row_labels, rowData(ds$handle$processed())[after$entities, label])
    expect_equal(after$entities, before$entities)
    expect_equal(after$version, before$version)
  })
})

test_that("the R-code zip carries the Heatmap parameters under the template's names", {
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    run_heatmap(session, top_k_inputs(ds$handle, "diet"))
    par <- rcode_parameters(output$getR_Code_Heatmap)
    expect_true(all(c(
      "selection_type", "n_top_k", "top_k_type", "compare_within", "reference",
      "treatment", "significance_level", "title", "cluster_rows", "cluster_cols",
      "scale_rows", "n_max_row_labels", "row_labels", "row_annotations", "col_annotations"
    ) %in% names(par$Heatmap)))
    expect_equal(par$Heatmap$compare_within, "diet")
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("the snippet names the annotations the samples and entities were coloured after", {
  params <- list(Heatmap = list(
    selection_type = "all", col_annotations = c("diet", "sex"), row_annotations = "class",
    cluster_rows = TRUE, cluster_cols = FALSE, scale_rows = FALSE
  ))
  snippet <- snippet_heatmap(NULL, params)
  expect_match(snippet, "samples were colored after diet, sex.", fixed = TRUE)
  expect_match(snippet, "entities were colored after class.", fixed = TRUE)
})

test_that("dataset_server() puts the result into results()$heatmap; a new run clears it", {
  collected <- expect_collected("heatmap", function(session, handle){
    groups <- unique(colData(handle$processed())$diet)
    session$setInputs(
      "heatmap-row_selection_options" = "Top K", "heatmap-TopK" = 10, "heatmap-TopK_order" = "LogFoldChange",
      "heatmap-sample_annotation_types_cmp_heatmap" = "diet",
      "heatmap-Groups2Compare_ref_heatmap" = groups[1], "heatmap-Groups2Compare_treat_heatmap" = groups[2]
    )
    session$setInputs("heatmap-Do_Heatmap" = 1)
  })
  expect_length(collected$entities, 10)
})

test_that("the result area shows only while there is a result", {
  shown <- local_shown()
  ds <- changeable_dataset("Lipidomics")
  testServer(heatmap_server, args = list(dataset = ds$handle), {
    session$flushReact()
    expect_false(shown("Heatmap_div"))
    # Top K without a comparison: fewer than two rows, no result
    run_heatmap(session, list(row_selection_options = "Top K", TopK = 10, TopK_order = "LogFoldChange"))
    expect_false(shown("Heatmap_div"))
    run_heatmap(session, top_k_inputs(ds$handle, "diet"), go = 2)
    expect_true(shown("Heatmap_div"))
    # a selection with fewer than two rows after a successful run
    session$setInputs(TopK = 1)
    session$setInputs(Do_Heatmap = 3)
    expect_false(shown("Heatmap_div"))
    expect_null(session$returned())
    # a heatmap that can't be drawn
    session$setInputs(TopK = 10)
    local({
      local_failing("plot_heatmap")
      session$setInputs(Do_Heatmap = 4)
      expect_false(shown("Heatmap_div"))
    })
    session$setInputs(Do_Heatmap = 5)
    expect_true(shown("Heatmap_div"))
    ds$state$version <- 3
    session$flushReact()
    expect_false(shown("Heatmap_div"))
  })
})
