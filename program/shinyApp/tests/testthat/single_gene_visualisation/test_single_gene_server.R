# tests for Single Gene on the Dataset handle (issue 31)
source(test_path("..", "dataset", "helper-dataset.R"), local = TRUE)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# inputs for one entity of `data`, grouped by `group_by`, with the first
# comparison of its groups
gene_inputs <- function(data, group_by, stage = "data"){
  annotation <- colnames(rowData(data))[1]
  groups <- as.character(unique(colData(data)[, group_by]))
  list(
    type_of_data_gene = stage, accross_condition = group_by, add_testing = "TRUE",
    Select_GeneAnno = annotation, Select_Gene = as.character(rowData(data)[1, annotation]),
    chooseComparisons = paste(groups[1:2], collapse = ":")
  )
}
# what the module should show for `inputs` on `data`
expected_gene_data <- function(data, processed, inputs){
  get_single_gene_data(
    data = data, selected_type = inputs$Select_GeneAnno, selected_gene = inputs$Select_Gene,
    group_by = inputs$accross_condition,
    post_selection_check = unique(c(colData(processed)[, inputs$accross_condition]))
  )
}
run <- function(session, inputs){
  do.call(session$setInputs, inputs)
  session$setInputs(singleGeneGo = 1)
}

test_that("the pickers follow the processed data; batch corrected data only with a batch column", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    session$flushReact()
    data <- ds$handle$processed()
    stages <- as.character(output$type_of_data_gene_ui$html)
    expect_match(stages, 'value="data_original"', fixed = TRUE)
    expect_match(stages, 'value="data"', fixed = TRUE)
    expect_false(grepl("data_batch_corrected", stages, fixed = TRUE))
    groups <- as.character(output$accross_condition_ui$html)
    for (column in colnames(colData(data))) expect_match(groups, sprintf('value="%s"', column), fixed = TRUE)
    annotations <- as.character(output$Select_GeneAnno_ui$html)
    for (column in colnames(rowData(data))) expect_match(annotations, sprintf('value="%s"', column), fixed = TRUE)
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(single_gene_visualisation_server, args = list(dataset = tmm), {
    session$flushReact()
    expect_match(as.character(output$type_of_data_gene_ui$html), 'value="data_batch_corrected"', fixed = TRUE)
  })
})

test_that("a run returns the entity's values with its parameters and the Dataset version", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    expect_null(session$returned())
    inputs <- gene_inputs(ds$handle$processed(), "diet")
    run(session, inputs)
    res <- session$returned()
    expect_equal(res$gene_data, expected_gene_data(ds$handle$processed(), ds$handle$processed(), inputs))
    expect_equal(res$params$selected_gene, inputs$Select_Gene)
    expect_equal(res$params$selected_type, inputs$Select_GeneAnno)
    expect_equal(res$params$group_by, "diet")
    expect_equal(res$params$data_process_stage, "data")
    expect_equal(res$params$comparisons, inputs$chooseComparisons)
    expect_true(res$params$add_testing)
    expect_equal(res$version, 2)
  })
})

test_that("raw data is the Selection before Pre-processing; batch corrected data is processed(batch = TRUE)", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    inputs <- gene_inputs(ds$handle$processed(), "diet", stage = "data_original")
    run(session, inputs)
    expect_equal(
      session$returned()$gene_data,
      expected_gene_data(ds$handle$raw(), ds$handle$processed(), inputs)
    )
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(single_gene_visualisation_server, args = list(dataset = tmm), {
    inputs <- gene_inputs(tmm$processed(), "condition", stage = "data_batch_corrected")
    run(session, inputs)
    gene_data <- session$returned()$gene_data
    expect_equal(gene_data, expected_gene_data(tmm$processed(batch = TRUE), tmm$processed(), inputs))
    expect_false(isTRUE(all.equal(gene_data, expected_gene_data(tmm$processed(), tmm$processed(), inputs))))
  })
})

test_that("a new Dataset version clears the result", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    run(session, gene_inputs(ds$handle$processed(), "diet"))
    expect_false(is.null(session$returned()))
    ds$state$version <- 3
    session$flushReact()
    expect_null(session$returned())
  })
})

test_that("changing the comparisons or the style updates the parameters, not the values", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    inputs <- gene_inputs(ds$handle$processed(), "diet")
    run(session, inputs)
    before <- session$returned()
    groups <- as.character(unique(colData(ds$handle$processed())$diet))
    comparisons <- c(paste(groups[1:2], collapse = ":"), paste(groups[c(1, 3)], collapse = ":"))
    session$setInputs(chooseComparisons = comparisons, add_testing = "FALSE")
    after <- session$returned()
    expect_equal(after$params$comparisons, comparisons)
    expect_false(after$params$add_testing)
    expect_identical(after$gene_data, before$gene_data)
  })
})

test_that("the R-code zip carries the Selection, Pre-processing and Single Gene parameters", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    inputs <- gene_inputs(ds$handle$processed(), "diet")
    run(session, inputs)
    dir <- unpack_zip(output$getR_Code_SingleEntities)
    expect_true(all(c("Code.R", "Data.rds") %in% list.files(dir)))
    par <- readRDS(file.path(dir, "Data.rds"))$parameters
    expect_equal(par$SingleGeneVis$selected_gene, inputs$Select_Gene)
    expect_equal(par$SingleGeneVis$comparisons, inputs$chooseComparisons)
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("dataset_server() puts the result into results()$single_gene; a new run clears it", {
  log10 <- list(preprocessing_procedure = "log10", preprocessing_filtering = "filterOnly", filter_threshold = 0)
  testServer(dataset_server, args = confirmed_dataset_args("Lipidomics"), {
    run_all <- preprocessing_run(selection$selected(), "Lipidomics", log10)
    preprocessing$result(run_all)
    session$flushReact()
    handle <- session$returned
    inputs <- gene_inputs(handle$processed(), "diet")
    names(inputs) <- paste0("single_gene_visualisation-", names(inputs))
    do.call(session$setInputs, inputs)
    session$setInputs("single_gene_visualisation-singleGeneGo" = 1)
    expect_equal(handle$results()$single_gene$version, handle$version())

    preprocessing$result(modifyList(run_all, list(run = 2)))
    session$flushReact()
    expect_null(handle$results()$single_gene)
  })
})
