# tests for Single Gene on the Dataset handle (issue 31)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData

# inputs for one entity of `data`, grouped by `group_by`, with the first
# comparison of its groups
gene_inputs <- function(data, group_by, stage = "processed"){
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
    expect_match(stages, 'value="raw"', fixed = TRUE)
    expect_match(stages, 'value="processed"', fixed = TRUE)
    expect_false(grepl("batch_corrected", stages, fixed = TRUE))
    groups <- as.character(output$accross_condition_ui$html)
    for (column in colnames(colData(data))) expect_match(groups, sprintf('value="%s"', column), fixed = TRUE)
    annotations <- as.character(output$Select_GeneAnno_ui$html)
    for (column in colnames(rowData(data))) expect_match(annotations, sprintf('value="%s"', column), fixed = TRUE)
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(single_gene_visualisation_server, args = list(dataset = tmm), {
    session$flushReact()
    expect_match(as.character(output$type_of_data_gene_ui$html), 'value="batch_corrected"', fixed = TRUE)
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
    expect_equal(res$params$data_stage, "processed")
    expect_equal(res$params$comparisons, inputs$chooseComparisons)
    expect_true(res$params$add_testing)
    expect_equal(res$version, 2)
  })
})

test_that("raw data is the Selection before Pre-processing; batch corrected data is data('batch_corrected')", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    inputs <- gene_inputs(ds$handle$processed(), "diet", stage = "raw")
    run(session, inputs)
    expect_equal(
      session$returned()$gene_data,
      expected_gene_data(ds$handle$data("raw"), ds$handle$processed(), inputs)
    )
  })
  tmm <- fake_dataset("Transcriptomics", "TMM", batch_column = "batch_factor")
  testServer(single_gene_visualisation_server, args = list(dataset = tmm), {
    inputs <- gene_inputs(tmm$processed(), "condition", stage = "batch_corrected")
    run(session, inputs)
    gene_data <- session$returned()$gene_data
    expect_equal(gene_data, expected_gene_data(tmm$data("batch_corrected"), tmm$processed(), inputs))
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
    par <- rcode_parameters(output$getR_Code_SingleEntities)
    expect_equal(par$SingleGeneVis$selected_gene, inputs$Select_Gene)
    expect_equal(par$SingleGeneVis$comparisons, inputs$chooseComparisons)
    expect_equal(par$preprocessing$preprocessing_procedure, "log10")
  })
})

test_that("dataset_server() puts the result into results()$single_gene; a new run clears it", {
  expect_collected("single_gene", function(session, handle){
    inputs <- gene_inputs(handle$processed(), "diet")
    names(inputs) <- paste0("single_gene_visualisation-", names(inputs))
    do.call(session$setInputs, inputs)
    session$setInputs("single_gene_visualisation-singleGeneGo" = 1)
  })
})

test_that("the report describes the run, whatever the controls say now; nothing before a run", {
  ds <- changeable_dataset("Lipidomics")
  test_env <- environment()
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    report <- local_session_report(session, test_env)
    data <- ds$handle$processed()
    session$setInputs(only2Report_SingleEntities = 1)
    expect_equal(report(), "")

    inputs <- gene_inputs(data, "diet")
    run(session, inputs)
    annotation <- inputs$Select_GeneAnno
    # another entity and data choice, not plotted
    session$setInputs(
      Select_Gene = as.character(rowData(data)[2, annotation]), type_of_data_gene = "raw"
    )
    session$setInputs(only2Report_SingleEntities = 2)
    expect_match(report(), paste("was plotted:", inputs$Select_Gene), fixed = TRUE)
    expect_match(report(), "Values shown are: pre-processed data", fixed = TRUE)
    groups <- levels(as.factor(colData(data)$diet))
    expect_match(report(), paste0("within: diet (", paste(groups, collapse = ";"), ")"), fixed = TRUE)
  })
})

test_that("a redrawn plot keeps the run's entity and data in its labels", {
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    data <- ds$handle$processed()
    inputs <- gene_inputs(data, "diet")
    run(session, inputs)
    groups <- as.character(unique(colData(data)$diet))
    # another entity and data choice, not run; then a new comparison redraws
    session$setInputs(
      Select_Gene = as.character(rowData(data)[2, inputs$Select_GeneAnno]), type_of_data_gene = "raw"
    )
    session$setInputs(chooseComparisons = paste(groups[2:3], collapse = ":"))
    labels <- single_gene_reactives$plot$labels
    expect_equal(labels$title, inputs$Select_Gene)
    expect_equal(labels$y, "Pre-Processed Data")
  })
})

test_that("the result area shows only while there is a result", {
  shown <- local_shown()
  ds <- changeable_dataset("Lipidomics")
  testServer(single_gene_visualisation_server, args = list(dataset = ds$handle), {
    session$flushReact()
    expect_false(shown("SingleGene_div"))
    do.call(session$setInputs, gene_inputs(ds$handle$processed(), "diet"))
    local({
      local_failing("get_single_gene_data")
      session$setInputs(singleGeneGo = 1)
    })
    expect_false(shown("SingleGene_div"))
    session$setInputs(singleGeneGo = 2)
    expect_true(shown("SingleGene_div"))
    # a failed run after a successful one
    local({
      local_failing("get_single_gene_data")
      session$setInputs(singleGeneGo = 3)
    })
    expect_false(shown("SingleGene_div"))
    session$setInputs(singleGeneGo = 4)
    ds$state$version <- 3
    session$flushReact()
    expect_false(shown("SingleGene_div"))
  })
})
