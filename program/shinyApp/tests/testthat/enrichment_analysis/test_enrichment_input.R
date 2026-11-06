# tests for the ORA and GSEA preparers of Enrichment (issue 54)
colData <- SummarizedExperiment::colData
rowData <- SummarizedExperiment::rowData
`rowData<-` <- SummarizedExperiment::`rowData<-`

HUMAN <- "Human genes (GRCh38.p14)"
# replaces the app function `name` by `fn` until the test ends; returns a
# function giving the arguments of every call
local_app_function <- function(name, fn, env = parent.frame()){
  calls <- list()
  rlang::local_bindings(
    !!name := function(...) { calls[[length(calls) + 1]] <<- list(...); fn(...) },
    .env = environment(dataset_server), .frame = env
  )
  function() calls
}
# three genes by Ensembl ID, `annotation` their rowData columns
small_data <- function(annotation = list(entrezgene_id = c("7105", NA, "8813"))){
  ids <- c("ENSG1", "ENSG2", "ENSG3")
  SummarizedExperiment::SummarizedExperiment(
    assays = list(processed = matrix(1:6, nrow = 3, dimnames = list(ids, c("s1", "s2")))),
    rowData = do.call(S4Vectors::DataFrame, c(annotation, list(row.names = ids)))
  )
}
ora_params <- function(...){
  modifyList(list(
    ora_gene_set_type = "heatmap_genes", uploaded_gene_set = NULL,
    heatmap_genes = c("ENSG3", "ENSG1"), organism = HUMAN
  ), list(...))
}
# the Transcriptomics test data (with Entrez IDs) and a GSEA of untrt vs. trt
transcriptomics <- function() isolate(fake_dataset("Transcriptomics", "TMM")$processed())
gsea_params <- function(...){
  modifyList(list(
    gse_gene_set_type = "LFC", compare_within = "condition",
    reference = "untrt", treatment = "trt", organism = HUMAN
  ), list(...))
}
# `data` without any annotation column
without_annotation <- function(data){
  rowData(data) <- rowData(data)[, 0]
  data
}

## ORA

test_that("ORA takes the Heatmap's genes, as Entrez IDs from the data's annotation", {
  prepared <- prepare_ora_input(small_data(), ora_params())
  expect_equal(prepared$gene_set, c("ENSG3", "ENSG1"))
  expect_equal(prepared$genes, c("8813", "7105"))
  expect_null(prepared$needs_annotation)
})

test_that("ORA takes the first column of an uploaded gene set", {
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(gene = c("ENSG1", "ENSG3")), csv, row.names = FALSE)
  prepared <- prepare_ora_input(small_data(), ora_params(
    ora_gene_set_type = "ProvidedGeneSet", uploaded_gene_set = list(datapath = csv, name = "genes.csv")
  ))
  expect_equal(prepared$gene_set, c("ENSG1", "ENSG3"))
  expect_equal(prepared$genes, c("7105", "8813"))
})

test_that("ORA stops without genes, with non-Ensembl uploads and without a gene set type", {
  expect_error(prepare_ora_input(small_data(), ora_params(heatmap_genes = NULL)), ERROR_NO_HEATMAP_GENES, fixed = TRUE)
  expect_error(
    prepare_ora_input(small_data(), ora_params(ora_gene_set_type = "ProvidedGeneSet")),
    ERROR_NO_GENESET, fixed = TRUE
  )
  csv <- withr::local_tempfile(fileext = ".csv")
  write.csv(data.frame(gene = c("ENSG1", "TP53")), csv, row.names = FALSE)
  expect_error(
    prepare_ora_input(small_data(), ora_params(
      ora_gene_set_type = "ProvidedGeneSet", uploaded_gene_set = list(datapath = csv)
    )),
    ERROR_NON_ENSEMBL_GENES, fixed = TRUE
  )
  expect_error(prepare_ora_input(small_data(), ora_params(ora_gene_set_type = NULL)), ERROR_INVALID_ANALYSIS, fixed = TRUE)
})

test_that("ORA on data without annotation translates with biomaRt and never needs the annotation", {
  biomart <- local_app_function("translate_genes_oa", function(gene_set, organism) c("11", "33"))
  prepared <- prepare_ora_input(without_annotation(small_data()), ora_params())
  expect_equal(prepared$genes, c("11", "33"))
  expect_equal(biomart()[[1]], list(c("ENSG3", "ENSG1"), HUMAN))
  expect_null(prepared$needs_annotation)
})

test_that("ORA names a failed translation", {
  local_app_function("translate_genes_oa", function(...) stop("biomaRt down"))
  expect_error(
    prepare_ora_input(without_annotation(small_data()), ora_params()),
    "could not be translated to Entrez IDs"
  )
})

## GSEA

test_that("GSEA ranks by the fold change; Entrez-annotated data starts without translation", {
  translate <- local_app_function("translate_genes_ea", function(...) stop("not expected"))
  data <- transcriptomics()
  prepared <- prepare_gsea_input(data, gsea_params())
  expect_false(prepared$needs_annotation)
  expect_equal(names(prepared$genes), rownames(data))
  expect_equal(prepared$gene_set, prepared$genes)
  expect_true("entrezgene_id" %in% colnames(rowData(prepared$data)))
  expect_length(translate(), 0)
  abs_lfc <- prepare_gsea_input(data, gsea_params(gse_gene_set_type = "LFC_abs"))$genes
  expect_equal(unname(abs_lfc), unname(abs(prepared$genes)))
})

test_that("GSEA on data annotated by gene symbols translates them", {
  # three genes have no groups to compare: a fixed ranking
  local_app_function("gsea_ranking", function(...) c(ENSG1 = 1, ENSG2 = 2, ENSG3 = 3))
  translate <- local_app_function("translate_genes_ea", function(data, annotation_results, organism){
    rowData(data)$entrezgene_id <- c("1", "2", "3")
    data
  })
  prepared <- prepare_gsea_input(small_data(list(external_gene_name = c("A", "B", "C"))), gsea_params())
  expect_false(prepared$needs_annotation)
  expect_equal(rowData(prepared$data)$entrezgene_id, c("1", "2", "3"))
  expect_equal(translate()[[1]][[2]]$base_annotation, "external_gene_name")
  expect_equal(translate()[[1]][[3]], HUMAN)
})

test_that("GSEA on data without a known annotation needs the annotation; then it translates", {
  translate <- local_app_function("translate_genes_ea", function(data, annotation_results, organism){
    rowData(data)$entrezgene_id <- rownames(data)
    data
  })
  data <- without_annotation(transcriptomics())
  prepared <- prepare_gsea_input(data, gsea_params())
  expect_true(prepared$needs_annotation)
  expect_length(translate(), 0)
  expect_equal(names(prepared$genes), rownames(data))

  annotated <- annotate_gsea_input(prepared, "ensembl_gene_id", HUMAN)
  expect_false(annotated$needs_annotation)
  expect_equal(translate()[[1]][[2]]$base_annotation, "ensembl_gene_id")
  expect_true(translate()[[1]][[2]]$no_ann)
  expect_equal(rowData(annotated$data)$entrezgene_id, rownames(data))
  expect_equal(annotated$genes, prepared$genes)
})

test_that("GSEA names a failed translation and stops without a ranking type", {
  local_app_function("translate_genes_ea", function(...) stop("biomaRt down"))
  data <- transcriptomics()
  rowData(data) <- S4Vectors::DataFrame(external_gene_name = rownames(data), row.names = rownames(data))
  expect_error(prepare_gsea_input(data, gsea_params()), "wrong organism was selected")
  expect_error(prepare_gsea_input(transcriptomics(), gsea_params(gse_gene_set_type = NULL)), ERROR_INVALID_ANALYSIS, fixed = TRUE)
})
