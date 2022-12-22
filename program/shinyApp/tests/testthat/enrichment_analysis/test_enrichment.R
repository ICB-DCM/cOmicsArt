# test translation of genes
test_that("translation works", {
  # create test data
  data <- list(
    Transcriptomics = list(
      Matrix = matrix(
        data = c(1, 2, 3, 4, 5, 6),
        nrow = 2,
        ncol = 3,
        dimnames = list(
          c("7105", "8813"),
          c("sample1", "sample2", "sample3")
        )
      ),
      annotation_rows = data.frame(
        Gene = c("7105", "8813"),
        GeneName = c("gene1", "gene2"),
        ENSEMBL = c("ENSG00000000003", "ENSG00000000419"),
        ENTREZID = c("7105", "8813")
      )
    )
  )
  # create input
  input <- list(
    OrganismChoice = "hsa"
  )
  # create annotation results for Entrez Id and for ENSEMBL
  anno_res <- list()
  anno_res[[1]] <- list(
    "no_ann" = FALSE,
    "base_annotation" = "ENTREZID",
    "can_start" = TRUE
  )
  anno_res[[2]] <- list(
      "no_ann" = FALSE,
      "base_annotation" = "ENSEMBL",
      "can_start" = FALSE
  )
  # run function for each annotation result
    for (i in 1:length(anno_res)){
      # run function
      res <- translate_genes_ea(data, anno_res[[i]], input)
      names(res) <- c("1", "2")
      # check if output is as expected
      expect_equal(as.list(res), list("1" = "7105", "2" = "8813"))
    }
})


# test the function that checks if the annotation is correct
test_that("check annotation works", {
  # create test data
  data <- list(
    Transcriptomics = list(
      Matrix = matrix(
        data = c(1, 2, 3, 4, 5, 6),
        nrow = 2,
        ncol = 3,
        dimnames = list(
          c("7105", "8813"),
          c("sample1", "sample2", "sample3")
        )
      ),
      # create annotation with different names
      annotation_rows = data.frame(
          Gene = c("7105", "8813"),
          SYMBOL = c("TSPAN6", "DPM1"),
          symbol = c("TSPAN6", "DPM1"),
          gene_symbol = c("TSPAN6", "DPM1"),
          Gene_Symbol = c("TSPAN6", "DPM1"),
          Nomenclature = c("TSPAN6", "DPM1"),
          Gene.Symbol = c("TSPAN6", "DPM1"),
          ENSEMBL = c("ENSG00000000003", "ENSG00000000419"),
          ensembl = c("ENSG00000000003", "ENSG00000000419"),
          ensembl_id = c("ENSG00000000003", "ENSG00000000419"),
          Ensembl = c("ENSG00000000003", "ENSG00000000419"),
          Ensembl_ID = c("ENSG00000000003", "ENSG00000000419"),
          Ensemble.ID = c("ENSG00000000003", "ENSG00000000419"),
          ENTREZID = c("7105", "8813"),
          entrez = c("7105", "8813"),
          Entrez = c("7105", "8813"),
          entrez_id = c("7105", "8813"),
          Entrez_ID = c("7105", "8813"),
          Entrez.ID = c("7105", "8813"),
          Entrez_Gene_ID = c("7105", "8813")
      )
    )
  )
  # create expected output for each annotation
  expected_output <- list()
  expected_output$no_match <- list(
    "no_ann" = TRUE,
    "base_annotation" = NULL,
    "can_start" = FALSE
  )
  expected_output$ENSEMBL <- list(
      "no_ann" = FALSE,
      "base_annotation" = "ENSEMBL",
      "can_start" = FALSE
  )
  expected_output$ENTREZID <- list(
      "no_ann" = FALSE,
      "base_annotation" = "ENTREZID",
      "can_start" = TRUE
  )
  expected_output$SYMBOL <- list(
      "no_ann" = FALSE,
      "base_annotation" = "SYMBOL",
      "can_start" = FALSE
  )
  # create a list that assigns each annotation to an expected output name.
  # has the same order as the annotation names in the test data
  results_bin_list <- list(
    Gene = "no_match",
    SYMBOL = "SYMBOL",
    symbol = "SYMBOL",
    gene_symbol = "SYMBOL",
    Gene_Symbol = "SYMBOL",
    Nomenclature = "SYMBOL",
    Gene.Symbol = "SYMBOL",
    ENSEMBL = "ENSEMBL",
    ensembl = "ENSEMBL",
    ensembl_id = "ENSEMBL",
    Ensembl = "ENSEMBL",
    Ensembl_ID = "ENSEMBL",
    Ensemble.ID = "ENSEMBL",
    ENTREZID = "ENTREZID",
    entrez = "ENTREZID",
    Entrez = "ENTREZID",
    entrez_id = "ENTREZID",
    Entrez_ID = "ENTREZID",
    Entrez.ID = "ENTREZID",
    Entrez_Gene_ID = "ENTREZID"
  )
  # for each annotation column, subset the data and check if the function returns the correct annotation
  for (i in 1:ncol(data$Transcriptomics$annotation_rows)){
    # subset data
    data_sub <- data
    data_sub$Transcriptomics$annotation_rows <- data_sub$Transcriptomics$annotation_rows[i]
    # browser()
    # run function
    res <- check_annotation_enrichment_analysis(data_sub)
    # check if output is as expected
    expect_equal(res, expected_output[[results_bin_list[[i]]]])
  }
  # check with all mixtures of ENSEMBL, ENTREZID and SYMBOL, if the function returns the correct annotation
  comparisons <- list(
    c("ENSEMBL", "ENTREZID"),
    c("ENSEMBL", "SYMBOL"),
    c("ENTREZID", "SYMBOL"),
    c("ENSEMBL", "ENTREZID", "SYMBOL")
  )
  # add them to the results bin list
  results_bin_list$ENSEMBL_ENTREZID <- "ENTREZID"
  results_bin_list$ENSEMBL_SYMBOL <- "ENSEMBL"
  results_bin_list$ENTREZID_SYMBOL <- "ENTREZID"
  results_bin_list$ENSEMBL_ENTREZID_SYMBOL <- "ENTREZID"
  # for each comparison, subset the data and check if the function returns the correct annotation
  for (i in 1:length(comparisons)){
    # subset data
    data_sub <- data
    data_sub$Transcriptomics$annotation_rows <- data_sub$Transcriptomics$annotation_rows[, comparisons[[i]]]
    # run function
    res <- check_annotation_enrichment_analysis(data_sub)
    # check if output is as expected
    expect_equal(res, expected_output[[results_bin_list[[i+20]]]])
  }
})
