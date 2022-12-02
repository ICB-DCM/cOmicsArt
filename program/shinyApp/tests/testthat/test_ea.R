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
      res <- translate_genes_ea(data, anno_res[[i]], input, testing = TRUE)
      names(res) <- c("1", "2")
      # check if output is as expected
      expect_equal(as.list(res), list("1" = "7105", "2" = "8813"))
    }
})