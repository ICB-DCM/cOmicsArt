# test for siltering of significant genes
test_that("filter significant genes work", {
  # create a test dataset
  data <- data.frame(
    gene = c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6", "gene7", "gene8", "gene9", "gene10"),
    log2FoldChange = c(-1, 2, -3, 4, -5, 6, -7, 8, -9, 10),
    pvalue = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.06, 0.07, 0.08, 0.09, 0.1),
    padj = c(0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 0.14, 0.16, 0.18, 0.2)
  )
  # define what we expect for each filter_type
  expected_res <- list(
    "Significant unadjusted" = c("gene1", "gene2", "gene3", "gene4"),
    "Significant" = c("gene1", "gene2"),
    "Upregulated" = c("gene2"),
    "Downregulated" = c("gene1")
  )
  # test for each filter_type
  for (filter_type in names(expected_res)){
      # filter the data
      res <- filter_significant_result(
        result = data,
        alpha = 0.05,
        filter_type = filter_type
      )
      # test if the result is as expected
      expect_equal(res$gene, expected_res[[filter_type]])
    }
})