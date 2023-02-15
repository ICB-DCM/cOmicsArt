# test for filtering of significant genes
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


# test the significance_analysis function
test_that("significance_analysis works", {
  # create a test dataset
  # C1 - C4 are equal to B1 - B4 except for an NA in the last column
  data <- data.frame(
    row.names = c("gene1", "gene2", "gene3", "gene4", "gene5", "gene6", "gene7", "gene8"),
    A1 = c(1, 1, 1, 1, 1, 1, 1, 1),
    A2 = c(1, 1, 2, 2, 2, 2, 1, 1),
    A3 = c(1, 1, 3, 3, 3, 3, 1, 1),
    B1 = c(1, 2, 4, 4, 1, NA, 1, 1),
    B2 = c(1, 3, 4, 5, 2, NA, 2, 1),
    B3 = c(1, 4, 4, 6, 3, NA, 2, 2)
  )
  # create test sample df
  samples <- data.frame(
    row.names = list("A1", "A2", "A3", "B1", "B2", "B3"),
    woNA = c("A", "A", "A", "B", "B", "B")
  )
  # create test contrast df
  contrasts <- list(
    "A:B" = list("A", "B")
  )
  # create test result
  expected_res <- list(
    "T-Test" = c(NA, 0.0741799, 0.0741799, 0.02131164, 1, NA, 0.1835034, 0.4226497),
    "Wilcoxon" = c(NaN, 0.06360257, 0.06360257, 0.1, 1, NA, 0.1876323, 0.5049851)
  )
  # test for woth T-Test and Wilcoxon
  res <- list()
  for (method in c("T-Test", "Wilcoxon")){
    # perform the test
    res[[method]] <- significance_analysis(
      df = data,
      samples = samples,
      contrasts = contrasts,
      method = method,
      alpha = 0.05,
      correction = "none",
      contrast_level = "woNA"
    )[["A:B"]]$pvalue
  }
  # test if the result is as expected up to 7 digits
  expect_equal(res, expected_res, tolerance = 1e-7)
})