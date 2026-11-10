# compare_groups(): one comparison of two groups, used by every module that
# shows a fold change or a p-value

# two features, three samples per group; "b" is the treatment, "a" the control
two_groups <- function(values = rbind(gene1 = c(1, 2, 4, 5, 6, 8), gene2 = c(3, 5, 4, 2, 9, 7))){
  colnames(values) <- paste0("s", 1:6)
  SummarizedExperiment::SummarizedExperiment(
    assays = list(processed = values),
    colData = S4Vectors::DataFrame(group = rep(c("a", "b"), each = 3), row.names = colnames(values))
  )
}

test_that("by default a Welch t-test of treatment against control, BH-adjusted", {
  res <- compare_groups(two_groups(), "group", treat = "b", ctrl = "a", preprocessing = "none")
  welch1 <- t.test(c(5, 6, 8), c(1, 2, 4))
  welch2 <- t.test(c(2, 9, 7), c(3, 5, 4))
  expect_equal(rownames(res), c("gene1", "gene2"))
  expect_equal(res$pvalue, c(welch1$p.value, welch2$p.value))
  expect_equal(res$stat, unname(c(welch1$statistic, welch2$statistic)))
  expect_equal(res$padj, p.adjust(res$pvalue, method = "BH"))
  # log2 of the ratio of the group means, treatment over control: 19/3 over 7/3
  expect_equal(res$log2FoldChange[1], log2(19 / 7))
  expect_equal(res$baseMean, c(7 / 3, 4))
})

test_that("the chosen test and correction are used: Student's t-test, Wilcoxon, no correction", {
  student <- compare_groups(two_groups(), "group", treat = "b", ctrl = "a",
                            test = "T-Test", correction = "bonferroni", preprocessing = "none")
  expected <- t.test(c(5, 6, 8), c(1, 2, 4), var.equal = TRUE)
  expect_equal(student$pvalue[1], expected$p.value)
  expect_equal(student$stat[1], unname(expected$statistic))
  expect_equal(student$padj, pmin(1, 2 * student$pvalue))
  wilcoxon <- compare_groups(two_groups(), "group", treat = "b", ctrl = "a",
                             test = "Wilcoxon rank sum test", correction = "none", preprocessing = "none")
  # all treatment values of gene1 above all control values: W = 9, exact p = 2/20
  expect_equal(wilcoxon$stat[1], 9)
  expect_equal(wilcoxon$pvalue[1], 0.1)
  expect_equal(wilcoxon$padj, wilcoxon$pvalue)
  expect_equal(wilcoxon$log2FoldChange[1], log2(19 / 7))
})

test_that("the fold change reads log10 and ln data back, as getLFC() does", {
  log_data <- two_groups(rbind(gene1 = c(1, 1, 1, 2, 2, 2), gene2 = c(1, 2, 3, 1, 2, 3)))
  log10_res <- compare_groups(log_data, "group", treat = "b", ctrl = "a", preprocessing = "log10")
  # a difference of 1 on log10 data is a fold change of 10
  expect_equal(log10_res$log2FoldChange, c(log2(10), 0))
  ln_res <- compare_groups(log_data, "group", treat = "b", ctrl = "a", preprocessing = "ln")
  expect_equal(ln_res$log2FoldChange, c(log2(exp(1)), 0))
})

test_that("a feature whose test fails gets NA, the others their result", {
  constant <- two_groups(rbind(gene1 = c(1, 2, 4, 5, 6, 8), gene2 = rep(3, 6)))
  expect_message(
    res <- compare_groups(constant, "group", treat = "b", ctrl = "a", preprocessing = "none"),
    "No test for gene2"
  )
  expect_false(is.na(res$pvalue[1]))
  expect_true(is.na(res$pvalue[2]))
  expect_true(is.na(res$stat[2]))
})

test_that("with a DESeq2 object, DESeq2's results for treatment against control", {
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq")
  dds <- shiny::isolate(deseq$deseq("processed"))
  groups <- as.character(unique(SummarizedExperiment::colData(dds)$condition))
  res <- compare_groups(
    shiny::isolate(deseq$processed()), "condition", treat = groups[1], ctrl = groups[2],
    correction = "bonferroni", preprocessing = "vst_DESeq", dds = dds
  )
  expected <- DESeq2::results(dds, contrast = c("condition", groups[1], groups[2]), pAdjustMethod = "bonferroni")
  expect_equal(res$log2FoldChange, expected$log2FoldChange)
  expect_equal(res$padj, expected$padj)
})

test_that("with a DESeq2 object, a column outside its design is refused with a hint", {
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq")
  data <- shiny::isolate(deseq$processed())
  cells <- as.character(unique(SummarizedExperiment::colData(data)$cell))
  expect_error(
    compare_groups(data, "cell", treat = cells[1], ctrl = cells[2],
                   preprocessing = "vst_DESeq", dds = shiny::isolate(deseq$deseq("processed"))),
    "\"cell\" is not in the DESeq2 design. Add it to the DESeq2 design in Pre-processing.",
    fixed = TRUE
  )
})

test_that("a group without samples is refused, not tested to NA", {
  expect_error(
    compare_groups(two_groups(), "group", treat = "c", ctrl = "a", preprocessing = "none"),
    "No samples of \"c\" in the sample annotation \"group\".", fixed = TRUE
  )
  expect_error(
    compare_groups(two_groups(), "condition", treat = "b", ctrl = "a", preprocessing = "none"),
    "No samples of \"b\" in the sample annotation \"condition\".", fixed = TRUE
  )
})
