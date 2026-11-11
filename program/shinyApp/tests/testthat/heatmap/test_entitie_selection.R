# Heatmap row selection, Top K by fold change (bug 02, and the order: the
# top K are the highest fold changes, as the docs say)
# 6 entities, 3 + 3 samples: e1-e3 differ clearly between the groups
# (significant; log2 fold changes about 2, -1 and 0.98), e4-e6 hardly (not
# significant)
topk_data <- function(){
  values <- rbind(
    e1 = c(10, 11, 10.5, 40, 41, 40.5),
    e2 = c(10, 10.5, 11, 5, 5.5, 5.2),
    e3 = c(10, 10.2, 10.4, 20, 20.3, 20.1),
    e4 = c(10, 12, 11, 11, 10, 12),
    e5 = c(10, 11, 12, 12, 10, 11),
    e6 = c(10, 12, 10, 11, 11, 12)
  )
  colnames(values) <- paste0("s", 1:6)
  SummarizedExperiment::SummarizedExperiment(
    assays = list(raw = values),
    colData = S4Vectors::DataFrame(group = rep(c("ctrl", "trt"), each = 3), row.names = colnames(values))
  )
}
select_top_k <- function(top_k_type, n_top_k = 10){
  entitieSelection(
    topk_data(), selection_type = "Top K", n_top_k = n_top_k, top_k_type = top_k_type,
    compare_within = "group", reference = "ctrl", treatment = "trt", significance_level = 0.05,
    preprocessing = "none"
  )
}

test_that("'... and Significant' keeps only the significant entities, highest fold change first, without NA rows", {
  selected <- select_top_k("LogFoldChange and Significant")
  expect_equal(rownames(selected), c("e1", "e3", "e2"))
  expect_false(anyNA(selected))
  selected_abs <- select_top_k("absolute LogFoldChange and Significant")
  expect_equal(rownames(selected_abs), c("e1", "e2", "e3"))
  expect_false(anyNA(selected_abs))
})

test_that("'... and Significant' takes the top K of the significant entities", {
  expect_equal(rownames(select_top_k("LogFoldChange and Significant", n_top_k = 2)), c("e1", "e3"))
})

test_that("plain fold-change orders take all entities, highest first", {
  expect_equal(nrow(select_top_k("LogFoldChange")), 6)
  expect_equal(rownames(select_top_k("LogFoldChange", n_top_k = 1)), "e1")
  expect_equal(rownames(select_top_k("absolute LogFoldChange", n_top_k = 2)), c("e1", "e2"))
})

test_that("Top K orders by compare_groups()'s fold change, which reads log10 data back", {
  # e1: a log10 difference of 1 (fold change 10), ratio of the means 2;
  # e2: a log10 difference of 0.4 (fold change 2.5), ratio of the means 5
  values <- rbind(
    e1 = c(0.9, 1, 1.1, 1.9, 2, 2.1),
    e2 = c(0.05, 0.1, 0.15, 0.45, 0.5, 0.55),
    e3 = c(1, 1.1, 1.2, 1, 1.1, 1.2)
  )
  colnames(values) <- paste0("s", 1:6)
  data <- SummarizedExperiment::SummarizedExperiment(
    assays = list(processed = values),
    colData = S4Vectors::DataFrame(group = rep(c("ctrl", "trt"), each = 3), row.names = colnames(values))
  )
  selected <- entitieSelection(
    data, selection_type = "Top K", n_top_k = 2, top_k_type = "LogFoldChange",
    compare_within = "group", reference = "ctrl", treatment = "trt", significance_level = 0.05,
    preprocessing = "log10"
  )
  expect_equal(rownames(selected), c("e1", "e2"))
})

test_that("Top K without a treatment group (picker not rendered yet) gives NULL, no comparison", {
  selected <- entitieSelection(
    topk_data(), selection_type = "Top K", n_top_k = 2, top_k_type = "LogFoldChange",
    compare_within = "group", reference = "ctrl", treatment = NULL, significance_level = 0.05,
    preprocessing = "none"
  )
  expect_null(selected)
})

test_that("with DESeq2, Top K orders only the stage's rows (batch correction drops incomplete ones)", {
  deseq <- fake_dataset("Transcriptomics", "vst_DESeq")
  data <- shiny::isolate(deseq$processed())[1:50, ]
  groups <- as.character(unique(SummarizedExperiment::colData(data)$condition))
  selected <- entitieSelection(
    data, selection_type = "Top K", n_top_k = 100, top_k_type = "absolute LogFoldChange",
    compare_within = "condition", reference = groups[1], treatment = groups[2],
    significance_level = 0.05, preprocessing = "vst_DESeq", dds = shiny::isolate(deseq$deseq())
  )
  expect_setequal(rownames(selected), rownames(data))
})
