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
    compare_within = "group", reference = "ctrl", treatment = "trt", significance_level = 0.05
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
