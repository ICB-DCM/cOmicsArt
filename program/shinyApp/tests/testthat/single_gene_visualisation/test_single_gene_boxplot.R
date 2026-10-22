# tests for the single gene boxplot in R/single_gene_visualisation/util.R
gene_data <- data.frame(
  value = c(1.0, 1.2, 0.9, 1.1, 5.0, 5.3, 4.8, 5.1, 1.3, 0.8, 1.0, 1.2),
  annotation = factor(rep(c("A", "B", "C"), each = 4))
)
# the texts plotly draws for `plot`, as the app shows it
plotly_texts <- function(plot){
  built <- plotly::plotly_build(plotly::ggplotly(plot))
  unlist(lapply(built$x$data, function(trace) if (identical(trace$mode, "text")) trace$text))
}

test_that("the shown boxplot has the t-test p-value of each comparison", {
  plot <- single_gene_boxplot(
    gene_data, add_testing = TRUE, comparisons = c("A:B", "A:C"),
    selected_gene = "gene", data_process_stage = "data", group_by = "group"
  )
  p_value <- function(x, y) {
    t.test(gene_data$value[gene_data$annotation == x], gene_data$value[gene_data$annotation == y])$p.value
  }
  texts <- plotly_texts(plot)
  expect_true(as.character(signif(p_value("A", "B"), 2)) %in% texts)
  expect_true(as.character(signif(p_value("A", "C"), 2)) %in% texts)
})

test_that("the boxplot without testing has no p-values", {
  plot <- single_gene_boxplot(
    gene_data, add_testing = FALSE, comparisons = c("A:B"),
    selected_gene = "gene", data_process_stage = "data", group_by = "group"
  )
  expect_null(plotly_texts(plot))
})
