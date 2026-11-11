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

test_that("the shown boxplot has the unadjusted Welch p-value of each comparison, labelled so (issue 60)", {
  plot <- single_gene_boxplot(
    gene_data, add_testing = TRUE, comparisons = c("A:B", "A:C"),
    selected_gene = "gene", data_stage = "processed", group_by = "group"
  )
  p_value <- function(x, y) {
    t.test(gene_data$value[gene_data$annotation == x], gene_data$value[gene_data$annotation == y])$p.value
  }
  texts <- plotly_texts(plot)
  expect_true(paste0("p = ", signif(p_value("A", "B"), 2), " (unadjusted)") %in% texts)
  expect_true(paste0("p = ", signif(p_value("A", "C"), 2), " (unadjusted)") %in% texts)
})

test_that("the boxplot without testing has no p-values", {
  plot <- single_gene_boxplot(
    gene_data, add_testing = FALSE, comparisons = c("A:B"),
    selected_gene = "gene", data_stage = "processed", group_by = "group"
  )
  expect_null(plotly_texts(plot))
})

test_that("a comparison that can't be tested says so instead of p = NA (issue 60)", {
  constant <- data.frame(value = c(rep(1, 4), rep(1, 4)), annotation = factor(rep(c("A", "B"), each = 4)))
  plot <- suppressMessages(single_gene_boxplot(
    constant, add_testing = TRUE, comparisons = "A:B",
    selected_gene = "gene", data_stage = "processed", group_by = "group"
  ))
  expect_true("p not computable" %in% plotly_texts(plot))
})
