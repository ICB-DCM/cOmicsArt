# Summary Generators for Significance Analysis Results
# Pure functions for generating summary statistics text

#' Generate summary statistics for manual preprocessing results
#'
#' Creates a list of summary strings describing the differential expression results
#' from manual preprocessing methods (non-DESeq2).
#'
#' @param result Data frame with differential expression results containing columns:
#'   pvalue, padj, log2FoldChange
#' @param alpha Numeric significance threshold (default 0.05)
#' @return Character vector of summary lines
#' @export
#' @examples
#' result <- data.frame(
#'   pvalue = c(0.001, 0.05, 0.5),
#'   padj = c(0.01, 0.1, 0.6),
#'   row.names = c("Gene1", "Gene2", "Gene3")
#' )
#' summary <- generate_manual_summary(result, alpha = 0.05)
generate_manual_summary <- function(result, alpha = 0.05) {
  # Validate inputs
  if (!is.data.frame(result)) {
    stop("result must be a data frame")
  }
  if (!all(c("pvalue", "padj") %in% colnames(result))) {
    stop("result must contain pvalue and padj columns")
  }

  total_genes <- length(rownames(result))

  list(
    paste("Total number of entities compared:", total_genes),
    paste("Number of NA values in pvalue:", sum(is.na(result$pvalue))),
    paste(
      "Number of entities with significant p-value:",
      length(which(result$padj < alpha)),
      ",",
      round(length(which(result$padj < alpha)) / total_genes * 100, 2),
      "%"
    ),
    paste(
      "Number of significant entities without correction:",
      length(which(result$pvalue < alpha)),
      ",",
      round(length(which(result$pvalue < alpha)) / total_genes * 100, 2),
      "%"
    )
  )
}

#' Generate summary statistics for DESeq2 preprocessing results
#'
#' Creates a list of summary strings describing the differential expression results
#' from DESeq2 preprocessing, including upregulated and downregulated counts.
#'
#' @param result Data frame with differential expression results containing columns:
#'   pvalue, padj, log2FoldChange
#' @param alpha Numeric significance threshold (default 0.05)
#' @return Character vector of summary lines
#' @export
#' @examples
#' result <- data.frame(
#'   pvalue = c(0.001, 0.01, 0.5),
#'   padj = c(0.01, 0.02, 0.6),
#'   log2FoldChange = c(1.5, -2.0, 0.5),
#'   row.names = c("Gene1", "Gene2", "Gene3")
#' )
#' summary <- generate_deseq_summary(result, alpha = 0.05)
generate_deseq_summary <- function(result, alpha = 0.05) {
  # Validate inputs
  if (!is.data.frame(result)) {
    stop("result must be a data frame")
  }
  if (!all(c("pvalue", "padj", "log2FoldChange") %in% colnames(result))) {
    stop("result must contain pvalue, padj, and log2FoldChange columns")
  }

  total_genes <- length(rownames(result))

  list(
    paste("Total number of entities compared:", total_genes),
    paste(
      "Number of entities with significant p-value:",
      length(which(result$padj < alpha)),
      ",",
      round(length(which(result$padj < alpha)) / total_genes * 100, 2),
      "%"
    ),
    paste(
      "Number of upregulated significant entities:",
      length(which(result$padj < alpha & result$log2FoldChange > 0)),
      ",",
      round(length(which(result$padj < alpha & result$log2FoldChange > 0)) / total_genes * 100, 2),
      "%"
    ),
    paste(
      "Number of downregulated significant entities:",
      length(which(result$padj < alpha & result$log2FoldChange < 0)),
      ",",
      round(length(which(result$padj < alpha & result$log2FoldChange < 0)) / total_genes * 100, 2),
      "%"
    ),
    paste(
      "Number of significant entities without correction:",
      length(which(result$pvalue < alpha)),
      ",",
      round(length(which(result$pvalue < alpha)) / total_genes * 100, 2),
      "%"
    )
  )
}
