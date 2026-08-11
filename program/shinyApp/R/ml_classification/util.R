# ML Classification Utility Functions

#' Validate that preprocessed data exists and is ready
#'
#' @param data_matrix Matrix of preprocessed data
#' @return TRUE if valid, otherwise throws validation error
validate_preprocessed_data <- function(data_matrix) {
  if (is.null(data_matrix)) {
    stop("No preprocessed data available. Please run preprocessing first.")
  }
  if (nrow(data_matrix) == 0) {
    stop("Data matrix is empty.")
  }
  if (ncol(data_matrix) == 0) {
    stop("No samples in data matrix.")
  }
  return(TRUE)
}

#' Validate no missing values in data
#'
#' @param data_matrix Matrix to check
#' @return TRUE if valid, otherwise throws validation error
validate_no_missing_values <- function(data_matrix) {
  if (any(is.na(data_matrix))) {
    stop("Data contains missing values (NA). Please handle missing data in preprocessing.")
  }
  return(TRUE)
}

#' Check minimum sample size and warn if low
#'
#' @param data_matrix Matrix with samples as columns
#' @param min_samples Minimum recommended samples
#' @return TRUE, with warning if below threshold
validate_minimum_samples <- function(data_matrix, min_samples = 10) {
  n_samples <- ncol(data_matrix)

  if (n_samples < 3) {
    stop("Too few samples for clustering (minimum 3 required).")
  }

  if (n_samples < min_samples) {
    if (exists("showNotification", mode = "function")) {
      showNotification(
        sprintf("Warning: Only %d samples available. Results may be unreliable with few samples.", n_samples),
        type = "warning",
        duration = 8
      )
    } else {
      warning(sprintf("Only %d samples available. Results may be unreliable with few samples.", n_samples))
    }
  }

  return(TRUE)
}

#' Filter data to top variable genes
#'
#' @param data_matrix Matrix with genes as rows
#' @param n_genes Number of genes to keep
#' @return Filtered matrix
filter_top_variable_genes <- function(data_matrix, n_genes) {
  # Calculate variance for each gene
  gene_vars <- apply(data_matrix, 1, var, na.rm = TRUE)

  # Get top N most variable genes
  top_genes <- order(gene_vars, decreasing = TRUE)[1:min(n_genes, nrow(data_matrix))]

  return(data_matrix[top_genes, , drop = FALSE])
}

#' Check for degenerate clustering (most samples in one cluster)
#'
#' @param cluster_assignments Vector of cluster assignments
#' @param threshold Proportion threshold for degeneracy (default 0.8)
#' @return TRUE, with warning if degenerate
check_degenerate_clustering <- function(cluster_assignments, threshold = 0.8) {
  cluster_sizes <- table(cluster_assignments)
  max_proportion <- max(cluster_sizes) / sum(cluster_sizes)

  if (max_proportion > threshold) {
    if (exists("showNotification", mode = "function")) {
      showNotification(
        sprintf(
          "Warning: %.0f%% of samples assigned to a single cluster. Consider reducing k or checking data quality.",
          max_proportion * 100
        ),
        type = "warning",
        duration = 10
      )
    } else {
      warning(sprintf(
        "%.0f%% of samples assigned to a single cluster. Consider reducing k or checking data quality.",
        max_proportion * 100
      ))
    }
  }

  return(TRUE)
}

#' Validate condition column for supervised learning
#'
#' @param condition_col Name of condition column
#' @param sample_annotation Sample annotation data frame
#' @return TRUE if valid, FALSE otherwise (with notifications)
validate_condition_column_supervised <- function(condition_col, sample_annotation) {

  # Check if column exists
  if (!condition_col %in% names(sample_annotation)) {
    showNotification(
      "Selected condition column not found in sample annotation.",
      type = "error"
    )
    return(FALSE)
  }

  condition_values <- sample_annotation[[condition_col]]

  # Check if continuous (not supported in v1)
  if (is.numeric(condition_values)) {
    showModal(modalDialog(
      title = "Continuous Variable Detected",
      HTML("
        <p><strong>SVM Classification only supports categorical outcomes.</strong></p>
        <p>Regression support for continuous variables is coming in a future release!</p>
        <p>For now, please:</p>
        <ul>
          <li>Select a categorical condition column (e.g., 'treated' vs 'control'), OR</li>
          <li>Use unsupervised clustering methods instead</li>
        </ul>
      "),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
    return(FALSE)
  }

  # Check number of levels
  n_levels <- length(unique(na.omit(condition_values)))

  if (n_levels < 2) {
    showNotification(
      "All samples have the same condition value. Cannot classify.",
      type = "error",
      duration = NULL
    )
    return(FALSE)
  }

  if (n_levels > 10) {
    showNotification(
      sprintf(
        "Warning: Condition has %d levels. Results may be difficult to interpret with many classes.",
        n_levels
      ),
      type = "warning",
      duration = 10
    )
  }

  return(TRUE)
}
