# k-means Clustering Implementation
# Phase 2: Full implementation

#' Run k-means clustering on preprocessed data
#'
#' @param data SummarizedExperiment object with preprocessed data
#' @param k Number of clusters
#' @param filter_genes Whether to filter to top variable genes
#' @param n_genes Number of genes to keep (if filtering)
#' @return List with kmeans result and filtered data
run_kmeans_analysis <- function(data, k, filter_genes = FALSE, n_genes = 500) {

  # Extract expression matrix from SummarizedExperiment
  data_matrix <- as.matrix(assay(data))

  # Filter genes if requested
  if (filter_genes) {
    data_matrix <- filter_top_variable_genes(data_matrix, n_genes)

    # Warn about large gene counts
    if (nrow(data_matrix) > 5000) {
      if (exists("showNotification", mode = "function")) {
        showNotification(
          sprintf("Running k-means on %d genes may cause performance issues. Consider reducing gene count.", nrow(data_matrix)),
          type = "warning",
          duration = 8
        )
      } else {
        warning(sprintf("Running k-means on %d genes may cause performance issues. Consider reducing gene count.", nrow(data_matrix)))
      }
    }
  }

  # Transpose: k-means expects samples as rows
  data_t <- t(data_matrix)

  # Run k-means
  # Explicitly set seed to NULL to make stochastic nature clear
  set.seed(NULL)

  kmeans_result <- kmeans(
    x = data_t,
    centers = k,
    nstart = 25,  # Try multiple random starts for stability
    iter.max = 100
  )

  # Check for degenerate clustering
  check_degenerate_clustering(kmeans_result$cluster)

  return(list(
    result = kmeans_result,
    data_matrix = data_matrix,
    cluster_assignments = kmeans_result$cluster
  ))
}
