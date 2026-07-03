# ML Classification Utility Functions
# Phase 2: k-means validation and code generation
# Phase 3: SVM validation and code generation

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
    showNotification(
      sprintf("Warning: Only %d samples available. Results may be unreliable with few samples.", n_samples),
      type = "warning",
      duration = 8
    )
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
    showNotification(
      sprintf(
        "Warning: %.0f%% of samples assigned to a single cluster. Consider reducing k or checking data quality.",
        max_proportion * 100
      ),
      type = "warning",
      duration = 10
    )
  }

  return(TRUE)
}

#' Generate R code for k-means clustering
#'
#' @param n_genes Number of genes used (NULL if all)
#' @param k Number of clusters
#' @param filtered Whether gene filtering was applied
#' @return Character string with R code
generate_kmeans_code <- function(n_genes = NULL, k, filtered = FALSE) {

  code <- sprintf("# Machine learning classification using k-means
# Purpose: Discover natural sample groupings in expression data
#
# Note: k-means is a stochastic algorithm. Results may vary between runs.
# To ensure reproducibility, set a random seed before running:
# set.seed(123)  # Use any integer

# Load required libraries
library(stats)  # For kmeans
library(ggplot2)  # For visualization

# Assume 'data_matrix' is your preprocessed expression matrix
# Rows = genes/features, Columns = samples

")

  if (filtered) {
    code <- paste0(code, sprintf("# Filter to top %d most variable genes
gene_vars <- apply(data_matrix, 1, var, na.rm = TRUE)
top_genes <- order(gene_vars, decreasing = TRUE)[1:%d]
data_filtered <- data_matrix[top_genes, , drop = FALSE]

", n_genes, n_genes))
    data_var <- "data_filtered"
  } else {
    data_var <- "data_matrix"
  }

  code <- paste0(code, sprintf("# Transpose data: kmeans expects samples as rows
data_t <- t(%s)

# Run k-means clustering
# nstart = 25 means try 25 different random starting configurations
# and pick the best one (more stable results)
kmeans_result <- kmeans(
  x = data_t,
  centers = %d,
  nstart = 25,
  iter.max = 100
)

# Extract cluster assignments
cluster_assignments <- kmeans_result$cluster

# View cluster sizes
table(cluster_assignments)

# Basic visualization using PCA projection
pca_result <- prcomp(data_t, scale. = FALSE)
pca_coords <- as.data.frame(pca_result$x[, 1:2])
pca_coords$cluster <- as.factor(cluster_assignments)

ggplot(pca_coords, aes(x = PC1, y = PC2, color = cluster)) +
  geom_point(size = 3) +
  labs(
    title = 'k-means Clustering (k = %d)',
    subtitle = 'PCA projection for visualization',
    color = 'Cluster'
  ) +
  theme_minimal()

# To add condition labels as shapes (if available):
# pca_coords$condition <- sample_annotation$your_condition_column
# Then add: shape = condition to the aes() above
", data_var, k, k))

  return(code)
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

#' Generate R code for SVM classification
#'
#' @param condition_col Name of condition column
#' @param n_genes Number of genes (NULL if all)
#' @param filtered Whether filtering was applied
#' @param kernel SVM kernel type
#' @return Character string with R code
generate_svm_code <- function(condition_col, n_genes = NULL, filtered = FALSE, kernel = "radial") {

  code <- sprintf("# Machine learning classification using SVM
# Purpose: AI-based pattern detection in expression data
#
# Note: This example trains on the full dataset without validation.
# For real analysis, implement train/test splitting or cross-validation.

# Load required libraries
library(e1071)  # For SVM
library(ggplot2)  # For visualization

# Assume 'data_matrix' is your preprocessed expression matrix (genes x samples)
# Assume 'sample_annotation' is your sample metadata data frame

")

  if (filtered) {
    code <- paste0(code, sprintf("# Filter to top %d most variable genes
gene_vars <- apply(data_matrix, 1, var, na.rm = TRUE)
top_genes <- order(gene_vars, decreasing = TRUE)[1:%d]
data_filtered <- data_matrix[top_genes, , drop = FALSE]

", n_genes, n_genes))
    data_var <- "data_filtered"
  } else {
    data_var <- "data_matrix"
  }

  code <- paste0(code, sprintf("# Prepare data
X <- t(%s)  # Transpose: samples as rows, genes as columns
y <- as.factor(sample_annotation$%s)  # Target variable

# Train SVM
# Note: Training on full data (no train/test split in this example)
svm_model <- svm(
  x = X,
  y = y,
  kernel = '%s',
  scale = TRUE  # Auto-scale features
)

# Get predictions on training data
predictions <- predict(svm_model, X)

# Calculate training accuracy
accuracy <- sum(predictions == y) / length(y)
cat(sprintf('Training accuracy: %%.1f%%%%\\n', accuracy * 100))

# Confusion matrix
confusion <- table(Predicted = predictions, Actual = y)
print(confusion)

# WARNING: This is training accuracy - likely overly optimistic!
# For real analysis:
# 1. Split data into train/test sets, OR
# 2. Use cross-validation (e.g., caret package)

# Visualization: Decision boundary on PCA projection
pca_result <- prcomp(X, scale. = FALSE)
pca_coords <- as.data.frame(pca_result$x[, 1:2])
pca_coords$actual <- y
pca_coords$predicted <- predictions

# Train 2D SVM for visualization
svm_2d <- svm(
  x = pca_coords[, 1:2],
  y = y,
  kernel = '%s'
)

# Create decision boundary grid
x_range <- range(pca_coords$PC1)
y_range <- range(pca_coords$PC2)
grid <- expand.grid(
  PC1 = seq(x_range[1], x_range[2], length.out = 100),
  PC2 = seq(y_range[1], y_range[2], length.out = 100)
)
grid$prediction <- predict(svm_2d, grid)

# Plot
ggplot() +
  geom_tile(data = grid, aes(PC1, PC2, fill = prediction), alpha = 0.3) +
  geom_point(
    data = pca_coords,
    aes(PC1, PC2, color = actual, shape = predicted),
    size = 3
  ) +
  labs(
    title = 'SVM Decision Boundary (PCA Projection)',
    subtitle = sprintf('Training accuracy: %%.1f%%%%', accuracy * 100),
    color = 'True Label',
    shape = 'Predicted',
    fill = 'Decision Region'
  ) +
  theme_minimal()
", data_var, condition_col, kernel, kernel))

  return(code)
}
