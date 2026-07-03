# ML Classification Plot Functions
# Phase 2: k-means visualization
# Phase 3: SVM visualization

#' Render k-means clustering plot with PCA projection
#'
#' @param kmeans_result Result from kmeans()
#' @param data_matrix Data matrix used for clustering (genes x samples)
#' @param sample_annotation Data frame with sample metadata (optional)
#' @param condition_column Name of condition column for overlay (optional)
#' @return ggplot object
render_kmeans_plot <- function(kmeans_result,
                               data_matrix,
                               sample_annotation = NULL,
                               condition_column = NULL) {

  require(ggplot2)

  # Run PCA for 2D visualization
  data_t <- t(data_matrix)  # Samples as rows for PCA
  pca_result <- prcomp(data_t, scale. = FALSE)

  # Create data frame for plotting
  plot_data <- data.frame(
    PC1 = pca_result$x[, 1],
    PC2 = pca_result$x[, 2],
    cluster = as.factor(kmeans_result$cluster),
    sample = rownames(data_t)
  )

  # Add condition overlay if provided
  has_condition <- FALSE
  if (!is.null(sample_annotation) &&
      !is.null(condition_column) &&
      condition_column != "none") {

    plot_data$condition <- sample_annotation[[condition_column]]
    has_condition <- TRUE
  }

  # Calculate variance explained
  var_explained <- summary(pca_result)$importance[2, 1:2] * 100

  # Build plot
  p <- ggplot(plot_data, aes(x = PC1, y = PC2))

  if (has_condition) {
    # Show both cluster (color) and condition (shape)
    p <- p + geom_point(
      aes(color = cluster, shape = condition),
      size = 4,
      alpha = 0.8
    )
  } else {
    # Show cluster only
    p <- p + geom_point(
      aes(color = cluster),
      size = 4,
      alpha = 0.8
    )
  }

  p <- p +
    labs(
      title = sprintf("k-means Clustering (k = %d)", length(unique(kmeans_result$cluster))),
      subtitle = "PCA projection for visualization",
      x = sprintf("PC1 (%.1f%% variance)", var_explained[1]),
      y = sprintf("PC2 (%.1f%% variance)", var_explained[2]),
      color = "Cluster"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16)
    )

  if (has_condition) {
    p <- p + labs(shape = "Condition")
  }

  return(p)
}

#' Render SVM decision boundary plot with PCA projection
#'
#' @param svm_model SVM model object
#' @param X Data matrix (samples x genes)
#' @param y True labels
#' @param predictions Predicted labels
#' @param accuracy Classification accuracy
#' @return ggplot object
render_svm_plot <- function(svm_model, X, y, predictions, accuracy) {

  require(ggplot2)
  require(e1071)

  # Run PCA for 2D visualization
  pca_result <- prcomp(X, scale. = FALSE)
  pca_coords <- as.data.frame(pca_result$x[, 1:2])
  pca_coords$actual <- as.factor(y)
  pca_coords$predicted <- as.factor(predictions)
  pca_coords$correct <- (y == predictions)

  # Train 2D SVM on PCA coordinates (for visualization only)
  # Convert numeric kernel code to kernel name
  # e1071 stores kernel as: 0=linear, 1=polynomial, 2=radial, 3=sigmoid
  kernel_names <- c("linear", "polynomial", "radial", "sigmoid")
  kernel_type <- kernel_names[svm_model$kernel + 1]  # +1 because R is 1-indexed

  svm_2d <- svm(
    x = pca_coords[, 1:2],
    y = y,
    kernel = kernel_type
  )

  # Create decision boundary grid
  x_range <- range(pca_coords$PC1)
  y_range <- range(pca_coords$PC2)

  grid <- expand.grid(
    PC1 = seq(x_range[1], x_range[2], length.out = 100),
    PC2 = seq(y_range[1], y_range[2], length.out = 100)
  )

  grid$prediction <- predict(svm_2d, grid)

  # Calculate variance explained
  var_explained <- summary(pca_result)$importance[2, 1:2] * 100

  # Build plot
  p <- ggplot() +
    # Decision boundary background
    geom_tile(
      data = grid,
      aes(x = PC1, y = PC2, fill = prediction),
      alpha = 0.3
    ) +
    # Sample points
    geom_point(
      data = pca_coords,
      aes(x = PC1, y = PC2, color = actual, shape = predicted),
      size = 4,
      alpha = 0.8
    ) +
    # Highlight misclassifications
    geom_point(
      data = pca_coords[!pca_coords$correct, ],
      aes(x = PC1, y = PC2),
      color = "red",
      size = 6,
      shape = 1,  # Circle outline
      stroke = 2
    ) +
    labs(
      title = "SVM Decision Boundary (PCA Projection)",
      subtitle = sprintf("Training accuracy: %.1f%% | Red circles = misclassified", accuracy * 100),
      x = sprintf("PC1 (%.1f%% variance)", var_explained[1]),
      y = sprintf("PC2 (%.1f%% variance)", var_explained[2]),
      color = "True Label",
      shape = "Predicted",
      fill = "Decision Region"
    ) +
    theme_minimal(base_size = 14) +
    theme(
      legend.position = "right",
      plot.title = element_text(face = "bold", size = 16)
    )

  return(p)
}
