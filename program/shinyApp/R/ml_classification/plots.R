# ML Classification Plot Functions

#' Render k-means clustering plot with PCA projection
#'
#' @param kmeans_result Result from run_kmeans_analysis
#' @param data SummarizedExperiment object with sample metadata
#' @param condition_column Name of condition column for overlay (optional)
#' @return ggplot object
render_kmeans_plot <- function(kmeans_result,
                               data,
                               condition_column = NULL) {

  # Extract filtered data matrix from kmeans result
  data_matrix <- kmeans_result$data_matrix

  # Extract sample annotation from data
  sample_annotation <- as.data.frame(colData(data))

  # Run PCA for 2D visualization
  data_t <- t(data_matrix)  # Samples as rows for PCA
  pca_result <- prcomp(data_t, scale. = TRUE)

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

  # Build plot with tooltip-friendly aes mappings for ggplotly
  if (has_condition) {
    # Show both cluster (color) and condition (shape)
    # Add tooltip-friendly mappings: label, Sample, Cluster, Condition
    cluster_plot <- ggplot(plot_data, aes(
      x = PC1,
      y = PC2,
      color = cluster,
      shape = condition,
      label = sample,
      Sample = sample,
      Cluster = cluster,
      Condition = condition
    )) +
      geom_point(size = 3) +
      labs(
        title = sprintf("k-means Clustering (k = %d)", length(unique(kmeans_result$cluster))),
        subtitle = "PCA projection for visualization",
        x = sprintf("PC1 (%.1f%% variance)", var_explained[1]),
        y = sprintf("PC2 (%.1f%% variance)", var_explained[2]),
        color = "Cluster",
        shape = "Condition"
      ) +
      CUSTOM_THEME +
      theme(aspect.ratio = 1)
  } else {
    # Show cluster only
    # Add tooltip-friendly mappings: label, Sample, Cluster
    cluster_plot <- ggplot(plot_data, aes(
      x = PC1,
      y = PC2,
      color = cluster,
      label = sample,
      Sample = sample,
      Cluster = cluster
    )) +
      geom_point(size = 3) +
      labs(
        title = sprintf("k-means Clustering (k = %d)", length(unique(kmeans_result$cluster))),
        subtitle = "PCA projection for visualization",
        x = sprintf("PC1 (%.1f%% variance)", var_explained[1]),
        y = sprintf("PC2 (%.1f%% variance)", var_explained[2]),
        color = "Cluster"
      ) +
      CUSTOM_THEME +
      theme(aspect.ratio = 1)
  }

  return(cluster_plot)
}

#' Render SVM decision boundary plot with PCA projection
#'
#' @param svm_result Result from run_svm_classification
#' @return ggplot object
render_svm_plot <- function(svm_result) {

  require(ggplot2)
  require(e1071)

  # Extract components from svm_result
  svm_model <- svm_result$model
  X <- svm_result$X
  y <- svm_result$y
  predictions <- svm_result$predictions
  accuracy <- svm_result$accuracy

  # Run PCA for 2D visualization
  pca_result <- prcomp(X, scale. = FALSE)
  pca_coords <- as.data.frame(pca_result$x[, 1:2])
  pca_coords$sample <- rownames(X)
  pca_coords$actual <- as.factor(y)
  pca_coords$predicted <- as.factor(predictions)
  pca_coords$correct <- (y == predictions)

  # Train 2D SVM on PCA coordinates (for visualization only)
  # Convert numeric kernel code to kernel name
  # e1071 stores kernel as: 0=linear, 1=polynomial, 2=radial, 3=sigmoid
  kernel_names <- c("linear", "polynomial", "radial", "sigmoid")
  kernel_type <- kernel_names[svm_model$kernel + 1]

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

  # Add misclassification indicator to data for better visualization
  pca_coords$misclassified <- ifelse(pca_coords$correct, "Correct", "Misclassified")

  # Build plot with tooltip-friendly aes mappings for ggplotly
  svm_plot <- ggplot() +
    # Decision boundary background
    geom_tile(
      data = grid,
      aes(x = PC1, y = PC2, fill = prediction),
      alpha = 0.2,
      show.legend = TRUE
    ) +
    # Sample points with tooltip-friendly mappings
    geom_point(
      data = pca_coords,
      aes(
        x = PC1,
        y = PC2,
        color = actual,
        shape = predicted,
        text = paste0(
          "Sample: ", sample,
          "\nTrue Label: ", actual,
          "\nPredicted: ", predicted,
          "\nStatus: ", misclassified
        )
      ),
      size = 3
    ) +
    labs(
      title = "SVM Decision Boundary (PCA Projection)",
      subtitle = sprintf("Training accuracy: %.1f%% | Hover over points for details", accuracy * 100),
      x = sprintf("PC1 (%.1f%% variance)", var_explained[1]),
      y = sprintf("PC2 (%.1f%% variance)", var_explained[2]),
      color = "True Label",
      shape = "Predicted",
      fill = "Decision Region"
    ) +
    CUSTOM_THEME +
    theme(aspect.ratio = 1)

  return(svm_plot)
}
