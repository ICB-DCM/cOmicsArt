# SVM Classification Implementation
# Phase 3: Full implementation

#' Run SVM classification on preprocessed data
#'
#' @param data_matrix Preprocessed expression matrix (genes x samples)
#' @param condition_vector Vector of condition labels
#' @param filter_genes Whether to filter to top variable genes
#' @param n_genes Number of genes to keep (if filtering)
#' @param kernel SVM kernel type (default: "radial")
#' @return List with SVM model, predictions, accuracy, and data
run_svm_classification <- function(data_matrix,
                                   condition_vector,
                                   filter_genes = FALSE,
                                   n_genes = 500,
                                   kernel = "radial") {

  require(e1071)

  # Filter genes if requested
  if (filter_genes) {
    data_matrix <- filter_top_variable_genes(data_matrix, n_genes)

    if (nrow(data_matrix) > 5000) {
      showNotification(
        sprintf("Running SVM on %d genes may cause performance issues. Consider reducing gene count.", nrow(data_matrix)),
        type = "warning",
        duration = 8
      )
    }
  }

  # Prepare data
  X <- t(data_matrix)  # Samples as rows, genes as columns
  y <- as.factor(condition_vector)

  # Train SVM
  # Note: No train/test split in v1 - training on full data
  svm_model <- svm(
    x = X,
    y = y,
    kernel = kernel,
    scale = TRUE  # Auto-scale features
  )

  # Get predictions on training data
  predictions <- predict(svm_model, X)

  # Calculate accuracy
  accuracy <- sum(predictions == y) / length(y)

  # Confusion matrix
  confusion <- table(Predicted = predictions, Actual = y)

  return(list(
    model = svm_model,
    predictions = predictions,
    accuracy = accuracy,
    confusion = confusion,
    data_matrix = data_matrix,
    X = X,
    y = y
  ))
}
