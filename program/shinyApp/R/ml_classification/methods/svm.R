# SVM Classification Implementation
# Phase 3: Full implementation

#' Run SVM classification on preprocessed data
#'
#' @param data SummarizedExperiment object with preprocessed data
#' @param condition_column Name of column in colData with condition labels
#' @param filter_genes Whether to filter to top variable genes
#' @param n_genes Number of genes to keep (if filtering)
#' @param kernel SVM kernel type (default: "radial")
#' @return List with SVM model, predictions, accuracy, and data
run_svm_classification <- function(data,
                                   condition_column,
                                   filter_genes = FALSE,
                                   n_genes = 500,
                                   kernel = "radial") {

  # Extract expression matrix from SummarizedExperiment
  data_matrix <- as.matrix(assay(data))

  # Extract condition vector from sample annotation
  condition_vector <- as.data.frame(colData(data))[[condition_column]]

  # Filter genes if requested
  if (filter_genes) {
    data_matrix <- filter_top_variable_genes(data_matrix, n_genes)

    if (nrow(data_matrix) > 5000) {
      if (exists("showNotification", mode = "function")) {
        showNotification(
          sprintf("Running SVM on %d genes may cause performance issues. Consider reducing gene count.", nrow(data_matrix)),
          type = "warning",
          duration = 8
        )
      } else {
        warning(sprintf("Running SVM on %d genes may cause performance issues. Consider reducing gene count.", nrow(data_matrix)))
      }
    }
  }

  # Prepare data
  X <- t(data_matrix)  # Samples as rows, genes as columns
  y <- as.factor(condition_vector)

  # Train SVM
  # Note: No train/test split in v1 - training on full data
  svm_model <- e1071::svm(
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
