# ML Classification Server Module
# Phase 2-3: Full implementation

ml_classification_Server <- function(id, session_data, session_params, data_input_shiny){
  moduleServer(id, function(input, output, session){

    # Reactive values for ML results
    ml_results <- reactiveValues(
      kmeans = NULL,
      svm = NULL,
      cluster_assignments = NULL,
      predictions = NULL,
      accuracy = NULL,
      data_matrix = NULL,
      svm_X = NULL,
      svm_y = NULL,
      confusion = NULL
    )

    # Reactive value for generated code
    ml_code <- reactiveValues(
      current = ""
    )

    ns <- session$ns
    file_path <- paste0("/www/", session$token, "/")

    ## UI Section ----
    observeEvent(input$refreshUI, {
      print("Refreshing UI ML Classification")
      data <- reactiveValuesToList(session_data)

      # Populate condition overlay dropdown for unsupervised
      output$condition_overlay_ui <- renderUI({
        req(data_input_shiny())
        req(data$data)

        condition_choices <- c("None" = "none", colnames(colData(data$data)))

        selectInput(
          inputId = ns("condition_overlay"),
          label = "Overlay condition (optional):",
          choices = condition_choices,
          selected = "none"
        )
      })

      # Populate condition column dropdown for supervised
      output$condition_column_supervised_ui <- renderUI({
        req(data_input_shiny())
        req(data$data)

        # Get all columns from sample annotation
        all_cols <- colnames(colData(data$data))

        selectInput(
          inputId = ns("condition_column_supervised"),
          label = "Condition column (target):",
          choices = all_cols,
          selected = all_cols[1]
        )
      })

      # Clear previous results
      ml_results$kmeans <- NULL
      ml_results$svm <- NULL
      ml_results$cluster_assignments <- NULL
      ml_results$predictions <- NULL
      ml_results$accuracy <- NULL
      ml_results$data_matrix <- NULL
      ml_code$current <- ""
    })

    ## k-means Section ----

    # Run k-means clustering
    observeEvent(input$run_clustering, {
      # Get preprocessed data
      data <- reactiveValuesToList(session_data)
      data_matrix <- data$data

      # Validation
      tryCatch({
        # Convert SummarizedExperiment to matrix if needed
        if (inherits(data_matrix, "SummarizedExperiment")) {
          data_matrix <- assay(data_matrix)
        }

        validate_preprocessed_data(data_matrix)
        validate_no_missing_values(data_matrix)
        validate_minimum_samples(data_matrix, min_samples = 10)

        # Run k-means
        kmeans_analysis <- run_kmeans_analysis(
          data_matrix = data_matrix,
          k = input$k_clusters,
          filter_genes = input$filter_genes_unsupervised,
          n_genes = input$n_genes_unsupervised
        )

        # Store results
        ml_results$kmeans <- kmeans_analysis$result
        ml_results$cluster_assignments <- kmeans_analysis$cluster_assignments
        ml_results$data_matrix <- kmeans_analysis$data_matrix

        # Generate code
        generated_code <- generate_kmeans_code(
          n_genes = if(input$filter_genes_unsupervised) input$n_genes_unsupervised else NULL,
          k = input$k_clusters,
          filtered = input$filter_genes_unsupervised
        )

        # Verify code is a character string before storing
        if (!is.character(generated_code)) {
          stop("Generated code is not a character string. Type: ", class(generated_code))
        }

        ml_code$current <- generated_code

        showNotification("k-means clustering completed successfully!", type = "default")

      }, error = function(e) {
        # Print full error to console for debugging
        print("=== K-MEANS ERROR ===")
        print(e)
        print("Message:")
        print(e$message)
        print("Call:")
        print(e$call)
        print("Traceback:")
        print(traceback())

        showNotification(
          paste("Error running k-means:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })

    # Render clustering plot
    output$cluster_plot <- renderPlot({
      req(ml_results$kmeans)
      req(ml_results$data_matrix)

      data <- reactiveValuesToList(session_data)

      # Get condition column for overlay (if selected)
      condition_col <- NULL
      if (!is.null(input$condition_overlay) && input$condition_overlay != "none") {
        condition_col <- input$condition_overlay
      }

      # Get sample annotation
      sample_annotation <- NULL
      if (!is.null(data$data)) {
        sample_annotation <- as.data.frame(colData(data$data))
      }

      render_kmeans_plot(
        kmeans_result = ml_results$kmeans,
        data_matrix = ml_results$data_matrix,
        sample_annotation = sample_annotation,
        condition_column = condition_col
      )
    })

    # Render cluster assignments table
    output$cluster_table <- DT::renderDataTable({
      req(ml_results$cluster_assignments)

      # Create table
      cluster_df <- data.frame(
        Sample = names(ml_results$cluster_assignments),
        Cluster = ml_results$cluster_assignments
      )

      # Add condition if available
      data <- reactiveValuesToList(session_data)
      if (!is.null(data$data) && !is.null(input$condition_overlay) && input$condition_overlay != "none") {
        sample_annotation <- as.data.frame(colData(data$data))
        cluster_df$Condition <- sample_annotation[[input$condition_overlay]]
      }

      DT::datatable(
        cluster_df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })

    # Download handlers for k-means
    output$download_cluster_plot <- downloadHandler(
      filename = function() {
        paste0("kmeans_clustering_k", input$k_clusters, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        data <- reactiveValuesToList(session_data)
        condition_col <- if(!is.null(input$condition_overlay) && input$condition_overlay != "none") input$condition_overlay else NULL
        sample_annotation <- if(!is.null(data$data)) as.data.frame(colData(data$data)) else NULL

        ggsave(
          file,
          plot = render_kmeans_plot(
            ml_results$kmeans,
            ml_results$data_matrix,
            sample_annotation,
            condition_col
          ),
          width = 10,
          height = 8,
          dpi = 300
        )
      }
    )

    output$download_cluster_code <- downloadHandler(
      filename = function() {
        paste0("kmeans_clustering_code_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(ml_code$current, file)
      }
    )

    output$download_cluster_results <- downloadHandler(
      filename = function() {
        paste0("kmeans_results_k", input$k_clusters, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        cluster_df <- data.frame(
          Sample = names(ml_results$cluster_assignments),
          Cluster = ml_results$cluster_assignments
        )

        data <- reactiveValuesToList(session_data)
        if (!is.null(data$data) && !is.null(input$condition_overlay) && input$condition_overlay != "none") {
          sample_annotation <- as.data.frame(colData(data$data))
          cluster_df$Condition <- sample_annotation[[input$condition_overlay]]
        }

        write.csv(cluster_df, file, row.names = FALSE)
      }
    )

    ## SVM Section ----

    # Run SVM classification
    observeEvent(input$run_classification, {
      # Get data
      data <- reactiveValuesToList(session_data)
      data_matrix <- data$data

      tryCatch({
        # Convert SummarizedExperiment to matrix if needed
        if (inherits(data_matrix, "SummarizedExperiment")) {
          sample_annotation <- as.data.frame(colData(data_matrix))
          data_matrix <- assay(data_matrix)
        } else {
          sample_annotation <- as.data.frame(colData(data$data))
        }

        # Validation
        validate_preprocessed_data(data_matrix)
        validate_no_missing_values(data_matrix)
        validate_minimum_samples(data_matrix, min_samples = 5)

        if (!validate_condition_column_supervised(input$condition_column_supervised, sample_annotation)) {
          return()
        }

        # Get condition vector
        condition_vector <- sample_annotation[[input$condition_column_supervised]]

        # Check for NAs in condition
        if (any(is.na(condition_vector))) {
          showNotification(
            "Condition column contains missing values (NA). Please clean data first.",
            type = "error"
          )
          return()
        }

        # Run SVM
        svm_analysis <- run_svm_classification(
          data_matrix = data_matrix,
          condition_vector = condition_vector,
          filter_genes = input$filter_genes_supervised,
          n_genes = input$n_genes_supervised,
          kernel = "radial"
        )

        # Store results
        ml_results$svm <- svm_analysis$model
        ml_results$predictions <- svm_analysis$predictions
        ml_results$accuracy <- svm_analysis$accuracy
        ml_results$data_matrix <- svm_analysis$data_matrix
        ml_results$svm_X <- svm_analysis$X
        ml_results$svm_y <- svm_analysis$y
        ml_results$confusion <- svm_analysis$confusion

        # Generate code
        ml_code$current <- generate_svm_code(
          condition_col = input$condition_column_supervised,
          n_genes = if(input$filter_genes_supervised) input$n_genes_supervised else NULL,
          filtered = input$filter_genes_supervised,
          kernel = "radial"
        )

        showNotification("SVM classification completed successfully!", type = "message")

      }, error = function(e) {
        showNotification(
          paste("Error running SVM:", e$message),
          type = "error",
          duration = NULL
        )
      })
    })

    # Render accuracy text
    output$svm_accuracy_text <- renderText({
      req(ml_results$accuracy)

      sprintf("Training accuracy: %.1f%%", ml_results$accuracy * 100)
    })

    # Render SVM plot
    output$svm_plot <- renderPlot({
      req(ml_results$svm)
      req(ml_results$svm_X)
      req(ml_results$svm_y)
      req(ml_results$predictions)
      req(ml_results$accuracy)

      render_svm_plot(
        svm_model = ml_results$svm,
        X = ml_results$svm_X,
        y = ml_results$svm_y,
        predictions = ml_results$predictions,
        accuracy = ml_results$accuracy
      )
    })

    # Render predictions table
    output$svm_predictions_table <- DT::renderDataTable({
      req(ml_results$predictions)
      req(ml_results$svm_y)

      predictions_df <- data.frame(
        Sample = names(ml_results$predictions),
        True_Label = as.character(ml_results$svm_y),
        Predicted_Label = as.character(ml_results$predictions),
        Correct = ml_results$predictions == ml_results$svm_y
      )

      DT::datatable(
        predictions_df,
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Correct',
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da'))
        )
    })

    # Download handlers for SVM
    output$download_svm_plot <- downloadHandler(
      filename = function() {
        paste0("svm_classification_", input$condition_column_supervised, "_", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(
          file,
          plot = render_svm_plot(
            ml_results$svm,
            ml_results$svm_X,
            ml_results$svm_y,
            ml_results$predictions,
            ml_results$accuracy
          ),
          width = 12,
          height = 8,
          dpi = 300
        )
      }
    )

    output$download_svm_code <- downloadHandler(
      filename = function() {
        paste0("svm_classification_code_", Sys.Date(), ".R")
      },
      content = function(file) {
        writeLines(ml_code$current, file)
      }
    )

    output$download_svm_results <- downloadHandler(
      filename = function() {
        paste0("svm_predictions_", input$condition_column_supervised, "_", Sys.Date(), ".csv")
      },
      content = function(file) {
        predictions_df <- data.frame(
          Sample = names(ml_results$predictions),
          True_Label = as.character(ml_results$svm_y),
          Predicted_Label = as.character(ml_results$predictions),
          Correct = ml_results$predictions == ml_results$svm_y
        )

        write.csv(predictions_df, file, row.names = FALSE)
      }
    )
  })
}
