# ML Classification Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive list with an entry per method that ran on the current
# Dataset version (list() before a run and after a new version):
#   kmeans: assignments (cluster by sample), k, params, version
#   svm: predictions and truth (by sample), accuracy, confusion, params, version
# `params` are the inputs of the run; the R-code download reads them as
# parameters$MLClassification.

ml_classification_Server <- function(id, dataset){
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

    ns <- session$ns
    file_path <- paste0("/www/", session$token, "/")

    ## UI Section ----
    # Condition dropdowns: the sample annotation of the processed data. A new
    # run gives new processed data, which resets the user's choice.
    output$condition_overlay_ui <- renderUI({
      data <- req(dataset$processed())
      selectInput(
        inputId = ns("condition_overlay"),
        label = "Overlay condition (optional):",
        choices = c("None" = "none", colnames(colData(data))),
        selected = "none"
      )
    })

    output$condition_column_supervised_ui <- renderUI({
      data <- req(dataset$processed())
      all_cols <- colnames(colData(data))
      selectInput(
        inputId = ns("condition_column_supervised"),
        label = "Condition column (target):",
        choices = all_cols,
        selected = all_cols[1]
      )
    })

    # Results belong to one Dataset version: a new Dataset, run or clear
    # discards them
    observeEvent(dataset$version(), {
      for (name in names(reactiveValuesToList(ml_results))) ml_results[[name]] <- NULL
    })

    ## k-means Section ----

    # Run k-means clustering
    observeEvent(input$run_clustering, {
      data <- dataset$processed()

      # Validation
      tryCatch({
        # Validate data exists and is correct type
        if (is.null(data)) {
          stop("No preprocessed data available. Please run preprocessing first.")
        }
        if (!inherits(data, "SummarizedExperiment")) {
          stop("Data must be a SummarizedExperiment object.")
        }

        # Extract matrix for validation
        data_matrix <- as.matrix(assay(data))
        validate_preprocessed_data(data_matrix)
        validate_no_missing_values(data_matrix)
        validate_minimum_samples(data_matrix, min_samples = 10)

        # Run k-means
        kmeans_analysis <- run_kmeans_analysis(
          data = data,
          k = input$k_clusters,
          filter_genes = input$filter_genes_unsupervised,
          n_genes = input$n_genes_unsupervised
        )

        # Store results
        ml_results$kmeans_result <- kmeans_analysis  # Store entire result
        ml_results$kmeans <- kmeans_analysis$result  # Keep for backward compatibility
        ml_results$cluster_assignments <- kmeans_analysis$cluster_assignments
        ml_results$data_matrix <- kmeans_analysis$data_matrix

        # Returned result; its params feed the R-code download
        ml_results$kmeans_out <- list(
          assignments = kmeans_analysis$cluster_assignments,
          k = input$k_clusters,
          params = list(
            k_clusters = input$k_clusters,
            filter_genes_unsupervised = input$filter_genes_unsupervised,
            n_genes_unsupervised = input$n_genes_unsupervised,
            condition_overlay = input$condition_overlay
          ),
          version = dataset$version()
        )

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
    output$cluster_plot <- renderPlotly({
      req(ml_results$kmeans_result)

      data <- dataset$processed()

      # Get condition column for overlay (if selected)
      condition_col <- NULL
      if (!is.null(input$condition_overlay) && input$condition_overlay != "none") {
        condition_col <- input$condition_overlay
      }

      # Generate and store plot
      plot_obj <- render_kmeans_plot(
        kmeans_result = ml_results$kmeans_result,
        data = data,
        condition_column = condition_col
      )

      # Store for download/report handlers
      ml_results$cluster_plot <- plot_obj

      # Convert to interactive plotly with clipboard functionality
      create_clipboard_plotly(
        gg_plot = plot_obj,
        plot_id = "cluster_plot",
        tooltip = "all"
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
      data <- dataset$processed()
      if (!is.null(data) && !is.null(input$condition_overlay) && input$condition_overlay != "none") {
        sample_annotation <- as.data.frame(colData(data))
        cluster_df$Condition <- sample_annotation[[input$condition_overlay]]
      }

      results_datatable(cluster_df, download_name("ML", NULL, "KMeansResults"))
    }, server = FALSE)

    ## SVM Section ----

    # Run SVM classification
    observeEvent(input$run_classification, {
      data <- dataset$processed()

      tryCatch({
        # Validate data exists and is correct type
        if (is.null(data)) {
          stop("No preprocessed data available. Please run preprocessing first.")
        }
        if (!inherits(data, "SummarizedExperiment")) {
          stop("Data must be a SummarizedExperiment object.")
        }

        # Extract matrix and sample annotation for validation
        data_matrix <- as.matrix(assay(data))
        sample_annotation <- as.data.frame(colData(data))

        # Validation
        validate_preprocessed_data(data_matrix)
        validate_no_missing_values(data_matrix)
        validate_minimum_samples(data_matrix, min_samples = 5)

        if (!validate_condition_column_supervised(input$condition_column_supervised, sample_annotation)) {
          return()
        }

        # Get condition vector for NA check
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
          data = data,
          condition_column = input$condition_column_supervised,
          filter_genes = input$filter_genes_supervised,
          n_genes = input$n_genes_supervised,
          kernel = "radial"
        )

        # Store results
        ml_results$svm_result <- svm_analysis  # Store entire result
        ml_results$svm <- svm_analysis$model  # Keep for backward compatibility
        ml_results$predictions <- svm_analysis$predictions
        ml_results$accuracy <- svm_analysis$accuracy
        ml_results$data_matrix <- svm_analysis$data_matrix
        ml_results$svm_X <- svm_analysis$X
        ml_results$svm_y <- svm_analysis$y
        ml_results$confusion <- svm_analysis$confusion

        # Returned result; its params feed the R-code download
        ml_results$svm_out <- list(
          predictions = svm_analysis$predictions,
          truth = svm_analysis$y,
          accuracy = svm_analysis$accuracy,
          confusion = svm_analysis$confusion,
          params = list(
            condition_column_supervised = input$condition_column_supervised,
            filter_genes_supervised = input$filter_genes_supervised,
            n_genes_supervised = input$n_genes_supervised,
            svm_kernel = "radial"
          ),
          version = dataset$version()
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
    output$svm_plot <- renderPlotly({
      req(ml_results$svm_result)

      # Generate and store plot
      plot_obj <- render_svm_plot(
        svm_result = ml_results$svm_result
      )

      # Store for download/report handlers
      ml_results$svm_plot <- plot_obj

      # Convert to interactive plotly with clipboard functionality
      # Using "text" tooltip since we created custom tooltip text in the plot
      create_clipboard_plotly(
        gg_plot = plot_obj,
        plot_id = "svm_plot",
        tooltip = "text"
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

      results_datatable(predictions_df, download_name("ML", NULL, "SVMPredictions")) %>%
        DT::formatStyle(
          'Correct',
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da'))
        )
    }, server = FALSE)

    ## Download and Reporting Handlers ----

    # K-means clustering handlers
    observeEvent(input$only2Report_cluster, {
      req(ml_results$cluster_plot)
      notificationID <- showNotification("Saving to Report...", duration = 0)

      tmp_filename <- paste0(
        getwd(), file_path,
        "KMeans_Clustering_", Sys.time(), input$file_ext_cluster
      )

      ggsave(
        tmp_filename,
        plot = ml_results$cluster_plot,
        width = 16,
        height = 9,
        units = "in",
        device = gsub("\\.","", input$file_ext_cluster)
      )

      # Log to report with proper section structure
      fun_LogIt(session, message = "## ML Classification - k-means Clustering {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0(
        "**k-means Clustering** - Number of clusters: k=", input$k_clusters
      ))
      if (input$filter_genes_unsupervised) {
        fun_LogIt(session, message = paste0(
          "**k-means** - Filtered to top ", input$n_genes_unsupervised, " variable genes"
        ))
      }
      if (!is.null(input$condition_overlay) && input$condition_overlay != "none") {
        fun_LogIt(session, message = paste0(
          "**k-means** - Overlay condition: ", input$condition_overlay
        ))
      }
      fun_LogIt(session, message = paste0("![k-means Clustering](", tmp_filename, ")"))

      removeNotification(notificationID)
      showNotification("Saved!", type = "message", duration = 1)
    })

    output$SavePlot_cluster <- downloadHandler(
      filename = function() {
        download_name("ML", input$file_ext_cluster, "KMeans")
      },
      content = function(file) {
        req(ml_results$cluster_plot)
        ggsave(
          file,
          plot = ml_results$cluster_plot,
          device = gsub("\\.","", input$file_ext_cluster)
        )
        on.exit({shinyjs::click(ns("only2Report_cluster"))})
      }
    )

    output$getR_Code_cluster <- rcode_download("ML", "KMeans", function(file) {
      rcode_zip(
        file, dataset$code_context(), KMEANS_CLUSTERING_PIPELINE,
        "MLClassification", ml_results$kmeans_out$params
      )
    })

    # SVM classification handlers
    observeEvent(input$only2Report_svm, {
      req(ml_results$svm_plot)
      notificationID <- showNotification("Saving to Report...", duration = 0)

      tmp_filename <- paste0(
        getwd(), file_path,
        "SVM_Classification_", Sys.time(), input$file_ext_svm
      )

      ggsave(
        tmp_filename,
        plot = ml_results$svm_plot,
        width = 16,
        height = 9,
        units = "in",
        device = gsub("\\.","", input$file_ext_svm)
      )

      # Log to report with proper section structure
      fun_LogIt(session, message = "## ML Classification - SVM {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0(
        "**SVM Classification** - Predicting: ", input$condition_column_supervised
      ))
      fun_LogIt(session, message = paste0(
        "**SVM** - Training accuracy: ", round(ml_results$accuracy * 100, 1), "%"
      ))
      fun_LogIt(session, message = paste0(
        "**SVM** - Note: This is training accuracy only (no validation). ",
        "Results are exploratory."
      ))
      if (input$filter_genes_supervised) {
        fun_LogIt(session, message = paste0(
          "**SVM** - Filtered to top ", input$n_genes_supervised, " variable genes"
        ))
      }
      fun_LogIt(session, message = paste0("![SVM Classification](", tmp_filename, ")"))

      removeNotification(notificationID)
      showNotification("Saved!", type = "message", duration = 1)
    })

    output$SavePlot_svm <- downloadHandler(
      filename = function() {
        download_name("ML", input$file_ext_svm, "SVM")
      },
      content = function(file) {
        req(ml_results$svm_plot)
        ggsave(
          file,
          plot = ml_results$svm_plot,
          device = gsub("\\.","", input$file_ext_svm)
        )
        on.exit({shinyjs::click(ns("only2Report_svm"))})
      }
    )

    output$getR_Code_svm <- rcode_download("ML", "SVM", function(file) {
      rcode_zip(
        file, dataset$code_context(), SVM_CLASSIFICATION_PIPELINE,
        "MLClassification", ml_results$svm_out$params
      )
    })

    reactive({
      res <- list()
      res$kmeans <- ml_results$kmeans_out
      res$svm <- ml_results$svm_out
      res
    })
  })
}
