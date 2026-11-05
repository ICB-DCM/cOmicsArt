# ML Classification Server Module
# Reads its Dataset through the handle (`dataset`, see dataset_handle()).
# Returns a reactive list with an entry per method that ran on the current
# Dataset version (list() before a run and after a new version):
#   kmeans: assignments (cluster by sample), k, params, version
#   svm: predictions and truth (by sample), accuracy, confusion, params, version
# `params` are the inputs of the run; the R-code download reads them as
# parameters$MLClassification.
# ML always uses the processed data without batch correction, so it has no
# batch picker and its R code starts from "processed".

ml_classification_Server <- function(id, dataset){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    file_path <- paste0("/www/", session$token, "/")

    # One result per method; behind each the analysis its plot draws
    # (run_kmeans_analysis(), run_svm_classification())
    kmeans_fit <- reactiveVal(NULL)
    svm_fit <- reactiveVal(NULL)
    kmeans_result <- module_result(dataset, on_clear = function() kmeans_fit(NULL))
    svm_result <- module_result(dataset, on_clear = function() svm_fit(NULL))

    ## UI Section ----
    output$condition_overlay_ui <- annotation_picker(
      "condition_overlay", "Overlay condition (optional):", ns, dataset, "samples",
      selected = "none", extra_choices = c("None" = "none")
    )
    output$condition_column_supervised_ui <- annotation_picker(
      "condition_column_supervised", "Condition column (target):", ns, dataset, "samples"
    )

    # each method's plot, table and buttons show while there is its result
    observe(shinyjs::toggleElement(id = "cluster_div", condition = !is.null(kmeans_result$get())))
    observe(shinyjs::toggleElement(id = "svm_div", condition = !is.null(svm_result$get())))

    # `analysis(data)` on the processed data after the input check, behind
    # the loading screen; an error goes to an error modal as `label`. NULL
    # when it fails or stops.
    run_analysis <- function(label, min_samples, analysis){
      with_loading(tryCatch({
        data <- dataset$processed()
        validate_ml_data(data, min_samples)
        analysis(data)
      }, error = function(e){
        error_modal(paste0("Error running ", label, ": ", e$message))
        NULL
      }))
    }

    ## k-means Section ----

    observeEvent(input$run_clustering, {
      # a run that fails leaves no result
      kmeans_result$clear()
      fit <- run_analysis("k-means", min_samples = 10, function(data){
        run_kmeans_analysis(
          data = data,
          k = input$k_clusters,
          filter_genes = input$filter_genes_unsupervised,
          n_genes = input$n_genes_unsupervised
        )
      })
      if (is.null(fit)) return()

      kmeans_fit(fit)
      # params feed the R-code download
      kmeans_result$set(list(
        assignments = fit$cluster_assignments,
        k = input$k_clusters,
        params = list(
          k_clusters = input$k_clusters,
          filter_genes_unsupervised = input$filter_genes_unsupervised,
          n_genes_unsupervised = input$n_genes_unsupervised,
          condition_overlay = input$condition_overlay
        )
      ))
      showNotification("k-means clustering completed successfully!", type = "default")
    })

    # the overlay column of the k-means run, NULL for none
    run_overlay <- function(){
      overlay <- kmeans_result$get()$params$condition_overlay
      if (!is.null(overlay) && overlay != "none") overlay else NULL
    }

    # The plot of the k-means run, with the run's overlay; the render, the
    # download and the report use it. The processed data are the run's: a
    # new Dataset version clears the run.
    cluster_plot <- reactive({
      render_kmeans_plot(
        kmeans_result = req(kmeans_fit()),
        data = dataset$processed(),
        condition_column = run_overlay()
      )
    })

    output$cluster_plot <- renderPlotly({
      create_clipboard_plotly(
        gg_plot = cluster_plot(),
        plot_id = "cluster_plot",
        tooltip = "all"
      )
    })

    output$cluster_table <- DT::renderDataTable({
      assignments <- req(kmeans_result$get())$assignments
      cluster_df <- data.frame(Sample = names(assignments), Cluster = assignments)
      # the run's overlay condition, if any
      if (!is.null(run_overlay())) {
        cluster_df$Condition <- as.data.frame(colData(dataset$processed()))[[run_overlay()]]
      }
      results_datatable(cluster_df, download_name("ML", NULL, "KMeansResults"))
    }, server = FALSE)

    ## SVM Section ----

    observeEvent(input$run_classification, {
      # a run that fails leaves no result
      svm_result$clear()
      condition_column <- input$condition_column_supervised
      fit <- run_analysis("SVM", min_samples = 5, function(data){
        sample_annotation <- as.data.frame(colData(data))
        if (!validate_condition_column_supervised(condition_column, sample_annotation)) {
          return(NULL)
        }
        if (any(is.na(sample_annotation[[condition_column]]))) {
          showNotification(
            "Condition column contains missing values (NA). Please clean data first.",
            type = "error"
          )
          return(NULL)
        }
        run_svm_classification(
          data = data,
          condition_column = condition_column,
          filter_genes = input$filter_genes_supervised,
          n_genes = input$n_genes_supervised,
          kernel = "radial"
        )
      })
      if (is.null(fit)) return()

      svm_fit(fit)
      # params feed the R-code download
      svm_result$set(list(
        predictions = fit$predictions,
        truth = fit$y,
        accuracy = fit$accuracy,
        confusion = fit$confusion,
        params = list(
          condition_column_supervised = condition_column,
          filter_genes_supervised = input$filter_genes_supervised,
          n_genes_supervised = input$n_genes_supervised,
          svm_kernel = "radial"
        )
      ))
      showNotification("SVM classification completed successfully!", type = "message")
    })

    output$svm_accuracy_text <- renderText({
      sprintf("Training accuracy: %.1f%%", req(svm_result$get())$accuracy * 100)
    })

    # The plot of the SVM run; the render, the download and the report use it
    svm_plot <- reactive(render_svm_plot(svm_result = req(svm_fit())))

    output$svm_plot <- renderPlotly({
      # "text" tooltip: the plot has its own tooltip text
      create_clipboard_plotly(
        gg_plot = svm_plot(),
        plot_id = "svm_plot",
        tooltip = "text"
      )
    })

    output$svm_predictions_table <- DT::renderDataTable({
      svm <- req(svm_result$get())
      predictions_df <- data.frame(
        Sample = names(svm$predictions),
        True_Label = as.character(svm$truth),
        Predicted_Label = as.character(svm$predictions),
        Correct = svm$predictions == svm$truth
      )
      results_datatable(predictions_df, download_name("ML", NULL, "SVMPredictions")) %>%
        DT::formatStyle(
          'Correct',
          backgroundColor = DT::styleEqual(c(TRUE, FALSE), c('#d4edda', '#f8d7da'))
        )
    }, server = FALSE)

    ## Download and Reporting Handlers ----

    # K-means clustering handlers
    # the report describes the run; its image is a png, which the HTML
    # report shows, whatever the download format
    observeEvent(input$only2Report_cluster, {
      params <- req(kmeans_result$get())$params
      notificationID <- showNotification("Saving to Report...", duration = 0)

      tmp_filename <- paste0(
        getwd(), file_path,
        "KMeans_Clustering_", Sys.time(), ".png"
      )

      ggsave(
        tmp_filename,
        plot = cluster_plot(),
        width = 16,
        height = 9,
        units = "in",
        device = "png"
      )

      # Log to report with proper section structure
      fun_LogIt(session, message = "## ML Classification - k-means Clustering {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0(
        "**k-means Clustering** - Number of clusters: k=", params$k_clusters
      ))
      if (params$filter_genes_unsupervised) {
        fun_LogIt(session, message = paste0(
          "**k-means** - Filtered to top ", params$n_genes_unsupervised, " variable genes"
        ))
      }
      if (!is.null(run_overlay())) {
        fun_LogIt(session, message = paste0(
          "**k-means** - Overlay condition: ", run_overlay()
        ))
      }
      fun_LogIt(session, message = paste0("![k-means Clustering](", tmp_filename, ")"))

      removeNotification(notificationID)
      showNotification("Saved!", type = "message", duration = 1)
    })

    output$SavePlot_cluster <- plot_download(
      "ML", "KMeans",
      ext = reactive(input$file_ext_cluster),
      save = function(file, device) ggsave(file, plot = cluster_plot(), device = device),
      report_button = ns("only2Report_cluster")
    )

    output$getR_Code_cluster <- dataset$rcode_download(
      "ML", "KMeans", KMEANS_CLUSTERING_PIPELINE, "MLClassification",
      params = function() kmeans_result$get()$params
    )

    # SVM classification handlers
    # as for k-means: the run's parameters, a png image
    observeEvent(input$only2Report_svm, {
      svm <- req(svm_result$get())
      params <- svm$params
      notificationID <- showNotification("Saving to Report...", duration = 0)

      tmp_filename <- paste0(
        getwd(), file_path,
        "SVM_Classification_", Sys.time(), ".png"
      )

      ggsave(
        tmp_filename,
        plot = svm_plot(),
        width = 16,
        height = 9,
        units = "in",
        device = "png"
      )

      # Log to report with proper section structure
      fun_LogIt(session, message = "## ML Classification - SVM {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0(
        "**SVM Classification** - Predicting: ", params$condition_column_supervised
      ))
      fun_LogIt(session, message = paste0(
        "**SVM** - Training accuracy: ", round(svm$accuracy * 100, 1), "%"
      ))
      fun_LogIt(session, message = paste0(
        "**SVM** - Note: This is training accuracy only (no validation). ",
        "Results are exploratory."
      ))
      if (params$filter_genes_supervised) {
        fun_LogIt(session, message = paste0(
          "**SVM** - Filtered to top ", params$n_genes_supervised, " variable genes"
        ))
      }
      fun_LogIt(session, message = paste0("![SVM Classification](", tmp_filename, ")"))

      removeNotification(notificationID)
      showNotification("Saved!", type = "message", duration = 1)
    })

    output$SavePlot_svm <- plot_download(
      "ML", "SVM",
      ext = reactive(input$file_ext_svm),
      save = function(file, device) ggsave(file, plot = svm_plot(), device = device),
      report_button = ns("only2Report_svm")
    )

    output$getR_Code_svm <- dataset$rcode_download(
      "ML", "SVM", SVM_CLASSIFICATION_PIPELINE, "MLClassification",
      params = function() svm_result$get()$params
    )

    # only the methods that ran
    reactive({
      methods <- list()
      methods$kmeans <- kmeans_result$get()
      methods$svm <- svm_result$get()
      methods
    })
  })
}
