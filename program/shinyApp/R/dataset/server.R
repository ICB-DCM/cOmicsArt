### Dataset module server
# Dataset 1 of the single-Dataset app: one long-lived handle whose accessors
# show the Dataset's Pre-processing run. A successful run replaces it; a
# confirmed Selection, failed run, new Upload, Clear or gene annotation
# removes it.
# Arguments: the returns of upload_server(), selection_server() and
#   preprocessing_server()
# Runs the analysis modules that are already on the handle (so far ML,
# Sample Correlation, Single Gene, PCA, Heatmap, Enrichment and Differential Analysis) in
# the Dataset's namespace; what they return is in results(), by module.
# Returns the Dataset handle (dataset_handle())
dataset_server <- function(id, upload, selection, preprocessing){
  moduleServer(id, function(input, output, session){
    ds <- reactiveValues(run = NULL, version = 0, results = list())

    # every change of the Dataset goes through here: the modules' results are
    # of the old run, and the new version tells the modules to clear
    set_run <- function(run){
      ds$run <- run
      ds$results <- list()
      ds$version <- ds$version + 1
    }

    observeEvent(upload$done(), set_run(NULL))
    observeEvent(upload$cleared(), set_run(NULL))
    observeEvent(upload$annotated(), set_run(NULL))
    observeEvent(selection$confirmed(), set_run(NULL))
    observeEvent(preprocessing$failed(), set_run(NULL))
    observeEvent(preprocessing$result(), set_run(preprocessing$result()))

    handle <- dataset_handle(id, function(name) ds[[name]])

    # Analysis modules; each returns a reactive with its results on the
    # current version (NULL or empty when there are none), kept in
    # results()[[name]]
    collect <- function(name, returned){
      observeEvent(returned(), {
        results <- ds$results
        results[[name]] <- if (length(returned())) returned()
        ds$results <- results
      }, ignoreNULL = FALSE)
    }
    collect("ml", ml_classification_Server("ml_classification", handle))
    collect("sample_correlation", sample_correlation_server("sample_correlation", handle))
    collect("single_gene", single_gene_visualisation_server("single_gene_visualisation", handle))
    collect("pca", pca_Server("pca", handle))
    collect("heatmap", heatmap_server("heatmap", handle))
    collect("differential", significance_analysis_server("differential", handle))
    # reads the genes the Heatmap saved from results()$heatmap
    collect("enrichment", enrichment_analysis_Server("enrichment_analysis", handle))

    handle
  })
}
