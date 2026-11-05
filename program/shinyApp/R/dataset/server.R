### Dataset module server
# Dataset 1 of the single-Dataset app: one long-lived
# handle whose accessors change only when its Dataset is replaced or cleared.
# A confirmed Selection, or a run on a Selection that was not confirmed,
# makes a new Dataset 1; a new Upload, Clear or gene annotation clears it.
# Arguments: the returns of upload_server(), selection_server() and
#   preprocessing_server()
# Runs the analysis modules that are already on the handle (so far ML,
# Sample Correlation, Single Gene, PCA, Heatmap, Enrichment and Differential Analysis) in
# the Dataset's namespace; what they return is in results(), by module.
# Returns the Dataset handle (dataset_handle())
dataset_server <- function(id, upload, selection, preprocessing){
  moduleServer(id, function(input, output, session){
    ds <- reactiveValues(
      selection = NULL,
      omic_type = NULL,
      organism = NULL,
      run = NULL,
      version = 0,
      results = list()
    )

    # every change of the Dataset goes through here: the modules' results are
    # of the old state, and the new version tells the modules to clear
    set_dataset <- function(...){
      fields <- list(...)
      for (name in names(fields)) ds[[name]] <- fields[[name]]
      ds$results <- list()
      ds$version <- ds$version + 1
    }
    new_dataset <- function(selected, omic_type, run = NULL){
      set_dataset(
        selection = selected, omic_type = omic_type,
        organism = upload$organism(), run = run
      )
    }
    clear <- function(){
      set_dataset(selection = NULL, omic_type = NULL, organism = NULL, run = NULL)
    }

    # before the Selection observers, so a new Upload and its confirm in the
    # same flush end with the new Dataset
    observeEvent(upload$done(), clear(), priority = 1)
    observeEvent(upload$cleared(), clear(), priority = 1)
    observeEvent(upload$annotated(), clear(), priority = 1)

    observeEvent(selection$confirmed(), {
      new_dataset(selection$selected(), upload$omic_type())
    })
    observeEvent(preprocessing$result(), {
      run <- preprocessing$result()
      if (identical(run$selection$params, ds$selection$params)) {
        set_dataset(run = run)
      } else {
        new_dataset(run$selection, run$omic_type, run)
      }
    })
    observeEvent(preprocessing$failed(), set_dataset(run = NULL))

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
    heatmap <- heatmap_server("heatmap", handle)
    collect("heatmap", heatmap)
    collect("differential", significance_analysis_server("differential", handle))
    # the genes the Heatmap saved feed Enrichment's over-representation analysis
    collect("enrichment", enrichment_analysis_Server(
      "enrichment_analysis", handle,
      heatmap_genes = reactive(heatmap()$gene_list)
    ))

    handle
  })
}
