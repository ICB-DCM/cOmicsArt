### Dataset module server
# Dataset 1 of the single-Dataset app: one long-lived
# handle whose accessors change only when its Dataset is replaced or cleared.
# A confirmed Selection, or a run on a Selection that was not confirmed,
# makes a new Dataset 1; a new Upload, Clear or gene annotation clears it.
# Arguments: the returns of upload_server(), selection_server() and
#   preprocessing_server()
# Runs the analysis modules that are already on the handle (so far ML and
# Sample Correlation) in
# the Dataset's namespace; what they return is in results(), by module.
# Returns the Dataset handle (dataset_handle())
dataset_server <- function(id, upload, selection, preprocessing){
  moduleServer(id, function(input, output, session){
    ds <- reactiveValues(
      upload_raw = NULL,
      selection = NULL,
      omic_type = NULL,
      organism = NULL,
      run = NULL,
      version = 0,
      results = list()
    )

    new_dataset <- function(selected, omic_type){
      ds$upload_raw <- upload$raw()
      ds$selection <- selected
      ds$omic_type <- omic_type
      ds$organism <- upload$organism()
      ds$run <- NULL
      ds$results <- list()
      ds$version <- ds$version + 1
    }
    clear <- function(){
      for (name in c("upload_raw", "selection", "omic_type", "organism", "run")) ds[[name]] <- NULL
      ds$results <- list()
      ds$version <- ds$version + 1
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
      if (!identical(selection_params(run$selection), selection_params(ds$selection))) {
        new_dataset(run$selection, run$omic_type)
      }
      ds$run <- run
      ds$results <- list()
      ds$version <- ds$version + 1
    })
    observeEvent(preprocessing$failed(), {
      ds$run <- NULL
      ds$results <- list()
    })

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

    handle
  })
}
