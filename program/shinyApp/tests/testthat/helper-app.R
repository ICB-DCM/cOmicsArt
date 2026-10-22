# Smoke-test helpers for the shinytest2 app tests (test-app-*.R).
#
# IDS is the single place that knows input IDs and tab values. When IDs are
# renamed (e.g. by namespacing in a refactor), change them here only.
# The checks are deliberately coarse: "it ran with defaults and nothing broke".
# Precise behaviour belongs in unit tests.

HEATMAP <- list(
  go = "Heatmap-Do_Heatmap",
  out = "#Heatmap-HeatmapPlot",
  rcode = "#Heatmap-getR_Code_Heatmap",
  after = "#Heatmap-continue_heatmap"  # confirm "more than 100 rows" if shown
)

# Example files the app ships (paths relative to tests/testthat/)
AIRWAY <- list(
  matrix = "../../www/airway-read-counts-LS.csv",
  samples = "../../www/airway-sample-sheet-LS.csv",
  rows = "../../www/airway-entitie_description-LS.csv",
  # airway sample sheet in Metadatasheet layout (sheet "Input")
  metadata = "fixtures/airway-metadata-sheet.xlsx",
  precompiled = "../../www/Transcriptomics_only_precompiled-LS.RDS"
)

IDS <- list(
  tabs = "tabsetPanel1",
  data_tab = "Data selection",
  pre_tab = "Pre-processing",
  upload_sidebar = "upload-sidebar_data_selection",
  status = "upload-debug",
  # One entry per Upload source: sidebar `tab`, omic-type input, `files`
  # (file input ID -> fixture) and the `upload` button.
  sources = list(
    testdata = list(
      tab = "Testdata", omic = "upload-omic_type_testdata",
      upload = "upload-EasyTestForUser"
    ),
    file_input = list(
      tab = "File Input", omic = "upload-omic_type_file_input",
      files = list(
        "upload-data_matrix1" = AIRWAY$matrix,
        "upload-data_sample_anno1" = AIRWAY$samples,
        "upload-data_row_anno1" = AIRWAY$rows
      ),
      upload = "upload-refresh_file_input"
    ),
    precompiled = list(
      tab = "Precompiled", omic = "upload-omic_type_precompiled",
      files = list("upload-data_preDone" = AIRWAY$precompiled),
      upload = "upload-refresh_precompiled"
    ),
    metadata = list(
      tab = "Metadata", omic = "upload-omic_type_metadata",
      files = list(
        "upload-data_matrix_metadata" = AIRWAY$matrix,
        "upload-metadataInput" = AIRWAY$metadata,
        "upload-data_row_anno_metadata" = AIRWAY$rows
      ),
      upload = "upload-refresh_metadata"
    )
  ),
  # Visual inspection of the File Input files
  inspect = list(
    open = "upload-inspect_data",
    run = "upload-DoVisualDataInspection",
    checks = "upload-OverallChecks",
    use = "upload-usingVIdata"
  ),
  to_pre = "use_full_data",
  # Selection: `open` the controls, pick by annotation `column` and `values`
  # for samples and entities, then `go` to Pre-processing
  selection = list(
    open = "select_data",
    sample_column = "providedSampleAnnotationTypes",
    samples = "sample_selection",
    row_column = "providedRowAnnotationTypes",
    rows = "row_selection",
    go = "NextPanel"
  ),
  processing_type = "processing_type",
  procedure = "PreProcessing_Procedure",
  preprocess = "Do_preprocessing",
  pre_out = "Statisitcs_Data",
  pre_rcode = "#getR_Code_Preprocess",
  report = "DownloadTestModule-DownloadReport",
  errors = paste(".shiny-output-error:not(.shiny-output-error-validation),",
                 ".modal-title font[color='red']"),  # error_modal() title
  # One entry per analysis tab: `go` button; CSS selectors for an `out` that
  # must show content and the R-code download `rcode`; optional `set` inputs
  # and `after` selectors clicked after `go` (if present); `tab` if the name
  # is not the tab value.
  modules = list(
    "Sample Correlation" = list(
      go = "sample_correlation-Do_SampleCorrelation",
      out = "#sample_correlation-SampleCorrelationPlot",
      rcode = "#sample_correlation-getR_SampleCorrelation"
    ),
    "PCA" = list(
      go = "PCA-Do_PCA",
      out = "#PCA-PCA_plot",
      rcode = "#PCA-getR_Code_PCA"
    ),
    "ML Classification" = list(
      go = "ml_classification-run_clustering",
      out = "#ml_classification-cluster_plot",
      rcode = "#ml_classification-getR_Code_cluster"
    ),
    "Differential Analysis" = list(
      go = "SignificanceAnalysis-significanceGo",
      # one results tab per comparison, IDs derived from the group names
      after = "#Significance_div a[data-value='Volcano']",
      out = "#Significance_div [id$='_Volcano']",
      rcode = "#Significance_div [id$='_getR_Code_Volcano']"
    ),
    "Heatmap" = HEATMAP,
    # Transcriptomics: 'all' rows is too slow; the gene list feeds Enrichment
    "Heatmap (top K)" = modifyList(HEATMAP, list(
      tab = "Heatmap",
      set = list(
        "Heatmap-row_selection_options" = "Top K",
        "Heatmap-TopK" = 500  # enough genes for a significant enrichment
      ),
      after = c(HEATMAP$after, "#Heatmap-SaveGeneList_Heatmap")
    )),
    "Heatmap (top K, significant)" = modifyList(HEATMAP, list(
      tab = "Heatmap",
      set = list(
        "Heatmap-row_selection_options" = "Top K",
        "Heatmap-TopK_order" = "LogFoldChange and Significant",
        "Heatmap-TopK" = 500
      )
    )),
    "Single Gene Visualisations" = list(
      go = "single_gene_visualisation-singleGeneGo",
      out = "#single_gene_visualisation-SingleGenePlot",
      rcode = "#single_gene_visualisation-getR_Code_SingleEntities"
    ),
    "Enrichment Analysis" = list(
      set = list(
        "EnrichmentAnalysis-organism_choice_ea" = "Human genes (GRCh38.p14)",
        "EnrichmentAnalysis-ORA_or_GSE" = "OverRepresentation_Analysis",
        "EnrichmentAnalysis-GeneSet2Enrich" = "heatmap_genes",
        "EnrichmentAnalysis-GeneSetChoice" = "Hallmarks"  # msigdbr, offline
      ),
      go = "EnrichmentAnalysis-enrichmentGO",
      out = "#EnrichmentAnalysis-Hallmarks-EnrichmentPlot",
      rcode = "#EnrichmentAnalysis-Hallmarks-getR_Code"
    )
  )
)

# Long default: app start loads ~45 packages, DESeq2 and enrichment are slow.
TIMEOUT <- as.numeric(Sys.getenv("SHINYTEST2_TIMEOUT", 120000))

start_app <- function(name) {
  shinytest2::AppDriver$new(
    app_dir = "../..", name = name,
    load_timeout = as.numeric(Sys.getenv("SHINYTEST2_LOAD_TIMEOUT", 300000)),
    timeout = TIMEOUT, height = 1000, width = 1400,
    check_names = FALSE  # known duplicate output IDs, not this test's concern
  )
}

set_and_wait <- function(app, ...) {
  app$set_inputs(..., wait_ = FALSE)
  app$wait_for_idle(timeout = TIMEOUT)
}

click_and_wait <- function(app, id) {
  app$click(id, wait_ = FALSE)
  app$wait_for_idle(duration = 1000, timeout = TIMEOUT)
}

# Open one Upload source in the Data selection sidebar, choose the omic type
# and fill its file inputs (no upload yet).
fill_source <- function(app, source, omic) {
  s <- IDS$sources[[source]]
  set_and_wait(app, !!IDS$tabs := IDS$data_tab)
  app$run_js(sprintf(
    "document.querySelector(\"#%s a[data-value='%s']\").click()",
    IDS$upload_sidebar, s$tab
  ))
  set_and_wait(app, !!s$omic := omic)
  for (id in names(s$files)) app$upload_file(!!id := s$files[[id]])
}

# Click "Go to Preprocessing" after a successful Upload.
go_to_preprocessing <- function(app) {
  click_and_wait(app, IDS$to_pre)
  expect_equal(app$get_value(input = IDS$tabs), IDS$pre_tab)
}

# Upload through one source and go to Pre-processing.
load_data <- function(app, source, omic) {
  fill_source(app, source, omic)
  click_and_wait(app, IDS$sources[[source]]$upload)
  if (source != "testdata") expect_status(app, "Upload successful")
  expect_no_app_errors(app, paste("Upload", source))
  go_to_preprocessing(app)
}

# Upload the bundled test data of one omic type and go to Pre-processing.
load_test_data <- function(app, omic) load_data(app, "testdata", omic)

# The Upload status box shows `text`.
expect_status <- function(app, text) {
  status <- app$get_js(sprintf(
    "document.getElementById('%s')?.innerText ?? ''", IDS$status
  ))
  expect_match(status, text, fixed = TRUE, label = "Upload status box")
}

# Open "Select data", choose samples and entities by annotation and go to
# Pre-processing. `samples`/`rows` are list(column = ..., values = ...).
select_subset <- function(app, samples, rows) {
  s <- IDS$selection
  click_and_wait(app, s$open)
  set_and_wait(app, !!s$sample_column := samples$column)
  set_and_wait(app, !!s$samples := samples$values)
  set_and_wait(app, !!s$row_column := rows$column)
  set_and_wait(app, !!s$rows := rows$values)
  expect_no_app_errors(app, "Selection")
  click_and_wait(app, s$go)
  expect_equal(app$get_value(input = IDS$tabs), IDS$pre_tab)
}

# The Pre-processing summary reports these dimensions of the processed data.
expect_processed_dim <- function(app, n_rows, n_samples) {
  summary <- app$get_js(sprintf(
    "document.getElementById('%s')?.innerText ?? ''", IDS$pre_out
  ))
  expect_match(
    summary, sprintf("processed data has dimensions: %d, %d", n_rows, n_samples),
    fixed = TRUE, label = "Pre-processing summary"
  )
}

# `type`/`procedure` are the two pre-processing dropdowns; NULL = defaults.
preprocess <- function(app, type = NULL, procedure = NULL) {
  if (!is.null(type)) set_and_wait(app, !!IDS$processing_type := type)
  if (!is.null(procedure)) set_and_wait(app, !!IDS$procedure := procedure)
  click_and_wait(app, IDS$preprocess)
  expect_output_shown(app, paste0("#", IDS$pre_out))
  expect_no_app_errors(app, "Pre-processing")
  expect_download(app, IDS$pre_rcode)
}

# Open an analysis tab, run it with defaults and apply the generic checks.
visit_tab <- function(app, tab) {
  m <- IDS$modules[[tab]]
  set_and_wait(app, !!IDS$tabs := if (is.null(m$tab)) tab else m$tab)
  for (id in names(m$set)) set_and_wait(app, !!id := m$set[[id]])
  click_and_wait(app, m$go)
  for (sel in m$after) {
    app$run_js(sprintf("document.querySelector(\"%s\")?.click()", sel))
    app$wait_for_idle(duration = 1000, timeout = TIMEOUT)
  }
  expect_output_shown(app, m$out, tab)
  expect_no_app_errors(app, tab)
  expect_download(app, m$rcode)
}

# The output element is visible and has rendered content (plot, table, text).
expect_output_shown <- function(app, selector, label = selector) {
  js <- sprintf(
    "(function(){var e=document.querySelector(\"%s\"); if(!e) return false;
      return e.offsetParent !== null &&
        (e.querySelector('img[src],svg,canvas,table') !== null ||
         e.innerText.trim().length > 0);})()", selector)
  shown <- tryCatch({
    app$wait_for_js(js, timeout = TIMEOUT); TRUE
  }, error = function(e) FALSE)
  expect_true(shown, label = paste0(label, ": output ", selector, " shown"))
}

# Number of log lines already checked, per app (keyed by URL), so an error is
# reported once, for the step that caused it, and not again for later steps.
LOG_SEEN <- new.env()

# No red Shiny error outputs (validation messages are fine), no error_modal()
# dialog, no R or browser errors in the log since the previous check.
expect_no_app_errors <- function(app, label) {
  n <- app$get_js(sprintf("document.querySelectorAll(\"%s\").length", IDS$errors))
  expect_equal(n, 0, label = paste0(label, ": error outputs or dialogs"))
  logs <- as.data.frame(app$get_logs())$message
  seen <- if (is.null(LOG_SEEN[[app$get_url()]])) 0 else LOG_SEEN[[app$get_url()]]
  LOG_SEEN[[app$get_url()]] <- length(logs)
  errors <- grep("\\bError\\b", logs[seq_along(logs) > seen], value = TRUE)
  expect(
    length(errors) == 0,
    paste0(label, ": Error lines in log:\n", paste(errors, collapse = "\n"))
  )
}

expect_download <- function(app, selector) {
  file <- tryCatch({
    id <- app$get_js(sprintf("document.querySelector(\"%s\").id", selector))
    app$get_download(id)
  }, error = function(e) NA_character_)
  expect_true(isTRUE(file.size(file) > 0), label = paste0("download ", selector, " non-empty"))
}

# The report is rendered on click and linked from a modal.
expect_report <- function(app) {
  click_and_wait(app, IDS$report)
  href <- app$get_js("document.querySelector('.modal a[download]')?.getAttribute('href')")
  expect_true(is.character(href), label = "report link shown")
  res <- httr::GET(paste0(sub("/?(\\?.*)?$", "/", app$get_url()), href))
  expect_equal(httr::status_code(res), 200L)
  expect_gt(length(httr::content(res, as = "raw")), 0)
}
