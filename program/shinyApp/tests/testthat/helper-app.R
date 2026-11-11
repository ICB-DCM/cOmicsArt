# Smoke-test helpers for the shinytest2 app tests (test-app-*.R).
#
# IDS is the single place that knows input IDs and tab values. When IDs are
# renamed (e.g. by namespacing in a refactor), change them here only.
# The checks are deliberately coarse: "it ran with defaults and nothing broke".
# Precise behaviour belongs in unit tests.

HEATMAP <- list(
  go = "dataset_1-heatmap-Do_Heatmap",
  out = "#dataset_1-heatmap-HeatmapPlot",
  rcode = "#dataset_1-heatmap-getR_Code_Heatmap",
  after = "#dataset_1-heatmap-continue_heatmap"  # confirm "more than 100 rows" if shown
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
      upload = "upload-refresh_file_input",
      clear = "upload-clear_FileInput"
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
  to_pre = "selection-use_full_data",
  # Selection: `open` the controls, pick by annotation `column` and `values`
  # for samples and entities, then `go` to Pre-processing
  selection = list(
    block = "selection-mainPanel_other",
    open = "selection-select_data",
    sample_column = "selection-providedSampleAnnotationTypes",
    samples = "selection-sample_selection",
    row_column = "selection-providedRowAnnotationTypes",
    rows = "selection-row_selection",
    go = "selection-NextPanel"
  ),
  processing_type = "preprocessing-processing_type",
  procedure = "preprocessing-preprocessing_procedure",
  # Pre-processing options shown for some procedures only
  pre = list(
    filtering = "preprocessing-preprocessing_filtering",  # filtering in front
    threshold = "preprocessing-filter_threshold",                   # global filtering
    threshold_samplewise = "preprocessing-filter_threshold_samplewise",
    samplesize = "preprocessing-filter_samplesize",
    deseq_factors = "preprocessing-deseq_factors",
    limma_intercept = "preprocessing-limma_intercept",
    limma_formula = "preprocessing-limma_formula",
    batch = "preprocessing-batch_column"
  ),
  preprocess = "preprocessing-get_preprocessing",
  pca_colour = "dataset_1-pca-coloring_options",  # a PCA sidebar choice
  pre_out = "preprocessing-summary",
  pre_rcode = "#preprocessing-rcode_violin",
  report = "DownloadTestModule-DownloadReport",
  errors = paste(".shiny-output-error:not(.shiny-output-error-validation),",
                 ".modal-title font[color='red']"),  # error_modal() title
  # One entry per analysis tab: `go` button; CSS selectors for an `out` that
  # must show content and the R-code download `rcode`; optional `set` inputs
  # and `after` selectors clicked after `go` (if present); `tab` if the name
  # is not the tab value.
  modules = list(
    "Sample Correlation" = list(
      go = "dataset_1-sample_correlation-Do_SampleCorrelation",
      out = "#dataset_1-sample_correlation-SampleCorrelationPlot",
      rcode = "#dataset_1-sample_correlation-getR_SampleCorrelation"
    ),
    "PCA" = list(
      go = "dataset_1-pca-Do_PCA",
      out = "#dataset_1-pca-PCA_plot",
      rcode = "#dataset_1-pca-getR_Code_PCA"
    ),
    "ML Classification" = list(
      go = "dataset_1-ml_classification-run_clustering",
      out = "#dataset_1-ml_classification-cluster_plot",
      rcode = "#dataset_1-ml_classification-getR_Code_cluster"
    ),
    "Differential Analysis" = list(
      go = "dataset_1-differential-significanceGo",
      # one results tab per comparison, IDs derived from the group names
      after = "#dataset_1-differential-Significance_div a[data-value='Volcano']",
      out = "#dataset_1-differential-Significance_div [id$='-Volcano']",
      rcode = "#dataset_1-differential-Significance_div [id$='-getR_Code_Volcano']"
    ),
    "ML Classification (supervised)" = list(
      tab = "ML Classification",
      set = list("dataset_1-ml_classification-ml_method_type" = "supervised"),
      go = "dataset_1-ml_classification-run_classification",
      out = "#dataset_1-ml_classification-svm_plot",
      rcode = "#dataset_1-ml_classification-getR_Code_svm"
    ),
    # the overview of several comparisons is only shown for more than one;
    # the comparisons are Lipidomics diet groups
    "Differential Analysis (2 comparisons)" = list(
      tab = "Differential Analysis",
      # Student's t-test: with the default Welch, the raw Lipidomics data have no
      # adjusted hit in HFD_CD_CD:CD_CD_CD, so the overview would have nothing to draw
      set = list(
        "dataset_1-differential-comparisons" = c("CD_CD_HFD:CD_CD_CD", "HFD_CD_CD:CD_CD_CD"),
        "dataset_1-differential-test_method" = "T-Test"
      ),
      go = "dataset_1-differential-significanceGo",
      out = "#dataset_1-differential-Significant_Plot_final",
      rcode = "#dataset_1-differential-getR_Code_Sig"
    ),
    "Heatmap" = HEATMAP,
    # after a pre-processing with a batch column
    "PCA (batch corrected)" = list(
      tab = "PCA",
      set = list("dataset_1-pca-UseBatch" = "batch_corrected"),
      go = "dataset_1-pca-Do_PCA",
      out = "#dataset_1-pca-PCA_plot",
      rcode = "#dataset_1-pca-getR_Code_PCA"
    ),
    "Differential Analysis (batch corrected)" = list(
      tab = "Differential Analysis",
      set = list("dataset_1-differential-UseBatch" = "batch_corrected"),
      go = "dataset_1-differential-significanceGo",
      after = "#dataset_1-differential-Significance_div a[data-value='Volcano']",
      out = "#dataset_1-differential-Significance_div [id$='-Volcano']",
      rcode = "#dataset_1-differential-Significance_div [id$='-getR_Code_Volcano']"
    ),
    # Transcriptomics: 'all' rows is too slow; the gene list feeds Enrichment
    "Heatmap (top K)" = modifyList(HEATMAP, list(
      tab = "Heatmap",
      set = list(
        "dataset_1-heatmap-row_selection_options" = "Top K",
        # significant genes: by DESeq2's fold change alone (issue 58) the top
        # 500 are mostly noisy low-count genes, with no significant Hallmark
        "dataset_1-heatmap-TopK_order" = "LogFoldChange and Significant",
        "dataset_1-heatmap-TopK" = 500  # enough genes for a significant enrichment
      ),
      after = c(HEATMAP$after, "#dataset_1-heatmap-SaveGeneList_Heatmap")
    )),
    "Heatmap (top K, significant)" = modifyList(HEATMAP, list(
      tab = "Heatmap",
      set = list(
        "dataset_1-heatmap-row_selection_options" = "Top K",
        "dataset_1-heatmap-TopK_order" = "LogFoldChange and Significant",
        "dataset_1-heatmap-TopK" = 500
      )
    )),
    "Single Gene Visualisations" = list(
      go = "dataset_1-single_gene_visualisation-singleGeneGo",
      out = "#dataset_1-single_gene_visualisation-SingleGenePlot",
      rcode = "#dataset_1-single_gene_visualisation-getR_Code_SingleEntities"
    ),
    "Enrichment Analysis" = list(
      set = list(
        "dataset_1-enrichment_analysis-organism_choice_ea" = "Human genes (GRCh38.p14)",
        "dataset_1-enrichment_analysis-ORA_or_GSE" = "OverRepresentation_Analysis",
        "dataset_1-enrichment_analysis-GeneSet2Enrich" = "heatmap_genes",
        "dataset_1-enrichment_analysis-GeneSetChoice" = "Hallmarks"  # msigdbr, offline
      ),
      go = "dataset_1-enrichment_analysis-enrichmentGO",
      out = "#dataset_1-enrichment_analysis-Hallmarks-EnrichmentPlot",
      rcode = "#dataset_1-enrichment_analysis-Hallmarks-getR_Code"
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

# Click "Go to Preprocessing" after a successful Upload. Waits for the tab
# to change (the switch can outlast the idle wait); without a change, the
# failure shows the app's log lines since the click.
go_to_preprocessing <- function(app) {
  n_logs <- nrow(as.data.frame(app$get_logs()))
  click_and_wait(app, IDS$to_pre)
  tab <- tryCatch(
    app$wait_for_value(input = IDS$tabs, ignore = list(NULL, "", IDS$data_tab), timeout = TIMEOUT),
    error = function(e) app$get_value(input = IDS$tabs)
  )
  logs <- as.data.frame(app$get_logs())
  expect(identical(tab, IDS$pre_tab), paste0(
    "Pre-processing tab selected, not ", deparse(tab), "; app log since the click:\n",
    paste(utils::tail(logs$message[seq_len(nrow(logs)) > n_logs], 60), collapse = "\n")
  ))
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
# Returns the violin R-code zip (invisibly).
# `options`: further inputs by IDS$pre name, set in order after the procedure,
# e.g. list(threshold = 100, batch = "batch_factor").
preprocess <- function(app, type = NULL, procedure = NULL, options = list()) {
  # also when coming back from an analysis tab: hidden outputs don't update
  set_and_wait(app, !!IDS$tabs := IDS$pre_tab)
  if (!is.null(type)) set_and_wait(app, !!IDS$processing_type := type)
  if (!is.null(procedure)) set_and_wait(app, !!IDS$procedure := procedure)
  for (name in names(options)) set_and_wait(app, !!IDS$pre[[name]] := options[[name]])
  click_and_wait(app, IDS$preprocess)
  expect_output_shown(app, paste0("#", IDS$pre_out))
  expect_no_app_errors(app, "Pre-processing")
  expect_download(app, IDS$pre_rcode)  # returns the R-code zip
}

# Open an analysis tab, run it with defaults and apply the generic checks.
# Returns the tab's R-code zip (invisibly).
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

# The download behind `selector` is a non-empty file named by download_name().
# Waits until the link is usable first: Shiny sets its href once the output
# is bound, and some links are enabled only after a run (shinyjs), which can
# arrive after the output the test checked. Returns the file's path (NA if it
# failed), invisibly; a failure reports why.
expect_download <- function(app, selector) {
  error <- NULL
  file <- tryCatch({
    app$wait_for_js(sprintf(
      "(function(){var e=document.querySelector(\"%s\"); if(!e) return false;
        var href=e.getAttribute('href');
        return !!href && href !== '#' && !e.hasAttribute('disabled') &&
          !e.classList.contains('disabled');})()", selector), timeout = TIMEOUT)
    id <- app$get_js(sprintf("document.querySelector(\"%s\").id", selector))
    app$get_download(id)
  }, error = function(e) {
    error <<- conditionMessage(e)
    NA_character_
  })
  expect_true(
    isTRUE(file.size(file) > 0),
    label = paste0("download ", selector, " non-empty", if (!is.null(error)) paste0(" (", error, ")"))
  )
  if (!is.na(file)) expect_match(
    basename(file),
    "^cOmicsArt_[A-Za-z0-9_]+_\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}\\.[A-Za-z]+$",
    label = paste0("download ", selector, " name")
  )
  invisible(file)
}

# The report is rendered on click and linked from a modal. Waits for the link
# (rendering can outlast the idle wait); without one, the failure shows the
# app's log lines since the click.
expect_report <- function(app) {
  n_logs <- nrow(as.data.frame(app$get_logs()))
  click_and_wait(app, IDS$report)
  link <- "document.querySelector('.modal a[download]')?.getAttribute('href')"
  shown <- tryCatch({
    app$wait_for_js(paste0("!!", link), timeout = TIMEOUT); TRUE
  }, error = function(e) FALSE)
  logs <- as.data.frame(app$get_logs())
  expect(shown, paste0(
    "report link shown; app log since the click:\n",
    paste(utils::tail(logs$message[seq_len(nrow(logs)) > n_logs], 60), collapse = "\n")
  ))
  href <- app$get_js(link)
  res <- httr::GET(paste0(sub("/?(\\?.*)?$", "/", app$get_url()), href))
  expect_equal(httr::status_code(res), 200L)
  expect_gt(length(httr::content(res, as = "raw")), 0)
}
