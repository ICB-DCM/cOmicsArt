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

IDS <- list(
  tabs = "tabsetPanel1",
  data_tab = "Data selection",
  pre_tab = "Pre-processing",
  omic = "omic_type_testdata",
  upload = "EasyTestForUser",
  to_pre = "use_full_data",
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

# Upload the bundled test data of one omic type and go to Pre-processing.
load_test_data <- function(app, omic) {
  set_and_wait(app, !!IDS$tabs := IDS$data_tab)
  set_and_wait(app, !!IDS$omic := omic)
  click_and_wait(app, IDS$upload)
  click_and_wait(app, IDS$to_pre)
  expect_equal(app$get_value(input = IDS$tabs), IDS$pre_tab)
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

# No red Shiny error outputs (validation messages are fine), no error_modal()
# dialog, no R errors in the log.
expect_no_app_errors <- function(app, label) {
  n <- app$get_js(sprintf("document.querySelectorAll(\"%s\").length", IDS$errors))
  expect_equal(n, 0, label = paste0(label, ": error outputs or dialogs"))
  logs <- as.data.frame(app$get_logs())
  errors <- grep("\\bError\\b", logs$message, value = TRUE)
  expect_equal(errors, character(0), label = paste0(label, ": Error lines in log"))
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
