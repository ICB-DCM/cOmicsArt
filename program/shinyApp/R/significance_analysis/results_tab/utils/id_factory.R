# ID Factory for Significance Analysis Results Tabs

#' Create a factory for generating namespaced IDs for a specific contrast
#'
#' This function creates a list of all IDs needed for a significance analysis
#' results tab, properly namespaced and following a consistent pattern.
#'
#' @param ns Namespace function from the module
#' @param contrast Character vector of length 2 (e.g., c("Treatment", "Control"))
#' @return List of namespaced ID strings
#' @export
#' @examples
#' ns <- NS("sig_analysis")
#' ids <- create_contrast_ids(ns, c("Treatment", "Control"))
#' # Access specific IDs: ids$volcano_plot_adj, ids$table, etc.
create_contrast_ids <- function(ns, contrast) {
  # Validate inputs
  if (!is.character(contrast) || length(contrast) != 2) {
    stop("contrast must be a character vector of length 2")
  }

  # Base prefix for this contrast
  base <- paste(contrast[1], contrast[2], sep = "_")

  list(
    # Summary IDs
    summary = ns(paste(base, "summary", sep = "_")),

    # Table IDs
    table = ns(paste(base, "table", sep = "_")),

    # Volcano UI IDs
    volcano_plot_adj = ns(paste(base, "Volcano", sep = "_")),
    volcano_plot_raw = ns(paste(base, "Volcano_praw", sep = "_")),
    psig_th_ui = ns(paste(base, "psig_th_ui", sep = "_")),
    psig_th = ns(paste(base, "psig_th", sep = "_")),
    lfc_th_ui = ns(paste(base, "lfc_th_ui", sep = "_")),
    lfc_th = ns(paste(base, "lfc_th", sep = "_")),
    volcano_anno_ui = ns(paste(base, "Volcano_anno_tooltip_ui", sep = "_")),
    volcano_anno = ns(paste(base, "Volcano_anno_tooltip", sep = "_")),
    show_legend_adj = ns(paste(base, "show_legend_adj", sep = "_")),
    show_legend_raw = ns(paste(base, "show_legend_raw", sep = "_")),
    aligned_row = ns("aligned_row"),

    # Action button IDs
    only2report_volcano = ns(paste(base, "only2Report_Volcano", sep = "_")),
    only2report_volcano_both = ns(paste(base, "only2Report_Volcano_both", sep = "_")),
    only2report_volcano_raw = ns(paste(base, "only2Report_Volcano_raw", sep = "_")),

    # Download IDs
    get_r_code_volcano = ns(paste(base, "getR_Code_Volcano", sep = "_")),
    get_r_code_volcano_raw = ns(paste(base, "getR_Code_Volcano_raw", sep = "_")),
    save_plot_volcano = ns(paste(base, "SavePlot_Volcano", sep = "_")),
    save_plot_volcano_both = ns(paste(base, "SavePlot_Volcano_both", sep = "_")),
    save_plot_volcano_raw = ns(paste(base, "SavePlot_Volcano_raw", sep = "_")),
    file_ext_volcano = ns(paste(base, "file_ext_Volcano", sep = "_")),
    file_ext_volcano_both = ns(paste(base, "file_ext_Volcano_both", sep = "_")),
    file_ext_volcano_raw = ns(paste(base, "file_ext_Volcano_raw", sep = "_")),

    # Store the base for use in plotlyProxy calls (which don't use ns)
    base_id = ns(paste(base, "Volcano", sep = "_")),
    base_id_raw = ns(paste(base, "Volcano_praw", sep = "_"))
  )
}
