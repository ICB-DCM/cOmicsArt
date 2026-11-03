# Main UI for Significance Analysis Results Tab

#' Create the UI for a single significance analysis results tab
#'
#' A tabPanel with the results table and the volcano plots of one
#' comparison; its IDs are in the namespace of the comparison's module `id`
#' (see results_tab_server()).
#'
#' @param id Module id of the comparison (namespaced by the parent)
#' @param title Tab title displayed to user
#' @param value Tab value for programmatic access
#' @param contrast Character vector c(group1, group2)
#' @return tabPanel UI object ready to be added with appendTab()
results_tab_ui <- function(id, title, value, contrast) {
  ids <- create_contrast_ids(NS(id), contrast)
  tabPanel(
    title = title,
    value = value,
    tabsetPanel(
      # Table tab
      create_table_tab_ui(ids, contrast),

      # Volcano tab
      create_volcano_tab_ui(ids, contrast)
    )
  )
}
