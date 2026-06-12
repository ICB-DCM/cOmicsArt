# Table UI Component for Significance Analysis Results

#' Create the table tab UI component
#'
#' Creates a tabPanel containing the summary and results table for
#' a significance analysis comparison.
#'
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @return tabPanel for results table
#' @export
create_table_tab_ui <- function(ids, contrast) {
  tabPanel(
    title = "Table",
    # Summary of results
    h4(paste("Summary of the results comparing", contrast[1], "and", contrast[2])),
    htmlOutput(outputId = ids$summary, container = pre),
    # Note about downloading
    p("Note that the sig_level column will not show icons if downloaded"),
    # Results table
    DT::dataTableOutput(outputId = ids$table)
  )
}
