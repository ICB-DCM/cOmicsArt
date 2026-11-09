# UI of one comparison of a Differential Analysis run (results_tab_server())

# The volcano plots a comparison can save and send to the report: the
# adjusted one, both together (raw above adjusted) and the one of the raw
# p-values. The single ones are shown (their copy button) and have R code.
VOLCANO_ITEMS <- list(
  Volcano = list(label = "Volcano plot padj", raw = FALSE, single = TRUE),
  VolcanoBoth = list(label = "Both Volcano plots", raw = FALSE, single = FALSE),
  VolcanoRaw = list(label = "Volcano plot pvalue", raw = TRUE, single = TRUE)
)

#' The tab of one comparison: its summary and table, and its volcano plots
#'
#' @param id Module id of the comparison (namespaced by the parent)
#' @param title Tab title displayed to user
#' @param value Tab value for programmatic access
#' @param contrast Character vector c(group1, group2)
#' @param annotations The entity annotations the volcano tooltip can show
#' @return tabPanel UI object ready to be added with appendTab()
results_tab_ui <- function(id, title, value, contrast, annotations) {
  ns <- NS(id)
  # one cell per volcano item, in VOLCANO_ITEMS' order
  item_row <- function(cell) {
    do.call(splitLayout, c(
      list(style = "border: 1px solid silver:", cellWidths = c("35%", "35%", "30%")),
      unname(Map(cell, names(VOLCANO_ITEMS), VOLCANO_ITEMS))
    ))
  }
  tabPanel(
    title = title,
    value = value,
    tabsetPanel(
      tabPanel(
        title = "Table",
        h4(paste("Summary of the results comparing", contrast[1], "and", contrast[2])),
        htmlOutput(outputId = ns("summary"), container = pre),
        p("Note that the sig_level column will not show icons if downloaded"),
        DT::dataTableOutput(outputId = ns("table"))
      ),
      tabPanel(
        title = "Volcano",
        plotlyOutput(outputId = ns("Volcano")),
        plotlyOutput(outputId = ns("VolcanoRaw")),
        hr(style = "border-top: 1px solid #000000;"),
        fluidRow(
          class = "align-bottom",
          column(3, numericInput(
            ns("psig_th"), "adj. p-value threshold", min = 0, max = 0.1, step = 0.01, value = 0.05
          )),
          column(3, numericInput(
            ns("lfc_th"), "Log FC threshold (both sides!)", min = 0, max = 10, step = 0.1, value = 1.0
          )),
          column(3, selectInput(
            ns("anno"), "Select the anno to be shown at tooltip",
            choices = annotations, selected = annotations[1]
          )),
          column(3,
            checkboxInput(ns("show_legend"), "Show Legend Corrected", value = TRUE),
            checkboxInput(ns("show_legend_raw"), "Show Legend Uncorrected", value = TRUE)
          )
        ),
        item_row(function(item, spec) h5(spec$label)) %>%
          helper(type = "markdown", content = "SampleCorr_Downloads"),
        item_row(function(item, spec) actionButton(
          ns(paste0("only2Report_", item)), "Send only to Report", class = "btn-info"
        )),
        item_row(function(item, spec) if (spec$single) downloadButton(
          ns(paste0("getR_Code_", item)), "Get underlying R code and data", icon = icon("code")
        )),
        item_row(function(item, spec) div(
          style = "display: flex; justify-content: flex-start; gap: 5px; margin-bottom: 5px;",
          downloadButton(ns(paste0("SavePlot_", item)), "Save plot", class = "btn-info"),
          if (spec$single) clipboard_button(ns(paste0("copy_", item)), plot_id = ns(item))
        )),
        item_row(function(item, spec) radioGroupButtons(
          ns(paste0("file_ext_", item)), "File Type:",
          choices = c(".png", ".tiff", ".svg", ".pdf"), selected = ".png"
        ))
      )
    )
  )
}
