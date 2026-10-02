# Volcano Plot UI Components for Significance Analysis Results

#' Create the volcano tab UI component
#'
#' Creates a tabPanel containing volcano plots and all related controls,
#' including download buttons and parameter inputs.
#'
#' @param ids List of IDs from create_contrast_ids()
#' @param contrast Character vector c(group1, group2)
#' @return tabPanel for volcano plots
#' @export
create_volcano_tab_ui <- function(ids, contrast) {
  tabPanel(
    title = "Volcano",

    # Volcano plots
    plotlyOutput(outputId = ids$volcano_plot_adj),
    plotlyOutput(outputId = ids$volcano_plot_raw),

    hr(style = "border-top: 1px solid #000000;"),

    # Controls row
    create_volcano_controls_ui(ids),

    # Download buttons
    create_volcano_downloads_ui(ids)
  )
}

#' Create volcano plot control UI
#'
#' Creates the row of controls for adjusting volcano plot parameters
#' and legend visibility.
#'
#' @param ids List of IDs from create_contrast_ids()
#' @return fluidRow containing control inputs
#' @export
create_volcano_controls_ui <- function(ids) {
  fluidRow(
    id = ids$aligned_row,
    class = "align-bottom",
    column(3, uiOutput(outputId = ids$psig_th_ui)),
    column(3, uiOutput(outputId = ids$lfc_th_ui)),
    column(3, uiOutput(outputId = ids$volcano_anno_ui)),
    column(3,
      checkboxInput(ids$show_legend_adj, "Show Legend Corrected", value = TRUE),
      checkboxInput(ids$show_legend_raw, "Show Legend Uncorrected", value = TRUE)
    )
  )
}

#' Create volcano plot download UI
#'
#' Creates all download buttons, action buttons, and file type selectors
#' for the volcano plots.
#'
#' @param ids List of IDs from create_contrast_ids()
#' @return tagList containing all download UI elements
#' @export
create_volcano_downloads_ui <- function(ids) {
  tagList(
    # Headers
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("35%", "35%", "30%"),
      h5("Volcano plot padj"),
      h5("Both Volcano plots"),
      h5("Volcano plot pvalue")
    ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),

    # Send to Report buttons
    create_split_layout_buttons(
      ids = c(ids$only2report_volcano, ids$only2report_volcano_both, ids$only2report_volcano_raw),
      label = "Send only to Report",
      class = "btn-info",
      type = "action"
    ),

    # Get R Code buttons
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("35%", "35%", "30%"),
      downloadButton(ids$get_r_code_volcano, "Get underlying R code and data", icon = icon("code")),
      NULL,
      downloadButton(ids$get_r_code_volcano_raw, "Get underlying R code and data", icon = icon("code"))
    ),

    # Save Plot buttons
    create_split_layout_buttons(
      ids = c(ids$save_plot_volcano, ids$save_plot_volcano_both, ids$save_plot_volcano_raw),
      label = "Save plot",
      class = "btn-info",
      type = "download"
    ),

    # File type selectors
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("35%", "35%", "30%"),
      radioGroupButtons(ids$file_ext_volcano, "File Type:",
        choices = c(".png", ".tiff", ".svg", ".pdf"), selected = ".png"),
      radioGroupButtons(ids$file_ext_volcano_both, "File Type:",
        choices = c(".png", ".tiff", ".svg", ".pdf"), selected = ".png"),
      radioGroupButtons(ids$file_ext_volcano_raw, "File Type:",
        choices = c(".png", ".tiff", ".svg", ".pdf"), selected = ".png")
    )
  )
}

#' Helper to create split layout with buttons
#'
#' Creates a splitLayout with three buttons of the same type.
#'
#' @param ids Character vector of length 3 with button IDs
#' @param label Button label text
#' @param class CSS class for buttons
#' @param type Either "action" or "download" to determine button type
#' @return splitLayout with three buttons
#' @export
create_split_layout_buttons <- function(ids, label, class, type = "action") {
  # downloadButton uses 'outputId', actionButton uses 'inputId'
  if(type == "download") {
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("35%", "35%", "30%"),
      downloadButton(outputId = ids[1], label = label, class = class),
      downloadButton(outputId = ids[2], label = label, class = class),
      downloadButton(outputId = ids[3], label = label, class = class)
    )
  } else {
    splitLayout(
      style = "border: 1px solid silver:",
      cellWidths = c("35%", "35%", "30%"),
      actionButton(inputId = ids[1], label = label, class = class),
      actionButton(inputId = ids[2], label = label, class = class),
      actionButton(inputId = ids[3], label = label, class = class)
    )
  }
}
