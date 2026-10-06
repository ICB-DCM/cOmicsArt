#' UI Component Factory Functions for cOmicsArt
#'
#' This file contains reusable UI component builders with consistent styling.
#' These functions eliminate duplication and ensure visual consistency.
#'
#' @author cOmicsArt Development Team
#' @date 2026-06-14

# Button Components ------------------------------------------------------------

#' Create a primary action button with cOmicsArt styling
#'
#' Primary buttons have the green background and are used for main actions.
#'
#' @param inputId Character. Button ID
#' @param label Character. Button label
#' @param icon_name Character. Font Awesome icon name (optional)
#' @param full_width Logical. Make button 100% width (default: FALSE)
#' @param width Character. Custom width (default: NULL, uses full_width if TRUE)
#' @param ... Additional arguments passed to actionButton()
#'
#' @return Shiny action button with primary styling
#'
#' @examples
#' \dontrun{
#' primary_action_button(
#'   inputId = "start_analysis",
#'   label = "Start Analysis",
#'   icon_name = "rocket",
#'   full_width = TRUE
#' )
#' }
#'
#' @export
primary_action_button <- function(inputId, label, icon_name = NULL,
                                   full_width = FALSE, width = NULL, ...) {
  icon_obj <- if (!is.null(icon_name)) icon(icon_name) else NULL

  # Determine width
  if (is.null(width)) {
    width <- if (full_width) "100%" else NULL
  }

  actionButton(
    inputId = inputId,
    label = label,
    icon = icon_obj,
    width = width,
    style = STYLE_PRIMARY_BUTTON,
    ...
  )
}

#' Create a secondary action button with cOmicsArt styling
#'
#' Secondary buttons have white background and are used for less prominent actions.
#'
#' @inheritParams primary_action_button
#' @export
secondary_action_button <- function(inputId, label, icon_name = NULL,
                                     full_width = FALSE, width = NULL, ...) {
  icon_obj <- if (!is.null(icon_name)) icon(icon_name) else NULL

  # Determine width
  if (is.null(width)) {
    width <- if (full_width) "100%" else NULL
  }

  actionButton(
    inputId = inputId,
    label = label,
    icon = icon_obj,
    width = width,
    style = STYLE_SECONDARY_BUTTON,
    ...
  )
}

#' Create a transparent action button with cOmicsArt styling
#'
#' Transparent buttons have no background and are used for toggles or minimal actions.
#'
#' @inheritParams primary_action_button
#' @export
transparent_action_button <- function(inputId, label, icon_name = NULL,
                                       full_width = FALSE, width = NULL, ...) {
  icon_obj <- if (!is.null(icon_name)) icon(icon_name) else NULL

  # Determine width
  if (is.null(width)) {
    width <- if (full_width) "100%" else NULL
  }

  actionButton(
    inputId = inputId,
    label = label,
    icon = icon_obj,
    width = width,
    style = STYLE_TRANSPARENT_BUTTON,
    ...
  )
}

# Download Components ----------------------------------------------------------

#' Create a download button with optional helper
#'
#' Creates a download button with consistent styling and optional help tooltip.
#'
#' @param outputId Character. Download handler ID
#' @param label Character. Button label
#' @param help_content Character. Markdown help content ID (optional)
#' @param class Character. CSS class (default: "btn-default")
#' @param ... Additional arguments passed to downloadButton()
#'
#' @return Download button with optional helper
#'
#' @examples
#' \dontrun{
#' download_button_with_help(
#'   outputId = "download_results",
#'   label = "Download Results",
#'   help_content = "download_help"
#' )
#' }
#'
#' @export
download_button_with_help <- function(outputId, label, help_content = NULL,
                                        class = "btn-default", ...) {
  btn <- downloadButton(
    outputId = outputId,
    label = label,
    class = class,
    ...
  )

  if (!is.null(help_content)) {
    # Attach helper if shinyhelper is available
    if (requireNamespace("shinyhelper", quietly = TRUE)) {
      btn <- btn %>% shinyhelper::helper(type = "markdown", content = help_content)
    } else {
      warning("shinyhelper package not available. Helper not attached to download button.")
    }
  }

  btn
}

# Input Components -------------------------------------------------------------

#' Create a select input with standard width
#'
#' Creates a selectInput with the standard 80% width used throughout cOmicsArt.
#'
#' @param inputId Character. Input ID
#' @param label Character. Label
#' @param choices Vector. Choices
#' @param selected Character. Selected value (default: "")
#' @param width Character. Width (default: "80%")
#' @param multiple Logical. Allow multiple selections (default: FALSE)
#' @param ... Additional arguments passed to selectInput()
#'
#' @return Select input with standard width
#'
#' @examples
#' \dontrun{
#' standard_select_input(
#'   inputId = "omic_type",
#'   label = "Omic Type",
#'   choices = c("Transcriptomics", "Lipidomics", "Metabolomics")
#' )
#' }
#'
#' @export
standard_select_input <- function(inputId, label, choices, selected = "",
                                   width = "80%", multiple = FALSE, ...) {
  selectInput(
    inputId = inputId,
    label = label,
    choices = choices,
    selected = selected,
    width = width,
    multiple = multiple,
    ...
  )
}

#' Create a file input with standard width
#'
#' Creates a fileInput with the standard 80% width.
#'
#' @param inputId Character. Input ID
#' @param label Character or HTML. Label (can include HTML for links)
#' @param accept Character vector. Accepted file types (default: c(".csv"))
#' @param width Character. Width (default: "80%")
#' @param ... Additional arguments passed to fileInput()
#'
#' @return File input with standard width
#'
#' @examples
#' \dontrun{
#' standard_file_input(
#'   inputId = "data_matrix",
#'   label = "Upload Data Matrix",
#'   accept = c(".csv", ".xlsx")
#' )
#' }
#'
#' @export
standard_file_input <- function(inputId, label, accept = c(".csv"),
                                 width = "80%", ...) {
  shiny::fileInput(
    inputId = inputId,
    label = label,
    accept = accept,
    width = width,
    ...
  )
}

# Layout Components ------------------------------------------------------------

#' Create a triple split layout with identical buttons
#'
#' Creates a splitLayout with 3 identical buttons (action or download).
#' Commonly used in volcano plot controls.
#'
#' @param ids Character vector of length 3. Button IDs
#' @param label Character. Button label (same for all 3)
#' @param class Character. Button class (default: "btn-default")
#' @param type Character. "action" or "download" (default: "action")
#' @param ... Additional arguments passed to button function
#'
#' @return Split layout with 3 buttons
#'
#' @examples
#' \dontrun{
#' triple_button_layout(
#'   ids = c("copy_adj", "copy_raw", "copy_both"),
#'   label = "Copy to Clipboard",
#'   type = "action"
#' )
#' }
#'
#' @export
triple_button_layout <- function(ids, label, class = "btn-default",
                                  type = "action", ...) {
  if (length(ids) != 3) {
    stop("ids must be a vector of length 3")
  }

  if (type == "download") {
    # Download buttons use outputId
    button_fn <- function(id) {
      downloadButton(outputId = id, label = label, class = class, ...)
    }
  } else {
    # Action buttons use inputId
    button_fn <- function(id) {
      actionButton(inputId = id, label = label, class = class, ...)
    }
  }

  splitLayout(
    button_fn(ids[1]),
    button_fn(ids[2]),
    button_fn(ids[3])
  )
}

# Clipboard Button Components --------------------------------------------------

#' Create an action button with clipboard data attribute
#'
#' Creates an action button that automatically works with the generic
#' JavaScript clipboard handler (no need to add JS code).
#'
#' @param inputId Character. Button ID
#' @param label Character. Button label
#' @param plot_id Character. ID of the plot to copy (for data-clipboard-plot attribute)
#' @param icon_name Character. Font Awesome icon (default: "clipboard")
#' @param class Character. CSS class (default: "btn-default")
#' @param ... Additional arguments passed to actionButton()
#'
#' @return Action button with data-clipboard-plot attribute
#'
#' @examples
#' \dontrun{
#' clipboard_button(
#'   inputId = "copy_pca_btn",
#'   label = "Copy PCA Plot",
#'   plot_id = "PCA_plot"
#' )
#' }
#'
#' @export
clipboard_button <- function(inputId, label = "Copy to Clipboard", plot_id,
                              icon_name = "clipboard", class = "btn-default", ...) {
  actionButton(
    inputId = inputId,
    label = label,
    icon = icon(icon_name),
    class = class,
    `data-clipboard-plot` = plot_id,  # This attribute triggers JS handler
    ...
  )
}

# Helper Attachment ------------------------------------------------------------

#' Attach helper to any UI element
#'
#' Convenience wrapper for shinyhelper::helper that checks if package is available.
#'
#' @param ui_element Shiny UI element
#' @param content Character. Markdown file name (without .md extension)
#' @param type Character. Helper type (default: "markdown")
#' @param ... Additional arguments passed to shinyhelper::helper()
#'
#' @return UI element with helper attached, or original element if shinyhelper unavailable
#'
#' @examples
#' \dontrun{
#' selectInput("my_input", "Label", choices = c("A", "B")) %>%
#'   attach_helper("my_input_help")
#' }
#'
#' @export
attach_helper <- function(ui_element, content, type = "markdown", ...) {
  if (requireNamespace("shinyhelper", quietly = TRUE)) {
    ui_element %>% shinyhelper::helper(type = type, content = content, ...)
  } else {
    warning("shinyhelper package not available. Helper not attached.")
    ui_element
  }
}
