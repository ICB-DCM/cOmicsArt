#' Plotly Helper Functions for cOmicsArt
#'
#' This file contains reusable functions for creating Plotly plots with
#' clipboard functionality and standard layouts.
#'
#' @author cOmicsArt Development Team
#' @date 2026-06-14

#' Add clipboard button to Plotly plot
#'
#' Adds a custom "Copy to clipboard" button to the Plotly mode bar that allows
#' users to copy the plot as a PNG image directly to their clipboard.
#'
#' @param plotly_obj A plotly object (from ggplotly or plot_ly)
#' @param plot_id Character. Plot identifier used for Shiny notifications
#'   (e.g., "PCA_plot"). This ID will be used in success/error messages.
#' @param additional_buttons List. Additional modeBar buttons (optional, not currently used)
#' @param buttons_to_remove Character vector. Buttons to remove from mode bar (default: 'toImage')
#'
#' @return Plotly object with clipboard functionality added
#'
#' @details
#' The function injects JavaScript code that:
#' 1. Adds a "Copy to clipboard" button to the Plotly mode bar
#' 2. Converts the plot to PNG when clicked
#' 3. Copies the PNG to the system clipboard
#' 4. Sends success/error notifications back to Shiny via input$plot_copied_status
#'
#' Success notification format: "<plot_id>_success_<timestamp>"
#' Error notification format: "<plot_id>_clipboard_error_<timestamp>"
#'
#' @examples
#' \dontrun{
#' library(plotly)
#' p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, type = 'scatter')
#' p_with_clipboard <- add_plotly_clipboard(p, plot_id = "iris_plot")
#' }
#'
#' @export
add_plotly_clipboard <- function(plotly_obj,
                                  plot_id,
                                  additional_buttons = NULL,
                                  buttons_to_remove = c('toImage')) {

  # Validate inputs
  if (!inherits(plotly_obj, "plotly")) {
    stop("plotly_obj must be a plotly object")
  }
  if (!is.character(plot_id) || length(plot_id) != 1) {
    stop("plot_id must be a single character string")
  }

  # Convert buttons_to_remove to JSON array
  buttons_json <- jsonlite::toJSON(buttons_to_remove, auto_unbox = FALSE)

  # Build JavaScript code with plot_id injected
  # Note: Using sprintf to safely inject plot_id into the JavaScript string
  js_code <- sprintf("
    function(el, x) {
      Plotly.newPlot(el, x.data, x.layout, {
        modeBarButtonsToAdd: [{
          name: 'Copy to clipboard',
          icon: Plotly.Icons.camera,
          click: function(gd) {
            Plotly.toImage(gd, {format: 'png'}).then(function(url) {
              fetch(url)
                .then(res => res.blob())
                .then(blob => {
                  navigator.clipboard.write([
                    new ClipboardItem({ [blob.type]: blob })
                  ]).then(function() {
                    Shiny.setInputValue('plot_copied_status', '%s_success_' + Date.now(), {priority: 'event'});
                  }).catch(function(err) {
                    Shiny.setInputValue('plot_copied_status', '%s_clipboard_error_' + Date.now(), {priority: 'event'});
                  });
                });
            });
          }
        }],
        modeBarButtonsToRemove: %s
      });
    }
  ", plot_id, plot_id, buttons_json)

  # Apply onRender and return
  htmlwidgets::onRender(plotly_obj, js_code)
}

#' Apply standard Plotly layout for cOmicsArt plots
#'
#' Applies consistent axis formatting (font sizes) to Plotly plots.
#'
#' @param plotly_obj A plotly object
#' @param font_size Numeric. Font size for axis tick labels (default: 15)
#' @param ... Additional layout parameters passed to plotly::layout()
#'
#' @return Plotly object with standard layout applied
#'
#' @details
#' This function ensures consistent axis label formatting across all cOmicsArt plots.
#' Additional layout parameters can be passed via ... to customize beyond the defaults.
#'
#' @examples
#' \dontrun{
#' library(plotly)
#' p <- plot_ly(data = iris, x = ~Sepal.Length, y = ~Sepal.Width, type = 'scatter')
#' p_styled <- apply_standard_plotly_layout(p, font_size = 18)
#' }
#'
#' @export
apply_standard_plotly_layout <- function(plotly_obj, font_size = 15, ...) {
  plotly::layout(
    plotly_obj,
    yaxis = list(tickfont = list(size = font_size)),
    xaxis = list(tickfont = list(size = font_size)),
    ...
  )
}

#' Create ggplotly with clipboard and standard layout (convenience wrapper)
#'
#' Convenience function that combines ggplotly conversion, standard layout,
#' and clipboard functionality in one call.
#'
#' @param gg_plot A ggplot object
#' @param plot_id Character. Plot identifier for clipboard notifications
#' @param font_size Numeric. Axis font size (default: 15)
#' @param tooltip Character or formula. Tooltip aesthetic(s) to display (default: "all").
#'   See plotly::ggplotly documentation for details.
#' @param ... Additional arguments passed to ggplotly()
#'
#' @return Plotly object ready for rendering with clipboard functionality
#'
#' @details
#' This is the recommended function for most use cases. It handles the entire
#' pipeline of converting a ggplot to an interactive Plotly plot with:
#' - Standard cOmicsArt axis formatting
#' - Clipboard copy functionality
#' - Customizable tooltips
#'
#' @examples
#' \dontrun{
#' library(ggplot2)
#' library(plotly)
#'
#' p <- ggplot(iris, aes(x = Sepal.Length, y = Sepal.Width, color = Species)) +
#'   geom_point()
#'
#' # In a Shiny server:
#' output$iris_plot <- renderPlotly({
#'   create_clipboard_plotly(p, plot_id = "iris_plot")
#' })
#' }
#'
#' @export
create_clipboard_plotly <- function(gg_plot,
                                     plot_id,
                                     font_size = 15,
                                     tooltip = "all",
                                     ...) {
  # Convert ggplot to plotly
  p <- plotly::ggplotly(gg_plot, tooltip = tooltip, ...)

  # Apply standard layout
  p <- apply_standard_plotly_layout(p, font_size = font_size)

  # Add clipboard functionality
  add_plotly_clipboard(p, plot_id = plot_id)
}
