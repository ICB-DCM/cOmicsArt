#' UI Style Constants for cOmicsArt
#'
#' Centralized styling definitions for consistent UI appearance.
#' This file contains all color palettes and style strings used throughout the app.
#'
#' @author cOmicsArt Development Team
#' @date 2026-06-14

# Color Palette ----------------------------------------------------------------

#' Primary background color (light green with transparency)
COMICSART_PRIMARY_BG <- "#70BF4F47"

#' Primary text color (white)
COMICSART_PRIMARY_TEXT <- "#ffffff"

#' Secondary background color (white)
COMICSART_SECONDARY_BG <- "white"

#' Secondary text color (black)
COMICSART_SECONDARY_TEXT <- "black"

#' Border color (black)
COMICSART_BORDER_COLOR <- "#000000"

#' Link color (black)
COMICSART_LINK_COLOR <- "black"

#' Transparent background
COMICSART_TRANSPARENT_BG <- "transparent"

#' Loading/waiter overlay color (blue with transparency)
COMICSART_WAITER_COLOR <- "#3897F147"

# Button Styles ----------------------------------------------------------------

#' Primary button style (green background, white text)
#'
#' Use for main action buttons like "Start the Journey", "Go to Preprocessing"
#'
#' @export
STYLE_PRIMARY_BUTTON <- sprintf(
  "color: %s; background-color: %s; border-color: %s",
  COMICSART_PRIMARY_TEXT,
  COMICSART_PRIMARY_BG,
  COMICSART_BORDER_COLOR
)

#' Secondary button style (white background, black text)
#'
#' Use for secondary actions like "Upload new data", "GO!"
#'
#' @export
STYLE_SECONDARY_BUTTON <- sprintf(
  "color: %s; background-color: %s; border-color: %s",
  COMICSART_SECONDARY_TEXT,
  COMICSART_SECONDARY_BG,
  COMICSART_BORDER_COLOR
)

#' Transparent button style (no background, black text)
#'
#' Use for toggle buttons or less prominent actions
#'
#' @export
STYLE_TRANSPARENT_BUTTON <- sprintf(
  "color: %s; background-color: %s; border-color: %s",
  COMICSART_SECONDARY_TEXT,
  COMICSART_TRANSPARENT_BG,
  COMICSART_TRANSPARENT_BG
)

# Link Styles ------------------------------------------------------------------

#' Underlined link style (18px font, black, underlined)
#'
#' Use for prominent links like "Go To Documentation"
#'
#' @export
STYLE_UNDERLINED_LINK <- "font-size: 18px; color: black; text-decoration: underline;"

# Layout Styles ----------------------------------------------------------------

#' Horizontal line style (gray border)
#'
#' Use with hr() for section dividers
#'
#' @export
STYLE_HORIZONTAL_LINE <- "border-top: 1px solid #858585;"

# Helper Functions -------------------------------------------------------------

#' Get button style by type
#'
#' Convenience function to retrieve button styles by name.
#'
#' @param type Character. One of: "primary", "secondary", "transparent"
#' @return Character. CSS style string
#'
#' @examples
#' \dontrun{
#' actionButton("my_btn", "Click Me", style = get_button_style("primary"))
#' }
#'
#' @export
get_button_style <- function(type = "primary") {
  switch(type,
    primary = STYLE_PRIMARY_BUTTON,
    secondary = STYLE_SECONDARY_BUTTON,
    transparent = STYLE_TRANSPARENT_BUTTON,
    stop("Unknown button type: ", type, ". Use 'primary', 'secondary', or 'transparent'.")
  )
}

#' Get standardized horizontal line
#'
#' Creates an hr() element with consistent styling.
#'
#' @return Shiny hr element with standard styling
#'
#' @examples
#' \dontrun{
#' # In UI:
#' get_styled_hr()
#' }
#'
#' @export
get_styled_hr <- function() {
  hr(style = STYLE_HORIZONTAL_LINE)
}
