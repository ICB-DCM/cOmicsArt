add_notes_report <<- function(notes) {
  # Add notes to the report
  # Parameters:
  #   notes: character vector of notes to add to the report
  html_notes <- NOTES_BASE_HTML
  # find "__notes_placeholder__" in the html_notes and replace it with the notes
  html_notes <- gsub("__notes_placeholder__", notes, html_notes)
  return(html_notes)
}