# Create modal to User
popupModal <- function(failed = FALSE) {
  modalDialog(
    textInput("userInput", "File Name?"),
    if (failed)
      div(tags$b("You did not input anything", style = "color: red;")),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton("ok", "OK")
    )
  )
}

