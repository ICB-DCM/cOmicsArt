# Create modal to User
popupModal <- function(failed = FALSE) {
  modalDialog(
    textInput(inputId = "userInput", label = "File Name?"),
    if (failed)
      div(tags$b("You did not input anything", style = "color: red;")),
    
    footer = tagList(
      modalButton(label = "Cancel"),
      actionButton(inputId = "ok", label = "OK")
    )
  )
}

