create_new_tab <- function(title, targetPanel, result, contrast){
  print("create_new_tab")
  print(title)
  print(result)
  # create a new tabPanel
  appendTab(
    inputId = targetPanel,
    tabPanel(
      title = title,
      # summary of the results
        h4(paste("Summary of the results comparing ", contrast[1], " and ", contrast[2]))
    )
  )
}