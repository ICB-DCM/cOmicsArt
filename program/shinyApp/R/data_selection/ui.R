data_selection_sidebar_panel <- upload_sidebar_ui("upload")


data_selection_main_panel <- mainPanel(
  id = "mainPanel_DataSelection",
  upload_main_ui("upload"),
  selection_ui("selection")
)


data_selection_panel <- tabPanel(
  title = tagList(tags$span("1. Data selection")), 
  value = "Data selection",
  id = "Data_selection",
  fluid = T,
  h4("Data Selection"),
  ################################################################################
  # Data Selection
  ################################################################################
  data_selection_sidebar_panel,
  data_selection_main_panel
)
