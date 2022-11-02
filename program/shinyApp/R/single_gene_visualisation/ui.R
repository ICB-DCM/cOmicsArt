single_gene_visualisation_sidebar <- sidebarPanel(
  id = "sidebar_single_gene_visualisation",
  uiOutput("type_of_data_gene_ui"),
  uiOutput("type_of_visualitsation_ui"),
  uiOutput("Select_GeneAnno_ui"),
  uiOutput("Select_Gene_ui"),
  helpText("Note: if you choose a group rather than a single entitie, the values will be summarized by taking the median"),
  uiOutput("accross_condition_ui"),
  actionButton("singleGeneGo", label = "Get single gene visualisation"),
  hr(style = "border-top: 1px solid #858585;")
)


single_gene_visualisation_main <- mainPanel(
  id = "main_single_gene_visualisation",
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
    plotOutput("SingleGenePlot"), NULL
  ),
  uiOutput("chooseComparisons_ui"),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    actionButton(
      inputId = "only2Report_SingleEntities",
      label = "Send only to Report",
      class = "btn-info"
    ),
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    downloadButton(
      "getR_Code_SingleEntities",
      label = "Get underlying R code and data",
      icon = icon("code")
    )
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    downloadButton("SavePlot_singleGene", label = "Save plot", class = "btn-info")
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    radioGroupButtons(
      inputId = "file_ext_singleGene",
      label = "File Type:",
      choices = c(".png", ".tiff", ".pdf"),
      selected = ".png"
    )
  ),
  textAreaInput(
    inputId="NotesSingleEntities",
    label="Notes:",
    placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
  helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")

)


single_gene_visualisation_panel <- tabPanel(
  "Single Gene Visualisations",
  id = "single_gene_visualisation",
  fluid = TRUE,
  h4("Single Gene Visualisations"),
  #########################################
  # Single Gene Visualisations
  #########################################
  single_gene_visualisation_sidebar,
  single_gene_visualisation_main
)