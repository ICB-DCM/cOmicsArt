heatmap_sidebar <- sidebarPanel(
  id = "sidebar_heatmap",
  #########################################
  # Heatmap
  #########################################
  uiOutput("row_selection_options_ui"),
  uiOutput("LFC_toHeatmap_ui"),
  h5("Further row selection (LFC based)"),
  uiOutput("TopK_ui"),
  switchInput(
    inputId = "Selection_show_LFC",
    label = "show options (LFC-related)",
    inline = T,
    size = "mini"
  ),
  uiOutput("sample_annotation_types_cmp_heatmap_ui"),
  uiOutput("Groups2Compare_ref_heatmap_ui"),
  uiOutput("Groups2Compare_treat_heatmap_ui"),
  uiOutput("psig_threhsold_heatmap_ui"),
  actionButton(
    inputId = "Do_Heatmap",
    label = "Do Heatmap to display",
    icon("fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  h5("Aesthetics"),
  switchInput(
    inputId = "Aesthetics_show",
    label = "show options",
    size = "mini", value = T
  ),
  uiOutput("anno_options_ui"),
  uiOutput("row_anno_options_ui"),
  uiOutput("rowWiseScaled_ui"),
  uiOutput("cluster_cols_ui"),
  uiOutput("cluster_rows_ui"),
  hr(style = "border-top: 1px solid #858585;"),
  h5("Further row selection (annotation based)"),
  switchInput(
    inputId = "Selection_show_annoBased",
    label = "show options (annotation-related)",
    inline = T,
    size = "mini",
    value = F
  ),
  uiOutput("rowAnno_based_ui"),
  uiOutput("row_anno_factor_ui"),
  uiOutput("anno_options_heatmap_ui"),
  uiOutput("row_anno_options_heatmap_ui")
  # hr(style = "border-top: 1px solid #858585;")
)


heatmap_main <- mainPanel(
  id = "main_heatmap",
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("100%"),
    # plotOutput("PCA_final_gg"),
    plotOutput("HeatmapPlot")
    # %>% withSpinner(type=8,color = getOption("spinner.color", default = "#b8cee0"))
  ),
  textOutput("Options_selected_out_3", container = pre) %>% withSpinner(type = 8),
  uiOutput("row_label_options_ui"), numericInput(inputId = "row_label_no", min = 0, step = 1, label = "Threshold upon which explicit labels are shown", value = 25),
  downloadButton("SaveGeneList_Heatmap", label = "Save genes shown in Heatmap as list"),
  actionButton(inputId = "SendHeatmap2Enrichment", label = "Send genes shown to enrichment analysis", block = F),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    actionButton(inputId = "only2Report_Heatmap", label = "Send only to Report", class = "btn-info"),
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    downloadButton("getR_Code_Heatmap", label = "Get underlying R code and data",icon = icon("code"))
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    downloadButton("SavePlot_Heatmap", label = "Save plot", class = "btn-info")
  ),
  splitLayout(
    style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
    NULL,
    radioGroupButtons(
      input = "file_ext_Heatmap", label = "File Type:",
      choices = c(".png", ".tiff", ".pdf"), selected = ".png"
    )
  ),
  textAreaInput(inputId="NotesHeatmap", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
  helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
)


heatmap_panel <- tabPanel(
  "Heatmap",
  id = "heatmap",
  fluid = T,
  h4("Heatmap"),
  heatmap_sidebar,
  heatmap_main
)