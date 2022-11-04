heatmap_sidebar_UI<- function(){
  sidebarPanel(
    id = "sidebar_heatmap",
    #########################################
    # Heatmap
    #########################################
    uiOutput(outputId = "row_selection_options_ui"),
    uiOutput(outputId = "LFC_toHeatmap_ui"),
    h5("Further row selection (LFC based)"),
    uiOutput(outputId = "TopK_ui"),
    switchInput(
      inputId = "Selection_show_LFC",
      label = "show options (LFC-related)",
      inline = T,
      size = "mini"
    ),
    uiOutput(outputId = "sample_annotation_types_cmp_heatmap_ui"),
    uiOutput(outputId = "Groups2Compare_ref_heatmap_ui"),
    uiOutput(outputId = "Groups2Compare_treat_heatmap_ui"),
    uiOutput(outputId = "psig_threhsold_heatmap_ui"),
    actionButton(
      inputId = "Do_Heatmap",
      label = "Do Heatmap to display",
      icon(name = "fas fa-laptop-code")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    h5("Aesthetics"),
    switchInput(
      inputId = "Aesthetics_show",
      label = "show options",
      size = "mini", 
      value = T
    ),
    uiOutput(outputId = "anno_options_ui"),
    uiOutput(outputId = "row_anno_options_ui"),
    uiOutput(outputId = "rowWiseScaled_ui"),
    uiOutput(outputId = "cluster_cols_ui"),
    uiOutput(outputId = "cluster_rows_ui"),
    hr(style = "border-top: 1px solid #858585;"),
    h5("Further row selection (annotation based)"),
    switchInput(
      inputId = "Selection_show_annoBased",
      label = "show options (annotation-related)",
      inline = T,
      size = "mini",
      value = F
    ),
    uiOutput(outputId = "rowAnno_based_ui"),
    uiOutput(outputId = "row_anno_factor_ui"),
    uiOutput(outputId = "anno_options_heatmap_ui"),
    uiOutput(outputId = "row_anno_options_heatmap_ui")
  )
}

heatmap_main <- function(){
  mainPanel(
    id = "main_heatmap",
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("100%"),
      plotOutput(
        outputId = "HeatmapPlot"
      ) %>% withSpinner(type=8,color = getOption("spinner.color", default = "#b8cee0"))
    ),
    textOutput(outputId = "Options_selected_out_3", container = pre) %>% withSpinner(type = 8),
    uiOutput(outputId = "row_label_options_ui"),
    numericInput(
      inputId = "row_label_no",
      label = "Threshold upon which explicit labels are shown",
      min = 0, 
      step = 1, 
      value = 25
    ),
    downloadButton(
      outputId = "SaveGeneList_Heatmap",
      label = "Save genes shown in Heatmap as list"
    ),
    actionButton(
      inputId = "SendHeatmap2Enrichment",
      label = "Send genes shown to enrichment analysis",
      block = F
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      actionButton(
        inputId = "only2Report_Heatmap",
        label = "Send only to Report",
        class = "btn-info"
      ),
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = "getR_Code_Heatmap",
        label = "Get underlying R code and data",
        icon = icon(name = "code")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = "SavePlot_Heatmap",
        label = "Save plot",
        class = "btn-info"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      radioGroupButtons(
        inputId = "file_ext_Heatmap", 
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"), 
        selected = ".png"
      )
    ),
    textAreaInput(
      inputId="NotesHeatmap",
      label = "Notes:",
      placeholder = NOTES_PlACEHOLDER,
      width = "1000px") %>% helper(
        type = "markdown", 
        content = "TakingNotesMD_help"
        ),
    helpText(NOTES_HELP)
  )
}
 
enrichment_analysis_UI <- function(id){
  ns <- NS(id)
  heatmap_panel <- tabPanel(
    title = "Heatmap",
    id = "heatmap",
    fluid = T,
    h4("Heatmap"),
    heatmap_sidebar,
    heatmap_main
  )
}

