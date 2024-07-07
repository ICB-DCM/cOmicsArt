heatmap_sidebar<- function(ns){
  sidebarPanel(
    id = "sidebar_heatmap",
    #########################################
    # Heatmap
    #########################################
    uiOutput(outputId = ns("UseBatch_ui")),
    selectInput(
      inputId = ns("row_selection_options"),
      label = "Select Entities to show",
      choices = c("all", "Select based on Annotation", "Top K"),
    # TODO: needs to be incoporated or deleted
    # "TopK","significant_LFC","LFC_onlySig","rowAnno_based"),
      multiple = F,
      selected = "all"
    ) %>% helper(type = "markdown", content = "Heatmap_Options"),
    uiOutput(outputId = ns("LFC_toHeatmap_ui")),
    h5("Further row selection (LFC based)") %>% helper(type = "markdown", content = "Heatmap_FurtherOptions"),
    uiOutput(outputId = ns("TopK_ui")),
    switchInput(
      inputId = ns("Selection_show_LFC"),
      label = "show options (LFC-related)",
      inline = T,
      size = "mini"
    ),
    uiOutput(outputId = ns("sample_annotation_types_cmp_heatmap_ui")),
    uiOutput(outputId = ns("Groups2Compare_ref_heatmap_ui")),
    uiOutput(outputId = ns("Groups2Compare_treat_heatmap_ui")),
    uiOutput(outputId = ns("psig_threhsold_heatmap_ui")),
    h5("Further row selection (annotation based)") %>% helper(type = "markdown", content = "Heatmap_RowAnnoBased"),
    helpText("Note: This only shows options if 'rowAnno_based' is selected for 'Row selection' (top of the sidebar)"),
    switchInput(
      inputId = ns("Selection_show_annoBased"),
      label = "show options (annotation-related)",
      inline = T,
      size = "mini",
      value = F
    ),
    uiOutput(outputId = ns("anno_options_heatmap_ui")),
    uiOutput(outputId = ns("row_anno_options_heatmap_ui")),
    actionButton(
      inputId = ns("Do_Heatmap"),
      label = "Get Heatmap",
      icon(name = "fas fa-laptop-code")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    h5("Aesthetics") %>% helper(type = "markdown", content = "Heatmap_Aesthetics"),
    switchInput(
      inputId = ns("Aesthetics_show"),
      label = "show options",
      size = "mini", 
      value = T
    ),
    conditionalPanel(
      condition = "input.Aesthetics_show",
      uiOutput(outputId = ns("anno_options_ui")),
      uiOutput(outputId = ns("row_anno_options_ui")),
      uiOutput(outputId = ns("row_label_options_ui")),
      uiOutput(outputId = ns("cluster_cols_ui")),
      uiOutput(outputId = ns("cluster_rows_ui"))
    )
  )
}

heatmap_main <- function(ns){
  mainPanel(
    id = "main_heatmap",
    textOutput(outputId = ns("Heatmap_Info"), container = pre),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("100%"),
      plotOutput(
        outputId = ns("HeatmapPlot")
      ) %>% withSpinner(type=8,color = getOption("spinner.color", default = "#b8cee0"))
    ),
    textOutput(outputId = ns("Options_selected_out_3"), container = pre),
    uiOutput(outputId = ns("row_label_options_ui")),
    numericInput(
      inputId = ns("row_label_no"),
      label = "Threshold upon which explicit labels are shown",
      min = 0, 
      step = 1, 
      value = 25
    ),
    downloadButton(
      outputId = ns("SaveGeneList_Heatmap"),
      label = "Save genes shown in Heatmap as list"
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      actionButton(
        inputId = ns("only2Report_Heatmap"),
        label = "Send only to Report",
        class = "btn-info"
      )
    ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("getR_Code_Heatmap"),
        label = "Get underlying R code and data",
        icon = icon(name = "code")
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      downloadButton(
        outputId = ns("SavePlot_Heatmap"),
        label = "Save plot",
        class = "btn-info"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
      NULL,
      radioGroupButtons(
        inputId = ns("file_ext_Heatmap"), 
        label = "File Type:",
        choices = c(".png", ".tiff", ".pdf"), 
        selected = ".png"
      )
    ),
    splitLayout(
      style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
      cellArgs = list(style = "padding: 5px"),
      div(textAreaInput(
        inputId = ns("NotesHeatmap"),
        label = "Notes:",
        placeholder = NOTES_PlACEHOLDER,
        width = "1000px"
      ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
      helpText(NOTES_HELP)),
      NULL
    ),
  )
}
 
heatmap_UI <- function(id){
  ns <- NS(id)
  heatmap_panel <- tabPanel(
    title = "Heatmap",
    id = "heatmap",
    fluid = T,
    h4("Heatmap"),
    heatmap_sidebar(ns),
    heatmap_main(ns)
  )
}
