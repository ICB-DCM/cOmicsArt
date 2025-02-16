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
      multiple = F,
      selected = "all"
    ) %>% helper(type = "markdown", content = "Heatmap_Options"),
    conditionalPanel(
      condition = "input.row_selection_options == 'Top K'",
      h5("Further row selection (LFC based)") %>% helper(type = "markdown", content = "Heatmap_FurtherOptions"),
      selectInput(
        inputId = ns("TopK_order"),
        label = "Order based on",
        choices = c(
          "LogFoldChange", "absolute LogFoldChange",
          "LogFoldChange and Significant", "absolute LogFoldChange and Significant"
        ),
        selected = "LogFoldChange"
      ),
      numericInput(
        inputId = ns("TopK"),
        label = "Choose number of top entities to show (order based on p-val (LFC) or rowCount)",
        min = 1,
        step = 1,
        value = 20
      ),
      uiOutput(outputId = ns("sample_annotation_types_cmp_heatmap_ui")),
      uiOutput(outputId = ns("Groups2Compare_ref_heatmap_ui")),
      uiOutput(outputId = ns("Groups2Compare_treat_heatmap_ui")),
      numericInput(
        inputId = ns("psig_threhsold_heatmap"),
        label = "adj. p-value threshold",
        min = 0,
        max = 0.1,
        step = 0.01,
        value = 0.05
      ),
      ns = ns
    ),
    conditionalPanel(
      condition = "input.row_selection_options == 'Select based on Annotation'",
      h5("Further row selection (annotation based)") %>% helper(type = "markdown", content = "Heatmap_RowAnnoBased"),
      uiOutput(outputId = ns("anno_options_heatmap_ui")),
      uiOutput(outputId = ns("row_anno_options_heatmap_ui")),
      ns = ns
    ),
    actionButton(
      inputId = ns("Do_Heatmap"),
      label = "Get Heatmap",
      icon(name = "fas fa-mouse-pointer")
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
      condition = "input.Aesthetics_show == true",
      uiOutput(outputId = ns("anno_options_ui")),
      uiOutput(outputId = ns("row_anno_options_ui")),
      uiOutput(outputId = ns("row_label_options_ui")),
      checkboxInput(
        inputId = ns("cluster_rows"),
        label="Row Clustering?",
        value = TRUE,
        width = "20%"
      ),
      checkboxInput(
        inputId = ns("cluster_cols"),
        label="Column Clustering?",
        value = TRUE,
        width = "20%"
      ),
      ns = ns
    ),
    checkboxInput(
      inputId = ns("rowWiseScaled"),
      label = "row-wise scaling?",
      value = FALSE
    ),
    # hidden Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh"
    )),
  )
}

heatmap_main <- function(ns){
  mainPanel(
    id = "main_heatmap",
    textOutput(outputId = ns("Heatmap_Info"), container = pre),
    div(id = "Heatmap_div",
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("100%"),
        plotOutput(
          outputId = ns("HeatmapPlot")
        )
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
      actionButton(
        inputId = ns("SaveGeneList_Heatmap"),
        label = "Save genes shown in Heatmap for OA within Enrichment Analysis tab",
        icon = icon('seedling'),
        style = "color: #fffff; background-color: #70BF4F47; border-color: #000000"
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
        cellArgs = list(style = "padding: 15px"),
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
