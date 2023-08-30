volcano_sidebar_panel <- function(ns){
  sidebarPanel(
    id = "sidebar_volcano_plot",
    #########################################
    # VOLCANO
    #########################################
    uiOutput(outputId = ns("sample_annotation_types_cmp_ui")),
    uiOutput(outputId = ns("Groups2Compare_ref_ui")),
    uiOutput(outputId = ns("Groups2Compare_treat_ui")),

    uiOutput(outputId = ns("chooseTest_ui")),
    uiOutput(outputId = ns("chooseSignificanceLevel_ui")),
    uiOutput(outputId = ns("chooseTestCorrection_ui")),
    
    actionButton(
      inputId = ns("Do_Volcano"),
      label = "Do Volcano Plot",
      icon("fas fa-laptop-code")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(outputId = ns("psig_threhsold_ui")),
    uiOutput(outputId = ns("lfc_threshold_ui")),
    switchInput(
      inputId = ns("get_entire_table"),
      label = "show next to LFCs also the raw data that was used to calculate",
      inline = T
    ),
    # Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh UI"
    ))
  )
}


volcano_main_panel <- function(ns){
  mainPanel(
    id = "main_volcano_plot",
    tabsetPanel(
      tabPanel(
        title = "Volcano_Plot",
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          plotlyOutput(outputId = ns("Volcano_Plot_final")) %>% withSpinner(type = 8),
          plotlyOutput(outputId = ns("Volcano_Plot_final_default")),
        ),
        uiOutput(outputId = ns("VOLCANO_anno_tooltip_ui")),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          actionButton(
            inputId = ns("only2Report_Volcano"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            outputId = ns("getR_Code_Volcano"),
            label = "Get underlying R code and data",
            icon = icon("code")
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            outputId = ns("SavePlot_Volcano"),
            label = "Save plot",
            class = "btn-info"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            inputId = ns("file_ext_Volcano"),
            label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"),
            selected = ".png"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("50%", "50%"),
          downloadButton(
            outputId = ns("SaveDE_List"),
            label = "Save interesting entities (all red points)"
          ),
          actionButton(
            inputId = ns("SendDE_Genes2Enrichment"),
            label = "Send DE Genes to enrichment analysis",
            block = F
          )
        )
      ),
      tabPanel(
        title = "Volcano_table",
        DT::dataTableOutput(outputId = ns("Volcano_table_final"))
      ),

    ),
    textAreaInput(
      inputId = ns("NotesVolcano"),
      label = "Notes:",
      placeholder = NOTES_PlACEHOLDER,
      width = "1000px"
    )%>% helper(type = "markdown", content = "TakingNotesMD_help"
    ),
    helpText(NOTES_HELP)
  )
}

volcano_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = "Volcano Plot",
    id = "volcano",
    fluid = T,
    h4("Volcano Plot"),
    volcano_sidebar <- volcano_sidebar_panel(ns),
    volcano_main <- volcano_main_panel(ns),
  )
}