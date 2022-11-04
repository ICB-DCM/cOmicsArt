volcano_plot_sidebar <- sidebarPanel(
  id = "sidebar_volcano_plot",
  #########################################
  # VOLCANO
  #########################################
  uiOutput(outputId = "sample_annotation_types_cmp_ui"),
  uiOutput(outputId = "Groups2Compare_ref_ui"),
  uiOutput(outputId = "Groups2Compare_treat_ui"),
  actionButton(
    inputId = "Do_Volcano",
    label = "Do Volcano Plot",
    icon("fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  uiOutput(outputId = "psig_threhsold_ui"),
  uiOutput(outputId = "lfc_threshold_ui"),
  switchInput(
    inputId = "get_entire_table",
    label = "show next to LFCs also the raw data that was used to calculate",
    inline = T
  )
)


volcano_plot_main <- mainPanel(
  id = "main_volcano_plot",
  tabsetPanel(
    tabPanel(
      title = "Volcano_Plot",
      plotlyOutput("Volcano_Plot_final") %>% withSpinner(type = 8),
      uiOutput(outputId = "VOLCANO_anno_tooltip_ui"),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = "only2Report_Volcano",
          label = "Send only to Report",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "getR_Code_Volcano",
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "SavePlot_Volcano",
          label = "Save plot",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        radioGroupButtons(
          inputId = "file_ext_Volcano",
          label = "File Type:",
          choices = c(".png", ".tiff", ".pdf"), 
          selected = ".png"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("50%", "50%"),
        downloadButton(
          outputId = "SaveDE_List",
          label = "Save interesting entities (all red points)"
        ),
        actionButton(
          inputId = "SendDE_Genes2Enrichment",
          label = "Send DE Genes to enrichment analysis",
          block = F
        )
      )
    ),
    tabPanel(
      title = "Volcano_table",
      DT::dataTableOutput("Volcano_table_final")
    ),

  ),
  textAreaInput(
    inputId="NotesVolcano",
    label="Notes:",
    placeholder=NOTES_PlACEHOLDER,
    width = "1000px"
  )%>% helper(type = "markdown", content = "TakingNotesMD_help"
  ),
  helpText(NOTES_HELP)
)


volcano_plot_panel <- tabPanel(
  title = "Volcano Plot",
  id = "volcano",
  fluid = T,
  h4("Volcano Plot"),
  volcano_plot_sidebar,
  volcano_plot_main
)