volcano_plot_sidebar <- sidebarPanel(
  id = "sidebar_volcano_plot",
  #########################################
  # VOLCANO
  #########################################
  uiOutput("sample_annotation_types_cmp_ui"),
  uiOutput("Groups2Compare_ref_ui"),
  uiOutput("Groups2Compare_treat_ui"),
  actionButton(
    inputId = "Do_Volcano",
    label = "Do Volcano Plot",
    icon("fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  uiOutput("psig_threhsold_ui"),
  uiOutput("lfc_threshold_ui"),
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
      "Volcano_Plot", plotlyOutput("Volcano_Plot_final") %>% withSpinner(type = 8),
      uiOutput("VOLCANO_anno_tooltip_ui"),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = "only2Report_Volcano",
          label = "Send only to Report",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "getR_Code_Volcano",
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
        outputId = "SavePlot_Volcano",
        label = "Save plot",
        class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        radioGroupButtons(
          inputId = "file_ext_Volcano",
          label = "File Type:",
          choices = c(".png", ".tiff", ".pdf"), 
          selected = ".png"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
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
    tabPanel("Volcano_table", DT::dataTableOutput("Volcano_table_final")),

  ),
  textAreaInput(
    inputId="NotesVolcano",
    label="Notes:",
    placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"
  ),
  helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
)


volcano_plot_panel <- tabPanel(
  "Volcano Plot",
  id = "volcano",
  fluid = T,
  h4("Volcano Plot"),
  volcano_plot_sidebar,
  volcano_plot_main
)