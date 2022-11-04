pca_sidebar_panel <- sidebarPanel(
  id = "sidebar_pca",
  #########################################
  # explorative analysis
  # PCA
  #########################################
  h4("Explorative Analysis"),
  actionButton(
  inputId = "Do_PCA", 
  label = "Perform PCA", 
  icon("fas fa-laptop-code")
  ),
  hr(style = "border-top: 1px solid #000000;"),
  uiOutput("coloring_options_ui"),
  uiOutput("x_axis_selection_ui"),
  uiOutput("y_axis_selection_ui"),
  uiOutput("Show_loadings_ui"),
  helpText("Note: if you would like to change the annotation of the indicated loading vectors please select an option the the tab Loadings"),
  hr(style = "border-top: 1px dashed #000000;")

)


pca_main_panel <- mainPanel(
  id = "mainpanel_pca",
  tabsetPanel(
    type = "pills",
    tabPanel(
      title = "PCA",
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        plotlyOutput("PCA_plot") %>% withSpinner(type = 8),
        textOutput(outputId = "PCA_plot_Options_selected", container = pre)
      ),
      uiOutput(outputId = "PCA_anno_tooltip_ui"),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
        inputId = "only2Report_pca",
        label = "Send only to Report"
         )
      ),
      splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "getR_Code_PCA",
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "SavePlot_pos1",
          label = "Save plot",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        NULL,
        radioGroupButtons(
          inputId = "file_ext_plot1",
          label = "File Type:",
          choices = c(".png", ".tiff", ".pdf"),
          selected = ".png"
        )
      ),
      textAreaInput(
        inputId="NotesPCA",
        label="Notes:",
        placeholder=NOTES_PlACEHOLDER,
        width = "1000px"
      )%>% helper(type = "markdown", content = "TakingNotesMD_help"),
      helpText(NOTES_HELP)
    ),
    tabPanel(
      title = "PCA_Loadings",
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
        plotOutput(outputId = "PCA_Loadings_plot") %>% withSpinner(type = 8),
        textOutput(outputId = "Loadings_plot_Options_selected_out", container = pre)
      ),
      uiOutput(outputId = "EntitieAnno_Loadings_ui"),
      sliderInput(
        inputId = "topSlider",
        label = "Top k positive Loadings",
        min = 1, 
        max = 25, 
        value = 10, 
        step = 1
      ),
      sliderInput(
        inputId = "bottomSlider",
        label = "Top k negative Loadings",
        min = 1, 
        max = 25,
        value = 10, 
        step = 1
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = "only2Report_Loadings",
          label = "Send only to Report",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "getR_Code_Loadings",
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
        outputId = "SavePlot_Loadings", 
        label = "Save plot",
        class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        radioGroupButtons(
          inputId = "file_ext_Loadings", 
          label = "File Type:",
          choices = c(".png", ".tiff", ".pdf"), 
          selected = ".png"
        )
      )
    ),
    tabPanel(
      title = "Scree_Plot",
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        plotlyOutput("Scree_Plot"),
        textOutput(outputId = "Scree_Plot_Options_selected_out", container = pre)
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = "only2Report_Scree_Plot",
          label = "Send only to Report",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "getR_Code_Scree_Plot",
          label = "Get underlying R code and data",
          icon = icon(name = "code")
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = "SavePlot_Scree",
          label = "Save plot",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        radioGroupButtons(
          inputId = "file_ext_Scree", 
          label = "File Type:",
          choices = c(".png", ".tiff", ".pdf"),
          selected = ".png"
        )
      )
    )
  )
)


pca_panel <- tabPanel(
  title = "PCA",  # can be renamed after UMAP is added
  id = "pca",
  fluid = T,
  h4("PCA"),
  pca_sidebar_panel,
  pca_main_panel
)