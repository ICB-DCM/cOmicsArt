pca_sidebar_panel <- function(ns){
  sidebarPanel(
    id = "sidebar_pca",
    #########################################
    # explorative analysis
    # PCA
    #########################################
    h4("Explorative Analysis") %>% helper(type = "markdown", content = "PCA_Choices"),
    uiOutput(outputId = ns("UseBatch_ui")),
    ### data selection
    switchInput(
      inputId = ns("data_selection_pca"),
      label = "Select Data",
      inline = T,
      size = "mini"
    ),
    uiOutput(outputId = ns("SampleAnnotationTypes_pca_ui")),
    uiOutput(outputId = ns("sample_selection_pca_ui")),
    # Scale data to unit variance y/n
    radioButtons(
      inputId = ns("scale_data"),
      label = "Scale data to unit variance?",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE
    ),
    ### start pca ---
    actionButton(
      inputId = ns("Do_PCA"),
      label = "Get PCA",
      icon("fas fa-mouse-pointer")
    ),
    ### further visualizations
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(outputId = ns("coloring_options_ui")),
    radioButtons(
      inputId = ns("x_axis_selection"),
      label = "PC for x-Axis",
      choices = c("PC1","PC2", "PC3", "PC4"),
      # direction = "horizontal",
      selected = "PC1"
    ),
    radioButtons(
      inputId = ns("y_axis_selection"),
      label = "PC for y-Axis",
      choices = c("PC1","PC2", "PC3", "PC4"),
      # direction = "horizontal",
      selected = "PC2"
    ),
    radioButtons(
      inputId = ns("Show_loadings"),
      label = "Plot Loadings on top? (currently top 5)",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = FALSE
    ),
    radioButtons(
      inputId = ns("Plot_ellipses"),
      label = "Plot Ellipses indicating a 95% confidence interval?",
      choices = c("Yes" = TRUE, "No" = FALSE),
      selected = TRUE
    ),
    uiOutput(outputId = ns("entitie_anno_ui")),
    hr(style = "border-top: 1px dashed #000000;"),
    # hidden Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh"
    ))
  )
}


pca_main_panel <- function(ns){
  mainPanel(
    id = "mainpanel_pca",
    textOutput(outputId = ns("PCA_Info"), container = pre),
      div(id = "PCA_main_panel_div",
      tabsetPanel(
        id = "plot_panels_pca",
        type = "pills",
        tabPanel(
          title = "PCA_plot",
          plotlyOutput(outputId = ns("PCA_plot")),
          uiOutput(outputId = ns("PCA_anno_tooltip_ui")),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            actionButton(
              inputId = ns("only2Report_pca"),
              label = "Send only to Report"
            )
          ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("getR_Code_PCA"),
              label = "Get underlying R code and data",
              icon = icon("code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("SavePlot_pos1"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            radioGroupButtons(
              inputId = ns("file_ext_plot1"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".svg", ".pdf"),
              selected = ".png"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
            cellArgs = list(style = "padding: 15px"),
            div(textAreaInput(
              inputId = ns("NotesPCA"),
              label = "Notes:",
              placeholder = NOTES_PlACEHOLDER,
              width = "1000px"
            ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText(NOTES_HELP)),
            NULL
          ),
        ),
        tabPanel(
          title = "PCA_Loadings",
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            plotlyOutput(outputId = ns("PCA_Loadings_plot")),
            textOutput(outputId = ns("Loadings_plot_Options_selected_out"), container = pre)
          ),
          sliderInput(
            inputId = ns("topSlider"),
            label = "Top k positive Loadings",
            min = 1,
            max = 25,
            value = 10,
            step = 1
          ),
          sliderInput(
            inputId = ns("bottomSlider"),
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
              inputId = ns("only2Report_Loadings"),
              label = "Send only to Report",
              class = "btn-info"
            )
          ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("getR_Code_Loadings"),
              label = "Get underlying R code and data",
              icon = icon("code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("SavePlot_Loadings"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            radioGroupButtons(
              inputId = ns("file_ext_Loadings"),
              label = "File Type:",
choices = c(".png", ".tiff", ".svg", ".pdf"),
              selected = ".png"
            )
          )
        ),
        tabPanel(
          title = "PCA_Loadings_matrix",
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            plotOutput(outputId = ns("PCA_Loadings_matrix_plot")),
            NULL
            #textOutput(outputId = ns("Loadings_plot_Options_selected_out"), container = pre)
          ),
          uiOutput(outputId = ns("nPCAs_to_look_at_ui")),
          sliderInput(
            inputId = ns("filterValue"),
            label = "absolute Loading threshold to filter entities with low impact",
            min = 0,
            max = 1, # renderui?
            value = 0.3,
            step = 0.01
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            actionButton(
              inputId = ns("only2Report_Loadings_matrix"),
              label = "Send only to Report",
              class = "btn-info"
            )
          ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("getR_Code_Loadings_matrix"),
              label = "Get underlying R code and data",
              icon = icon("code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("SavePlot_Loadings_matrix"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            radioGroupButtons(
              inputId = ns("file_ext_Loadings_matrix"),
              label = "File Type:",
choices = c(".png", ".tiff", ".svg", ".pdf"),
              selected = ".png"
            )
          )
        ),
        tabPanel(
          title = "Scree_Plot",
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            plotlyOutput(outputId = ns("Scree_Plot")),
            textOutput(outputId = ns("Scree_Plot_Options_selected_out"), container = pre)
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            actionButton(
              inputId = ns("only2Report_Scree_Plot"),
              label = "Send only to Report",
              class = "btn-info"
            )
          ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("getR_Code_Scree_Plot"),
              label = "Get underlying R code and data",
              icon = icon(name = "code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            downloadButton(
              outputId = ns("SavePlot_Scree"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("70%", "30%"),
            NULL,
            radioGroupButtons(
              inputId = ns("file_ext_Scree"),
              label = "File Type:",
choices = c(".png", ".tiff", ".svg", ".pdf"),
              selected = ".png"
            )
          )
        )
      )
    )
  )
}


pca_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = tagList(tags$span("4. PCA")), 
    value = "PCA",
    id = "pca",
    fluid = T,
    h4("PCA"),
    pca_sidebar_panel(ns),
    pca_main_panel(ns),
  )
}