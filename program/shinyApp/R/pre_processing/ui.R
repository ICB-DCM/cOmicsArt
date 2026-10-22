### Pre-processing module UI
# The "2. Pre-processing" tab: the sidebar with the options and "Get
# Pre-Processing", the main panel with the summary, console output, plots,
# downloads and notes.

preprocessing_sidebar_ui <- function(id){
  ns <- NS(id)
  sidebarPanel(
    id = ns("sidebar_pre_processing"),
    #########################################
    # Do Center & scaling + potential other pre-processing stuff
    #########################################
    # this could be enhanced with personalized procedures
    # First dropdown: Choose Processing Type
    selectInput(
      inputId = ns("processing_type"),
      label = "Choose Processing Type",
      choices = c("No pre-processing",
                  "Filtering",
                  "Omic-Specific",
                  "Log-Based",
                  "Miscellaneous"),
      selected = NULL,
      multiple = FALSE
    ) %>% helper(type = "markdown", content = "PreProcessing_Procedures"),

    # Second dropdown: Options based on Processing Type
    uiOutput(outputId = ns("dynamic_options_ui")),

    # Additional UI elements based on the selected option
    uiOutput(outputId = ns("additional_inputs_filter_ui")),


    uiOutput(outputId = ns("formula_sub_ui")),
    uiOutput(outputId = ns("batch_effect_ui")) %>% helper(type = "markdown", content = "PreProcessing_Batch"),
    actionButton(
      inputId = ns("get_preprocessing"),
      label = "Get Pre-Processing",
      icon(name = "fas fa-mouse-pointer")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(ns("violin_color_ui"))
  )
}


preprocessing_main_ui <- function(id){
  ns <- NS(id)
  mainPanel(
    id = ns("mainpanel_pre_processing"),
    # Statistics to the data
    htmlOutput(outputId = ns("summary"), container = pre),
    div(
      id = ns("data_summary"),
      HTML(text = "<br>"),
      fluidRow(column(5,
                      actionButton(
                        ns("start_discovering"), "Start Discovering",
                        width = "100%",
                        icon = icon("fas fa-mouse-pointer"),
                        style = "
            background-color: #3897F147;
            color: black;
            border: 2px solid darkgrey;
            font-size: 15px;
            font-weight: bold;
            box-shadow: 3px 3px 5px rgba(255, 0, 0, 0.2);
            padding: 5px 5px;
            border-radius: 10px;"
                      )
      )),
      actionButton(
        inputId = ns("console_toggle_button"),
        label = "(show/hide) Further Console Output",
        icon = icon('plus'),
        style = "color: #000000; background-color: transparent; border-color: transparent"
      ),
      div(
        id = ns("console_toggle"),
        style = "display: none;",
        class = "ConsoleOutput_ui",
        uiOutput(ns("console_display")),
        hr(style = "border-top: 1px solid #858585;")
      ),
      fluidRow(
        column(
          6,
          h4("Raw Data"),
          plotOutput(ns("raw_violin_plot"))
        ),
        column(
          6,
          h4("Pre-processed Data"),
          plotOutput(ns("preprocessed_violin_plot")),
        )
      ),
      h4("Mean and Standard Deviation of preprocessed data"),
      plotOutput(ns("mean_sd_plot")),
      fluidRow(column(4, ""), column(
        4,
        h5("Mean and SD Plot Download"),
        div(
          style = "margin-bottom: 5px;",
          actionButton(
            inputId = ns("report_mean_sd"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
        downloadButton(
          outputId = ns("rcode_mean_sd"),
          label = "Get underlying R code and data",
          icon = icon(name = "code")
        ),
        div(
          style = "margin-top: 5px; margin-bottom: 5px;",
          downloadButton(
            outputId = ns("save_mean_sd"),
            label = "Save plot",
            class = "btn-info"
          ),
          clipboard_button(
            inputId = ns("copy_mean_sd"),
            plot_id = ns("mean_sd_plot")
          )
        ),
        radioGroupButtons(
          inputId = ns("mean_sd_file_type"),
          label = "File Type:",
          choices = c(".png", ".tiff", ".svg", ".pdf"),
          selected = ".png"
        )
      ), column(
        4,
        h5("Violin Plot Download"),
        div(
          style = "margin-bottom: 5px;",
          actionButton(
            inputId = ns("report_violin"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
        downloadButton(
          outputId = ns("rcode_violin"),
          label = "Get underlying R code and data",
          icon = icon(name = "code")
        ),
        div(
          style = "margin-top: 5px; margin-bottom: 5px;",
          downloadButton(
            outputId = ns("save_violin"),
            label = "Save both plots",
            class = "btn-info"
          ),
          clipboard_pair_button(
            inputId = ns("copy_violin"),
            first_plot_id = ns("raw_violin_plot"),
            second_plot_id = ns("preprocessed_violin_plot")
          )
        ),
        radioGroupButtons(
          inputId = ns("violin_file_type"),
          label = "File Type:",
          choices = c(".png", ".tiff", ".svg", ".pdf"),
          selected = ".png"
        )
      )),
      splitLayout(
        style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
        cellArgs = list(style = "padding: 15px"),
        div(textAreaInput(
          inputId = ns("notes"),
          label = "Notes:",
          placeholder = NOTES_PlACEHOLDER,
          width = "1000px"
        ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText(NOTES_HELP)),
        NULL
      )
    )
  )
}



preprocessing_ui <- function(id){
  ns <- NS(id)
  tabPanel(
    title = tagList(tags$span("2. Pre-processing")),
    value = "Pre-processing",
    id = ns("pre_processing_panel"),
    fluid = T,
    h4("Data Pre-processing"),
    preprocessing_sidebar_ui(id),
    preprocessing_main_ui(id)
  )
}
