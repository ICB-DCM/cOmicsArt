## cOmicsArt UI Definition
##
## Note: All library loading and sourcing has been moved to global.R
## for improved startup performance and proper package loading order.

ui <- shiny::fluidPage(
  # Loading Bars?
  # useWaitress(),
  useWaiter(),
  use_cicerone(),
  # JS to reset input values
  tags$script("
    Shiny.addCustomMessageHandler('resetValue', function(variableName) {
      Shiny.onInputChange(variableName, null);
    });
  "),
  tags$head(
    ##########
    # Styling Setting
    ##########
    includeCSS("www/styles.css"),
    tags$script(src = "javascript_functions.js")
  ),
  ##########
  use_cicerone(),
  #useShinyalert(),
  shinyjs::useShinyjs(),
  ##########
  div(
    style = "display: inline-block; float:right;",
    # Quit App Button
      actionButton(
        inputId = "Quit_App",
        label = "Quit App",
        class = "btn-secondary"
      )
  ),
  div(
    id = "TitleID_normal",
    column(
      width = 1,
      tags$img(src = "Logo_cOmicsArt_clear.png", height = "100%", width = "100%")
    ),
    h1(
      "cOmicsArt",
      style = "background: linear-gradient(to right, #EC0014 8%, #FD8D33 10%, #3897F1 12%, #FFD335 14%, #A208BA 16%, #EF0089 18%, #EC0014 20%);
      background-size: 100%;
      -webkit-background-clip: text; 
      -webkit-text-fill-color: transparent;
      font-weight: bold;"
    )
  ),
  div(
    id = "UsefulLinks",
    splitLayout(
      cellWidths = c("75%", "5%", "20%"),
      DownloadReport_ui("DownloadTestModule"),
      NULL,
      div(
        style = "display: inline-block; float:left;",
        actionLink(
          inputId = "set_cookie",
          label = "Delete 'Skip first help' cookie",
          style = "font-size: 0.9em; color: #555; text-decoration: underline;"
        )
      )
    ),
    splitLayout(
      cellWidths = c("75%", "10%", "15%"),
      tags$a(
        href = "https://icb-dcm.github.io/cOmicsArt/", 
        target = "_blank",
        tagList(
          icon("book"),  # Replace "book" with any other suitable icon
          span("Go To Documentation", style = "margin-left: 5px;")
        ),
        style = "font-size: 18px; color: black; text-decoration: underline;"
      ),
      NULL
    ),
    splitLayout(
      cellWidths = c("75%", "10%", "15%"),
      tags$a(
        href = "https://lea-orga.notion.site/12eab506afb581bf8ecfeeb2bb07c319", 
        target = "_blank",
        tagList(
          icon("comment-dots"),  # Replace "comment-dots" with another icon if desired
          span("Give Us Feedback!", style = "margin-left: 5px;")
        ),
        style = "font-size: 18px; color: black;text-decoration: underline;"
      ),
      NULL
    )
  ),

  tabsetPanel(
    id = "tabsetPanel1",
    ################################################################################
    # Tab Selection w Upload
    ################################################################################
    help_tab_panel,
    data_selection_panel,
    pre_processing_panel,
    ml_classification_UI("ml_classification"),
    sampleCorrelation_UI("sample_correlation"),
    pca_UI("PCA"),
    significance_analysis_UI("SignificanceAnalysis"),
    heatmap_UI("Heatmap"),
    single_gene_visualisation_UI("single_gene_visualisation"),
    enrichment_analysis_UI("EnrichmentAnalysis")
  ),
  hidden(selectInput(
    "element_02",
    label = "AuthorBirthdays",
    choices = c(0, 1, 2),
    selected = if(format(as.POSIXct(Sys.time()), "%d-%m") == "22-11"){
      1  # Lea's Birthday
    } else if (format(as.POSIXct(Sys.time()), "%d-%m") == "19-12"){
      2  # Paul's Birthday
    } else {
      0  # No Birthday
    }
  )),
  conditionalPanel(
    condition = "input.element_02 == 0",
    absolutePanel("Brought to you by Lea Seep & Paul Jonas Jost",
                         bottom = 0, left = 10, fixed = TRUE)
  ),
  conditionalPanel(
    condition = "input.element_02 == 1",
    div(
      id = "foot_birthday",
      absolutePanel(
        "It is Lea's birthday today :)",
        bottom = 0, left = 10, fixed = TRUE,style = "background-color: #a9d96a;"
      )
    )
  ),
  conditionalPanel(
    condition = "input.element_02 == 2",
    div(
      id = "foot_birthday_Paul",
      absolutePanel(
        "It is Paul's birthday today :)",
        bottom = 0, left = 10, fixed = TRUE,style = "background-color: #a9d96a;"
      )
    )
  ),
  # Pannel displaying the current session id
  absolutePanel(
    # text output needs to be defined in server
    textOutput("session_id"),
    bottom = 0, right = 10, fixed = TRUE
  )
)
