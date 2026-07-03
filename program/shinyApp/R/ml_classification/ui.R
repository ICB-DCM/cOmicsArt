# ML Classification UI Module
# Phase 2: k-means implementation

ml_classification_sidebar_panel <- function(ns){
  sidebarPanel(
    id = "sidebar_ml_classification",
    h4("ML Classification Settings") %>% helper(type = "markdown", content = "ML_Classification_Overview"),

    # Refresh UI button (hidden, triggered programmatically)
    actionButton(
      inputId = ns("refreshUI"),
      label = "",
      style = "display: none;"
    ),

    # Method type selection
    radioButtons(
      inputId = ns("ml_method_type"),
      label = "Analysis Type:",
      choices = c("Unsupervised" = "unsupervised",
                  "Supervised" = "supervised"),
      selected = "unsupervised"
    ),

    # Conditional panel for unsupervised
    conditionalPanel(
      condition = "input.ml_method_type == 'unsupervised'",
      ns = ns,

      h5("Unsupervised Method Options") %>% helper(type = "markdown", content = "ML_Classification_Unsupervised"),

      selectInput(
        inputId = ns("unsupervised_method"),
        label = "Method:",
        choices = c("k-means" = "kmeans"),
        selected = "kmeans"
      ),

      numericInput(
        inputId = ns("k_clusters"),
        label = "Number of clusters (k):",
        value = 3,
        min = 2,
        max = 10,
        step = 1
      ),

      checkboxInput(
        inputId = ns("filter_genes_unsupervised"),
        label = "Filter to top variable genes",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.filter_genes_unsupervised",
        ns = ns,
        numericInput(
          inputId = ns("n_genes_unsupervised"),
          label = "Number of genes:",
          value = 500,
          min = 50,
          max = 5000,
          step = 50
        )
      ),

      # Optional: condition column for overlay visualization
      uiOutput(outputId = ns("condition_overlay_ui")),

      actionButton(
        inputId = ns("run_clustering"),
        label = "Run Clustering",
        class = "btn-primary"
      )
    ),

    # Supervised panel - Phase 3
    conditionalPanel(
      condition = "input.ml_method_type == 'supervised'",
      ns = ns,

      h5("Supervised Method Options") %>% helper(type = "markdown", content = "ML_Classification_Supervised"),

      selectInput(
        inputId = ns("supervised_method"),
        label = "Method:",
        choices = c("SVM" = "svm"),
        selected = "svm"
      ),

      uiOutput(outputId = ns("condition_column_supervised_ui")),

      helpText("Select the categorical variable to predict (e.g., 'treatment', 'cell_type')"),

      checkboxInput(
        inputId = ns("filter_genes_supervised"),
        label = "Filter to top variable genes",
        value = FALSE
      ),

      conditionalPanel(
        condition = "input.filter_genes_supervised",
        ns = ns,
        numericInput(
          inputId = ns("n_genes_supervised"),
          label = "Number of genes:",
          value = 500,
          min = 50,
          max = 5000,
          step = 50
        )
      ),

      actionButton(
        inputId = ns("run_classification"),
        label = "Run Classification",
        class = "btn-primary"
      )
    )
  )
}

ml_classification_main_panel <- function(ns){
  mainPanel(
    id = "mainPanel_ml_classification",

    # Unsupervised results
    conditionalPanel(
      condition = "input.ml_method_type == 'unsupervised'",
      ns = ns,

      h4("Clustering Results"),

      plotOutput(ns("cluster_plot"), height = "600px"),

      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = ns("only2Report_cluster"),
          label = "Send only to Report"
        )
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = ns("getR_Code_cluster"),
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = ns("SavePlot_cluster"),
          label = "Save plot",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        selectInput(
          inputId = ns("file_ext_cluster"),
          label = "Select file type:",
          choices = c(".png", ".pdf", ".svg"),
          selected = ".png"
        )
      ),

      hr(),

      h5("Cluster Assignments"),
      DT::dataTableOutput(ns("cluster_table"))
    ),

    # Supervised results
    conditionalPanel(
      condition = "input.ml_method_type == 'supervised'",
      ns = ns,

      h4("Classification Results"),

      # Accuracy display with disclaimer
      wellPanel(
        h5("Training Accuracy"),
        textOutput(ns("svm_accuracy_text")),
        helpText(
          "Note: This is training accuracy only (no validation). ",
          "Results are exploratory. Cross-validation coming in future release."
        )
      ),

      plotOutput(ns("svm_plot"), height = "600px"),

      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        actionButton(
          inputId = ns("only2Report_svm"),
          label = "Send only to Report"
        )
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = ns("getR_Code_svm"),
          label = "Get underlying R code and data",
          icon = icon("code")
        )
      ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        downloadButton(
          outputId = ns("SavePlot_svm"),
          label = "Save plot",
          class = "btn-info"
        )
      ),
      splitLayout(
        style = "border: 1px solid silver:",
        cellWidths = c("70%", "30%"),
        NULL,
        selectInput(
          inputId = ns("file_ext_svm"),
          label = "Select file type:",
          choices = c(".png", ".pdf", ".svg"),
          selected = ".png"
        )
      ),

      hr(),

      h5("Predictions"),
      DT::dataTableOutput(ns("svm_predictions_table"))
    )
  )
}

ml_classification_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = tagList(tags$span("3. ML Classification")),
    value = "ML Classification",
    id = "ml_classification",
    fluid = TRUE,

    h4("ML Classification"),
    ml_classification_sidebar_panel(ns),
    ml_classification_main_panel(ns)
  )
}