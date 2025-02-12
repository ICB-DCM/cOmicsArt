geneset_panel_UI <- function(
  id
){
  ns <- NS(id)
  id_wo_ns <- gsub(".*-", "", id)

  tabPanel(
    title = id_wo_ns,
    tabsetPanel(
      tabPanel(
        title = paste(id_wo_ns, " Enrichment"),
        plotOutput(outputId = ns("EnrichmentPlot")),
        textOutput(outputId = ns("EnrichmentFailure"), container = pre),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          actionButton(
            inputId = ns("only2Report"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
             outputId = ns("getR_Code"),
             label = "Get underlying R code and data",
             icon = icon("code")
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            outputId = ns("SavePlot"),
            label = "Save plot",
            class = "btn-info"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            inputId = ns("file_ext"),
            label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"),
            selected = ".pdf"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          cellArgs = list(style = "padding: 15px"),
          div(textAreaInput(
            inputId = ns("Notes"),
            label = "Notes:",
            placeholder = NOTES_PlACEHOLDER,
            width = "1000px"
          ) %>% helper(type = "markdown", content = "TakingNotesMD_help"),
          helpText(NOTES_HELP)),
          NULL
        ),
      ),
      tabPanel(
        title = paste(id_wo_ns, "Enrichment Table"),
        DT::dataTableOutput(outputId = ns("EnrichmentTable"))
      )
    )
  )
}


ea_sidebar <- function(ns){
  sidebarPanel(
    id = "sidebar_enrichment_analysis",
    uiOutput(outputId = ns("OrganismChoice_ui")) %>% helper(type = "markdown", content = "EA_Options"),
    radioButtons(
      inputId = ns("ORA_or_GSE"),
      label = "Choose type of Analysis",
      choices = c("GeneSetEnrichment","OverRepresentation_Analysis"),
      selected = "GeneSetEnrichment"
    ),
    uiOutput(outputId = ns("UseBatch_ui")),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'GeneSetEnrichment'", ns("ORA_or_GSE")),
      selectInput(
        inputId = ns("ValueToAttach"),
        label = "Select the metric to sort the genes after",
        choices = list(
          "log fold change (LFC)" = "LFC",
          "absolute LFC" = "LFC_abs",
          "t-statistic value" = "statistic_value"
        )
      ),
      uiOutput(outputId = ns("sample_annotation_types_cmp_GSEA_ui")),
      uiOutput(outputId = ns("Groups2Compare_ref_GSEA_ui")),
      uiOutput(outputId = ns("Groups2Compare_treat_GSEA_ui"))
    ),
    conditionalPanel(
      condition = sprintf("input['%s'] == 'OverRepresentation_Analysis'", ns("ORA_or_GSE")),
      selectInput(
        inputId = ns("GeneSet2Enrich"),
        label = "Choose a gene set to hand over to enrich",
        choices = c(
          "ProvidedGeneSet",
          "heatmap_genes"
        ),
        multiple = F
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] == 'ProvidedGeneSet'", ns("GeneSet2Enrich")),
        renderUI({shiny::fileInput(
          inputId = ns("UploadedGeneSet"),
          label = "Select a file (.csv, 1 column, ENSEMBL, e.g. ENSMUSG....)"
        )})
      ),
      selectInput(
        inputId = ns("UniverseOfGene"),
        label = "Select an Universe for enrichment (default is clusterProfilers default",
        choices = c(
          "default",
          "after_pre_process",
          "before_pre_process"
        ),
        selected = "default"
      )
    ),
    selectInput(
      inputId = ns("GeneSetChoice"),
      label = "Choose sets to do enrichment for",
      choices = names(GENESETS_RESET),
      multiple = T ,
      selected = c(
        "KEGG", "Hallmarks", "GO_CC"
      )
    ) %>% helper(type = "markdown", content = "EA_GeneSets"),
    selectInput(
        inputId = ns("test_correction"),
        label = "Test correction",
        choices = c(
          "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
          "Holm", "Hommel", "Hochberg", "FDR"
        ),
        selected = "Benjamini-Hochberg"
    ),
    actionButton(
      inputId = ns("enrichmentGO"),
      label = "Get Enrichment Analysis",
      icon = icon("fas fa-mouse-pointer")
    ),
    hr(style = "border-top: 1px solid #000000;"),
    uiOutput(outputId = ns("KeggPathwayID_ui")),
    # Button to refresh the UI
    hidden(actionButton(
      inputId = ns("refreshUI"),
      label = "Refresh UI"
    ))
  )
}

ea_main <- function(ns){
  mainPanel(
    textOutput(outputId = ns("EnrichmentInfo"), container = pre),
    div(id = "enrichment_div",
      tabsetPanel(
        id = ns("EnrichmentTabs"),
        geneset_panel_UI(ns("Hallmarks")),
        geneset_panel_UI(ns("C1")),
        geneset_panel_UI(ns("C2")),
        geneset_panel_UI(ns("CGP")),
        geneset_panel_UI(ns("CP")),
        geneset_panel_UI(ns("BIOCARTA")),
        geneset_panel_UI(ns("KEGG")),
        geneset_panel_UI(ns("PID")),
        geneset_panel_UI(ns("REACTOME")),
        geneset_panel_UI(ns("WIKIPATHWAYS")),
        geneset_panel_UI(ns("C3")),
        geneset_panel_UI(ns("MIRDB")),
        geneset_panel_UI(ns("MIR_Legacy")),
        geneset_panel_UI(ns("GTRD")),
        geneset_panel_UI(ns("TFT_Legacy")),
        geneset_panel_UI(ns("C4")),
        geneset_panel_UI(ns("CGN")),
        geneset_panel_UI(ns("CM")),
        geneset_panel_UI(ns("C5")),
        geneset_panel_UI(ns("GO")),
        geneset_panel_UI(ns("GO_BP")),
        geneset_panel_UI(ns("GO_CC")),
        geneset_panel_UI(ns("GO_MF")),
        geneset_panel_UI(ns("HPO")),
        geneset_panel_UI(ns("C6")),
        geneset_panel_UI(ns("C7")),
        geneset_panel_UI(ns("IMMUNESIGDB")),
        geneset_panel_UI(ns("VAX")),
        geneset_panel_UI(ns("C8"))
      )
    )
  )
}

enrichment_analysis_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    title = "Enrichment Analysis",
    fluid = TRUE,
    h4("Enrichment Analysis"),
    #########################################
    # Enrichment
    #########################################
    ea_sidebar(ns),
    ea_main(ns),
  )
}
