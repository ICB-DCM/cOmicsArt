geneset_panel_UI <- function(
  id
){
  ns <- NS(id)
  id_wo_ns <- gsub(".*-", "", id)

  tabPanel(
    title = id_wo_ns,
    #textOutput(outputId = ns("EnrichmentInfo"), container = pre),
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
          cellArgs = list(style = "padding: 5px"),
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
    uiOutput(outputId = ns("ValueToAttach_ui")),
    uiOutput(outputId = ns("sample_annotation_types_cmp_GSEA_ui")),
    uiOutput(outputId = ns("Groups2Compare_ref_GSEA_ui")),
    uiOutput(outputId = ns("Groups2Compare_treat_GSEA_ui")),
    uiOutput(outputId = ns("psig_threhsold_GSEA_ui")),
    uiOutput(outputId = ns("GeneSetChoice_ui")) %>% helper(type = "markdown", content = "EA_GeneSets"),
    selectInput(
        inputId = ns("test_correction"),
        label = "Test correction",
        choices = c(
          "None", "Bonferroni", "Benjamini-Hochberg", "Benjamini Yekutieli",
          "Holm", "Hommel", "Hochberg", "FDR"
        ),
        selected = "Benjamini-Hochberg"
    ),
    uiOutput(outputId = ns("GeneSet2Enrich_ui")),
    uiOutput(outputId = ns("UploadedGeneSet_ui")),
    uiOutput(outputId = ns("UniverseOfGene_ui")),
    actionButton(
      inputId = ns("enrichmentGO"),
      label = "Get Enrichment Analysis"
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
        geneset_panel_UI(ns("C8")),
        tabPanel(
          title = "KeggPathwayOutput",
          helpText("Specificy on the left which pathway (all sig. enriched) to display in picture-format"),
          actionButton(
            inputId = ns("OverlayOnPathway"),
            label = "Show overlay on Pathway"
          ),
          selectInput(
            inputId = ns("plotOnTopOption"),
            label = "Specifiy the what the colored overlay should indicate",
            choices = c("LFC", "presence"),
            selected = "presence"
          ),
          uiOutput(outputId = ns("sample_anno_types_KEGG_ui")),
          uiOutput(outputId = ns("ComparisonOptionsCRTL_ui")),
          uiOutput(outputId = ns("ComparisonOptionsCOMP_ui")),
          uiOutput(outputId = ns("psig_KEGG_ui")),
          sliderInput(
            inputId = ns("imageWidth"),
            label = "Adjust Width",
            min = 400, max = 1500, step = 20, value = 1000
          ),
          sliderInput(
            inputId = ns("imageHeight"),
            label = "Adjust Height",
            min = 400, max = 1500, step = 20, value = 640
          ),
          imageOutput(outputId = ns("KeggPathwayOutput_img"))
        )
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
