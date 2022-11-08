geneset_panel_UI <- function(
  id
){
  ns <- NS(id)
  id_wo_ns <- gsub(".*-", "", id)

  tabPanel(
    title = id_wo_ns,
    # textOutput("Info", container = pre),
    tabsetPanel(
      tabPanel(
        title=paste(id_wo_ns, " Enrichment"),
        plotOutput(outputId = ns("EnrichmentPlot")),
        splitLayout(
          style = "border: 1px solid silver:",
          cellWidths = c("70%", "30%"),
          NULL,
          actionButton(
            inputId = ns("only2Report"),
            label = "Send only to Report",
            class = "btn-info"
          )
        ),
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
        textAreaInput(
          inputId=ns("Notes"),
          label="Notes:",
          placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ",
          width = "1000px"
        )%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
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
    uiOutput(outputId = ns("OrganismChoice_ui")),
    uiOutput(outputId = ns("ORA_or_GSE_ui")),
    uiOutput(outputId = ns("ValueToAttach_ui")),
    uiOutput(outputId = ns("sample_annotation_types_cmp_GSEA_ui")),
    uiOutput(outputId = ns("Groups2Compare_ref_GSEA_ui")),
    uiOutput(outputId = ns("Groups2Compare_treat_GSEA_ui")),
    uiOutput(outputId = ns("psig_threhsold_GSEA_ui")),
    uiOutput(outputId = ns("GeneSetChoice_ui")),
    uiOutput(outputId = ns("GeneSet2Enrich_ui")),
    uiOutput(outputId = ns("UploadedGeneSet_ui")),
    uiOutput(outputId = ns("UniverseOfGene_ui")),
    actionButton(
      inputId = ns("enrichmentGO"),
      label = "Do enrichment analysis"
    ),
    radioButtons(
      inputId = ns("ontologyForGO"),
      label = "Choose ontology for GO enrichment",
      choices = c("BP","MF","CC","ALL"),
      selected = "BP"
    ),
    helpText("Note: ALL sometime fails due to cryptic reasons. You will get notified. If this happens to you please try out the indiviudal ontologies to check whether enriched term are found. "),
    hr(style = "border-top: 1px solid #858585;"),
    uiOutput(outputId = ns("KeggPathwayID_ui"))
  )
}

ea_main <- function(ns){
  mainPanel(
    textOutput(outputId = ns("EnrichmentInfo"), container = pre),
    tabsetPanel(
      id = ns("EnrichmentTabs"),
      geneset_panel_UI(ns("KEGG")),
      geneset_panel_UI(ns("GO")),
      geneset_panel_UI(ns("REACTOME")),
      geneset_panel_UI(ns("Hallmarks")),
      geneset_panel_UI(ns("C1")),
      geneset_panel_UI(ns("C2")),
      geneset_panel_UI(ns("C3")),
      geneset_panel_UI(ns("C4")),
      geneset_panel_UI(ns("C5")),
      geneset_panel_UI(ns("C6")),
      geneset_panel_UI(ns("C7")),
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
        imageOutput(outputId = ns("KeggPathwayOutput_img")) %>% withSpinner(type = 8)
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
    h4("NOTE THAT THIS ONLY MAKES SENSE FOR TRANSCRIPTOMICS DATA AT THE MOMENT!"),
    enrichment_analysis_sidebar <- ea_sidebar(ns),
    enrichment_analysis_main <- ea_main(ns),
  )
}
