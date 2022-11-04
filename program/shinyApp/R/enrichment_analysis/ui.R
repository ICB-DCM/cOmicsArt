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


ea_sidebar <- function(){
  sidebarPanel(
    uiOutput(outputId = "OrganismChoice_ui"),
    uiOutput(outputId = "ORA_or_GSE_ui"),
    uiOutput(outputId = "ValueToAttach_ui"),
    uiOutput(outputId = "sample_annotation_types_cmp_GSEA_ui"),
    uiOutput(outputId = "Groups2Compare_ref_GSEA_ui"),
    uiOutput(outputId = "Groups2Compare_treat_GSEA_ui"),
    uiOutput(outputId = "psig_threhsold_GSEA_ui"),
    uiOutput(outputId = "GeneSet2Enrich_ui"),
    uiOutput(outputId = "UploadedGeneSet_ui"),
    uiOutput(outputId = "UniverseOfGene_ui"),
    actionButton(
      inputId = "enrichmentGO",
      label = "Do enrichment analysis"
    ),
    radioButtons(
      inputId = "ontologyForGO",
      label = "Choose ontology for GO enrichment",
      choices = c("BP","MF","CC","ALL"),
      selected = "BP"
    ),
    helpText("Note: ALL sometime fails due to cryptic reasons. You will get notified. If this happens to you please try out the indiviudal ontologies to check whether enriched term are found. "),
    hr(style = "border-top: 1px solid #858585;"),
    uiOutput(outputId = "KeggPathwayID_ui")
  )
}

ea_main <- function(ns){
  mainPanel(
    textOutput(outputId = "EnrichmentInfo", container = pre),
    tabsetPanel(
      geneset_panel_UI(ns("KEGG")),
      geneset_panel_UI(ns("GO")),
      geneset_panel_UI(ns("REACTOME")),
      tabPanel(
        title = "KeggPathwayOutput",
        helpText("Specificy on the left which pathway (all sig. enriched) to display in picture-format"),
        actionButton(
          inputId = "OverlayOnPathway",
          label = "Show overlay on Pathway"
        ),
        selectInput(
          inputId = "plotOnTopOption",
          label = "Specifiy the what the colored overlay should indicate",
          choices = c("LFC", "presence"),
          selected = "presence"
        ),
        uiOutput(outputId = "sample_anno_types_KEGG_ui"),
        uiOutput(outputId = "ComparisonOptionsCRTL_ui"),
        uiOutput(outputId = "ComparisonOptionsCOMP_ui"),
        uiOutput(outputId = "psig_KEGG_ui"),
        sliderInput(
          inputId = "imageWidth",
          label = "Adjust Width",
          min = 400, max = 1500, step = 20, value = 1000
        ),
        sliderInput(
          inputId = "imageHeight",
          label = "Adjust Height",
          min = 400, max = 1500, step = 20, value = 640
        ),
        imageOutput(outputId = "KeggPathwayOutput_img") %>% withSpinner(type = 8)
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
    enrichment_analysis_sidebar <- ea_sidebar(),
    enrichment_analysis_main <- ea_main(ns),
  )
}
