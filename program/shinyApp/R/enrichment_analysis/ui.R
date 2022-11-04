geneset_panel_UI <- function(
  id
){
  ns <- NS(id)

  tabPanel(
    title = id,
    tabsetPanel(
      tabPanel(
        title=paste(id, "Enrichment"),
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
            input = ns("file_ext"),
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
        title = paste(id, "Enrichment Table"),
        DT::dataTableOutput(ns("EnrichmentTable"))
      )
    )
  )
}


ea_sidebar_UI <- function(id){
  ns <- NS(id)

  sidebarPanel(
    uiOutput("OrganismChoice_ui"),
    uiOutput("ORA_or_GSE_ui"),
    uiOutput("ValueToAttach_ui"),
    uiOutput("sample_annotation_types_cmp_GSEA_ui"),
    uiOutput("Groups2Compare_ref_GSEA_ui"),
    uiOutput("Groups2Compare_treat_GSEA_ui"),
    uiOutput("psig_threhsold_GSEA_ui"),
    uiOutput("GeneSet2Enrich_ui"),
    uiOutput("UploadedGeneSet_ui"),
    uiOutput("UniverseOfGene_ui"),
    actionButton(
      "enrichmentGO",
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
    uiOutput("KeggPathwayID_ui")
  )
}

ea_main_UI <- function(id){
  ns <- NS(id)

  mainPanel(
    textOutput("EnrichmentInfo", container = pre),
    tabsetPanel(
      tabPanel(
        "KEGG_Enrichment",
        plotOutput("KEGG_Enrichment") %>% withSpinner(type = 8),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          actionButton(inputId = "only2Report_KEGG", label = "Send only to Report", class = "btn-info"),
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("getR_Code_KEGG", label = "Get underlying R code and data",icon = icon("code"))
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("SavePlot_KEGG", label = "Save plot", class = "btn-info")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            input = "file_ext_KEGG", label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"), selected = ".png"
          )
        ),
        textAreaInput(
          inputId="NotesKEGG",
          label="Notes:",
          placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
      ),
      tabPanel("KEGG_Enrichment_table", DT::dataTableOutput("EnrichmentResults_KEGG")),
      tabPanel(
        "KeggPathwayOutput",
        helpText("Specificy on the left which pathway (all sig. enriched) to display in picture-format"),
        actionButton("OverlayOnPathway", label = "Show overlay on Pathway"),
        selectInput(
          "plotOnTopOption",
          label = "Specifiy the what the colored overlay should indicate",
          choices = c("LFC", "presence"),
          selected = "presence"
        ),
        uiOutput("sample_anno_types_KEGG_ui"),
        uiOutput("ComparisonOptionsCRTL_ui"),
        uiOutput("ComparisonOptionsCOMP_ui"),
        uiOutput("psig_KEGG_ui"),
        sliderInput(
          "imageWidth",
          label = "Adjust Width",
          min = 400, max = 1500, step = 20, value = 1000
        ),
        sliderInput(
          "imageHeight",
          label = "Adjust Height",
          min = 400, max = 1500, step = 20, value = 640
        ),
        imageOutput("KeggPathwayOutput_img") %>% withSpinner(type = 8)
      ),
      geneset_panel_UI("GO"),
      tabPanel(
        "REACTOME_Enrichment", plotOutput("REACTOME_Enrichment"),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          actionButton(
            inputId = "only2Report_REACTOME",
            label = "Send only to Report",
            class = "btn-info"
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton(
            "getR_Code_Reactome",
            label = "Get underlying R code and data",
            icon = icon("code")
          )
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("SavePlot_REACTOME", label = "Save plot", class = "btn-info")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            input = "file_ext_REACTOME", label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"), selected = ".png"
          )
        ),
        textAreaInput(
          inputId="NotesREACTOME",
          label="Notes:",
          placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
      ),
      tabPanel(
        "REACTOME_Enrichment_table", DT::dataTableOutput("EnrichmentResults_REACTOME")
      )
    )
  )
}

ea_tabPanel_UI <- function(id){
  ns <- NS(id)

  tabPanel(
    "Enrichment Analysis",
    fluid = TRUE,
    h4("Enrichment Analysis"),
    #########################################
    # Enrichment
    #########################################
    h4("NOTE THAT THIS ONLY MAKES SENSE FOR TRANSCRIPTOMICS DATA AT THE MOMENT!"),
    enrichment_analysis_sidebar <- ea_sidebar_UI(id),
    enrichment_analysis_main <- ea_main_UI(id),
  )
}
