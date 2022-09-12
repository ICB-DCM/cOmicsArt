## Server 2.0

# eigentlich getestet auf 4.1.2
# setwd("program")
# if(!(renv::status()$synchronized)){
#  renv::restore(lockfile = "renv.lock")
# }

library(DT)
library(plotly)
library(shiny, lib.loc = .libPaths()[1])
library(shinyWidgets)
library(shinymanager)
library(shinyjs)
library(DESeq2)
library(grid)
library(pheatmap)
library(pathview)
library(clusterProfiler)
library(BiocManager)
library(shinyhelper)
library(dplyr)
library(shinycssloaders)
library(ggpubr)
library(org.Mm.eg.db)
library(org.Hs.eg.db)
library(jsonlite)
library(rmarkdown)
library(tinytex)
library(testthat)
library(shinytest)
library(biomaRt)
library(zip)
library(cicerone)
# library(svglite)

options(repos = BiocManager::repositories())
options(spinner.color = "#1c8a3b", spinner.color.background = "#ffffff", spinner.size = 2)
########
# Set Up security
########
credentials <- data.frame(
  user = c("Clivia", "Lea"), # mandatory
  password = c("Cii@31", "Lea"), # mandatory
  # start = c("2019-04-15"), # optinal (all others)
  # expire = c(NA, "2019-12-31"),
  admin = c(FALSE, TRUE),
  comment = "Log In to Run secret Shiny",
  stringsAsFactors = FALSE
)
# source("fun_filterRNA.R",local = T)
# source("fun_plotPCA.R")
# source("fun_LFC.R")
# source("fun_volcano.R")
# source("fun_popupModal.R")
# source("fun_entitieSelection.R")

# source("server_dev.R")
# source("ui.R")



ui <- shiny::fluidPage(
  tags$head(
    ##########
    # Styling Setting & Loading Gif
    ##########
    # Note the wrapping of the string in HTML()
    tags$style(HTML("
      body {
        background-color: #f8f7fa;
      }
      body, label, input, button, select, h4, h3, h2 {
        color: #3b3b3b;
      }
      .tabbable > .nav > li > a {
         background-color: #d5d4d6;
         color: #3b3b3b
      }
      #sidebar1 {
        background-color: #cbedca;
      }
      #sidebar2 {
        background-color: #edceec;
      }
      #sidebar3 {
        background-color: #cecef2;
      }
      #sidebar4 {
        background-color: #f2e6c2;
      }
      #sidebar5 {
        background-color: #d4c2f2;
      }
  "))
  ),
  ##########
  use_cicerone(),
  ##########

  div(style = "display:inline-block; float:right", actionButton(inputId = "Quit_App", label = "Quit App", class = "btn-secondary")),
  div(style = "display:inline-block; float:right", actionButton(inputId = "guide", label = "Guide me!", class = "btn-secondary")),
  div(style = "display:inline-block; float:right", helpText(" ", align = "right") %>% helper(type = "markdown", content = "Inital_help", size = "l", colour = "red", style = "zoom: 600%;")),
  hidden(selectInput("element", label = "PrideMonth?", choices = c(0, 1), selected = ifelse(format(as.POSIXct(Sys.time()), "%m") == "06", 1, 0))),
  conditionalPanel(
    condition = "input.element == 0",
    div(id="TitleID_normal",titlePanel("ShinyOmics")),
  ),
  conditionalPanel(
    condition = "input.element == 1",
    div(
      id="TitleID_pride",
      h2(HTML('<span style="color:#E75A5A">S</span><span style="color:#E7AF5A">h</span><span style="color:#CBE75A">i</span><span style="color:#76E75A">n</span><span style="color:#5AE792">y</span><span style="color:#5AE7E7">O</span><span style="color:#5A92E7">m</span><span style="color:#765AE7">i</span><span style="color:#CB5AE7">c</span><span style="color:#E75AAF">s</span>'))
      ),
  ),
  splitLayout(
    cellWidths = c("75%", "10%", "15%"),
    actionLink(inputId = "DownloadReport", label = "Download Report (as html)"),
    helpText("Metabolon Help", align = "center") %>% helper(type = "markdown", content = "Metabolon_help", size = "l", colour = "blue", style = "position: relative;top: -18px;left: 15px;; zoom: 200%;"),
    NULL
  ),
  shinyjs::useShinyjs(),
  tabsetPanel(
    id = "tabsetPanel1",
    # textOutput('debug', container = pre),

    ################################################################################
    # Tab Selection w Upload
    ################################################################################
    tabPanel("Data selection",
      fluid = T,
      h4("Data Selection"),

      ################################################################################
      # Data Selection
      ################################################################################
      sidebarPanel(
        id = "sidebar1",
        selectInput(
          inputId = "omicType", # RNAorLIPID
          label = "Omic Type that is uploaded",
          choices = c("Transcriptomics", "Lipidomics", "Metabolomics"),
          selected = ""
        ),
        uiOutput("AddGeneSymbols_ui"),
        uiOutput("AddGeneSymbols_organism_ui"),
        actionButton(inputId = "refresh1", label = "Do"),
        hr(style = "border-top: 1px solid #000000;"),
        h4("Row selection -  biochemical entities"),
        uiOutput("providedRowAnnotationTypes_ui"),
        uiOutput("row_selection_ui"),
        uiOutput("propensityChoiceUser_ui"),
        # Outlier Selection -> for fixed removal pre-processing needs to be redone!
        h4("Sample selection"),
        uiOutput("providedSampleAnnotationTypes_ui"),
        uiOutput("sample_selection_ui")
      ),
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
        title="Upload section",
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("85%", "10%", "5%"),
          NULL,
          actionButton(inputId = "Reset", label = "Reset"),
          NULL
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          uiOutput("data_matrix1_ui"),
          uiOutput("data_sample_anno1_ui")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          uiOutput("data_row_anno1_ui"),
          uiOutput("data_preDone_ui") %>% helper(type = "markdown", content = "SummarizedExp_help")
        ),
        hr(style = "border-top: 2px solid #cbedca;"),
        uiOutput("metadataInput_ui"),
        hr(style = "border-top: 2px solid #cbedca;"),
        downloadButton("SaveInputAsList", label = "Save file input to upload later") %>% helper(type = "markdown", content = "compilation_help"),
        htmlOutput("debug", container = pre),
        HTML("<br>"),
        HTML("<br>")),
        tabPanel(
          title="Upload visual inspection",
          helpText("If you have uploaded your data, you might want to visually check the tables to confirm the correct data format. If you notice irregualarities you will need to correct the input data - this cannot be done in ShinyOmics, See the help on how your data is expected."),
          actionButton(inputId = "DoVisualDataInspection", label = "Upload data for visual inspection"),
          splitLayout(
            style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
            DT::dataTableOutput("DataMatrix_VI"),
            htmlOutput("DataMatrix_VI_Info", container = pre)
          ),
          splitLayout(
            style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
            DT::dataTableOutput("SampleMatrix_VI"),
            htmlOutput("SampleMatrix_VI_Info", container = pre)
          ),
          splitLayout(
            style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
            DT::dataTableOutput("EntitieMatrix_VI"),
            htmlOutput("EntitieMatrix_VI_Info", container = pre)
          ),
          htmlOutput("OverallChecks", container = pre)
        )
        ),
        uiOutput("NextPanel_ui")
      )
    ),
    tabPanel("Pre-processing",
      fluid = T,
      h4("Data Pre-processing") %>% helper(type = "markdown", content = "PreProcessing_help"),
      sidebarPanel(
        id = "sidebar2",
        #########################################
        # Do Center & scaling + potential other pre-processing stuff
        #########################################
        h5("Pre-Processing Procedures"), # this could be enhanced with personalized procedures
        radioButtons(
          inputId = "PreProcessing_Procedure",
          label = "Pre-Processing Procedures",
          choices = c(
            "none", "vst_DESeq", "simpleCenterScaling", "Scaling_0_1",
            "log10", "pareto_scaling", "ln"
          ),
          selected = "none"
        ),
        uiOutput("DESeq_formula_ui"),
        actionButton(
          inputId = "Do_preprocessing",
          label = "Pre-Process",
          icon("fas fa-laptop-code")
        ),
        hr(style = "border-top: 1px solid #000000;")
      ),
      mainPanel(
        # Statistics to the data
        helpText("general statistics to the input data, stuff like dimensions"),
        # hidden(div(id = 'Spinner_Statisitcs_Data', plotOutput("Statisitcs_Data")%>% withSpinner(type=8))),
        htmlOutput("Statisitcs_Data") %>% withSpinner(type = 8),
        HTML("<br>"),
        HTML("<br>"),
        splitLayout(
          cellWidths = c("25%", "25%", "25%"),
          uiOutput("NextPanel2_ui"),
          uiOutput("NextPanel3_ui"),
          uiOutput("NextPanel4_ui")
        )
      )
    ),
    tabPanel("Projection to lower Dimensions",
      fluid = T,
      h4("Projection to lower Dimensions"),
      sidebarPanel(
        id = "sidebar3",
        #########################################
        # explorative analysis
        # PCA , UMAP
        #########################################
        h4("Explorative Analysis"),
        actionButton(inputId = "Do_PCA", label = "Perform PCA", icon("fas fa-laptop-code")),
        hr(style = "border-top: 1px solid #000000;"),
        uiOutput("coloring_options_ui"),
        uiOutput("x_axis_selection_ui"),
        uiOutput("y_axis_selection_ui"),
        uiOutput("Show_loadings_ui"),
        # actionButton(inputId = "Do_UMAP",
        #              label = "Perform UMAP",
        #              icon("fas fa-laptop-code")),
        helpText("Note: if you would like to change the annotation of the indicated loading vectors please select an option the the tab Loadings"),
        hr(style = "border-top: 1px dashed #000000;")
        
      ),
      mainPanel(
        tabsetPanel(
          type = "pills",
          tabPanel(
            "PCA",
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              plotlyOutput("PCA_plot") %>% withSpinner(type = 8),
              textOutput("PCA_plot_Options_selected", container = pre)
            ),
            uiOutput("PCA_anno_tooltip_ui"),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_pca", label = "Send only to Report")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton( "getR_Code_PCA", label = "Get underlying R code and data",icon = icon("code"))
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("SavePlot_pos1", label = "Save plot", class = "btn-info")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              radioGroupButtons(
                input = "file_ext_plot1", label = "File Type:",
                choices = c(".png", ".tiff", ".pdf"), selected = ".png"
              )
            ),
            textAreaInput(inputId="NotesPCA", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
          ),
          tabPanel(
            "PCA_Loadings",
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              plotOutput("PCA_Loadings_plot") %>% withSpinner(type = 8),
              textOutput("Loadings_plot_Options_selected_out", container = pre)
            ),
            uiOutput("EntitieAnno_Loadings_ui"),
            sliderInput(inputId = "topSlider", label = "Top k positive Loadings", min = 1, max = 25, value = 10, step = 1),
            sliderInput(inputId = "bottomSlider", label = "Top k negative Loadings", min = 1, max = 25, value = 10, step = 1),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_Loadings", label = "Send only to Report", class = "btn-info"),
            ), 
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("getR_Code_Loadings", label = "Get underlying R code and data",icon = icon("code"))
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("SavePlot_Loadings", label = "Save plot", class = "btn-info")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              radioGroupButtons(
                input = "file_ext_Loadings", label = "File Type:",
                choices = c(".png", ".tiff", ".pdf"), selected = ".png"
              )
            )
          ),
          tabPanel(
            "Scree_Plot",
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              plotlyOutput("Scree_Plot"),
              textOutput("Scree_Plot_Options_selected_out", container = pre)
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_Scree_Plot", label = "Send only to Report", class = "btn-info"),
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("getR_Code_Scree_Plot", label = "Get underlying R code and data",icon = icon("code"))
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("SavePlot_Scree", label = "Save plot", class = "btn-info")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              radioGroupButtons(
                input = "file_ext_Scree", label = "File Type:",
                choices = c(".png", ".tiff", ".pdf"), selected = ".png"
              )
            )
          )
        )
      )
    ),
    tabPanel("Volcano Plot",
      fluid = T,
      h4("Volcano Plot"),
      sidebarPanel(
        id = "sidebar4",
        #########################################
        # VOLCANO
        #########################################
        uiOutput("sample_annotation_types_cmp_ui"),
        uiOutput("Groups2Compare_ref_ui"),
        uiOutput("Groups2Compare_treat_ui"),
        actionButton(
          inputId = "Do_Volcano",
          label = "Do Volcano Plot",
          icon("fas fa-laptop-code")
        ),
        hr(style = "border-top: 1px solid #000000;"),
        uiOutput("psig_threhsold_ui"),
        uiOutput("lfc_threshold_ui"),
        switchInput(
          inputId = "get_entire_table",
          label = "show next to LFCs also the raw data that was used to calculate",
          inline = T
        )
      ),
      mainPanel(
        tabsetPanel(
          tabPanel(
            "Volcano_Plot", plotlyOutput("Volcano_Plot_final") %>% withSpinner(type = 8),
            uiOutput("VOLCANO_anno_tooltip_ui"),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_Volcano", label = "Send only to Report", class = "btn-info"),
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("getR_Code_Volcano", label = "Get underlying R code and data",icon = icon("code"))
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("SavePlot_Volcano", label = "Save plot", class = "btn-info")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              radioGroupButtons(
                input = "file_ext_Volcano", label = "File Type:",
                choices = c(".png", ".tiff", ".pdf"), selected = ".png"
              )
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
              downloadButton("SaveDE_List", label = "Save intresting entities (all red points)"),
              actionButton(inputId = "SendDE_Genes2Enrichment", label = "Send DE Genes to enrichment analysis", block = F)
            )
          ),
          tabPanel("Volcano_table", DT::dataTableOutput("Volcano_table_final")),
          
        ),
        textAreaInput(inputId="NotesVolcano", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
      )
    ),
    tabPanel("Heatmap",
      fluid = T,
      h4("Heatmap"),
      sidebarPanel(
        id = "sidebar5",
        #########################################
        # Heatmap
        #########################################
        uiOutput("row_selection_options_ui"),
        uiOutput("LFC_toHeatmap_ui"),
        h5("Further row selection (LFC based)"),
        uiOutput("TopK_ui"),
        switchInput(
          inputId = "Selection_show_LFC",
          label = "show options (LFC-related)",
          inline = T,
          size = "mini"
        ),
        uiOutput("sample_annotation_types_cmp_heatmap_ui"),
        uiOutput("Groups2Compare_ref_heatmap_ui"),
        uiOutput("Groups2Compare_treat_heatmap_ui"),
        uiOutput("psig_threhsold_heatmap_ui"),
        actionButton(
          inputId = "Do_Heatmap",
          label = "Do Heatmap to display",
          icon("fas fa-laptop-code")
        ),
        hr(style = "border-top: 1px solid #000000;"),
        h5("Aesthetics"),
        switchInput(
          inputId = "Aesthetics_show",
          label = "show options",
          size = "mini", value = T
        ),
        uiOutput("anno_options_ui"),
        uiOutput("row_anno_options_ui"),
        uiOutput("rowWiseScaled_ui"),
        uiOutput("cluster_cols_ui"),
        uiOutput("cluster_rows_ui"),
        hr(style = "border-top: 1px solid #858585;"),
        h5("Further row selection (annotation based)"),
        switchInput(
          inputId = "Selection_show_annoBased",
          label = "show options (annotation-related)",
          inline = T,
          size = "mini",
          value = F
        ),
        uiOutput("rowAnno_based_ui"),
        uiOutput("row_anno_factor_ui"),
        uiOutput("anno_options_heatmap_ui"),
        uiOutput("row_anno_options_heatmap_ui")
        # hr(style = "border-top: 1px solid #858585;")
      ),
      mainPanel(
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("100%"),
          # plotOutput("PCA_final_gg"),
          plotOutput("HeatmapPlot")
          # %>% withSpinner(type=8,color = getOption("spinner.color", default = "#b8cee0"))
        ),
        textOutput("Options_selected_out_3", container = pre) %>% withSpinner(type = 8),
        uiOutput("row_label_options_ui"), numericInput(inputId = "row_label_no", min = 0, step = 1, label = "Threshold upon which explicit labels are shown", value = 25),
        downloadButton("SaveGeneList_Heatmap", label = "Save genes shown in Heatmap as list"),
        actionButton(inputId = "SendHeatmap2Enrichment", label = "Send genes shown to enrichment analysis", block = F),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          actionButton(inputId = "only2Report_Heatmap", label = "Send only to Report", class = "btn-info"),
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("getR_Code_Heatmap", label = "Get underlying R code and data",icon = icon("code"))
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("SavePlot_Heatmap", label = "Save plot", class = "btn-info")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            input = "file_ext_Heatmap", label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"), selected = ".png"
          )
        ),
        textAreaInput(inputId="NotesHeatmap", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
      )
    ),
    tabPanel("Single Gene Visualisations",
      fluid = TRUE,
      h4("Single Gene Visualisations"),
      #########################################
      # Single Gene Visualisations
      #########################################
      sidebarPanel(
        uiOutput("type_of_data_gene_ui"),
        uiOutput("type_of_visualitsation_ui"),
        uiOutput("Select_GeneAnno_ui"),
        uiOutput("Select_Gene_ui"),
        helpText("Note: if you choose a group rather than a single entitie, the values will be summarized by taking the median"),
        uiOutput("accross_condition_ui"),
        actionButton("singleGeneGo", label = "Get single gene visualisation"),
        hr(style = "border-top: 1px solid #858585;")
      ),
      mainPanel(
        # hidden(div(id = 'Spinner_SingleGene', plotOutput("SingleGenePlot")%>% withSpinner(type=8))),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("50%", "50%"),
          plotOutput("SingleGenePlot"), NULL
        ),
        uiOutput("chooseComparisons_ui"),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          actionButton(inputId = "only2Report_SingleEntities", label = "Send only to Report", class = "btn-info"),
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("getR_Code_SingleEntities", label = "Get underlying R code and data",icon = icon("code"))
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          downloadButton("SavePlot_singleGene", label = "Save plot", class = "btn-info")
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          NULL,
          radioGroupButtons(
            input = "file_ext_singleGene", label = "File Type:",
            choices = c(".png", ".tiff", ".pdf"), selected = ".png"
          )
        ),
        textAreaInput(inputId="NotesSingleEntities", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
        helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
        
      )
    ),
    tabPanel("Enrichment Analysis",
      fluid = TRUE,
      h4("Enrichment Analysis"),
      #########################################
      # Enrichment
      #########################################
      h4("NOTE THAT THIS ONLY MAKES SENSE FOR TRANSCRIPTOMICS DATA AT THE MOMENT!"),
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
        actionButton("enrichmentGO", label = "Do enrichment analysis"),
        radioButtons(inputId = "ontologyForGO", label = "Choose ontology for GO enrichment",choices = c("BP","MF","CC","ALL"),selected = "BP"),
        helpText("Note: ALL sometime fails due to cryptic reasons. You will get notified. If this happens to you please try out the indiviudal ontologies to check whether enriched term are found. "),
        hr(style = "border-top: 1px solid #858585;"),
        uiOutput("KeggPathwayID_ui")
      ),
      mainPanel(
        textOutput("EnrichmentInfo", container = pre),
        tabsetPanel(
          tabPanel(
            "KEGG_Enrichment",
            # hidden(div(id = 'Spinner_KEGG_Enrichment', plotOutput("KEGG_Enrichment")%>% withSpinner(type=8))),
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
            textAreaInput(inputId="NotesKEGG", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
          ),
          tabPanel("KEGG_Enrichment_table", DT::dataTableOutput("EnrichmentResults_KEGG")),
          tabPanel(
            "KeggPathwayOutput",
            helpText("Specificy on the left which pathway (all sig. enriched) to display in picture-format"),
            actionButton("OverlayOnPathway", label = "Show overlay on Pathway"),
            selectInput("plotOnTopOption", "Specifiy the what the colored overlay should indicate", choices = c("LFC", "presence"), selected = "presence"),
            uiOutput("sample_anno_types_KEGG_ui"),
            uiOutput("ComparisonOptionsCRTL_ui"),
            uiOutput("ComparisonOptionsCOMP_ui"),
            uiOutput("psig_KEGG_ui"),
            sliderInput("imageWidth", label = "Adjust Width", min = 400, max = 1500, step = 20, value = 1000),
            sliderInput("imageHeight", label = "Adjust Height", min = 400, max = 1500, step = 20, value = 640),
            # downloadButton("SavePlot_KeggPathwayOutput",label="Save plot"),
            # selectInput(input="file_ext_SavePlot_KeggPathwayOutput",label = "File Type:",
            #             choices = c(".png",".tiff",".pdf"),selected = ".png"),
            imageOutput("KeggPathwayOutput_img") %>% withSpinner(type = 8)
          ),
          tabPanel(
            "GO_Enrichment", plotOutput("GO_Enrichment"),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_GO", label = "Send only to Report", class = "btn-info"),
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("getR_Code_GO", label = "Get underlying R code and data",icon = icon("code"))
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("SavePlot_GO", label = "Save plot", class = "btn-info")
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              radioGroupButtons(
                input = "file_ext_GO", label = "File Type:",
                choices = c(".png", ".tiff", ".pdf"), selected = ".png"
              )
            ),
            textAreaInput(inputId="NotesGO", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
          ),
          tabPanel("GO_Enrichment_table", DT::dataTableOutput("EnrichmentResults_GO")),
          tabPanel(
            "REACTOME_Enrichment", plotOutput("REACTOME_Enrichment"),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              actionButton(inputId = "only2Report_REACTOME", label = "Send only to Report", class = "btn-info"),
            ),
            splitLayout(
              style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
              NULL,
              downloadButton("getR_Code_Reactome", label = "Get underlying R code and data",icon = icon("code"))
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
            textAreaInput(inputId="NotesREACTOME", label="Notes:", placeholder="Notes you want to take alongside the Plot (will be saved in the report) \nYou may want to use markdown syntay for structering the notes ", width = "1000px")%>% helper(type = "markdown", content = "TakingNotesMD_help"),
            helpText("Notes: For structure reasons you should start with Heading Level 4 (hence #### My personal Title)")
          ),
          tabPanel("REACTOME_Enrichment_table", DT::dataTableOutput("EnrichmentResults_REACTOME"))
        )
      )
    )
  ),
  absolutePanel("Brought to you by Lea Seep", bottom = 0, left = 10, fixed = TRUE)
)

# Wrap your UI with secure_app
# ui <- secure_app(ui)
