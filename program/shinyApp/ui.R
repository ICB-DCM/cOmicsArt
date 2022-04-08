## Server 2.0
#setwd("program")
#if(!(renv::status()$synchronized)){
#  renv::restore(lockfile = "renv.lock")
#}

library(DT)
library(plotly)
library(shiny,lib.loc = .libPaths()[1])
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

options(repos = BiocManager::repositories())
options(spinner.color="#1c8a3b", spinner.color.background="#ffffff", spinner.size=2)
########
# Set Up security 
########
credentials <- data.frame(
  user = c("Clivia", "Lea"), # mandatory
  password = c("Cii@31", "Lea"), # mandatory
  #start = c("2019-04-15"), # optinal (all others)
  #expire = c(NA, "2019-12-31"),
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

#source("server_dev.R")
#source("ui.R")



ui <- shiny::fluidPage(
  tags$head(
    ##########
    # Styling Setting & Loading Gif
    ##########
    #Note the wrapping of the string in HTML()
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
  "))),
  ##########
  titlePanel("Omics-Analysis")%>% helper(type = "markdown",content="Inital_help",size="l",colour = "red",style="zoom: 500%;"),
  shinyjs::useShinyjs(),
  tabsetPanel(id = "tabsetPanel1",
              #textOutput('debug', container = pre),
              ################################################################################
              # Tab Selection w Upload
              ################################################################################
              tabPanel("Data selection",fluid=T,
                       h4("Data Selection + explorative Analysis"),
                       ################################################################################
                       # Data Selection 
                       ################################################################################
                       sidebarPanel(id="sidebar1",
                                    selectInput(
                                      inputId = "omicType",  #RNAorLIPID
                                      label = "Omic Type that is uploaded", 
                                      choices = c("Transcriptomics", "Lipidomics","Metabolomics"),
                                      selected = ""
                                    ) ,
                                    actionButton(inputId = "refresh1",label="Do"),
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
                         h3("Upload section"),
                         
                         splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                     uiOutput("data_matrix1_ui"),
                                     uiOutput("data_sample_anno1_ui"),
                         ),
                         splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                     uiOutput("data_row_anno1_ui"),
                                     uiOutput("data_preDone_ui")
                         ),
                         downloadButton("SaveInputAsList",label="Save file input to upload later") %>% helper(type = "markdown",content="compilation_help"),
                         htmlOutput('debug', container = pre),
                         HTML("<br>"),
                         HTML("<br>"),
                         uiOutput("NextPanel_ui")
                       )
              ),
              tabPanel("Pre-processing",fluid=T,
                       h4("Data Pre-processing")%>% helper(type = "markdown",content="PreProcessing_help"),
                       sidebarPanel(id="sidebar2",
                                    #########################################
                                    # Do Center & scaling + potential other pre-processing stuff
                                    #########################################
                                    h5("Pre-Processing Procedures"), # this could be enhanced with personalized procedures
                                    radioButtons(inputId = "PreProcessing_Procedure",
                                                 label = "Pre-Processing Procedures",
                                                 choices = c("none","vst_DESeq","simpleCenterScaling","Scaling_0_1",
                                                             "log10","pareto_scaling"),
                                                 selected = "none"),
                                    uiOutput("DESeq_formula_ui"),
                                    actionButton(inputId = "Do_preprocessing",
                                                 label = "Pre-Process",
                                                 icon("fas fa-laptop-code")),
                                    hr(style = "border-top: 1px solid #000000;")),
                       mainPanel(
                         # Statistics to the data
                         helpText("general statistics to the input data, stuff like dimensions"),
                         htmlOutput("Statisitcs_Data") %>% withSpinner(type=8),
                         HTML("<br>"),
                         HTML("<br>"),
                         splitLayout(cellWidths = c("25%","25%","25%"),
                                     uiOutput("NextPanel2_ui"),
                                     uiOutput("NextPanel3_ui"),
                                     uiOutput("NextPanel4_ui")
                         )
                       )),
              tabPanel("Projection to lower Dimensions", fluid=T,
                       h4("Projection to lower Dimensions"),
                       sidebarPanel(id="sidebar3",
                                    #########################################
                                    # explorative analysis
                                    # PCA , UMAP
                                    #########################################
                                    h4("Explorative Analysis"),
                                    
                                    actionButton(inputId = "Do_PCA",label = "Perform PCA",icon("fas fa-laptop-code")),
                                    hr(style = "border-top: 1px solid #000000;"),
                                    uiOutput("coloring_options_ui"),
                                    uiOutput("x_axis_selection_ui"),
                                    uiOutput("y_axis_selection_ui"),
                                    uiOutput("Show_loadings_ui"),
                                    # actionButton(inputId = "Do_UMAP",
                                    #              label = "Perform UMAP",
                                    #              icon("fas fa-laptop-code")),
                                    
                                    hr(style = "border-top: 1px dashed #000000;")
                                    
                       ),
                       mainPanel(
                         tabsetPanel(type = "pills",
                                     tabPanel("PCA",
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          plotlyOutput("PCA_plot")%>% withSpinner(type=8),
                                                          textOutput('PCA_plot_Options_selected', container = pre)),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          downloadButton("SavePlot_pos1",label="Save plot",class = "btn-info")
                                                          ),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          radioGroupButtons(input="file_ext_plot1",label = "File Type:",
                                                                      choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                                          )
                                              
                                     ),
                                     tabPanel("PCA_Loadings",
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          plotOutput("PCA_Loadings_plot")%>% withSpinner(type=8),
                                                          textOutput('Loadings_plot_Options_selected_out', container = pre)
                                              ),
                                              sliderInput(inputId = "topSlider",label = "Top k positive Loadings",min = 1,max = 25,value = 10,step = 1),
                                              sliderInput(inputId = "bottomSlider",label = "Top k negative Loadings",min = 1,max = 25,value = 10,step = 1),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          downloadButton("SavePlot_Loadings",label="Save plot",class = "btn-info")
                                              ),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          radioGroupButtons(input="file_ext_Loadings",label = "File Type:",
                                                                            choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                              )
                                     ),
                                     tabPanel("Scree_Plot",
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          plotlyOutput("Scree_Plot"),
                                                          textOutput('Scree_Plot_Options_selected_out', container = pre)),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          downloadButton("SavePlot_Scree",label="Save plot",class = "btn-info")
                                              ),
                                              splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                          NULL,
                                                          radioGroupButtons(input="file_ext_Scree",label = "File Type:",
                                                                            choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                              )
                                     )
                         )
                       )),
              tabPanel("Volcano Plot",fluid=T,
                       h4("Volcano Plot"),
                       sidebarPanel(id="sidebar4",
                                    #########################################
                                    # VOLCANO
                                    #########################################
                                    uiOutput("sample_annotation_types_cmp_ui"),
                                    uiOutput("Groups2Compare_ref_ui"),
                                    uiOutput("Groups2Compare_treat_ui"),
                                    switchInput(
                                      inputId="get_entire_table",
                                      label="show next to LFCs also the raw data that was used to calculate",
                                      inline=T,
                                      size="mini"
                                    ),
                                    actionButton(inputId = "Do_Volcano",
                                                 label = "Do Volcano Plot",
                                                 icon("fas fa-laptop-code")),
                                    hr(style = "border-top: 1px solid #000000;"),
                                    uiOutput("psig_threhsold_ui"),
                                    uiOutput("lfc_threshold_ui")),
                       
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Volcano_Plot",plotlyOutput("Volcano_Plot_final")%>% withSpinner(type=8),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                downloadButton("SavePlot_Volcano",label="Save plot",class = "btn-info")
                                    ),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                radioGroupButtons(input="file_ext_Volcano",label = "File Type:",
                                                                  choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                    )
                                    ),
                           tabPanel("Volcano_table",DT::dataTableOutput("Volcano_table_final"))
                         )
                       )),
              tabPanel("Heatmap",fluid=T,
                       h4("Heatmap"),
                       sidebarPanel(id="sidebar5",
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
                                      size="mini"
                                    ),
                                    uiOutput("sample_annotation_types_cmp_heatmap_ui"),
                                    uiOutput("Groups2Compare_ref_heatmap_ui"),
                                    uiOutput("Groups2Compare_treat_heatmap_ui"),
                                    uiOutput("psig_threhsold_heatmap_ui"),
                                    actionButton(inputId = "Do_Heatmap",
                                                 label = "Do Heatmap to display",
                                                 icon("fas fa-laptop-code")),
                                    hr(style = "border-top: 1px solid #000000;"),
                                    h5("Aesthetics"),
                                    switchInput(
                                      inputId = "Aesthetics_show",
                                      label = "show options",
                                      size="mini",value=T
                                    ),
                                    uiOutput("anno_options_ui"),
                                    uiOutput("row_anno_options_ui"),
                                    uiOutput("cluster_cols_ui"),
                                    uiOutput("cluster_rows_ui"),
                                    hr(style = "border-top: 1px solid #858585;"),
                                    
                                    h5("Further row selection (annotation based)"),
                                    
                                    switchInput(
                                      inputId = "Selection_show_annoBased",
                                      label = "show options (annotation-related)",
                                      inline = T,
                                      size="mini",
                                      value=F
                                    ),
                                    
                                    uiOutput("rowAnno_based_ui"),
                                    uiOutput("row_anno_factor_ui"),
                                    uiOutput("anno_options_heatmap_ui"),
                                    uiOutput("row_anno_options_heatmap_ui")
                                    # hr(style = "border-top: 1px solid #858585;")
                                    
                                    
                       ),
                       mainPanel(
                         splitLayout(style = "border: 1px solid silver:", cellWidths = c("100%"),
                                     #plotOutput("PCA_final_gg"),
                                     plotOutput("HeatmapPlot")%>% withSpinner(type=8)),
                         textOutput('Options_selected_out_3', container = pre)%>% withSpinner(type=8),
                         downloadButton("SaveGeneList_Heatmap",label="Save genes shown in Heatmap as list"),
                         actionButton(inputId = "SendHeatmap2Enrichment",label = "Send genes shown to enrichment analysis",block = F ),
                         splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                     NULL,
                                     downloadButton("SavePlot_Heatmap",label="Save plot",class = "btn-info")
                         ),
                         splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                     NULL,
                                     radioGroupButtons(input="file_ext_Heatmap",label = "File Type:",
                                                       choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                         )
                       )
              ),
              tabPanel("Enrichment Analysis", fluid = TRUE,
                       h4("Enrichment Analysis"),
                       #########################################
                       # Enrichment
                       #########################################
                       h4("NOTE THAT THIS ONLY MAKES SENSE FOR TRANSCRIPTOMICS DATA AT THE MOMENT!"),
                       sidebarPanel(
                         uiOutput("OrganismChoice_ui"),
                         uiOutput("GeneSet2Enrich_ui"),
                         uiOutput("UploadedGeneSet_ui"),
                         selectInput("UniverseOfGene","Select an Universe for enrichment (default is clusterProfilers default",choices = c("default","allPresentGenes_after_pre_process","allPresentGenes_before_pre_process"),selected = "default"),
                         actionButton("enrichmentGO",label="Do enrichment analysis"),
                         hr(style = "border-top: 1px solid #858585;"),
                         uiOutput("KeggPathwayID_ui")
                         
                       ),
                       mainPanel(
                         textOutput("EnrichmentInfo",container = pre),
                         tabsetPanel(
                           tabPanel("KEGG_Enrichment",plotOutput("KEGG_Enrichment")%>% withSpinner(type=8),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                downloadButton("SavePlot_KEGG",label="Save plot",class = "btn-info")
                                    ),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                radioGroupButtons(input="file_ext_KEGG",label = "File Type:",
                                                                  choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                    )
                           ),
                           tabPanel("KEGG_Enrichment_table",DT::dataTableOutput("EnrichmentResults_KEGG")),
                           tabPanel("KeggPathwayOutput",
                                    helpText("Specificy on the left which pathway (all sig. enriched) to display in picture-format"),
                                    actionButton("OverlayOnPathway",label="Show overlay on Pathway"),
                                    selectInput("plotOnTopOption","Specifiy the what the colored overlay should indicate",choices = c("LFC","presence"),selected="presence"),
                                    uiOutput("sample_anno_types_KEGG_ui"),
                                    uiOutput("ComparisonOptionsCRTL_ui"),
                                    uiOutput("ComparisonOptionsCOMP_ui"),
                                    uiOutput("psig_KEGG_ui"),
                                    sliderInput("imageWidth",label="Adjust Width",min=400,max=1500,step = 20,value=1000),
                                    sliderInput("imageHeight",label="Adjust Height",min=400,max=1500,step = 20,value=640),
                                    # downloadButton("SavePlot_KeggPathwayOutput",label="Save plot"),
                                    # selectInput(input="file_ext_SavePlot_KeggPathwayOutput",label = "File Type:",
                                    #             choices = c(".png",".svg",".tiff",".pdf"),selected = ".png"),
                                    imageOutput("KeggPathwayOutput_img")%>% withSpinner(type=8)),
                           tabPanel("GO_Enrichment",plotOutput("GO_Enrichment"),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                downloadButton("SavePlot_GO",label="Save plot",class = "btn-info")
                                    ),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                radioGroupButtons(input="file_ext_GO",label = "File Type:",
                                                                  choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                    )
                           ),
                           tabPanel("GO_Enrichment_table",DT::dataTableOutput("EnrichmentResults_GO")),
                           tabPanel("REACTOME_Enrichment",plotOutput("REACTOME_Enrichment"),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                downloadButton("SavePlot_REACTOME",label="Save plot",class = "btn-info")
                                    ),
                                    splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                                NULL,
                                                radioGroupButtons(input="file_ext_REACTOME",label = "File Type:",
                                                                  choices = c(".png",".svg",".tiff",".pdf"),selected = ".png")
                                    )
                           ),
                           tabPanel("REACTOME_Enrichment_table",DT::dataTableOutput("EnrichmentResults_REACTOME")),
                           
                         )
                       )
              )
  )
)

# Wrap your UI with secure_app
#ui <- secure_app(ui)
