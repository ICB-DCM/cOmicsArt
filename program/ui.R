
# # Data to Select
# geneAnno=read.csv("../data/geneAnnotation.csv",header = T, row.names = 1)
# GeneClasses=unique(geneAnno$type) # more than 100 genes
# FreqTable=as.data.frame(table(geneAnno$type))
# toRemove=as.character(FreqTable[FreqTable$Freq<500,"Var1"])
# GeneClasses=setdiff(GeneClasses,toRemove)
# 
# metabAnno=read.csv("../data/MetabAnnotation.csv",header=T, row.names = 1)
# metabClasses=unique(metabAnno$SUPER_PATHWAY)
# metabClasses_sub=unique(metabAnno$SUB_PATHWAY)
# 
# sampleAnno_RNA=read.csv("../data/sample_anno_HighSalt.csv",header = T, row.names = 1)
# colnames(sampleAnno_RNA)[9]="TREATMENT"
# #sampleAnno_RNA$SAMPLE_NAME=paste0("RNA",sampleAnno_RNA$global_ID)
# 
# sampleAnno_METAB=read.csv("../data/sampleAnno_Metab.csv",header = T, row.names = 1)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  tabsetPanel(id = "inTabset",
              ################################################################################
              # Tab Selection w Upload
              ################################################################################
              tabPanel("Data selection",fluid=T,
                       headerPanel("Data Selection + explorative Analysis"),
                       ################################################################################
                       # Sidebar
                       ################################################################################
                       sidebarPanel(
                         #actionButton(inputId = "Plot_PCA1",label = "Plot Spot 1"),
                         #actionButton(inputId = "Plot_PCA2",label = "Plot Spot 2"),
                         radioGroupButtons(
                           inputId = "omicType",  #RNAorLIPID
                           label = "Omic Type (THIS IS IMPORTANT FOR DATA UPLOAD SPECIFICATION)", 
                           choices = c("Transcriptomics", "Lipidomics","Metabolomics"),
                           status = "info"
                         ),
                         h3("Row selection -  biochemical entities"),
                         h5("Depending on the additional annotation you provided, you can pre-select the provided data. You have provided the following additional annotations you may base your row-selection on:"),
                         uiOutput("providedRowAnnotationTypes_ui"),
                         uiOutput("row_selection_ui"),
                         uiOutput("propensityChoiceUser_ui"),
                            # Outlier Selection -> for fixed removal pre-processing needs to be redone!
                         h3("Sample selection"),
                         uiOutput("providedSampleAnnotationTypes_ui"),
                         uiOutput("sample_selection_ui"),
                         #########################################
                         # Do Center & scaling + potential other pre-processing stuff
                         #########################################
                         h3("Pre-Processing Procedures"), # this could be enhanced with personalized procedures
                         radioButtons(inputId = "PreProcessing_Procedure",
                                      label = "Pre-Processing Procedures",
                                      choices = c("none","vst_DESeq","simpleCenterScaling","Scaling_0_1",
                                                  "log10","pareto_scaling"),
                                      selected = "none"),
                         uiOutput("DESeq_formula_ui"),
                         actionButton(inputId = "Do_preprocessing",
                                      label = "Pre-Process",
                                      icon("fas fa-laptop-code")),
                         #########################################
                         # explorative analysis
                         # PCA , UMAP 
                         #########################################
                         h3("Explorative Analysis"),
                         actionButton(inputId = "Do_PCA",
                                    label = "Perform PCA",
                                    icon("fas fa-laptop-code")),
                          uiOutput("coloring_options_ui"), # dependent on Do_PCA
                          uiOutput("x_axis_selection_ui"), # dependent on Do_PCA
                          uiOutput("y_axis_selection_ui"), # dependent on Do_PCA
                          uiOutput("Show_loadings_ui"),    # dependent on Do_PCA
                         actionButton(inputId = "Do_UMAP",
                                      label = "Perform UMAP",
                                      icon("fas fa-laptop-code")),
                         actionButton(inputId = "Do_Volcano",
                                      label = "Do Volcano Plot",
                                      icon("fas fa-laptop-code")),
                         uiOutput("sample_annotation_types_cmp_ui"),
                         uiOutput("Groups2Compare_ref_ui"),
                         uiOutput("Groups2Compare_treat_ui"),
                         uiOutput("psig_threhsold_ui"),
                         uiOutput("lfc_threshold_ui"),
                         
                         actionButton(inputId = "Do_Heatmap",
                                      label = "Do Heatmap to display",
                                      icon("fas fa-laptop-code")),
                         uiOutput("anno_options_ui"),
                         uiOutput("row_anno_options_ui"),
                         uiOutput("cluster_cols_ui"),
                         uiOutput("cluster_rows_ui"),
                         uiOutput("row_selection_options_ui"),
                         uiOutput("TopK_ui")
                         
                       ),
                       mainPanel(
                         ################################################################################
                         # Main Panel
                         ################################################################################
                         fluidRow(
                           textOutput('debug', container = pre),
                           h3("Upload section"),
                            splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                        fileInput("data_matrix1", "Upload data Matrix (rows entities, cols samples)", accept = ".csv"),
                                        fileInput("data_sample_anno1", "Upload sample Annotation (rows must be samples)", accept = ".csv"),
                                        ),
                           splitLayout(style = "border: 1px solid silver:", cellWidths = c("50%","50%"),
                                       fileInput("data_row_anno1", "Upload entities Annotatio Matrix (rows must be entities)", accept = ".csv"),
                                       fileInput("data_preDone", "Load precompiled data (saved in this procedure)", accept = ".RDS")),
                           downloadButton("SaveInputAsList",label="Save file input to upload later",class = "btn-info"),
                           #downloadHandler()downloadButton('foo')
                           splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                       #plotOutput("PCA_final_gg"),
                                       plotlyOutput("Plot_position_01"),
                                       textOutput('Options_selected_out_1', container = pre)),
                           splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","15%","15%"),
                                       NULL,
                                       downloadButton("SavePlot_pos1",label="Save plot",class = "btn-info"),
                                       radioButtons(input="file_ext_plot1",label = "File Type:",
                                                   choices = c(".png",".svg",".tiff",".pdf"))),
                          splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                       #plotOutput("PCA_final_gg"),
                                       plotlyOutput("Plot_position_02"),
                                       textOutput('Options_selected_out_2', container = pre)),
                          splitLayout(style = "border: 1px solid silver:", cellWidths = c("100%"),
                                      #plotOutput("PCA_final_gg"),
                                      plotOutput("Plot_position_03")),
                          textOutput('Options_selected_out_3', container = pre)
                           
                         ))),
              ################################################################################
              # Tab Kegg Pathway
              ################################################################################
              tabPanel("KEGG pathway", fluid = TRUE,
                       sidebarPanel(
                         selectInput("OrganismChoice","Specificy your current organism",choices=c("hsa","mmu"),selected="mmu"),
                         selectInput("GeneSet2Enrich","Choose a gene set to hand over to enrich",choices=c("DE_Genes","ProvidedGeneSet"),selected="geneSetChoice"),
                         uiOutput("UploadedGeneSet"),
                         selectInput("UniverseOfGene","Select an Universe for enrichment (default is clusterProfilers default",choices = c("default","allPresentGenes_used","allPresentGenes_RNASeq"),selected = "default"),
                         actionButton("enrichmentGO",label="Do enrichment analysis",class = "btn-info"),
                         uiOutput("KeggPathwayID"),
                         selectInput("plotOnTopOption","Specifiy the what the colored overlay should indicate",choices = c("FC","presence"),selected="presence"),
                         uiOutput("ComparisonOptionsCRTL"),
                         uiOutput("ComparisonOptionsCOMP"),
                         actionButton("OverlayOnPathway",label="Show overlay on Pathway ",class = "btn-info"),
                         sliderInput("imageWidth",label="Adjust Width",min=400,max=1500,step = 20,value=1000),
                         sliderInput("imageHeight",label="Adjust Height",min=400,max=1500,step = 20,value=640)
                       ),
                       mainPanel(
                         plotOutput("KEGG_Enrichment"),
                         DT::dataTableOutput("EnrichmentResults"),
                         textOutput("WorkAroundLegend"),
                         imageOutput("KeggPathwayOutput")
                         
                       ))
  )#end tabset
)

# Wrap your UI with secure_app
#ui <- secure_app(ui)
