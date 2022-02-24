
# Data to Select
geneAnno=read.csv("../data/geneAnnotation.csv",header = T, row.names = 1)
GeneClasses=unique(geneAnno$type) # more than 100 genes
FreqTable=as.data.frame(table(geneAnno$type))
toRemove=as.character(FreqTable[FreqTable$Freq<500,"Var1"])
GeneClasses=setdiff(GeneClasses,toRemove)

metabAnno=read.csv("../data/MetabAnnotation.csv",header=T, row.names = 1)
metabClasses=unique(metabAnno$SUPER_PATHWAY)
metabClasses_sub=unique(metabAnno$SUB_PATHWAY)

sampleAnno_RNA=read.csv("../data/sample_anno_HighSalt.csv",header = T, row.names = 1)
colnames(sampleAnno_RNA)[9]="TREATMENT"
#sampleAnno_RNA$SAMPLE_NAME=paste0("RNA",sampleAnno_RNA$global_ID)

sampleAnno_METAB=read.csv("../data/sampleAnno_Metab.csv",header = T, row.names = 1)

ui <- fluidPage(
  tabsetPanel(id = "inTabset",
              ################################################################################
              # Tab 1
              ################################################################################
              tabPanel("Data selection",fluid=T,
                       headerPanel("Data Selection"),
                       sidebarPanel(
                         actionButton(inputId = "Plot_PCA1",label = "Plot Spot 1"),
                         actionButton(inputId = "Plot_PCA2",label = "Plot Spot 2"),
                         radioGroupButtons(
                           inputId = "RNAorLIPID",
                           label = "RNA or Metabolite", 
                           choices = c("RNA", "LIPIDs"),
                           status = "info"
                         ),
                         h3("RNA-Seq selection"),
                         checkboxGroupButtons(
                           inputId = "RNASeq_selection",
                           label = "Which Genes to use? (Will be the union if multiple selected)",
                           choices = c(GeneClasses,"High Expression+IQR","all"),
                           status = "info",
                           selected="protein_coding",
                           checkIcon = list(
                             yes = icon("ok",lib = "glyphicon"),
                             no = icon("remove", lib = "glyphicon"))
                         ),
                         numericInput("propensityChoiceUser",label = "Specifcy the propensity for variablity & Expr",value = 0.85,min=0,max=1),
                         h3("Metabolomics selection"),
                         #Select for Lipid classes
                         #radioButtons(inputId ="TAG_collapsed", label=h4("Do you want to have the TAG-classes collapsed?"),choices=list("No"=0,"Yes"=1),selected=1),
                         #uiOutput("LipidClasses_selection_2"),
                         pickerInput(
                            inputId = "LipidClasses_selection",
                            label = "Select a all Lipid classes to include", 
                            choices = metabClasses,
                            options = list(`actions-box` = TRUE), 
                            multiple = TRUE,
                            selected = metabClasses
                        ),
                         # Outlier Selection -> for fixed removal pre-processing needs to be redone!
                         #h4("Sample selection"),
                         multiInput(
                           inputId="OutlierInput",
                           label=NULL,
                           choices = NULL,
                           choiceNames = union(sampleAnno_METAB$SAMPLE_NAME,sampleAnno_RNA$global_ID),
                           choiceValues = union(sampleAnno_METAB$SAMPLE_NAME,sampleAnno_RNA$global_ID),
                           options=list(non_selected_header="Choose outliers: ",
                                        selected_header="Selected Outliers: ")
                         ),
                
                         h3("PCA - Plotting options"),
                         ## Which PC to plot?
                         splitLayout(radioGroupButtons(
                           inputId = "PC_selection_x",
                           label = "PC for x-Axis",
                           choices = c("PC1","PC2", "PC3", "PC4"),
                           direction = "vertical",
                           selected = "PC1"
                         ),
                         radioGroupButtons(
                           inputId = "PC_selection_y",
                           label = "PC for y-Axis",
                           choices = c("PC1","PC2", "PC3", "PC4"),
                           direction = "vertical",
                           selected = "PC2")
                         ),
                         # Show Loadings Radio Button
                        radioButtons(inputId ="ShowLoadings", label=h4("Show top 5 Loadings?"),
                                      choices=list("No"=0,"Yes"=1),selected=0),
                        selectInput(
                          inputId = "color_Choice",
                          label = "Choose what to color",
                          choices = c(unique(colnames(sampleAnno_RNA),colnames(sampleAnno_METAB))),
                          selected = "TREATMENT")
                         # 
                       ),
                       mainPanel(
                         fluidRow(
                           splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                       #plotOutput("PCA_final_gg"),
                                       plotlyOutput("PCA_final_pp_1"),
                                       textOutput('Options_selected_out_1', container = pre)),
                           radioButtons(inputId = "send2O2PLS_1",label = "Data used for O2PLS",choices = c("RNA","Lipid"),selected = "RNA"),
                           splitLayout(style = "border: 1px solid silver:", cellWidths = c("70%","30%"),
                                       #plotOutput("PCA_final_gg"),
                                       plotlyOutput("PCA_final_pp_2"),
                                       textOutput('Options_selected_out_2', container = pre)),
                           radioButtons(inputId = "send2O2PLS_2",label = "Data used for O2PLS",choices = c("RNA","Lipid"),selected = "Lipid"),
                           h3("Upload section"),
                           fileInput("LOAD_O2PLS_file", "Uploade a previously saved O2PLS object", accept = ".RDS"),
                           radioButtons(inputId = "useUploadData",label="Use Uploaded data",choices = c("Yes","No"),selected = "Yes"),
                           actionButton(inputId = "send2O2PLS",label = "Send to O2PLS",class="btn-success",width = "60%",icon = icon("magic")),
                           actionButton(inputId = "send2O2PLS_robustness",label = "Perform O2PLS robustness analysis",class="btn-warning",width = "60%",icon = icon("sync"))
                           #actionButton(inputId = "LOAD_O2PLS_btn",label = "Load a previously computed O2PLS object"),
                           #uiOutput("LOAD_O2PLS_file")
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
ui <- secure_app(ui)
