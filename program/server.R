######### 
#Load custom functions 
#########
source("fun_plotPCA.R")
source("fun_filterRNA.R")


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

########
# Load Data an put to lists
#######

data_input1=list(type="RNA",
                 Matrix=read.csv("../data/count_matrix_raw_HighSalt.csv",header = T, row.names = 1),
                 sample_table=read.csv("../data/sample_anno_HighSalt.csv",header = T, row.names = 1),
                 annotation_rows=read.csv("../data/geneAnnotation.csv",header = T, row.names = 1))
# Gene annotation colnames change
colnames(data_input1$annotation_rows)=c("GENEID","GENE_TYPE")

data_input2=list(type="Metab",
                 Matrix=read.csv("../data/scaledData_unsure.csv",header = T, row.names = 1),
                 sample_table=read.csv("../data/sampleAnno_Metab.csv",header = T, row.names = 1),
                 annotation_rows=read.csv("../data/MetabAnnotation.csv",header = T, row.names = 1))
# Metan annotation
data_input2$annotation_rows$NAME=rownames(data_input2$annotation_rows)
data_input2$annotation_rows$CLASS=data_input2$annotation_rows$SUPER_PATHWAY

#check if both have global_ID ( necassary for matching)
if(c("global_ID")%in%colnames(data_input1$sample_table) & c("global_ID")%in%colnames(data_input2$sample_table)){
  if(length(setdiff(data_input1$sample_table$global_ID,data_input2$sample_table$global_ID)>0)){
    warning("global ID does not match") 
  }
}else{
  warning("No global ID for both dataset") 
}

server <- function(input, output, session) {
  
  ################################################################################################
  # Security section
  ################################################################################################
  
  # call the server part
  # check_credentials returns a function to authenticate users
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials),
    timeout = 0
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # your classic server logic

  ################################################################################################
  # Custom Functions
  ################################################################################################

  ################################################################################################
  # Global Settings
  ################################################################################################
  # fix Color for merged
  #col_Merged <<- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
  #names(col_Merged) <<- c("wt_cdcdcd_HC","ko_cdcdcd_HC", "wt_cdcdhfd_HC","ko_cdcdhfd_HC", "wt_hfdcdcd_HC","ko_hfdcdcd_HC", "wt_hfdhfdhfd_HC","ko_hfdhfdhfd_HC")
  
  
  ###############################################################################################
  # Plotting function
  ###############################################################################################
  plot_spot=function(actionButton,outpout_plot,out_put_text){
    observeEvent(input[[actionButton]],{
      
      ########
      # Row Selection 
      ########
      # only not applicable if Lipid is chosen
      selected=c()
      if(input$RNAorLIPID!="LIPIDs"){
        if(any(input$RNASeq_selection%in%unique(data_input1$annotation_rows$GENE_TYPE))){
          print(input$RNASeq_selection)
          selected=c(selected,data_input1$annotation_rows$GENEID[data_input1$annotation_rows$GENE_TYPE%in%input$RNASeq_selection])
        }
        if("union of DE-Genes"%in%input$RNASeq_selection){
          warning("Not Yet done! - Think if either inside or outside shiny - load list or not")
          selected=c(selected,DE_GenesGlobal_4comp)
        }
        
        if("High Expression+IQR"%in%input$RNASeq_selection){
          filteredIQR_Expr <- data_input1$dataMatrix[filter_rna(data_input1$dataMatrix,prop=input$propensityChoiceUser),]
          selected=c(selected,rownames(filteredIQR_Expr))
        }
        if("all"%in%input$RNASeq_selection){
          
          selected=data_input1$annotation_rows$GENEID
        }
        selected=unique(selected)
      }
      print("Selection RNA Works")
      if(input$RNAorLIPID!="RNA"){
        selectedLipidClasses=input$LipidClasses_selection
        for(i in unique(selectedLipidClasses)){
          selected=c(selected,data_input2$annotation_rows$NAME[grepl(paste0("^",i,"$"),data_input2$annotation_rows$CLASS)])
        }
      }
      if(length(selected)<1){
        #print(data_input2$annotation_rows$CLASS)
        warning("Nothing selected!")
      }
      
      # finally subset to selected
      if(input$RNAorLIPID=="RNA"){
        matchedData_all=data_input1
      }else if(input$RNAorLIPID=="LIPIDs"){
        matchedData_all=data_input2
      }
      
      print("Metabs Done")
      tmp_selected<<-selected
      subsettedData=matchedData_all
      subsettedData$Matrix=matchedData_all$Matrix[which(rownames(matchedData_all$Matrix)%in%selected),]
      
      ########
      # remove Outlier 
      ########
      samples2Keep=setdiff(colnames(subsettedData$Matrix),input$OutlierInput)
      print(samples2Keep)
      subsettedData$Matrix=subsettedData$Matrix[,samples2Keep]
      TEST<<-subsettedData$Matrix
      
      if(!(length(samples2Keep)==length(subsettedData$sample_table$global_ID))){
        subsettedData$sample_table=subsettedData$sample_table[subsettedData$sample_table$global_ID%in%samples2Keep,]
      }
      
      ###### remove rows (genes or Lipids) with std dev=0
      
      std.dev=apply(subsettedData$Matrix,1,sd)
      
      paste0("The following is constant and will be removed:",names(which(std.dev==0)))
      subsettedData$Matrix=subsettedData$Matrix[which(std.dev!=0),]
      

       ########
      # Finally Plot 
      ########
      customTitle=paste0("PCA - ",input$RNAorLIPID,"-",ncol(subsettedData$Matrix)," samples",ifelse(length(input$OutlierInput)>0,paste0(" (w/o",paste(input$OutlierInput,collapse = ", "),")"),""))
      
      plotData<<-subsettedData

        output[[outpout_plot]] <- renderPlotly({ggplotly(plotPCA(pca_input=as.data.frame(subsettedData$Matrix),
                                                                 xPC = as.numeric(gsub("PC","",input$PC_selection_x)),
                                                                 yPC = as.numeric(gsub("PC","",input$PC_selection_y)),
                                                                 color = input$color_Choice,
                                                                 title = customTitle,
                                                                 #anno_colour = col_Merged,
                                                                 sample_table = subsettedData$sample_table,
                                                                 plotLoadingsFlag = ifelse(input$ShowLoadings==1,TRUE,FALSE)),
                                                         tooltip = "ID",legendgroup="color")})
      
      
      ##### Diagnostics Shows
      ## Show selected Options of the plot shown
      output[[out_put_text]] <- renderPrint({
        cat(paste("Selected Outliers: ",paste(input$OutlierInput,collapse = ", "),"\n"))
        #if(randomVals==1){
        #  cat(read(paste0(nrow(RNA_seq$sample_table)," matched Samples left\n")))
        #}
        cat(paste("RNA selection: ",paste(input$RNASeq_selection,collapse = ", "),"\n"))
        cat(paste("Lipids selection: ",paste(subsettedData$LipidClasses,collapse = ", "),"\n"))
        cat(paste0("Number of vars: ",nrow(subsettedData$Matrix),"\n"))
        cat(paste0("Number of samples: ",ncol(subsettedData$Matrix),"\n"))
      })
      ## TO DO could built in here some input validation checks
      
      if(input$send2O2PLS_1=="RNA" & outpout_plot=="PCA_final_pp_1"){
        savedData$RNA<<-subsettedData
      }
      if(input$send2O2PLS_1=="Lipid"& outpout_plot=="PCA_final_pp_1"){
        savedData$Lipid<<-subsettedData
      }
      if(input$send2O2PLS_2=="Lipid"& outpout_plot=="PCA_final_pp_2"){
        savedData$Lipid<<-subsettedData
      }
      if(input$send2O2PLS_2=="RNA"& outpout_plot=="PCA_final_pp_2"){
        savedData$RNA<<-subsettedData
      }
      
    })
    #print(paste0(nrow(RNA_seq$sample_table)," matched Samples left"))
    
  }
  
  ###############################################################################################
  # Finally Plotting
  ###############################################################################################
  savedData<<-list()
  plot_spot(actionButton = "Plot_PCA1",outpout_plot = "PCA_final_pp_1",out_put_text = "Options_selected_out_1")
  plot_spot(actionButton = "Plot_PCA2",outpout_plot = "PCA_final_pp_2",out_put_text = "Options_selected_out_2")
  
}

