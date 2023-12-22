
getPlotCode <- function(
    numberOfScenario,
    preProcessing_Snippet = par_tmp$PreProcessing_Procedure,
    row_selection = par_tmp$row_selection,
    col_selection = par_tmp$col_selection) {
  #TODO  change all data download to par_tmp and res_tmp
 # Selection ----
  if(any(par_tmp$row_selection == "all")){
    stringSelection <- 'selected <- rownames(rowData(res_tmp$data_original))
    '
  }else{
    if(!(length(par_tmp$row_selection) == 1 & any(par_tmp$row_selection == "High Values+IQR"))){
    stringSelection <- 'selected <- c()
selected <- unique(
  c(selected,rownames(rowData(res_tmp$data_original))[
  which(rowData(res_tmp$data_original)
  [,par_tmp$providedRowAnnotationTypes]%in%par_tmp$row_selection)]
    )
  )
  '
  }
  if(any(par_tmp$row_selection == "High Values+IQR") ){
    stringSelection <- 'toKeep <- filter_rna(
    rna = assay(res_tmp$data_original),
    prop = par_tmp$propensityChoiceUser
    )
    filteredIQR_Expr <- assay(res_tmp$data_original)[toKeep,]
    '
    if(length(par_tmp$row_selection) == 1){
      stringSelection <- paste0(stringSelection,
                                'selected <- rownames(filteredIQR_Expr)')
    }else{
      stringSelection <- paste0(stringSelection,
                                'selected <- intersect(
                                selected,
                                rownames(filteredIQR_Expr)
                                )')
    }
  }
  }

  if(par_tmp$col_selection == "all"){
    stringSelection <- paste0(stringSelection,"\n",
                              'samples_selected <- colnames(assay(res_tmp$data_original))
                              ')
  }else{
    stringSelection <- paste0(stringSelection,
                              'samples_selected <- c(
                                samples_selected,
                                rownames(colData(res_tmp$data_original))[which(
                                colData(res_tmp$data_original)[,par_tmp$providedSampleAnnotationTypes] %in% par_tmp$sample_selection
                              )]
                              )
                              ')
  }
 # Preprocessing ----

  if(par_tmp$PreProcessing_Procedure != "none"){
    if(par_tmp$PreProcessing_Procedure == "filter_only"){
      if(par_tmp$omic_type == "Transcriptomics"){
        stringPreProcessing <- 'processedData <- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]'
      }
      if(par_tmp$omic_type == "Metabolomics"){
        stringPreProcessing <- 'processedData <- tmp_data_selected[which(apply(assay(tmp_data_selected),1,median)!=0),]'
      }
      prequel_stringPreProcessing <- c("")
    }else{
      if(par_tmp$omic_type == "Transcriptomics"){
        prequel_stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]'
      }
      if(par_tmp$omic_type == "Metabolomics"){
        prequel_stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(apply(assay(tmp_data_selected),1,median)!=0),]'
      }
    }
    
    if(par_tmp$PreProcessing_Procedure == "simpleCenterScaling"){
        stringPreProcessing <- 'processedData <- as.data.frame(t(
    scale(
      x = as.data.frame(t(as.data.frame(assay(res_tmp$data)))),
      scale = T,
      center = T
      )
    )
    )
    assay(res_tmp$data) <- as.data.frame(processedData)
    '
      }
    if(par_tmp$PreProcessing_Procedure == "vst_DESeq"){
        stringPreProcessing <- 'dds <- DESeq2::DESeqDataSetFromMatrix(
          countData = assay(res_tmp$data),
          colData = colData(res_tmp$data),
          design = as.formula(par_tmp$DESeq_formula)
        )
      de_seq_result <- DESeq2::DESeq(dds)
      res_tmp$DESeq_obj <- de_seq_result
      dds_vst <- vst(
      object = de_seq_result,
      blind = TRUE
      )
      assay(res_tmp$data) <- as.data.frame(assay(dds_vst))
      '
      }
    if(input$PreProcessing_Procedure == "Scaling_0_1"){
      stringPreProcessing <- 'processedData <- as.data.frame(t(
      apply(assay(res_tmp$data),1,function(x){
      (x - min(x))/(max(x) - min(x))
      })
      ))
      assay(res_tmp$data) <- as.data.frame(processedData)
      '
    }
    if(input$PreProcessing_Procedure == "ln"){
      stringPreProcessing <- 'processedData <- as.data.frame(log(
        as.data.frame(assay(res_tmp$data))
      ))
      assay(res_tmp$data) <- as.data.frame(processedData)
      '
    }
    if(input$PreProcessing_Procedure == "log10"){
      stringPreProcessing <- 'processedData <- as.data.frame(assay(res_tmp$data))
      if(any(processedData==0)){
        processedData <- as.data.frame(log10(
        processedData + 1)
       )
      assay(res_tmp$data) <- as.data.frame(processedData)
      }'
    }
    
    if(input$PreProcessing_Procedure == "pareto_scaling"){
      stringPreProcessing <- 'processedData <- as.data.frame(assay(res_tmp$data))
      centered <- as.data.frame(t(
        apply(processedData, 1, function(x){x - mean(x)})
      ))
      pareto.matrix <- as.data.frame(t(
        apply(centered, 1, function(x){x/sqrt(sd(x))})
      ))
      
      assay(res_tmp$data) <- as.data.frame(pareto.matrix)
      '
    }
    stringPreProcessing <- paste0(prequel_stringPreProcessing,"\n",stringPreProcessing)
  }else{
    stringPreProcessing <- ''
  }
    

# Plot Code ----
  ## PCA ----
  if(numberOfScenario >= 1 & numberOfScenario < 9){
    # Calculate all necessary intermediate data sets
    prequel_stringtosave <- 'pcaData <- data.frame(res_tmp$PCA$x,colData(res_tmp$data))
# Annotation (important for plotly)
    if(!any(colnames(pcaData) == "global_ID")){
    pcaData$global_ID <- rownames(pcaData)
    }
    if(!is.null(par_tmp$PCA$PCA_anno_tooltip)){
    adj2colname <- gsub(" ",".",par_tmp$PCA$PCA_anno_tooltip)
    pcaData$chosenAnno <- pcaData[,adj2colname]
    }else{
    pcaData$chosenAnno <- pcaData$global_ID
  }
# Scree Plot calculations
var_explained_df <- data.frame(
    PC = paste0("PC",1:ncol(res_tmp$PCA$x)),
    var_explained = (res_tmp$PCA$sdev)^2/sum((res_tmp$PCA$sdev)^2)
)
var_explained_df$Var <- paste0(round(var_explained_df$var_explained,4)*100,"%")
var_explained_df$PC <- factor(var_explained_df$PC,levels = paste0("PC",1:ncol(res_tmp$PCA$x)))
percentVar <- round(100 * var_explained_df$var_explained, digits = 1)
names(percentVar)<- var_explained_df$PC

# Loadings calculations
LoadingsDF <- data.frame(
  entitie = rownames(res_tmp$PCA$rotation),
  Loading = res_tmp$PCA$rotation[,par_tmp$PCA$x_axis_selection]
)
LoadingsDF <- LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]
LoadingsDF <- rbind(
LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF) - par_tmp$PCA$bottomSlider),],
  LoadingsDF[par_tmp$PCA$topSlider:1,]
)
LoadingsDF$entitie <- factor(LoadingsDF$entitie,levels = rownames(LoadingsDF))
if(!is.null(par_tmp$PCA$EntitieAnno_Loadings)){
  LoadingsDF$entitie=factor(
  make.unique(as.character(rowData(res_tmp$data)[rownames(LoadingsDF),par_tmp$PCA$EntitieAnno_Loadings])),
  levels = make.unique(as.character(rowData(res_tmp$data)[rownames(LoadingsDF),par_tmp$PCA$EntitieAnno_Loadings]))
  )
}

df_loadings <- data.frame(
  entity = row.names(res_tmp$PCA$rotation),
  res_tmp$PCA$rotation[, 1:par_tmp$PCA$nPCAs_to_look_at]
)
df_loadings_filtered <- as.matrix(df_loadings[,-1]) >= abs(par_tmp$PCA$filterValue)
entitiesToInclude <- apply(df_loadings_filtered, 1, any)

df_loadings <- df_loadings[entitiesToInclude,] %>%
  tidyr::gather(key = "PC", value = "loading", -entity)

if(!is.null(par_tmp$PCA$EntitieAnno_Loadings_matrix)){
  df_loadings$chosenAnno <- factor(
    make.unique(as.character(rowData(res_tmp$data)[unique(df_loadings$entity),par_tmp$PCA$EntitieAnno_Loadings_matrix])),
    levels = make.unique(as.character(rowData(res_tmp$data)[unique(df_loadings$entity),par_tmp$PCA$EntitieAnno_Loadings_matrix]))
  )
}else{
  df_loadings$chosenAnno <- df_loadings$entity
}
'
    
    if (numberOfScenario == 1) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = par_tmp$PCA$coloring_options,values=par_tmp$PCA$colorTheme)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 2) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                         y = pcaData[,par_tmp$PCA$y_axis_selection],
                                         color=pcaData[,par_tmp$PCA$coloring_options],
                                         label=global_ID,
                                         global_ID=global_ID,
                                         chosenAnno=chosenAnno)) +
           geom_point(size =3)+
           scale_color_discrete(name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 3) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=par_tmp$PCA$colorTheme,
                       name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 4) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = par_tmp$PCA$coloring_options,values=par_tmp$PCA$colorTheme)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
      
      
    }
    if (numberOfScenario == 5) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$PCA$x_axis_selection],
                                               y = pcaData[,par_tmp$PCA$PCA$y_axis_selection],
                                               color=pcaData[,par_tmp$PCA$PCA$coloring_options],
                                               label=global_ID,
                                               global_ID=global_ID,
                                               chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_discrete(name = par_tmp$PCA$coloring_options)+
    xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
    ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
    coord_fixed()+
    theme_classic()+
    theme(aspect.ratio = 1)+
    ggtitle(par_tmp$PCA$customTitle)+
    geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
    }
    if (numberOfScenario == 6) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=par_tmp$PCA$colorTheme,
                       name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                 aes(x=0, y=0, xend=v1, yend=v2),
                 arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                 #linetype="solid",
                 #alpha=0.5,
                 color="#ab0521")'
    }
### Scree 
    if (numberOfScenario == 7) {
      stringtosave = 'scree_plot=ggplot(var_explained_df,aes(x=PC,y=var_explained, group=1))+
                                  geom_point(size=4,aes(label=Var))+
                                  geom_line()+
                                  ylab("Variance explained")+
                                  theme_bw()+
                                  ggtitle("Scree-Plot for shown PCA")'
    }
### Loadings single
    if (numberOfScenario == 8) {
      stringtosave = 'plotOut=ggplot(LoadingsDF,aes(x = Loading,y = entitie)) +
      geom_col(aes(fill = Loading)) +
      scale_y_discrete(
        breaks = LoadingsDF$entitie,
        labels = stringr::str_wrap(gsub("\\\\.[0-9].*$","",LoadingsDF$entitie),20)) +
      scale_fill_gradient2(low = "#277d6a",mid = "white",high = "orange") +
      ylab(ifelse(is.null(par_tmp$PCA$EntitieAnno_Loadings),"",par_tmp$PCA$EntitieAnno_Loadings)) +
      xlab(paste0("Loadings: ",par_tmp$PCA$x_axis_selection)) +
      theme_bw(base_size = 15)'
    }
### Loadings matrix
  if (numberOfScenario == 8.1) {
    stringtosave =  'LoadingsMatrix <- ggplot(df_loadings,
    aes(x = PC,y = chosenAnno,fill = loading)) +
    geom_raster() +
    scale_fill_gradientn(
    colors = c("#277d6a", "white", "orange"),
    limits = c(-max(df_loadings$loading),max(df_loadings$loading))
    ) +
    labs(x = "PCs", y = "entity", fill = "Loading") +
    theme_bw(base_size = 15)'
  }

  stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave)
    
  }

  
  ## Volcano ----
  if (numberOfScenario == 9) {
    stringtosave='VolcanoPlot <- ggplot(res_tmp$Volcano,
    aes(label=probename,tooltip=annotation_add)) +
    geom_point(aes(x = LFC,y = -log10(p_adj),colour = threshold,alpha = threshold_fc)) +
    geom_hline(
      yintercept = -log10(par_tmp$Volcano$psig_threhsold),
      color="lightgrey"
    ) +
    geom_vline(
      xintercept = c(-par_tmp$Volcano$lfc_threshold,par_tmp$Volcano$lfc_threshold),
      color="lightgrey"
    ) +
    scale_color_manual(values=par_tmp$Volcano$colorScheme, name="")+
    scale_alpha_manual(values=par_tmp$Volcano$alphaScheme, name="")+
    xlab("Log FoldChange")+
    ylab("-log10(p-value)")+
    theme_bw()'
  }
  
  ## Heatmap ----
if(numberOfScenario >= 10  & numberOfScenario <= 11){
  prequel_stringtosave <- '
colorTheme <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c","#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
paletteLength <- 25
myColor_fill <- colorRampPalette(c("blue", "white", "firebrick"))(paletteLength)
  
# select and caluculate Heatmap input depending on users input - 
# check par_tmp$Heatmap for selected options or change accrodingly to what you desire
mycolors <- list()
if(length(par_tmp$Heatmap$anno_options) == 1){
  if(length(unique(colData(res_tmp$data)[,par_tmp$Heatmap$anno_options])) <= 8){
    names(colorTheme) <- unique(colData(res_tmp$data)[,par_tmp$Heatmap$anno_options])
    colorTheme <- colorTheme[!is.na(names(colorTheme))]
    mycolors[[par_tmp$Heatmap$anno_options]] <- colorTheme
  }
}

        
# Do PreSelection of input to Heatmap to show

# selection based on row Annotation:
if(!(any(par_tmp$Heatmap$row_selection_options == "all"))){
  if(any(par_tmp$Heatmap$row_selection_options == "rowAnno_based")){
    additionalInput_row_anno <- ifelse(any(par_tmp$Heatmap$row_selection_options == "rowAnno_based"),"yip",NA)
    if(!is.na(additionalInput_row_anno)){
      additionalInput_row_anno <- par_tmp$Heatmap$anno_options_heatmap
    }
    additionalInput_row_anno_factor <- par_tmp$Heatmap$row_anno_options_heatmap
  }else{
    additionalInput_row_anno <- ifelse(any(par_tmp$Heatmap$row_selection_options == "rowAnno_based"),par_tmp$Heatmap$anno_options_heatmap,NA)
    additionalInput_row_anno_factor <- ifelse(any(par_tmp$Heatmap$row_selection_options == "rowAnno_based"),c(par_tmp$Heatmap$row_anno_options_heatmap),NA)
  }
}else{
  additionalInput_row_anno <- "all"
  additionalInput_row_anno_factor <- NA
}
   
#Selection and/or ordering based on LFC
additionalInput_sample_annotation_types <- ifelse(is.null(par_tmp$Heatmap$sample_annotation_types_cmp_heatmap),NA,par_tmp$Heatmap$sample_annotation_types_cmp_heatmap)
additionalInput_ctrl_idx <- ifelse(is.null(par_tmp$Heatmap$Groups2Compare_ref_heatmap),NA,par_tmp$Heatmap$Groups2Compare_ref_heatmap)
additionalInput_cmp_idx <- ifelse(is.null(par_tmp$Heatmap$Groups2Compare_treat_heatmap),NA,par_tmp$Heatmap$Groups2Compare_treat_heatmap)
psig_threhsold <- ifelse(is.null(par_tmp$Heatmap$psig_threhsold_heatmap),NA,par_tmp$Heatmap$psig_threhsold_heatmap)

# select TopK (if there is an ordering)
TopK2Show <- ifelse(any(par_tmp$Heatmap$row_selection_options=="TopK"),par_tmp$Heatmap$TopK,NA)
        
if(any(par_tmp$Heatmap$row_selection_options=="all")){
  print("No entitie selection")
  data2HandOver <- as.data.frame(assay(res_tmp$data))
}else{
# Note entitieSelection is a custom function
#TODO its source code file should be provided along!
entitieSelection=function(data,
                          type,
                          TopK2Show=NA,
                          additionalInput_row_anno=NA,
                          additionalInput_row_anno_factor=NA,
                          additionalInput_sample_annotation_types=NA,
                          additionalInput_ctrl_idx=NA,
                          additionalInput_cmp_idx=NA,
                          psig_threhsold=NA){
  # to cover: c("TopK","significant_LFC","LFC_onlySig","rowAnno_based")
  filtered_data=assay(data)
  orderMakesSense_flag=FALSE
  print("Entitie Selection")
  #print(additionalInput_row_anno)
  if(any(type=="rowAnno_based") & !(any(is.na(additionalInput_row_anno) &is.na(additionalInput_row_anno_factor))) & !any(additionalInput_row_anno_factor=="all")){
    # Note here this only what to show, LFCs and more importantly multiple test correction will be done on the entire set (without the row anno based selection!!)
    if(any(additionalInput_row_anno_factor=="all")){
      filtered_data = filtered_data
    }else{
      filtered_data = filtered_data[which(data$annotation_rows[,additionalInput_row_anno] %in% additionalInput_row_anno_factor),]
    }
  }
  if(!(is.na(additionalInput_sample_annotation_types)) & !(is.na(additionalInput_ctrl_idx)) & !(is.na(additionalInput_cmp_idx))){
    if(any(type=="significant_LFC")){
      # sort based on significance
      # need LFCs
      # is reachable from here? selectedData_processed()[[input$omicType]]$sample_table
      ctrl_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
      comparison_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
      if((length(ctrl_samples_idx) <= 1) | (length(comparison_samples_idx) <= 1)){
        warning("LFC makes no sense just having a single sample per conidition, which is here the case!")
        filtered_data=NULL
      }else{
        LFC_output=getLFC(filtered_data,ctrl_samples_idx,comparison_samples_idx)
        filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$p_adj,decreasing = F)],,drop=F]
        orderMakesSense_flag=T
      }
      
    }
    if(any(type=="LFC_onlySig")){
      ctrl_samples_idx<-which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
      comparison_samples_idx<-which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
      LFC_output=getLFC(filtered_data,ctrl_samples_idx,comparison_samples_idx)
      if(!(any(LFC_output$p_adj<psig_threhsold))){
        warning("No single entry left! Maybe adjust psig_threhsold_heatmap (but do not put it arbitraly high!)")
        #req(FALSE) -> can we speak from here to output$debug?
        filtered_data=NULL
      }else{
        filtered_data=filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<psig_threhsold)],,drop=F]
        filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$LFC,decreasing = F)],,drop=F]
        orderMakesSense_flag=T
      }
      
    }
  }
  
  if(any(type=="TopK")){
    if(orderMakesSense_flag){
      #assumes the data to be sorted somehow
      if(nrow(filtered_data)>TopK2Show){
        filtered_data=filtered_data[c(1:TopK2Show),,drop=F]
      }else{
        filtered_data=filtered_data
      }
    }else{
      filtered_data=NULL
    }
    
  }
  
  
  
  return(filtered_data)
                          }

# get LFC
getLFC <- function(
  data,
  ctrl_samples_idx,
  comparison_samples_idx,
  completeOutput = FALSE
){
  df <- as.data.frame(data)
  # Todo by @Lea: discuss and finalize how to handle this. constant row are not removed but small noise is added should in here a check if all 0 rows?
  ttest_raw <- function(df, grp1, grp2) {
    x <- df[grp1]
    y <- df[grp2]
    x <- as.numeric(x)
    y <- as.numeric(y)
    results <- t.test(x, y)
    return(results$p.value)
  }
  #remove constant rows
  removedAsConst_1 <- which(apply(df[,ctrl_samples_idx],1,sd) < 1e-6)
  df[removedAsConst_1,ctrl_samples_idx] <- df[removedAsConst_1,ctrl_samples_idx] + t(apply(df[removedAsConst_1,ctrl_samples_idx],1,function(x){
    rnorm(
      n = length(x),
      mean = 0,
      sd=0.0000001
    )}))
  
  removedAsConst_2 <- which(apply(df[,comparison_samples_idx],1,sd) < 1e-6)
  df[removedAsConst_2,comparison_samples_idx] <- df[removedAsConst_2,comparison_samples_idx] + t(apply(df[removedAsConst_2,comparison_samples_idx],1,function(x){
    rnorm(
      n = length(x),
      mean = 0,
      sd=0.0000001
    )}))
  
  
  rawpvalue <- apply(df, 1, ttest_raw, grp1 = ctrl_samples_idx, grp2 = comparison_samples_idx)
  
  p_adj <- p.adjust(rawpvalue, method = "fdr")
  
  Ctrl_mean <- apply(df[,ctrl_samples_idx],1,mean)
  Cmp_mean <- apply(df[,comparison_samples_idx],1,mean)
  
  FC <- Cmp_mean/Ctrl_mean
  
  LFC <- log2(FC)
  
  # Data 2 Plot
  results <- cbind(LFC, rawpvalue,p_adj)
  results <- as.data.frame(results)
  results$probename <- rownames(results)
  if(completeOutput){
    # report results table + inital values that where used to calculate (mostly
    # for sainity checks)
    colnames(df)[ctrl_samples_idx]=paste0(colnames(df)[ctrl_samples_idx],"_ctrl")
    colnames(df)[comparison_samples_idx]=paste0(colnames(df)[comparison_samples_idx],"_cmp")
    results=cbind(results,df[rownames(df),])
  }
  return(results)
}


  data2HandOver <- entitieSelection(
    res_tmp$data,
    type = par_tmp$Heatmap$row_selection_options,
    additionalInput_row_anno = additionalInput_row_anno,
    additionalInput_row_anno_factor = additionalInput_row_anno_factor,
    additionalInput_sample_annotation_types = additionalInput_sample_annotation_types,
    additionalInput_ctrl_idx = additionalInput_ctrl_idx,
    additionalInput_cmp_idx = additionalInput_cmp_idx,
    psig_threhsold = psig_threhsold,
    TopK2Show = TopK2Show
  )
}
        
doThis_flag <- T
if(is.null(data2HandOver)){
  print("Nothing is left,e.g. no significant Terms or TopK is used but no inherent order of the data")
  heatmap_plot <- NULL
  doThis_flag <- F
}
'
  
  if(numberOfScenario == 10){
    stringtosave <- '
annotation_col <- rowData(res_tmp$data)[,par_tmp$Heatmap$row_anno_options,drop=F]

ctrl_samples_idx <- which(
    colData(res_tmp$data)[,par_tmp$Heatmap$sample_annotation_types_cmp_heatmap]%in%par_tmp$Heatmap$Groups2Compare_ref_heatmap
    )
comparison_samples_idx <- which(
  colData(res_tmp$data)[,par_tmp$Heatmap$sample_annotation_types_cmp_heatmap]%in%par_tmp$Heatmap$Groups2Compare_treat_heatmap
)
if(length(comparison_samples_idx) <=1 | length(ctrl_samples_idx) <=1){
  print("Choose variable with at least two samples per condition!")
  doThis_flag <- F
}

if(par_tmp$PreProcessing_Procedure == "simpleCenterScaling"| any(data2HandOver)< 0){
  print("Remember do not use normal center + scaling (negative Values!)")
}else if(doThis_flag){
  Data2Plot <- getLFC(
    data = as.data.frame(data2HandOver),
    ctrl_samples_idx = ctrl_samples_idx,
    comparison_samples_idx = comparison_samples_idx
  )
              
if(par_tmp$LFC_toHeatmap){
  myBreaks <- c(seq(min(res_tmp$Heatmap$LFC), 0, length.out=ceiling(paletteLength/2) + 1),
                seq(max(res_tmp$Heatmap$LFC)/paletteLength, max(res_tmp$Heatmap$LFC), length.out=floor(paletteLength/2)))
  annotation_col <- rowData(Data2Plot)[,par_tmp$row_anno_options,drop=F]
}


heatmap_plot <- pheatmap((t(Data2Plot[,"LFC",drop=F])),
  main="Heatmap - LFC",
  show_rownames=ifelse(nrow((assay(res_tmp$data))<=25,TRUE,FALSE),
  show_colnames=TRUE,
  cluster_cols = par_tmp$Heatmap$cluster_cols,
  cluster_rows = FALSE, # par_tmp$Heatmap$cluster_rows,
  scale=ifelse(par_tmp$Heatmap$rowWiseScaled,"row","none"),
  # cutree_cols = 4,
  #fontsize = font.size,
  annotation_col = res_tmp$data[[par_tmp$Heatmap$omicType]]$annotation_rows[,par_tmp$Heatmap$row_anno_options,drop=F],
  #annotation_row = res_tmp$data[[par_tmp$Heatmap$omicType]]$annotation_rows[,par_tmp$Heatmap$row_anno_options,drop=F],
  silent = F,
  breaks = myBreaks,
  color = myColor_fill)'
  }
  if(numberOfScenario == 11){
    stringtosave <- '
annotation_col <- colData(res_tmp$data)[,par_tmp$Heatmap$anno_options,drop=F]
annotation_row <- rowData(res_tmp$data)[,par_tmp$Heatmap$row_anno_options,drop=F]
# convert both to data.frame
annotation_col <- as.data.frame(annotation_col)
annotation_row <- as.data.frame(annotation_row)

clusterRowspossible <- ifelse(nrow(as.matrix(assay(res_tmp$data)))>1,par_tmp$Heatmap$cluster_rows,F)

heatmap_plot <- pheatmap(as.matrix(res_tmp$Heatmap),
  main="Heatmap",
  show_rownames=ifelse(nrow((assay(res_tmp$data)))<=par_tmp$Heatmap$row_label_no,TRUE,FALSE),
  labels_row = rowData(res_tmp$data)[rownames(assay(res_tmp$data)),par_tmp$Heatmap$row_label_options],
  show_colnames=TRUE,
  cluster_cols = par_tmp$Heatmap$cluster_cols,
  cluster_rows = clusterRowspossible,
  scale=ifelse(par_tmp$Heatmap$rowWiseScaled,"row","none"),
  # cutree_cols = 4,
  #fontsize = font.size,
  annotation_col = annotation_col,
  annotation_row =annotation_row,
  annotation_colors = mycolors,
  silent = F)'
  }
stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave)
}



## Single Gene Visualisation ----
if(numberOfScenario %in% c(12,13)){
  if(par_tmp$SingleEntVis$type_of_data_gene == "preprocessed"){
    prequel_stringtosave <- '#get IDX to data
idx_selected <- which(par_tmp$SingleEntVis$Select_Gene == rowData(res_tmp$data)[,par_tmp$SingleEntVis$Select_GeneAnno])
GeneData <- as.data.frame(t(as.data.frame(assay(res_tmp$data))[idx_selected,,drop=F]))
GeneData$anno <- colData(res_tmp$data)[,par_tmp$SingleEntVis$accross_condition]
if(length(idx_selected)>1){
  # summarise the data
  GeneData_medians <- rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
  GeneData <- GeneData[,ncol(GeneData),drop=F]
  GeneData$rowMedian <- GeneData_medians
  GeneData <- GeneData[,c("rowMedian","anno")]
}
GeneData$anno <- as.factor(GeneData$anno)
    '
  }else if(par_tmp$SingleEntVis$type_of_data_gene == "raw" ){
    prequel_stringtosave <- '#get IDX to data
idx_selected <- which(par_tmp$SingleEntVis$Select_Gene == rowData(res_tmp$data_original)[,par_tmp$SingleEntVis$Select_GeneAnno])
GeneData <- as.data.frame(t(assay(res_tmp$data_original)[idx_selected,,drop=F]))
GeneData$anno <- colData(res_tmp$data_original)[,par_tmp$SingleEntVis$accross_condition]
# select to selection of processed data
annoToSelect=unique(c(colData(res_tmp$data)[,par_tmp$SingleEntVis$accross_condition]))
GeneData = subset(GeneData, anno %in% annoToSelect)
if(length(idx_selected)>1){
  # summarise the data
  GeneData_medians <- rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
  GeneData <- GeneData[,ncol(GeneData),drop=F]
  GeneData$rowMedian <- GeneData_medians
  GeneData <- GeneData[,c("rowMedian","anno")]
  }
GeneData$anno <- as.factor(GeneData$anno)
    '
  }

  if (numberOfScenario == 12) {
    stringtosave = '# GeneData now contains the same as res_tmp$SingleEntVis
P_boxplots <- ggplot(res_tmp$SingleEntVis, 
  aes(y=res_tmp$SingleEntVis[,colnames(res_tmp$SingleEntVis)[-ncol(res_tmp$SingleEntVis)]],
      x=anno,
      fill=anno))+
  geom_boxplot()+ # unable if less then 4 samples in all groups to get the same plot as in the App
  geom_point(shape = 21,size=5)+
  scale_fill_brewer(palette="RdBu")+
  xlab(par_tmp$SingleEntVis$Select_Gene)+
  ylab(par_tmp$SingleEntVis$type_of_data_gene)+
  theme_bw()+
  geom_hline(yintercept = mean(res_tmp$SingleEntVis[,colnames(res_tmp$SingleEntVis)[-ncol(res_tmp$SingleEntVis)]]), linetype = 2)+ # Add horizontal line at base mean
  #stat_compare_means(method = "anova")+        # Add global annova p-value
  stat_compare_means(comparisons = par_tmp$SingleEntVis$chooseComparisons_list,
                     method = par_tmp$SingleEntVis$testMethod,
                     label = "p.signif",
                     hide.ns = TRUE)'
  }
  if (numberOfScenario == 13) {
    stringtosave = '# GeneData now contains the same as res_tmp$SingleEntVis
P_boxplots <- ggplot(res_tmp$SingleEntVis, 
  aes(y=res_tmp$SingleEntVis[,colnames(res_tmp$SingleEntVis)[-ncol(res_tmp$SingleEntVis)]],
      x=anno,
      fill=anno))+
  geom_boxplot()+# unable if less then 4 samples in all groups to get the same plot as in the App
  geom_point(shape = 21,size=5)+
  scale_fill_brewer(palette="RdBu")+
  xlab(par_tmp$SingleEntVis$Select_Gene)+
  ylab(par_tmp$SingleEntVis$type_of_data_gene)+
  theme_bw()'
  }
  stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave)
}
 
  ## TODO ensure this remains working with new output from Enrichment, needs a potential update!
  if(numberOfScenario == 14){
    stringtosave = 'KEGG_Plot_GSE=clusterProfiler::dotplot(EnrichmentRes_Kegg,split=".sign") +
                      facet_grid(.~.sign)'
  }
  if(numberOfScenario==15){
    stringtosave = 'KEGG_Plot_ORA=clusterProfiler::dotplot(EnrichmentRes_Kegg)'
  }
  if(numberOfScenario==16){
    stringtosave='GO_Plot=clusterProfiler::dotplot(EnrichmentRes_GO)'
  }
  if(numberOfScenario == 17){
    stringtosave='REACTOME_Plot=clusterProfiler::dotplot(EnrichmentRes_RACTOME)'
  }

## Sample Correlation plot ----
  if(numberOfScenario == 18){
    stringtosave = 'annotationDF <- colData(res_tmp$data)[,par_tmp$SampleCorr$SampleAnnotationChoice,drop = F]
cormat <- cor(
  x = as.matrix(assay(res_tmp$data)),
  method = par_tmp$SampleCorr$corrMethod
)

SampleCorrelationPlot <- pheatmap(
mat = cormat, #res_tmp$SampleCorr
annotation_row = par_tmp$SampleCorr$annotationDF,
main = par_tmp$SampleCorr$customTitleSampleCorrelation,
annotation_colors = par_tmp$SampleCorr$anno_colors
)'
  }
## Significance Analysis -----
### Venn Diagram ----
  if(numberOfScenario == 20){
    stringtosave <- 'VennDiagramm <- ggVennDiagram::ggVennDiagram(res_tmp$SignificanceAnalysis)'
  }
### Upset plot ----
  if(numberOfScenario == 21){
    stringtosave <- 'UpSetR::upset(fromList(res_tmp$SignificanceAnalysis))'
  }

### Volcano ----





  if(numberOfScenario == 0){
    stringtosave <- '# No_code_yet'
  }
  
  return(paste0(CODE_DOWNLOAD_PREFACE,
                "\n",
                stringSelection,
                CODE_DOWNLOAD_SELECTION,
                "\n",
                CODE_DOWNLOAD_PREPROCESSING,
                "\n",
                stringPreProcessing,
                "\n",
                stringtosave))
}
