
getPlotCode <- function(
    numberOfScenario,
    preProcessing_Snippet = par_tmp$PreProcessing_Procedure,
    row_selection = par_tmp$row_selection,
    col_selection = par_tmp$col_selection) {
  #TODO  change all data download to par_tmp and res_tmp
 # Selection ----
  if(par_tmp$row_selection == "all"){
    stringSelection <- 'selected <- rownames(rowData(res_tmp$data_original))
    '
  }else{
    if(!(length(par_tmp$row_selection) == 1 & any(par_tmp$row_selection == "High Values+IQR"))){
    stringSelection <- 'selected <- c()
    selected <- unique(
      c(selected,rownames(rowData(res_tmp$data_original))[
          which(rowData(res_tmp$data_original)
          [,par_tmp$providedRowAnnotationTypes]%in%par_tmp$row_selection)
        ]
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
    stringSelection <- paste0(stringSelection,
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
      res_tmp$DESeq_obj <<- de_seq_result
      dds_vst <- vst(
      object = de_seq_result,
      blind = TRUE
      )
      assay(res_tmp$data) <<- as.data.frame(assay(dds_vst))
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
    #TODO check if 0 then do ln (+1)
    if(input$PreProcessing_Procedure == "ln"){
      stringPreProcessing <- 'processedData <- as.data.frame(log(
        as.data.frame(assay(res_tmp$data))
      ))
      assay(res_tmp$data) <<- as.data.frame(processedData)
      '
      
    }
    if(input$PreProcessing_Procedure == "log10"){
      processedData <- as.data.frame(assay(res_tmp$data))
      if(any(processedData<0)){
        addWarning <- "<font color=\"#FF0000\"><b>Negative entries, cannot take log10!!</b></font>"
      }
      if(any(processedData==0)){
        processedData <- as.data.frame(log10(
          processedData + 1)
        )
      }
      processedData <- as.data.frame(log10(
        processedData + 1)
      )
      assay(res_tmp$data) <<- as.data.frame(processedData)
    }
    if(input$PreProcessing_Procedure == "pareto_scaling"){
      processedData <- as.data.frame(assay(res_tmp$data))
      centered <- as.data.frame(t(
        apply(processedData, 1, function(x){x - mean(x)})
      ))
      pareto.matrix <- as.data.frame(t(
        apply(centered, 1, function(x){x/sqrt(sd(x))})
      ))
      
      assay(res_tmp$data) <<- as.data.frame(pareto.matrix)
    }

    stringPreProcessing <- paste0(prequel_stringPreProcessing,"\n",stringPreProcessing)
    }
    

  
  # if(par_tmp$omic_type == "Transcriptomics"){
  #   print("Also remove anything of rowCount <=10")
  #   print(dim(tmp_data_selected))
  #   res_tmp$data <<- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]
  # }
  
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  if(preProcessing_Snippet == "none"){
    
  }
  
    # Plot Code ----
  if (numberOfScenario == 1) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                  y = pcaData[,input$y_axis_selection],
                                  color=pcaData[,input$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = input$coloring_options,values=colorTheme)+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)'
  }
  if (numberOfScenario == 2) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                         y = pcaData[,input$y_axis_selection],
                                         color=pcaData[,input$coloring_options],
                                         label=global_ID,
                                         global_ID=global_ID,
                                         chosenAnno=chosenAnno)) +
           geom_point(size =3)+
           scale_color_discrete(name = input$coloring_options)+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)'
  }
  if (numberOfScenario == 3) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                  y = pcaData[,input$y_axis_selection],
                                  color=pcaData[,input$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=colorTheme,
                       name = input$coloring_options)+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)'
  }
  if (numberOfScenario == 4) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                  y = pcaData[,input$y_axis_selection],
                                  color=pcaData[,input$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = input$coloring_options,values=colorTheme)+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
    
    
  }
  if (numberOfScenario == 5) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                               y = pcaData[,input$y_axis_selection],
                                               color=pcaData[,input$coloring_options],
                                               label=global_ID,
                                               global_ID=global_ID,
                                               chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_discrete(name = input$coloring_options)+
    xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
    ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
    coord_fixed()+
    theme_classic()+
    theme(aspect.ratio = 1)+
    ggtitle(customTitle)+
    geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
  }
  
  if (numberOfScenario == 6) {
    stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
                                  y = pcaData[,input$y_axis_selection],
                                  color=pcaData[,input$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=colorTheme,
                       name = input$coloring_options)+
      xlab(paste0(names(percentVar[input$x_axis_selection]),": ",percentVar[input$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[input$y_axis_selection]),": ", percentVar[input$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                 aes(x=0, y=0, xend=v1, yend=v2),
                 arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                 #linetype="solid",
                 #alpha=0.5,
                 color="#ab0521")'
  }
  
  if (numberOfScenario == 7) {
    stringtosave = 'scree_plot=ggplot(var_explained_df,aes(x=PC,y=var_explained, group=1))+
                                  geom_point(size=4,aes(label=Var))+
                                  geom_line()+
                                  ylab("Variance explained")+
                                  theme_bw()+
                                  ggtitle("Scree-Plot for shown PCA")'
  }
  if (numberOfScenario == 8) {
    stringtosave = 'plotOut=ggplot(LoadingsDF,aes(x=Loading,y=entitie))+
      geom_col(aes(fill=Loading))+
      scale_y_discrete(breaks=LoadingsDF$entitie,labels=gsub("\\.[0-9].*$","",LoadingsDF$entitie))+
      scale_fill_gradient2(low="#277d6a",mid="white",high="orange")+
      ylab(ifelse(is.null(input$EntitieAnno_Loadings),"",input$EntitieAnno_Loadings))+
      xlab(paste0("Loadings: ",input$x_axis_selection))+
      theme_bw(base_size = 20)'
  }
  # Volcano Missing
  if (numberOfScenario == 9) {
    stringtosave='VolcanoPlot=ggplot(VolcanoPlot_df,aes(label=probename)) +
                                geom_point(aes(x = LFC, y = -log10(p_adj), colour = threshold,alpha=threshold_fc))+
                                geom_hline(yintercept=-log10(input$psig_threhsold),color="lightgrey")+
                                geom_vline(xintercept = c(-input$lfc_threshold,input$lfc_threshold),color="lightgrey")+
                                scale_color_manual(values=colorScheme, name="")+
                                scale_alpha_manual(values=alphaScheme, name="")+
                                xlab("Log FoldChange")+
                                theme_bw()'
  }
  
  ## 2 heatmaps
  if(numberOfScenario==10){
    stringtosave='heatmap_plot<-pheatmap((t(Data2Plot[,"LFC",drop=F])),
                           main=gsub("^Heatmap","Heatmap_LFC",customTitleHeatmap),
                           show_rownames=ifelse(nrow(Data2Plot)<=25,TRUE,FALSE),
                           show_colnames=TRUE,
                           cluster_cols = input$cluster_cols,
                           cluster_rows = FALSE, # input$cluster_rows,
                           scale=ifelse(input$rowWiseScaled,"row","none"),
                           # cutree_cols = 4,
                           #fontsize = font.size,
                           annotation_col = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
                           #annotation_row = data2Plot[[input$omicType]]$annotation_rows[,input$row_anno_options,drop=F],
                           #annotation_colors = mycolors,
                           silent = F,
                           breaks = myBreaks,
                           color = myColor_fill)'
  }
  if(numberOfScenario==11){
    stringtosave='heatmap_plot<-pheatmap(as.matrix(data2HandOver),
                           main=customTitleHeatmap,
                           show_rownames=ifelse(nrow(data2HandOver)<=input$row_label_no,TRUE,FALSE),
                           labels_row = selectedData_processed_df[[input$omicType]]$annotation_rows[rownames(data2HandOver),input$row_label_options],
                           show_colnames=TRUE,
                           cluster_cols = input$cluster_cols,
                           cluster_rows = clusterRowspossible,
                           scale=ifelse(input$rowWiseScaled,"row","none"),
                           # cutree_cols = 4,
                           #fontsize = font.size,
                           annotation_col = annotation_col,
                           annotation_row =annotation_row,
                           annotation_colors = mycolors,
                           silent = F)'
  }
  #11
  
  if (numberOfScenario == 12) {
    stringtosave = 'P_boxplots=ggplot(GeneData, aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],x=anno,fill=anno))+
    geom_boxplot()+
    scale_fill_brewer(palette="RdBu")+
    xlab(input$Select_Gene)+
    ylab(input$type_of_data_gene)+
    theme_bw()+
        geom_hline(yintercept = mean(GeneData[,colnames(GeneData)[-ncol(GeneData)]]), linetype = 2)+ # Add horizontal line at base mean
        #stat_compare_means(method = "anova")+        # Add global annova p-value
        stat_compare_means(comparisons = xy.list,
                           method = testMethod,

                 label = "p.signif",
                           hide.ns = TRUE)'
  }
  if (numberOfScenario == 13) {
    stringtosave = 'P_boxplots=ggplot(GeneData, aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],x=anno,fill=anno))+
    geom_boxplot()+
    scale_fill_brewer(palette="RdBu")+
    xlab(input$Select_Gene)+
    ylab(input$type_of_data_gene)+
    theme_bw()'
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
  if(numberOfScenario == 18){
    stringtosave = 'SampleCorrelationPlot_final <- pheatmap(
          mat = cormat, 
          annotation_row = annotationDF,
          main = customTitleSampleCorrelation,
          annotation_colors = anno_colors
          )'
  }
  if(numberOfScenario == 19){
    stringtosave = 'LoadingsMatrix <- ggplot(
          df_loadings,
          aes(
            x = PC,
            y = entitie,
            fill = loading)
          ) +
          geom_raster() +
          scale_fill_gradientn(
            colors = c("#277d6a", "white", "orange"),
            limits = c(global_min,global_max)
          ) +
          labs(x = "PCs", y = "entity", fill = "Loading") +
          theme_bw(base_size = 15)'
  }
  if(numberOfScenario == 20){
    stringtosave <- 'VennDiagramm <- ggVennDiagram::ggVennDiagram(res2plot)'
  }
  if(numberOfScenario == 21){
    stringtosave <- 'UpSetR::upset(fromList(res2plot))'
  }
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
