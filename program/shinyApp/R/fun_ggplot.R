
getPlotCode <- function(numberOfScenario) {
  initalString= "# ShinyOmics R Code Download
# Load necassary packages (if errors please install respective packages)
library(ggplot2)
library(ggpubr)
library(rstudioapi)
# if not run in RStudio  you need to specify the directory fo the file yourself!

direcoty_of_files=dirname(rstudioapi::getSourceEditorContext()$path)
envList=readRDS(paste0(direcoty_of_files,"/",'Data.rds'))  

list2env(envList,envir = globalenv())

# Happy Adjusting! :)"
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
  if(numberOfScenario==14){
    stringtosave = 'KEGG_Plot_GSE=clusterProfiler::dotplot(EnrichmentRes_Kegg,split=".sign") +
                      facet_grid(.~.sign)'
  }
  if(numberOfScenario==15){
    stringtosave = 'KEGG_Plot_ORA=clusterProfiler::dotplot(EnrichmentRes_Kegg)'
  }
  if(numberOfScenario==16){
    stringtosave='GO_Plot=clusterProfiler::dotplot(EnrichmentRes_GO)'
  }
  if(numberOfScenario==17){
    stringtosave='REACTOME_Plot=clusterProfiler::dotplot(EnrichmentRes_RACTOME)'
  }
  
  return(paste0(initalString,"\n",stringtosave))
}
