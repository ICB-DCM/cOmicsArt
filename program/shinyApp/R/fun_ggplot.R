
getPlotCode <- function(numberOfScenario) {
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
  
  if (numberOfScenario == 9) {
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
  if (numberOfScenario == 10) {
    stringtosave = 'P_boxplots=ggplot(GeneData, aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],x=anno,fill=anno))+
    geom_boxplot()+
    scale_fill_brewer(palette="RdBu")+
    xlab(input$Select_Gene)+
    ylab(input$type_of_data_gene)+
    theme_bw()'
  }
  if (numberOfScenario == 11) {
    stringtosave = 'P_boxplots=ggplot() + theme_void()'
  }
  return(stringtosave)
}
