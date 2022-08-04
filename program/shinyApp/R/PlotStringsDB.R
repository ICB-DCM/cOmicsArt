getPlotCode<-function(numberOfScenario){
  if(numberOfScenario==1){
    stringtosave='pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
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
  if(numberOfScenario==2){
    stringtosave='pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
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
  if(numberOfScenario==3){
    stringtosave='pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
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
  if(numberOfScenario==4){
    stringtosave=0
  }
  if(numberOfScenario==5){
    stringtosave='pca_plot <- ggplot(pcaData, aes(x = pcaData[,input$x_axis_selection],
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
  if(numberOfScenario==7){
    stringtosave='    scree_plot=ggplot(var_explained_df,aes(x=PC,y=var_explained, group=1))+
      geom_point(size=4,aes(label=Var))+
      geom_line()+
      ylab("Variance explained")+
      theme_bw()+
      ggtitle("Scree-Plot for shown PCA")'
  }
  return(stringtosave)
}
