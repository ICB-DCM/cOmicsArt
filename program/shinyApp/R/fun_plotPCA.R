plotPCA <- function(pca_input = dds_vst,
                    ntop="all", 
                    xPC=1, 
                    yPC=2,
                    color,
                    anno_colour="default",
                    sample_table,
                    shape="MaternalDiet",
                    point_size=3,
                    title="PCA", 
                    label = NULL, 
                    label_subset = NULL,
                    plotLoadingsFlag=F,
                    onlyPlot=F){
  print("Start PLotting")
  if(!(shape%in%colnames(sample_table))){
    if(!(shape%in%colnames(sample_table)) & "Maternal.diet"%in%colnames(sample_table)){
      shape="Maternal.diet"
    }else{
      print(colnames(sample_table))
      shape="NULL"
    }
  }
  if(color=="Merged"|color=="merged"){
    anno_colour=col_Merged
  }
  
  if(xPC=="idx"){
    #print("DO WE COME TILL HERE?")
    # xPC="global_ID"
    pca_input$x[,"idx"]=as.character(sample_table$global_ID)
    
    yPC=as.numeric(yPC)
    pca_input$std.dev=c(0,pca_input$std.dev)
    names(pca_input$std.dev)=c("idx",1:(length(pca_input$std.dev)-1))
  }else{
    xPC=as.numeric(xPC)
    yPC=as.numeric(yPC)
  }
  if(onlyPlot==T){
    pca=list()
    pca$sdev=pca_input$std.dev # mit Vorsicht zu genie?en
    plotLoadingsFlag=F # set to False - cannot be plotted yet TO DO
    pca$x=pca_input$x
    percentVar <- round(100 * pca_input$std.dev[c(xPC,yPC)], digits=1)
    print("Specific Plot part 2")
  }else{
    if(!is.data.frame(pca_input)){
      print("Do We us this here?")
      vst_matrix <-as.matrix(SummarizedExperiment::assay(pca_input))
    }else{
      vst_matrix <- pca_input
    }
    
    if(ntop=="all"){
      #vst_matrix_centered <- sweep(t(vst_matrix),2,colMeans(t(vst_matrix))) # Centering
      pca <- prcomp(t(vst_matrix),center = T, scale. = T) # TRUE OR FALSE?
    }else{
      #vst_matrix_centered <- sweep(t(vst_matrix),2,colMeans(t(vst_matrix))) # Centering
      # select the ntop genes by variance
      select <- order(rowVars(vst_matrix), decreasing=TRUE)[c(1:ntop)]
      pca <- prcomp(t(vst_matrix[select,]),center = T, scale. = T) # TRUE OR FALSE?
    }
    #calculate explained variance per PC
    explVar <- pca$sdev^2/sum(pca$sdev^2)
    # transform variance to percent
    percentVar <- round(100 * explVar[c(xPC,yPC)], digits=1)
  }
  
  # set Up annocolor if not supplied
  
  print(sample_table[[color]])
  
  # Define data for plotting  
  pcaData <- data.frame(xPC=pca$x[,xPC], 
                        yPC=pca$x[,yPC], 
                        color = sample_table[[color]],
                        ID= as.character(sample_table$global_ID),
                        stringsAsFactors = F)
  #print("DO WE COME HERE")
  #print(anno_colour)
  print(unique(pcaData$color))
  if(anno_colour[1] != "default"){
    # Order PCA Data in fixed order
    pcaData$color=factor(pcaData$color,
                         levels = names(anno_colour))
  }
  
  #plot PCA
  if(is.factor(pcaData$color) || is.character(pcaData$color)|| is.integer(pcaData$color)){
    if(shape == "NULL"){
      pca_plot <- ggplot(pcaData, aes(x = xPC, y = yPC, colour=color,label=ID)) +
        geom_point(size =point_size)
    }else{
      pcaData$shape = sample_table[[shape]]
      pca_plot <- ggplot(pcaData, aes(x = xPC, y = yPC, colour=color, shape=shape,label=ID)) +
        geom_point(size =point_size) +
        scale_shape_discrete(name=shape)
    }
    
    if(anno_colour[1] == "NULL" || anno_colour[1] == "default"){
      pca_plot <- pca_plot + scale_color_discrete(name=color)
    }else{
      pca_plot <- pca_plot + scale_color_manual(values=anno_colour, name=color)
    }
    
  }else if(is.numeric(pcaData$color)){
    if(shape == "NULL"){
      pca_plot <- ggplot(pcaData, aes(x = xPC, y = yPC, colour=color)) +
        geom_point(size =point_size) +
        scale_color_gradientn(colours = bluered(100),name=color)
    }else{
      pcaData$shape = sample_table[[shape]]
      pca_plot <- ggplot(pcaData, aes(x = xPC, y = yPC, colour=color, shape=shape)) +
        geom_point(size =point_size) +
        scale_color_gradientn(colours = bluered(100),name=color)+
        scale_shape_discrete(name=shape)
    }
  }
  
  # adds a label to the plot. To label only specific points, put them in the argument label_subset
  if (!is.null(label) == TRUE){
    pcaData$label <- sample_table[[label]]
    if(!is.null(label_subset) == TRUE){
      pcaData_labeled <- pcaData[pcaData$label %in% label_subset,]
    } else {
      pcaData_labeled <- pcaData
    }
    pca_plot <- pca_plot + 
      geom_text_repel(data = pcaData_labeled, aes(label = label),nudge_x = 1,nudge_y = 1,colour = "black") 
  }
  
  pca_plot <- pca_plot+
    xlab(paste0("PC ",xPC, ": ", percentVar[1], "% variance")) +
    ylab(paste0("PC ",yPC,": ", percentVar[2], "% variance")) +
    coord_fixed()+
    theme_classic()+        
    theme(aspect.ratio = 1)+
    ggtitle(title)
  
  if(plotLoadingsFlag){
    df_out=pca$x
    df_out_r <- as.data.frame(pca$rotation)
    df_out_r$feature <- row.names(df_out_r)
    
    TopK=rownames(df_out_r)[order(sqrt((df_out_r[,paste0("PC",xPC)])^2+(df_out_r[,paste0("PC",yPC)])^2),decreasing = T)[1:5]]
    df_out_r$feature[!df_out_r$feature%in%TopK]=""
    
    mult <- min(
      (max(df_out[,paste0("PC",yPC)]) - min(df_out[,paste0("PC",yPC)])/(max(df_out_r[,paste0("PC",yPC)])-min(df_out_r[,paste0("PC",yPC)]))),
      (max(df_out[,paste0("PC",xPC)]) - min(df_out[,paste0("PC",xPC)])/(max(df_out_r[,paste0("PC",xPC)])-min(df_out_r[,paste0("PC",xPC)])))
    )
    df_out_r <- transform(df_out_r,
                          v1 = 1 * mult * (get(paste0("PC",xPC))),
                          v2 = 1 * mult * (get(paste0("PC",yPC)))
    )
    #pca_plot <- pca_plot +
    #  geom_text(data=df_out_r, aes(x=v1, y=v2, label=feature), size = 4, vjust=1,
    #                  color="#ab0521",
    #                  segment.color = '#cccccc',
    #                  segment.size = 0.5)
    df_out_r$ID=rownames(df_out_r)
    pca_plot2 <- pca_plot + geom_segment(data=df_out_r[which(df_out_r$feature!=""),], aes(x=0, y=0, xend=v1, yend=v2), 
                                         arrow=arrow(length=unit(1,"cm")),linetype="solid", alpha=0.5, color="#ab0521")
  }
  
  pca_plot
}