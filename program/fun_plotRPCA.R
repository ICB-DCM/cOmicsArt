plotRPCA <- function(pca_input = dds_vst,
                     ntop="all", 
                     xPC=1, 
                     yPC=2,
                     color,
                     anno_colour,
                     sample_table,
                     shape="MaternalDiet",
                     point_size=3,
                     title="PCA", 
                     label = NULL, 
                     label_subset = NULL,
                     plotLoadingsFlag=F){
  
  if(!(shape%in%colnames(sample_table))){
    if(!(shape%in%colnames(sample_table)) & "Maternal.diet"%in%colnames(sample_table)){
      shape="Maternal.diet"
    }else{
      print(colnames(sample_table))
      shape="NULL"
    }
  }
  
  if(!is.data.frame(pca_input)){
    vst_matrix <-as.matrix(SummarizedExperiment::assay(pca_input))
  }else{
    vst_matrix <- as.matrix(pca_input)
  }
  
  if(ntop=="all"){
    #vst_matrix_centered <- sweep(t(vst_matrix),2,colMeans(t(vst_matrix)))
    pca <- rpca(t(vst_matrix),max.iter = 100000)
    #pca <- rpca(t(vst_matrix),max.iter = 100000)
  }else{
    # select the ntop genes by variance
    #vst_matrix_centered <- sweep(t(vst_matrix),2,colMeans(t(vst_matrix)))
    select <- order(rowVars(vst_matrix), decreasing=TRUE)[c(1:ntop)]
    pca <- rpca(t(vst_matrix[select,]),max.iter = 100000)
  }
  
  rpc<-pca$L.svd$u%*%diag(pca$L.svd$d)
  print(paste0("Is it converged: ", pca$convergence$converged))
  #calculate explained variance per PC
  explVar <- pca$L.svd$d^2/sum(pca$L.svd$d^2)
  # transform variance to percent
  percentVar <- round(100 * explVar[c(xPC,yPC)], digits=1)
  
  # Define data for plotting  
  pcaData <- data.frame(xPC=rpc[,xPC], 
                        yPC=rpc[,yPC], 
                        color = sample_table[[color]],
                        ID= as.character(sample_table$global_ID),
                        stringsAsFactors = F)
  # Order PCA Data in fixed order
  pcaData$color=factor(pcaData$color,
                       levels = names(anno_colour))
  
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
    
    if(anno_colour[1] == "NULL"){
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
  
  if(FALSE){#default:  TO DO plotLoadingsFlag
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
                          v1 = .7 * mult * (get(paste0("PC",xPC))),
                          v2 = .7 * mult * (get(paste0("PC",yPC)))
    )
    #pca_plot <- pca_plot +
    #  geom_text(data=df_out_r, aes(x=v1, y=v2, label=feature), size = 4, vjust=1,
    #                  color="#ab0521",
    #                  segment.color = '#cccccc',
    #                  segment.size = 0.5)
    df_out_r$ID=rownames(df_out_r)
    pca_plot <- pca_plot + geom_segment(data=df_out_r[which(df_out_r$feature!=""),], aes(x=0, y=0, xend=v1, yend=v2), 
                                        arrow=arrow(length=unit(1,"cm")),linetype="solid", alpha=0.5, color="#ab0521")
  }
  
  pca_plot
}
