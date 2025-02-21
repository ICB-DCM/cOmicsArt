plot_pca <- function(
  pca, pcaData, data, percentVar, x_axis, y_axis, color_by, title, show_loadings, plot_ellipses, entitie_anno, tooltip_var
){
  # Plot the PCA plot using the principal components chosen in x_axis and y_axis.
  # Parameters:
  #   pca: PCA object, generated from the data with prcomp
  #   pcaData: data.frame, data with the PCA data
  #   percentVar: numeric, percentage of variance explained by each PC
  #   x_axis: str, name of the column in pcaData to use as x-axis, "PC1" or other PCs
  #   y_axis: str, name of the column in pcaData to use as y-axis, "PC2" or other PCs
  #   color_by: str, name of the sample data to group the samples by
  #   title: str, title of the plot
  #   show_loadings: bool, whether to show the loadings on top of the PCA plot
  #   entitie_anno: str, what to name the loadings after, part of the rowData
  #   tooltip_var: str, name of the column in pcaData to use as tooltip, Only useful
  #     when using ggplotly to make the plot interactive. For this wrap the final plot
  #     in ggplotly(final_plot).
  # Returns:
  #   ggplot object, PCA plot

  coloring <- prepare_coloring_pca(pcaData, color_by)
  color_theme <- coloring$color_theme
  pcaData <- coloring$pcaData
  if(!is.null(tooltip_var)){
    adj2colname <- gsub(" ",".",tooltip_var)
    pcaData$chosenAnno <- pcaData[,adj2colname]
  } else{
    pcaData$chosenAnno <- pcaData$global_ID
  }
  # Plotting routine
  pca_plot <- ggplot(
      pcaData,
      mapping = aes(
        x = pcaData[,x_axis],
        y = pcaData[,y_axis],
        color = pcaData[,color_by],
        group = pcaData[,color_by],
        label = global_ID,
        global_ID = global_ID,
        chosenAnno = chosenAnno
      )
    ) +
    geom_point(size = 3) +
    pca_ellipses(plot_ellipses) +
    scale_color_manual(
      values = color_theme,
      name = color_by
    ) +
    xlab(paste0(
      names(percentVar[x_axis]),
      ": ",
      round(percentVar[x_axis] * 100, 1),
      "% variance"
    )) +
    ylab(paste0(
      names(percentVar[y_axis]),
      ": ",
      round(percentVar[y_axis] * 100, 1),
      "% variance"
    )) +
    coord_fixed() +
    CUSTOM_THEME +
    theme(aspect.ratio = 1) +
    ggtitle(title) +
    pca_loadings(pca, x_axis, y_axis, show_loadings, entitie_anno, data)
  return(pca_plot)
}

plot_pca_loadings <- function(
  pca, data, x_axis, n_top, n_bottom, entitie_anno
){
  # Plot the top and bottom loadings of the x_axis (e.g. = first) Principal Component.
  # Parameters:
  #   pca: PCA object, generated from the data with prcomp
  #   data: data.frame, SummarizedExperiment object. Your Data.
  #   percentVar: numeric, percentage of variance explained by each PC
  #   x_axis: str, name of the column in pcaData to calculate loadings for. "PC1" or other PCs
  #   n_top: int, number of top loadings to show
  #   n_bottom: int, number of bottom loadings to show
  #   entitie_anno: str, what to name the loadings after, part of the rowData
  # Returns:
  #   ggplot object, PCA plot with loadings
  LoadingsDF <- data.frame(
    entitie = rownames(pca$rotation),
    Loading = pca$rotation[,x_axis]
  )
  LoadingsDF <- LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]

  # need to test if default of slider is below the number of entities
  if(n_top + n_bottom < nrow(LoadingsDF)){
    LoadingsDF <- rbind(
      LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF) - n_bottom),],
      LoadingsDF[n_top:1,]
    )
  }

  LoadingsDF$entitie <- factor(LoadingsDF$entitie, levels = rownames(LoadingsDF))
  if(!is.null(entitie_anno)){
    LoadingsDF$entitie <- factor(
      make.unique(as.character(rowData(data)[rownames(LoadingsDF),entitie_anno])),
      levels = make.unique(as.character(rowData(data)[rownames(LoadingsDF),entitie_anno]))
    )
  }
  loadings_plot <- ggplot(LoadingsDF,mapping = aes(x = Loading,y = entitie)) +
    geom_col(mapping = aes(fill = Loading)) +
    scale_y_discrete(
      breaks = LoadingsDF$entitie,
      labels = stringr::str_wrap(gsub("\\.[0-9].*$","",LoadingsDF$entitie),20)
    ) +
    scale_fill_gradient2(low = "#277d6a",mid = "white",high = "orange") +
    ylab(ifelse(is.null(entitie_anno),"",entitie_anno)) +
    xlab(paste0("Loadings: ",x_axis)) +
    ggtitle(paste0("Loadings for ", x_axis)) +
    CUSTOM_THEME
  return(loadings_plot)
}

plot_scree_pca <- function(pca, percentVar){
  # Plot the scree plot of the PCA. Plots the variance explained by each Principal Component.
  # Parameters:
  #   pca: PCA object, generated from the data with prcomp
  #   percentVar: numeric, percentage of variance explained by each PC
  # Returns:
  #   ggplot object, Scree plot of the PCA
  var_explained_df <- data.frame(
    PC = names(percentVar),
    var_explained = percentVar
  )
  var_explained_df$Var <- paste0(
    round(var_explained_df$var_explained,4)*100,"%"
  )
  var_explained_df$PC <- factor(
    var_explained_df$PC,levels = paste0("PC", seq_len(ncol(pca$x)))
  )
  scree_plot <- ggplot(
    var_explained_df,
    mapping = aes(x = PC, y = var_explained, group = 1)
  ) +
    geom_point(size = 4) +
    geom_line() +
    ylab("Variance explained") +
    ggtitle("Scree-Plot of PCA") +
    CUSTOM_THEME
  return(scree_plot)
}

plot_loadings_matrix <- function(pca, data, entitie_anno, n_pcs, cutoff){
  # Plot all loadings that are in one PC above the cutoff for all PCs.
  # Parameters:
  #   pca: PCA object, generated from the data with prcomp
  #   data: SummarizedExperiment object, your data.
  #   entitie_anno: str, what to name the loadings after, part of the rowData
  #   n_pcs: int, number of PCs to plot
  #   cutoff: numeric, cutoff for the loadings
  # Returns:
  #   ggplot object, Loadings matrix plot
  n_pcs <- min(n_pcs, ncol(pca$rotation))
  df_loadings <- data.frame(
    entity = row.names(pca$rotation),
    pca$rotation[, 1:n_pcs]
  )
  # Filter the loadings by cutoff value
  loadings_filter <- apply(
    as.matrix(df_loadings[,-1]) >= abs(cutoff), 1, any
  )
  df_loadings <- df_loadings[loadings_filter,] %>%
    tidyr::gather(key = "PC", value = "loading", -entity)

  if(!is.null(entitie_anno)){
    df_loadings$chosenAnno <- factor(
      make.unique(as.character(rowData(data)[unique(df_loadings$entity),entitie_anno])),
      levels = make.unique(as.character(rowData(data)[unique(df_loadings$entity),entitie_anno]))
    )
  } else{
    df_loadings$chosenAnno <- df_loadings$entity
  }
  # Change this to a complexHeatmap + ad possibility to cluster rows
  loadings_matirx <- ggplot(df_loadings, mapping = aes(
    x = PC,
    y = chosenAnno,
    fill = loading
  )) +
    geom_raster() +
    scale_fill_gradientn(
      colors = c("#277d6a", "white", "orange"),
      limits = c(-max(df_loadings$loading),max(df_loadings$loading))
    ) +
    labs(x = "PCs", y = entitie_anno, fill = "Loading") +
    ggtitle("Loadings Matrix") +
    CUSTOM_THEME
  return(loadings_matirx)
}