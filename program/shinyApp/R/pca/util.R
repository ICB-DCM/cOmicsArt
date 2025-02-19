check_calculations <- function(current_parameters, module){
  if (is.null(res_tmp[[session$token]][[module]])){  # check whether result is existent
    return(FALSE)
  }
  # check whether all parameters are identical to the current existing result
  if (identical(par_tmp[[session$token]][[module]], current_parameters)){
    return(TRUE)
  }
  # The remaining case is an existing result with other parameters,
  # which will trigger an alert
  return(FALSE)
}

create_default_title_pca <- function(pcs, preprocessing){
    # Create a default title for the correlation plot
    # Parameters:
    #   correlation_method: character, method used for correlation
    #   preprocessing: character, preprocessing method
    # Returns:
    #   character, default title
    return(paste0(
      pcs, " -",
      "- preprocessing: ",
      preprocessing
    ))
}

get_pca <- function(data, scale_data, sample_types, sample_selection){
  # Calculate the PCA
  # Parameters:
  #   data: data.frame, data to calculate PCA
  #   scale_data: logical, whether to scale the data
  #   sample_types: character, column name where to find the sample selection
  #   sample_selection: list, which samples to select
  # Returns:
  #   pca, PCA object
  #   pcaData, data.frame, PCA data
  #   percentVar, numeric, percentage of variance explained

  # Potentially select samples
  data <- select_data(data, sample_selection, sample_types)$data
  # Compute the Principal Components
  pca <- prcomp(
    x = as.data.frame(t(as.data.frame(assay(data)))),
    center = T,
    scale. = scale_data
  )
  # How much variance in the data is explained by the Principal Components
  percentVar <- pca$sdev^2 / sum(pca$sdev^2)
  names(percentVar) <- colnames(pca$x)
  # Create a data.frame with the PCA data
  pcaData <- data.frame(pca$x,colData(data))
  # add global_ID to pcaData
  pcaData$global_ID <- rownames(pcaData)
  return(list(
      pca = pca,
      pcaData = pcaData,
      percentVar = percentVar
  ))
}

prepare_coloring_pca <- function(pcaData, coloring_option) {
  message(paste0("Coloring option selected: ", coloring_option))

  # Check if the selected column is numeric and has more than 8 unique values
  if (is.numeric(pcaData[,coloring_option]) && n_distinct(pcaData[,coloring_option]) > 8) {
    message("Color option is numeric with many unique values. Automatically binned into 8 intervals.")
    pcaData[[coloring_option]] <- cut_interval(pcaData[[coloring_option]], n = 8)
    color_theme <- viridis::viridis(8)
  } else {
    pcaData[ ,coloring_option] <- as.factor(pcaData[, coloring_option])
    n_values <- n_distinct(pcaData[[coloring_option]])
    if (n_values <= 8) {
      color_theme <- RColorBrewer::brewer.pal(n = n_values, name = "Set1")
    } else {
      color_theme <- viridis::plasma(n_values)
    }
  }
  return(list(pcaData = pcaData, color_theme = color_theme))
}

pca_loadings <- function(
  pca, x_axis, y_axis, show_loadings = TRUE, entitie_anno = NULL, data = NULL
) {
  # Calculate the loadings of the PCA and add annotation
  # Parameters:
  #   pca: PCA object
  #   x_axis: str, Name of the x-axis
  #   y_axis: str, Name of the y-axis
  #   show_loadings: bool, whether to show the loadings
  #   entitie_anno: str, name of the column in the data to take annotation from
  #   data: data.frame, data to take annotation from
  # Returns:
  #   loadings: a ggplot geom_segment object to add to the plot | NULL

  if (!(show_loadings)) return(NULL)

  # Extract PCA scores and loadings
  scores <- as.data.frame(pca$x)
  loadings <- as.data.frame(pca$rotation)
  loadings$feature <- rownames(loadings)


  # Identify top 5 loadings based on their contribution to selected PCs
  top_features <- rownames(loadings)[order(
    sqrt((loadings[[x_axis]])^2 + (loadings[[y_axis]])^2),
    decreasing = TRUE
  )[1:5]]
  loadings$feature[!loadings$feature %in% top_features] <- ""

  # Scaling factor for loadings
  scaling_factor <- min(
    (max(scores[[y_axis]]) - min(scores[[y_axis]])) / (max(loadings[[y_axis]]) - min(loadings[[y_axis]])),
    (max(scores[[x_axis]]) - min(scores[[x_axis]])) / (max(loadings[[x_axis]]) - min(loadings[[x_axis]]))
  )

  loadings <- transform(
    loadings,
    v1 = 1.2 * scaling_factor * get(x_axis),
    v2 = 1.2 * scaling_factor * get(y_axis)
  )

  # Annotate loadings if annotation is provided
  loadings$global_ID <- rownames(loadings)
  loadings$chosenAnno <- rownames(loadings)

  if (!is.null(entitie_anno) && !is.null(data)) {
    loadings$chosenAnno <- factor(
      make.unique(as.character(rowData(data)[rownames(loadings), entitie_anno])),
      levels = make.unique(as.character(rowData(data)[rownames(loadings), entitie_anno]))
    )
  }
  # Return geom_segment layer for adding to plots
  return(geom_segment(
    data = loadings[loadings$feature != "", ],
    mapping = aes(
      x = 0,
      y = 0,
      xend = v1,
      yend = v2,
      chosenAnno = chosenAnno
    ),
    arrow = arrow(type = "closed", unit(0.01, "inches"), ends = "both"),
    color = "#ab0521"
  ))
}

pca_ellipses <- function(
    plot_ellipses = TRUE
){
  # Add ellipses to the PCA plot
  # Parameters:
  #   plot_ellipses: bool, whether to plot ellipses
  # Returns:
  #   geom_ellipse object to add to the plot | NULL
  if (!(plot_ellipses)) return(NULL)
  return(stat_ellipse(level = 0.95, linetype = 2, show.legend = FALSE))
}