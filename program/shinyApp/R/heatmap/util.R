# Utility functions for heatmap generation

generate_title_heatmap <- function(selection_type, top_k_type = NULL, n_top_k = NULL, comparison = NULL){
  # Generate title for heatmap
  # Parameters:
  #   selection_type: character, type of selection
  #   top_k_type: character, if selection_type is "Top K", type of top k selection
  #   n_top_k: numeric, if selection_type is "Top K", number of top k entities
  #   comparison: character, if selection_type is "Top K", comparison type
  # Returns:
  #   character, title for heatmap
  title <- "Heatmap "
  if (selection_type == "all") return(paste0(title, "of all entities"))
  if (selection_type == "Select based on Annotation") return(paste0(title, "of selected entities"))
  topk_k_type_to_title <- list(
    "LogFoldChange" = "ones by LFC",
    "absolute LogFoldChange" = "ones by absolute LFC",
    "LogFoldChange and Significant" = "sig ones by LFC",
    "absolute LogFoldChange and Significant" = "sig ones by absolute LFC"
  )
  return(paste0(
    title, "of top ", n_top_k, " ",
    topk_k_type_to_title[[top_k_type]],
    " for comparison ", comparison
  ))
}

plot_heatmap <- function(
  selected_data, cluster_rows, cluster_cols, scale_rows,
  n_max_row_labels, row_labels, title, row_annotation = NULL, col_annotation = NULL
){
  # Plot heatmap
  # Parameters:
  #   selected_data: data.frame, data to plot
  #   cluster_rows: logical, whether to cluster rows
  #   cluster_cols: logical, whether to cluster columns
  #   scale_rows: logical, whether to scale rows
  #   n_max_row_labels: numeric, maximum number of row labels to show
  #   row_labels: character, row labels
  # Returns:
  #   ComplexHeatmap object

  # Prepare data
  heatmap_data <- as.matrix(selected_data)
  if (scale_rows) heatmap_data <- t(scale(t(heatmap_data)))

  # rownames(heatmap_data) <- row_labels

  heatmap_plot <- Heatmap(
    matrix = heatmap_data,
    name = "Expression",
    column_title = title,
    cluster_rows = cluster_rows,
    cluster_columns = cluster_cols,
    show_row_names = n_max_row_labels < nrow(heatmap_data),
    show_column_names = TRUE,
    top_annotation = col_annotation,
    right_annotation = row_annotation,
    # Parameters to mimick CUSTOM_THEME
    rect_gp = gpar(col = "black"),
    column_title_gp = gpar(fontsize = 17, fontface = "bold"),
    row_names_gp = gpar(fontsize = 15),
    column_names_gp = gpar(fontsize = 15),
    heatmap_legend_param = list(
      title_gp = gpar(fontsize = 15, fontface = "bold"),  # Legend title size and style
      labels_gp = gpar(fontsize = 15)                     # Legend text size
    )
  )
  return(heatmap_plot)
}