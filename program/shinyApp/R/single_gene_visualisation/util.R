get_single_gene_data <- function(
  data, selected_type, selected_gene, group_by, post_selection_check
){
  # Function to get the visualization data for a single gene / group
  # Parameters:
  #   data: SummarizedExperiment, data to visualize
  #   selected_type: character, column name where to find the selected gene
  #   selected_gene: character, selected gene
  #   group_by: character, column by which the samples are to be grouped
  #   post_selection_check: list, which samples to select
  # Returns:
  #   gene_data, data.frame, data ready for visualization

  if (!(selected_gene %in% rowData(data)[, selected_type])) {
    stop("Selected gene not found in the data")
  }
  entitie_filter <- which(rowData(data)[, selected_type] == selected_gene)
  gene_data <- as.data.frame(t(
    as.data.frame(assay(data))[entitie_filter,,drop=F]
  ))
  gene_data$annotation <- colData(data)[, group_by]
  gene_data <- subset(gene_data, annotation %in% post_selection_check)

  # if we selected more than one gene, we need use the median of the gene expressions
  if (length(entitie_filter) > 1) {
    gene_data <- gene_data %>%
      dplyr::mutate(
        rowMedian = matrixStats::rowMedians(as.matrix(dplyr::select(., -annotation)))
      ) %>%
      dplyr::select(rowMedian, annotation)
  }
  # we turn anno into a factor for better plotting
  gene_data$annotation <- as.factor(gene_data$annotation)

  return(gene_data)
}

maybe_add_test <- function(gene_data, add_testing, comparisons) {
  # Function to add a t.test to the boxplot
  # Parameters:
  #   gene_data: data.frame, data to visualize
  #   add_testing: logical, whether to add the testing column
  #   comparisons: list[string], comparisons to test
  # Returns:
  #   ggplot2 layers for the testing
  if(!add_testing || length(comparisons) == 0) {
    return(NULL)
  }
  # Brackets and labels are drawn as segments and text, as plotly cannot draw
  # ggsignif's layer (stat_compare_means()); position and labels as there
  values <- gene_data[,-ncol(gene_data)]
  value_range <- diff(range(values))
  segments <- list()
  labels <- list()
  for (i in seq_along(comparisons)) {
    groups <- unlist(strsplit(x = comparisons[i], split = ":"))
    p_value <- t.test(
      values[gene_data$annotation == groups[1]],
      values[gene_data$annotation == groups[2]]
    )$p.value
    x <- match(groups, levels(gene_data$annotation))
    y <- max(values) + value_range * (0.05 + 0.12 * (i - 1))
    tip <- y - value_range * 0.03
    segments[[i]] <- data.frame(
      x = c(x[1], x[1], x[2]), xend = c(x[1], x[2], x[2]),
      y = c(tip, y, y), yend = c(y, y, tip)
    )
    labels[[i]] <- data.frame(x = mean(x), y = y, label = as.character(signif(p_value, 2)))
  }
  additional_layers <- list(
    geom_hline(
      yintercept = mean(values),
      linetype = 2
    ),
    geom_segment(
      data = do.call(rbind, segments),
      aes(x = x, xend = xend, y = y, yend = yend),
      inherit.aes = FALSE
    ),
    geom_text(
      data = do.call(rbind, labels),
      aes(x = x, y = y, label = label),
      vjust = 0,
      inherit.aes = FALSE
    )
  )
  return(additional_layers)
}

ready_to_plot <- function(gene_data, add_testing, comparisons) {
  # Function to check whether the data is ready to plot
  # Parameters:
  #   gene_data: data.frame, data to visualize
  #   add_testing: logical, whether to add the testing column
  #   comparisons: list[string], comparisons to test
  # Returns:
  #   boolean
  if (!add_testing) {
    return(TRUE)
  }
  # only check first entry, as other will follow suite!
  comps <- unlist(strsplit(x = comparisons[1],split = ":"))
  return(all(comps %in% gene_data$annotation))
}

single_gene_boxplot <- function(gene_data, add_testing, comparisons, selected_gene, data_process_stage, group_by) {
  # Function to create a boxplot for a single gene
  # Parameters:
  #   gene_data: data.frame, data to visualize
  #   add_testing: logical, whether to add the testing column
  #   comparisons: list[string], comparisons to test
  # Returns:
  #   ggplot2 object

  # Understandable names for the processing
  data_types <- list(
    "data_original" = "Raw Data",
    "data" = "Pre-processed Data",
    "data_batch_corrected" = "Batch Corrected Data"
  )
  plot_boxplots <- TRUE
  if(all(table(gene_data$annotation)<3)) plot_boxplots <- FALSE
  boxplot_layer <- NULL
  if(plot_boxplots){boxplot_layer <- geom_boxplot(alpha = 0.5)}
  boxplot_plot <-ggplot(
    gene_data,
    aes(
      x=annotation,
      y=gene_data[,-ncol(gene_data)],
      fill=annotation
    )
  ) +
    ggtitle(selected_gene) +
    geom_point(shape = 21, size=5)+
    scale_fill_brewer(palette="RdBu") +
    xlab(group_by) +
    ylab(data_types[[data_process_stage]]) +
    CUSTOM_THEME +
    boxplot_layer +
    maybe_add_test(gene_data, add_testing, comparisons)
  return(boxplot_plot)
}