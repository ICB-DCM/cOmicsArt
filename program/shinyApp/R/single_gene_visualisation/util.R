get_single_gene_data <- function(
  data, selected_type, selected_gene, groupy_by, post_selection_check
){
  # Function to get the visualization data for a single gene / group
  # Parameters:
  #   data: SummarizedExperiment, data to visualize
  #   selected_type: character, column name where to find the selected gene
  #   selected_gene: character, selected gene
  #   groupy_by: character, column by which the samples are to be grouped
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
  gene_data$annotation <- colData(data)[, groupy_by]
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
  testMethod <- "t.test"
  newList <- comparisons
  xy.list <- vector("list", length(newList))
  for (i in seq_along(newList)) {
    xy.list[[i]] <- unlist(strsplit(x = newList[i],split = ":"))
  }
  additional_layers <- list(
    geom_hline(
      yintercept = mean(gene_data[,-ncol(gene_data)]),
      linetype = 2
    ),
    stat_compare_means(
      comparisons = xy.list,
      method = testMethod,
      label = "p.format",
      hide.ns = TRUE
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