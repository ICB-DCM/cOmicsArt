entitieSelection <- function(
  data,
  selection_type,
  n_top_k=NULL,
  top_k_type=NULL,
  select_by=NULL,
  selection=NULL,
  compare_within=NULL,
  reference=NULL,
  treatment=NULL,
  significance_level=NULL,
  preprocessing=NULL,
  dds=NULL
){
  # Select entities for heatmap based on selection type
  # Parameters:
  #   data: SummarizedExperiment, data to select from
  #   selection_type: character, type of selection
  #   n_top_k: numeric, if selection_type is "Top K", number of top k entities
  #   top_k_type: character, if selection_type is "Top K", type of top k selection
  #   select_by: character, if selection_type is "Select based on Annotation", annotation to select by
  #   selection: character, if selection_type is "Select based on Annotation", selection to select by
  #   compare_within: character, if selection_type is "Top K", sample annotation to compare within
  #   reference: character, if selection_type is "Top K", reference group
  #   treatment: character, if selection_type is "Top K", treatment group
  #   significance_level: numeric, if selection_type is "Top K", significance level
  #   preprocessing: character, if selection_type is "Top K", the Pre-processing
  #     procedure (for the fold change)
  #   dds: if selection_type is "Top K", the DESeq2 object (vst_DESeq), else NULL;
  #     with one, DESeq2 compares, and compare_within must be in its design
  # Returns:
  #   assay of data, selected entities


  filtered_data <- assay(data)
  if(selection_type=="all"){
    return(filtered_data)
  }
  if (selection_type=="Select based on Annotation"){
    if(any(is.null(select_by) & is.null(selection))){
      print("No row annotation selected!")
      filtered_data <- NULL
      return(filtered_data)
    }
    if(any(selection=="all")){
      return(filtered_data)
    }
    filtered_data <- filtered_data[which(rowData(data)[, select_by] %in% selection),]
    return(filtered_data)
  }
  if (selection_type=="Top K"){
    if(is.null(compare_within) || is.null(reference) || is.null(treatment)){
      print("No sample annotation selected!")
      filtered_data <- NULL
      return(filtered_data)
    }
    ctrl_samples_idx <- which(colData(data)[,compare_within]%in%reference)
    comparison_samples_idx <- which(colData(data)[,compare_within]%in%treatment)
    if((length(ctrl_samples_idx) <= 1) | (length(comparison_samples_idx) <= 1)){
      warning("LFC makes no sense just having a single sample per conidition, which is here the case!")
      filtered_data <- NULL
      return(filtered_data)
    }
    # the comparison every module uses (Welch or DESeq2, BH-adjusted)
    LFC_output <- compare_groups(
      data, compare_within, treat = treatment, ctrl = reference,
      preprocessing = preprocessing, dds = dds
    )
    # DESeq2 compares all its genes; the stage's data can have fewer (rows
    # incomplete after batch correction are dropped)
    LFC_output <- LFC_output[rownames(LFC_output) %in% rownames(filtered_data), , drop = FALSE]
    # the top K are the highest (absolute) fold changes; NA fold changes last
    if(top_k_type=="LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(LFC_output$log2FoldChange, decreasing = TRUE)],, drop=F]
    } else if(top_k_type=="absolute LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(abs(LFC_output$log2FoldChange), decreasing = TRUE)],, drop=F]
    } else if(top_k_type %in% c("LogFoldChange and Significant", "absolute LogFoldChange and Significant")){
      # order only the significant entities (ordering all of them re-added
      # the others as NA rows)
      significant <- LFC_output[which(LFC_output$padj < significance_level), , drop = F]
      lfc <- if (top_k_type == "LogFoldChange and Significant") significant$log2FoldChange else abs(significant$log2FoldChange)
      filtered_data <- filtered_data[rownames(significant)[order(lfc, decreasing = TRUE)],, drop=F]
    }
    if(nrow(filtered_data)>n_top_k){
      filtered_data <- filtered_data[c(1:n_top_k),, drop=F]
    } else{
      print("Less than n_top_k entries left!")
    }
    return(filtered_data)
  }
}
