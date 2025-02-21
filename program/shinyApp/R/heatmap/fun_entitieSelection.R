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
  significance_level=NULL
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
    # TODO: Replace this with already calculated LFCs from Significance Analysis
    # calculate the LFCs
    if(any(is.null(compare_within)) & any(is.null(reference)) & any(is.null(treatment))){
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
    LFC_output <- getLFCs(filtered_data, ctrl_samples_idx, comparison_samples_idx)
    if(top_k_type=="LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(LFC_output$LFC)],, drop=F]
    } else if(top_k_type=="absolute LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(abs(LFC_output$LFC))],, drop=F]
    } else if(top_k_type=="LogFoldChange and Significant"){
      filtered_data <- filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<significance_level)],, drop=F]
      filtered_data <- filtered_data[rownames(LFC_output)[order(LFC_output$LFC)],, drop=F]
    } else if(top_k_type=="absolute LogFoldChange and Significant"){
      filtered_data <- filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<significance_level)],, drop=F]
      filtered_data <- filtered_data[rownames(LFC_output)[order(abs(LFC_output$LFC))],, drop=F]
    }
    if(nrow(filtered_data)>n_top_k){
      filtered_data <- filtered_data[c(1:n_top_k),, drop=F]
    } else{
      print("Less than n_top_k entries left!")
    }
    return(filtered_data)
  }
}
