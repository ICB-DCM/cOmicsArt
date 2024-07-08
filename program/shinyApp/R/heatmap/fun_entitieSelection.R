entitieSelection <- function(
  data,
  type,
  TopK2Show=NULL,
  TopKOrder=NULL,
  additionalInput_row_anno=NULL,
  additionalInput_row_anno_factor=NULL,
  additionalInput_sample_annotation_types=NULL,
  additionalInput_ctrl_idx=NULL,
  additionalInput_cmp_idx=NULL,
  psig_threhsold=NULL
){

  filtered_data <- assay(data)
  if (type=="Select based on Annotation"){
    if(any(is.null(additionalInput_row_anno) & is.null(additionalInput_row_anno_factor))){
      print("No row annotation selected!")
      filtered_data <- NULL
      return(filtered_data)
    }
    filtered_data <- filtered_data[which(rowData(data)[, additionalInput_row_anno] %in% additionalInput_row_anno_factor),]
    return(filtered_data)
  }
  if (type=="Top K"){
    # TODO: Replace this with already calculated LFCs from Significance Analysis
    # calculate the LFCs
    if(any(is.null(additionalInput_sample_annotation_types)) & any(is.null(additionalInput_ctrl_idx)) & any(is.null(additionalInput_cmp_idx))){
      print("No sample annotation selected!")
      filtered_data <- NULL
      return(filtered_data)
    }
    ctrl_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
    comparison_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
    if((length(ctrl_samples_idx) <= 1) | (length(comparison_samples_idx) <= 1)){
      warning("LFC makes no sense just having a single sample per conidition, which is here the case!")
      filtered_data <- NULL
      return(filtered_data)
    }
    LFC_output <- getLFCs(filtered_data, ctrl_samples_idx, comparison_samples_idx)
    if(TopKOrder=="LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(LFC_output$LFC)],, drop=F]
    } else if(TopKOrder=="absolute LogFoldChange"){
      filtered_data <- filtered_data[rownames(LFC_output)[order(abs(LFC_output$LFC))],, drop=F]
    } else if(TopKOrder=="LogFoldChange and Significant"){
      filtered_data <- filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<psig_threhsold)],, drop=F]
      filtered_data <- filtered_data[rownames(LFC_output)[order(LFC_output$LFC)],, drop=F]
    } else if(TopKOrder=="absolute LogFoldChange and Significant"){
      filtered_data <- filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<psig_threhsold)],, drop=F]
      filtered_data <- filtered_data[rownames(LFC_output)[order(abs(LFC_output$LFC))],, drop=F]
    }
    if(nrow(filtered_data)>TopK2Show){
      filtered_data <- filtered_data[c(1:TopK2Show),, drop=F]
    } else{
      print("Less than TopK2Show entries left!")
    }
    return(filtered_data)
  }
}
