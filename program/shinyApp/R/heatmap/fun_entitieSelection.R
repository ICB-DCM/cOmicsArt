entitieSelection=function(data,
                          type,
                          TopK2Show=NA,
                          additionalInput_row_anno=NA,
                          additionalInput_row_anno_factor=NA,
                          additionalInput_sample_annotation_types=NA,
                          additionalInput_ctrl_idx=NA,
                          additionalInput_cmp_idx=NA,
                          psig_threhsold=NA){
  # to cover: c("TopK","significant_LFC","LFC_onlySig","rowAnno_based")
  filtered_data=assay(data)
  orderMakesSense_flag=FALSE
  print("Entitie Selection")
  #print(additionalInput_row_anno)
  if(any(type=="rowAnno_based") & !(any(is.na(additionalInput_row_anno) &is.na(additionalInput_row_anno_factor))) & !any(additionalInput_row_anno_factor=="all")){
    # Note here this only what to show, LFCs and more importantly multiple test correction will be done on the entire set (without the row anno based selection!!)
    if(any(additionalInput_row_anno_factor=="all")){
      filtered_data = filtered_data
    }else{
      filtered_data = filtered_data[which(data$annotation_rows[,additionalInput_row_anno] %in% additionalInput_row_anno_factor),]
    }
  }
  if(!(is.na(additionalInput_sample_annotation_types)) & !(is.na(additionalInput_ctrl_idx)) & !(is.na(additionalInput_cmp_idx))){
    if(any(type=="significant_LFC")){
      # sort based on significance
      # need LFCs
      # is reachable from here? selectedData_processed()[[input$omicType]]$sample_table
      ctrl_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
      comparison_samples_idx <- which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
      if((length(ctrl_samples_idx) <= 1) | (length(comparison_samples_idx) <= 1)){
        warning("LFC makes no sense just having a single sample per conidition, which is here the case!")
        filtered_data=NULL
      }else{
        LFC_output=getLFC(filtered_data,ctrl_samples_idx,comparison_samples_idx)
        filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$p_adj,decreasing = F)],,drop=F]
        orderMakesSense_flag=T
      }
      
    }
    if(any(type=="LFC_onlySig")){
      ctrl_samples_idx<-which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
      comparison_samples_idx<-which(colData(data)[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
      LFC_output=getLFC(filtered_data,ctrl_samples_idx,comparison_samples_idx)
      if(!(any(LFC_output$p_adj<psig_threhsold))){
        warning("No single entry left! Maybe adjust psig_threhsold_heatmap (but do not put it arbitraly high!)")
        #req(FALSE) -> can we speak from here to output$debug?
        filtered_data=NULL
      }else{
        filtered_data=filtered_data[rownames(LFC_output)[which(LFC_output$p_adj<psig_threhsold)],,drop=F]
        filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$LFC,decreasing = F)],,drop=F]
        orderMakesSense_flag=T
      }
      
    }
  }
  
  if(any(type=="TopK")){
    if(orderMakesSense_flag){
      #assumes the data to be sorted somehow
      if(nrow(filtered_data)>TopK2Show){
        filtered_data=filtered_data[c(1:TopK2Show),,drop=F]
      }else{
        filtered_data=filtered_data
      }
    }else{
      filtered_data=NULL
    }
    
  }
  
  
  
  return(filtered_data)
}
