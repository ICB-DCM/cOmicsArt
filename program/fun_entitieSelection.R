entitieSelection=function(data,
                          type,
                          TopK2Show=NA,
                          additionalInput=NA,
                          additionalInput_sample_annotation_types=ifelse(isTruthy(input$sample_annotation_types_cmp_heatmap),input$sample_annotation_types_cmp_heatmap,NA),
                          additionalInput_ctrl_idx=ifelse(isTruthy(input$Groups2Compare_ref_heatmap),input$Groups2Compare_ref_heatmap,NA),
                          additionalInput_cmp_idx=ifelse(isTruthy(input$Groups2Compare_treat_heatmap),input$Groups2Compare_treat_heatmap,NA),
                          psig_threhsold=ifelse(isTruthy(input$psig_threhsold_heatmap),input$psig_threhsold_heatmap,NA)){
  # to cover: c("TopK","significant_LFC","LFC_onlySig","rowAnno_based")
  filtered_data=data
  orderMakesSense_flag=FALSE
  if(!(is.na(additionalInput_sample_annotation_types)) & !(is.na(additionalInput_ctrl_idx)) & !(is.na(additionalInput_cmp_idx))){
    if(type=="significant_LFC" ){
      # sort based on significance
      # need LFCs
      # is reachable from here? selectedData_processed()[[input$omicType]]$sample_table
      ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
      comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
      LFC_output=getLFC(data,ctrl_samples_idx,comparison_samples_idx)
      filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$p_adj,decreasing = F)],,drop=F]
      orderMakesSense_flag=T
    }
   if(type=="LFC_onlySig" ){
     ctrl_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,additionalInput_sample_annotation_types]%in%additionalInput_ctrl_idx)
     comparison_samples_idx<-which(selectedData_processed()[[input$omicType]]$sample_table[,additionalInput_sample_annotation_types]%in%additionalInput_cmp_idx)
     LFC_output=getLFC(data,ctrl_samples_idx,comparison_samples_idx)
     if(!any(LFC_output$p_adj<psig_threhsold)){
       warning("No single entry left! Maybe adjust psig_threhsold_heatmap (but do not put it arbitraly high!)")
       #req(FALSE) -> can we speak from here to output$debug?
       filtered_data=NULL
     }else{
       filtered_data=filtered_data[rownames(LFC_output)[LFC_output[LFC_output$p_adj<psig_threhsold,,drop=F]],,drop=F]
       filtered_data=filtered_data[rownames(LFC_output)[order(LFC_output$LFC,decreasing = F)],,drop=F]
       orderMakesSense_flag=T
     }
     
   }
  }
  if(type=="TopK"){
    if(orderMakesSense_flag){
      #assumes the data to be sorted somehow
      if(nrow(filtered_data)>TopK2Show){
        filtered_data=filtered_data[c(1:TopK2Show),,drop=F]
      }
    }else{
      filtered_data=NULL
    }
    
  }
  
  return(filtered_data)
}