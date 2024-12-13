# preprocessing procedures

preprocessing <- function(data, omic_type, procedure){
  if(procedure == "filterOnly"){
    return(prefiltering(data, omic_type))
  }
  if(procedure == "simpleCenterScaling"){
    return(simple_center_scaling(data, omic_type))
  }
  if(procedure %in% c("Scaling_0_1", "pareto_scaling")){
    return(scaling_normalisation(data, omic_type, procedure))
  }
  if(procedure %in% c("log10", "ln", "log2")){
    return(ln_normalisation(data, omic_type, procedure))
  }
  if(procedure == "none"){
    return(data)
  }
  # if nothing is chosen, raise an error
  stop("No valid Preprocessing procedure chosen")
}

prefiltering <- function(data, omic_type){
  # Filter out low abundant genes for Metabol- and Transcriptmics.
  if(omic_type == "Transcriptomics"){
    print("Remove anything of rowCount <=10")
    return(data[which(rowSums(assay(data)) > 10),])
  }
  if(omic_type == "Metabolomics"){
    print("Remove anything which has a row median of 0")
    return(data[which(apply(assay(data),1,median)!=0),])
  }
}


simple_center_scaling <- function(data, omic_type){
  # Center and scale the data
  # prefilter the data
  data <- prefiltering(data, omic_type)
  # center and scale the data
  processedData <- as.data.frame(t(scale(
    x = as.data.frame(t(as.data.frame(assay(data)))),
    scale = T,
    center = T
  )))
  assay(data) <- processedData
  return(data)
}


scaling_normalisation <- function(data, omic_type, scaling_procedure){
  # Center and scale the data
  # prefilter the data
  data <- prefiltering(data, omic_type)
  # scaling functions
  fun_scale <- function(x){
    return((x-min(x))/(max(x)-min(x)))
  }
  fun_pareto <- function(x){
    return((x-mean(x))/sqrt(var(x)))
  }
  scaling_function <- ifelse(scaling_procedure == "Scaling_0_1", fun_scale, fun_pareto)
  processedData <- as.data.frame(t(apply(
    X = assay(data),
    MARGIN = 1,
    FUN = scaling_function
  )))
  assay(data) <- processedData
  return(data)
}


ln_normalisation <- function(data, omic_type, logarithm_procedure){
  # Center and scale the data
  if(logarithm_procedure == "log10")
  {
    logarithm = log10
  }
  else if(logarithm_procedure == "log2")
  {
    logarithm = log2
  }
  else
  {
    logarithm = log
  }
  # prefilter the data
  data <- prefiltering(data, omic_type)
  # log the data and always add 1 to avoid -Inf
  processedData <- as.data.frame(logarithm(as.data.frame(assay(data)) + 1))
  assay(data) <- processedData
  return(data)
}


deseq_processing <- function(
  data, omic_type, formula_sub, session_token, batch_correct
){
  # Center and scale the data
  # prefilter the data
  data <- prefiltering(data, omic_type)
  # DESeq2
  if(omic_type == "Transcriptomics"){
    if(length(formula_sub) <= 0){
      stop(
        "Please select at least one factor for the DESeq2 analysis.",
        class = "InvalidInputError"
      )
    }
    design_formula <- paste("~", paste(formula_sub, collapse = " + "))
    # turn each factor into a factor
    for(i in formula_sub){
      colData(data)[,i] <- as.factor(colData(data)[,i])
    }
    par_tmp[[session_token]][["DESeq_factors"]] <<- c(formula_sub)
    print(design_formula)

    dds <- DESeq2::DESeqDataSetFromMatrix(
      countData = assay(data),
      colData = colData(data),
      design = as.formula(design_formula)
      )
    
    de_seq_result <- DESeq2::DESeq(dds)
    if (batch_correct){
      res_tmp[[session_token]]$DESeq_obj_batch_corrected <<- de_seq_result
      par_tmp[[session_token]]["DESeq_formula"] <<- design_formula
    } else {
      res_tmp[[session_token]]$DESeq_obj <<- de_seq_result
      par_tmp[[session_token]]["DESeq_formula_batch"] <<- design_formula
    }
    dds_vst <- vst(
      object = de_seq_result,
      blind = TRUE
    )
    assay(data) <- as.data.frame(assay(dds_vst))
    return(data)
  }
  return(data)
}
