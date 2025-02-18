# preprocessing procedures

preprocessing <- function(data, omic_type, preprocessing_procedure, deseq_factors = NULL){
  print("Remove all entities which are constant over all samples")
  data <- data[rownames(data[which(apply(assay(data),1,sd) != 0),]),]
  if(preprocessing_procedure == "vst_DESeq"){
      return(deseq_processing(data, omic_type, deseq_factors))
  }
  if(preprocessing_procedure == "filterOnly"){
    return(list(
      data = prefiltering(data, omic_type)
    ))
  }
  if(preprocessing_procedure == "simpleCenterScaling"){
    return(list(
      data = simple_center_scaling(data, omic_type)
    ))
  }
  if(preprocessing_procedure %in% c("Scaling_0_1", "pareto_scaling")){
    return(list(
      data = scaling_normalisation(data, omic_type, preprocessing_procedure)
    ))
  }
  if(preprocessing_procedure %in% c("log10", "ln", "log2")){
    return(list(
      data = ln_normalisation(data, omic_type, preprocessing_procedure)
    ))
  }
  if(preprocessing_procedure == "none"){
    return(list(
      data = data
    ))
  }
  # if nothing is chosen, raise an error
  stop("No valid Preprocessing procedure chosen")
}

prefiltering <- function(data, omic_type){
  # TODO: will be replaced with general "at least x in y samples" filter
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
  data, omic_type, deseq_factors
){
  # --- Sanity checks ---
  if(nrow(data) < 100){
    stop(
      "The number of samples is too low for a DESeq2 analysis. Please select at least 100 samples."
    )
  }
  if (omic_type != "Transcriptomics"){
    stop(
      "DESeq2 is only available for Transcriptomics data."
    )
  }
  if(length(deseq_factors) <= 0){
    stop(
      "Please select at least one factor for the DESeq2 analysis."
    )
  }
  # --- DESeq2 preprocessing ---
  data <- prefiltering(data, omic_type)
  design_formula <- paste("~", paste(deseq_factors, collapse = " + "))
  # turn each factor into a factor
  for(i in deseq_factors){
    colData(data)[,i] <- as.factor(colData(data)[,i])
  }
  print(design_formula)

  dds <- DESeq2::DESeqDataSetFromMatrix(
    countData = assay(data),
    colData = colData(data),
    design = as.formula(design_formula)
    )

  de_seq_result <- DESeq2::DESeq(dds)
  dds_vst <- vst(
    object = de_seq_result,
    blind = TRUE
  )
  assay(data) <- as.data.frame(assay(dds_vst))
  return(list(
    data = data,
    DESeq_obj = de_seq_result
  ))
}

batch_correction <- function(
  data, preprocessing_procedure, batch_column, deseq_factors = NULL, omic_type = NULL
){
  if (batch_column == "NULL"){
    return(list(
      data = NULL
    ))
  }
  batch_res <- list()
  if(preprocessing_procedure == "vst_DESeq"){
    batch_res <- deseq_processing(
      data = data,
      omic_type = omic_type,
      deseq_factors = c(deseq_factors, batch_column)
    )
    data <- batch_res$data
  }
  assay(data) <- sva::ComBat(
    dat = assay(data),
    batch = as.factor(colData(data)[,batch_column])
  )
  # only use complete cases
  data <- data[complete.cases(assay(data)),]
  batch_res$data <- data
  return(batch_res)
}
