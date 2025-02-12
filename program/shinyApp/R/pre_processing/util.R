# preprocessing procedures

preprocessing <- function(
    data, 
    omic_type, 
    procedure, 
    deseq_factors = NULL,
    filter_threshold = 10, 
    filter_threshold_samplewise = NULL,
    filter_samplesize = NULL,
    limma_intercept = NULL,
    limma_formula = NULL){
  print("Remove all entities which are constant over all samples")
  data <- data[rownames(data[which(apply(assay(data),1,sd) != 0),]),]
  if(procedure == "vst_DESeq"){
      return(deseq_processing(data, omic_type, deseq_factors))
  }
  if(procedure == "limma_voom"){
    return(list(
      data = limma_voom_processing(data, omic_type,limma_intercept,limma_formula)
    ))
  }
  if(procedure == "filterOnly"){
    return(list(
      data = prefiltering_user(data,filter_threshold = filter_threshold)
    ))
  }
  if(procedure == "filterPerSample"){
    return(list(
      data = prefiltering_user(
        data, 
        filter_threshold = NULL, 
        filter_threshold_samplewise = filter_threshold_samplewise,
        filter_samplesize = filter_samplesize
        )
    ))
  }
  if(procedure == "simpleCenterScaling"){
    return(list(
      data = simple_center_scaling(data, omic_type)
    ))
  }
  if(procedure %in% c("Scaling_0_1", "pareto_scaling")){
    return(list(
      data = scaling_normalisation(data, omic_type, procedure)
    ))
  }
  if(procedure %in% c("log10", "ln", "log2")){
    return(list(
      data = ln_normalisation(data, omic_type, procedure)
    ))
  }
  if(procedure == "none"){
    return(list(
      data = data
    ))
  }
  # if nothing is chosen, raise an error
  stop("No valid Preprocessing procedure chosen")
}

prefiltering <- function(data, omic_type){
  # Filter out low abundant genes for Metabol- and Transcriptomics.
  # One could let this baseline filter also be user defined but for now it is fixed.
  # Would require a workflow specification for the user
  if(omic_type == "Transcriptomics"){
    print(paste0("Remove anything of rowCount <= 10"))
    return(data[which(rowSums(assay(data)) > 10),])
  }
  if(omic_type %in% c("Metabolomics", "Lipidomics")){
    print("Remove anything which has a row median of 0")
    return(data[which(apply(assay(data),1,median) != 0),])
  }
}

prefiltering_user <- function(data, 
                              filter_threshold = NULL, 
                              filter_threshold_samplewise = NULL, 
                              filter_samplesize = NULL){
  if(!is.null(filter_threshold)){
    print(paste0("Remove anything of rowCount <=",filter_threshold))
    return(data[which(rowSums(assay(data)) > filter_threshold),])
  } else{
    print(paste0("Remove anything of rowCount <=",filter_threshold_samplewise," in at least ",filter_samplesize," samples"))
    return(data[which(rowSums(assay(data) > filter_threshold_samplewise) >= filter_samplesize),])
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
      "The number of samples is too low for a DESeq2 analysis. Please select at least 100 samples.",
    )
  }
  if (omic_type != "Transcriptomics"){
    stop(
      "DESeq2 is only available for Transcriptomics data.",
    )
  }
  if(length(deseq_factors) <= 0){
    stop(
      "Please select at least one factor for the DESeq2 analysis.",
      class = "InvalidInputError"
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

limma_voom_processing <- function(data, omic_type,limma_intercept,limma_formula){
  if(length(limma_formula) <= 0){
    stop(
      "Please select at least one factor for the limma voom design.",
      class = "InvalidInputError"
    )
  }
 data <- prefiltering(data, omic_type)
  # limma-voom
 limma_concat <- paste0(limma_formula, collapse = "+")

 if(limma_intercept){
   design_factors <- paste0("~",limma_concat)
 }else{
   design_factors <- paste0("~0+", limma_concat)
 }
 
 design_mat <- model.matrix(as.formula(design_factors), data = colData(data))
 
# TODO add limma plots to main panel
  data_voom <- limma::voom(
    counts = assay(data),
    design = design_mat,
    plot = FALSE
  )
  
  assay(data) <- as.data.frame(data_voom$E)
  return(data)
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
