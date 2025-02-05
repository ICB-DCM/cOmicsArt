filter_significant_result <- function(result, alpha, filter_type){
  # remove NAN value
  result <- result[!is.na(result$padj),]
  if(filter_type == "Significant unadjusted"){
    result <- result[result$pvalue < alpha,]
  }else{
    result <- result[result$padj < alpha,]
  }
  if(filter_type == "Upregulated"){
    result <- result[result$log2FoldChange > 0,]
  }
  if(filter_type == "Downregulated"){
      result <- result[result$log2FoldChange < 0,]
  }
  return(result)
}

performSigAnalysis <- function(
  data,              # a SummarizedExperiment or similar object
  preprocessing,     # e.g., "vst_DESeq" or other method
  comparisons,       # character vector of comparisons (e.g. "A:B")
  compare_within,    # the name of the sample annotation column used for contrasts
  test_method,       # the testing method (for non-DESeq2)
  useBatch,          # TRUE/FALSE for using batch correction
  padjust_method     # the correction method (e.g., "BH")
) {
  # Perform significance analysis for a given dataset and set of comparisons.
  # The function will return a list of results for each comparison.
  # Parameters:
  # data: a SummarizedExperiment or similar object
  # preprocessing: the preprocessing method used for the data
  # comparisons: a character vector of comparisons (e.g. "A:B")
  # compare_within: the name of the sample annotation column used for contrasts
  # test_method: the testing method (for non-DESeq2)
  # useBatch: TRUE/FALSE for using batch correction
  # padjust_method: the correction method (e.g., "BH")
  # Returns:
  # A list of results for each comparison.
  contrasts <- lapply(comparisons, function(comp) unlist(strsplit(comp, ":")))
  names(contrasts) <- comparisons
  if (preprocessing == "vst_DESeq") {
    # Choose the appropriate DESeq object based on the useBatch flag:
    dds <- if (useBatch) data$DESeq_obj_batch_corrected else data$DESeq_obj

    tryCatch({
      # For each contrast, run DESeq2::results and collect the result:
      sig_results <- lapply(contrasts, function(contrast_vals) {
        DESeq2::results(
          dds,
          contrast = c(compare_within, contrast_vals[1], contrast_vals[2]),
          pAdjustMethod = padjust_method
        )
      })
    }, error = function(e) {
      # extend the error message to include the contrast that caused the error
      stop(paste("Error in DESeq2::results for contrasts:", contrasts, "\n", e))
    })
  } else {
    # Find all unique contrast levels and select the corresponding columns from the data:
    data <- if (useBatch) data$data_batch_corrected else data$data
    contrasts_all <- unique(unlist(contrasts))
    index_comparisons <- which(colData(data)[, compare_within] %in% contrasts_all)
    samples_selected <- colData(data)[index_comparisons, ]
    data_selected <- as.matrix(assay(data))[, index_comparisons]

    # Wrap the custom significance_analysis in tryCatch (modify it so that it returns its results)
    sig_results <- tryCatch({
      significance_analysis(
        df = as.data.frame(data_selected),
        samples = as.data.frame(samples_selected),
        contrasts = contrasts,
        method = test_method,
        correction = padjust_method,
        contrast_level = compare_within,
        batch_corrected = useBatch
      )
      # Assume that significance_analysis returns the results directly.
    }, error = function(e) {
      stop(paste("Error in significance_analysis for contrast:", contrasts, "\n", e))
    })
  }
  return(sig_results)
}


significance_analysis <- function(
  df, samples, contrasts, method, correction, contrast_level, batch_corrected = FALSE
){
  # perform significance analysis
  # df: dataframe or matrix with the data
  # samples: dataframe with the samples
  # contrasts: list of contrasts to compare
  # method: method to use for the test
  # alpha: significance level
  # correction: correction method to use
  # get the test function
  if(method == "T-Test"){
    # TODO test for Varianz Homogenität (Levene Test) - 
    # intermediate Lösung könnte sein einfach standard abweichungen der Gruppen anzugeben
    # User hinweisen diese zu untersuchen!
    test_function <- function(...) t.test(..., var.equal = TRUE)
      
  }else if(method == "Welch-Test"){
    test_function <- function(...) t.test(..., var.equal = FALSE)
  }
  else{
      test_function <- wilcox.test
  }
  # add the rownames as a column
  df$gene <- rownames(df)
  t_test_per_gene <- function(df, grp1, grp2) {
    x <- df[grp1]
    y <- df[grp2]
    x <- as.numeric(x)
    y <- as.numeric(y)
    tryCatch(
      {
        results <- test_function(x, y)
        return(list(
          "pvalue" = results$p.value,
          "baseMean" = results$estimate[[2]],
          "treatMean" = results$estimate[[1]],
          "stat" = results$statistic
        ))
      },
      error = function(e) {
        cat(
          "Error in gene ", df["gene"], ":\n  ",
          as.character(e), "  Values are:\n",
          "   x: ", paste(x),
          "\n   y: ", paste(y), "\n",
          "NA will be returned instead. \n",
          sep = ""
          )
        return(NA)
      }
    )
  }
  sig_results <- list()

  # for each comparison get the data and do the test
  # introduce a running parameter alongside the loop for the name
  comp_name <- 1
  for(contrast in contrasts){
    # skip if already there
    if(identical(
      list(test_method = method, test_correction = correction, batch_corrected=batch_corrected),
      par_tmp[[session$token]]$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]
    )){
      print("Results exists, skipping calculations.")
      sig_results[[names(contrasts)[comp_name]]] <- res_tmp[[session$token]]$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]
      comp_name <- comp_name + 1
      next
    }
    # get the samples for the comparison
    idx <- rownames(samples[samples[contrast_level] == contrast[[1]],, drop = FALSE])
    idy <- rownames(samples[samples[contrast_level] == contrast[[2]],, drop = FALSE])
    # perform the test
    res <- apply(
      X = df,
      MARGIN = 1,
      FUN = t_test_per_gene,
      grp1 = idx,
      grp2 = idy
    )
    res <- as.data.frame(do.call(rbind, res))
    # turn columns to numerics again
    res <- transform(
      res, 
      baseMean=as.numeric(baseMean),
      treatMean=as.numeric(treatMean),
      pvalue=as.numeric(pvalue),
      stat=as.numeric(stat)
    )
    means <- subset(res, select = c(baseMean,treatMean))
    # drop mean of treatment
    res <- subset(res, select = -c(treatMean))
    # create a dataframe with the results
    res$padj <- p.adjust(res$pvalue, method = correction)
    res$log2FoldChange <- getLFC(means)
    res <- transform(res, pvalue=as.numeric(pvalue),
                     baseMean=as.numeric(baseMean), stat=as.numeric(stat))



    sig_results[[names(contrasts)[comp_name]]] <- res
    # fill res_tmp[[session$token]], par_tmp[[session$token]]
    res_tmp[[session$token]]$SigAna[[contrast_level]][[names(contrasts)[comp_name]]] <<- res
    par_tmp[[session$token]]$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]  <<- list(
      test_method = method,
      test_correction = correction,
      batch_corrected = batch_corrected
    )
    comp_name <- comp_name + 1
  }
  return(sig_results)
}


prepare_upset_plot <- function(res2plot){
  # Prepare a Significance analysis result for Upset plotting and for intersection
  # download.
  overlap_list <- UpSetR::fromList(res2plot)
  names <- c()
  for(i in 1:length(res2plot)){
    names <- append(names, res2plot[[i]])
  }
  names <- unique(names)
  rownames(overlap_list) <- names

  return(overlap_list)
}

map_intersects_for_highlight <- function(highlights, plot, overlap_list){
  namesInPlot <- ggplot_build(plot)$layout$panel_params[[1]]$y$get_labels()
  # maps the names of the intersections to hightlight to the correct ones in upset plot
  mapping <- match(
      colnames(overlap_list),
      namesInPlot[base::sort.default(names(namesInPlot))]
  )
  querie_names_pre <- lapply(strsplit(highlights, "-"), as.integer)
  querie_names <- vector("list", length(querie_names_pre))
  for(i_querie in seq_along(querie_names)){
    querie_names[[i_querie]] <- mapping[querie_names_pre[[i_querie]]]
  }
  return(querie_names)
}


getLFC <- function(means){
  # define function to calculate LFC in case of ln or log10 preprocessing
  # and all other cases
  lfc_per_gene <- function(df){
    df$LFC <- log2(df$treatMean)-log2(df$baseMean)
    # NA, Inf, -Inf and NaN values will bet set to NA
    # NA if the means are NA
    df$LFC[is.nan(df$LFC)] <- NA  # NaN if both means 0
    # Inf if baseMean==0, -Inf if treatMean==0
    df$LFC[is.infinite(df$LFC)] <- NA
    # print the rownames of NA values
    cat(
      "For the following genes, no meaningfull log fold change can be calculated:\n",
      rownames(df[is.na(df$LFC),]),
      "\nThis is caused by either one of the mean values being 0 or by NAs in the testing."
    )
    return (df$LFC)
  }
  lfc_per_gene_log <- function(df, log_base){
    df$LFC <- log2(log_base**(df$treatMean-df$baseMean))
    # NA, Inf, -Inf and NaN values will bet set to NA
    # NA if the means are NA
    df$LFC[is.nan(df$LFC)] <- NA  # NaN if both means 0
    # Inf if baseMean==0, -Inf if treatMean==0
    df$LFC[is.infinite(df$LFC)] <- NA
    # print the rownames of NA values
    cat(
      "For the following genes, no meaningfull log fold change can be calculated:\n",
      rownames(df[is.na(df$LFC),]),
      "\nThis is caused by either one of the mean values being 0 or by NAs in the testing."
    )
    return (df$LFC)
  }
  if(par_tmp[[session$token]]$PreProcessing_Procedure == "log10"){
    lfc_per_gene_log(means, log_base = 10)
  }else if(par_tmp[[session$token]]$PreProcessing_Procedure == "ln"){
    lfc_per_gene_log(means, log_base = exp(1))
  }else{
    lfc_per_gene(means)
  }
}


log_messages_volcano<- function(plot, table, contrast, file_path){
  if((is.null(plot)) | (is.null(table))){
    return()
  }
  notificationID <- showNotification("Saving...",duration = 0)

  tmp_filename <- paste0(
    getwd(),file_path,paste(paste0("VOLCANO_", Sys.time(), ".png"))
    )

  ggsave(tmp_filename, plot=plot, device = "png")

  # Add Log Messages

  fun_LogIt(message = paste(
    "**VOLCANO** - Underlying Volcano Comparison:", contrast[1],"vs", contrast[2]
  ))
  fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

  removeNotification(notificationID)
  showNotification("Saved!",type = "message", duration = 1)
}


addStars <- function(result){ # assumes padj column present
  result <- as.data.frame(result) %>%
    mutate(sig_level = case_when(
      padj < 0.0001 ~ as.character(HTML(paste0(icon("star", lib = "font-awesome"),
                                               icon("star", lib = "font-awesome"),
                                               icon("star", lib = "font-awesome")))),  # Three stars for padj < 0.0001
      padj < 0.001 ~ as.character(HTML(paste0(icon("star", lib = "font-awesome"),
                                              icon("star", lib = "font-awesome")))),  # Two stars for padj < 0.001
      padj < par_tmp[[session$token]]$SigAna$significance_level ~ as.character(icon("star", lib = "font-awesome")),  # One star if below significance threshold
      padj >= par_tmp[[session$token]]$SigAna$significance_level ~ as.character(icon("minus", lib = "font-awesome"))  # Default case
    ))
  result <- result[,c("sig_level", colnames(result)[1:(ncol(result)-1)])] 
  return(result)
}

plot_significant_results <- function(
  sig_results,           # List of significance results, keyed by comparison.
  comparisons_to_visualize,  # Vector of comparisons to visualize (or "all")
  visualization_method,  # "UpSetR plot" or "Venn diagram"
  significance_level,    # e.g. input$significance_level
  sig_to_look_at        # e.g. input$sig_to_look_at
) {
  # Initialize info message (empty if no issues)
  info_text <- ""

  # --- Validate that there are at least two comparisons to visualize ---
  if (length(comparisons_to_visualize) == 1 && comparisons_to_visualize[1] != "all") {
    info_text <- "You tried to compare only one set. Please choose at least two comparisons."
    return(list(plot = NULL, info_text = info_text))
  }

  # --- Determine the set of comparisons to plot ---
  if (any(comparisons_to_visualize == "all")) {
    chosenVizSet <- names(sig_results)
    if (length(chosenVizSet) > 4) {
      chosenVizSet <- chosenVizSet[1:2]
      info_text <- paste0(
        "Note: Although you chose 'all' to visualize, only the first 2 comparisons are shown to avoid ",
        "unwanted computational overhead (you got more than 4 comparisons). Please choose precisely ",
        "the comparisons for visualization."
      )
    }
  } else {
    chosenVizSet <- comparisons_to_visualize
  }

  # --- Filter significant results for each chosen comparison ---
  res2plot <- list()
  for (comp in chosenVizSet) {
    filtered <- filter_significant_result(
      result = sig_results[[comp]],
      alpha = significance_level,
      filter_type = sig_to_look_at
    )
    rows_found <- rownames(filtered)
    if (length(rows_found) > 0) {
      res2plot[[comp]] <- rows_found
    }
  }

  # --- Ensure that there is more than one (nonempty) comparison ---
  if (length(res2plot) <= 1) {
    info_text <- "You either have no significant results or only significant results in one comparison."
    if (sig_to_look_at == "Significant") {
      info_text <- paste0(
        info_text,
        "\nYou tried to look at adjusted pvalues.\nYou might want to look at raw pvalues (CAUTION!) or change the significance level."
      )
    }
    return(list(plot = NULL, info_text = info_text))
  }

  # --- Create the desired plot based on the visualization method ---
  if (visualization_method == "UpSetR plot") {
    # Prepare data for an UpSet plot
    overlap_list <- prepare_upset_plot(res2plot = res2plot)
    plot_obj <- ComplexUpset::upset(
      overlap_list,
      colnames(overlap_list),
      themes = list(default = CUSTOM_THEME)
    )
    intersect_names <- ggplot_build(plot_obj)$layout$panel_params[[1]]$x$get_labels()
    return(list(
      plot = plot_obj,
      info_text = ifelse(info_text == "", "Analysis is Done!", info_text),
      intersect_names = intersect_names
    ))
  } else if (visualization_method == "Venn diagram") {
    # Create a Venn diagram
    plot_obj <- ggvenn::ggvenn(
      res2plot,
      fill_color = c("#44af69", "#f8333c", "#fcab10", "#2b9eb3"),
      set_name_size = 3
    )
    return(list(
      plot = plot_obj, info_text = ifelse(info_text == "", "Analysis is Done!", info_text)
    ))
  } else {
    return(list(plot = NULL, info_text = "Unknown visualization method."))
  }
}
