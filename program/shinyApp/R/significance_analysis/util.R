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


create_new_tab <- function(title, targetPanel, result, contrast, alpha, ns, preprocess_method){
  # call create_new_tab based on preprocess_method used
  # preprocess_method: preprocess_method used
  # for other parameters see create_new_tab_*
  if (preprocess_method == "vst_DESeq"){
      create_new_tab_DESeq(title, targetPanel, result, contrast, alpha, ns)
  }
  else{
      create_new_tab_manual(title, targetPanel, result, contrast, alpha, ns)
  }
}


create_new_tab_manual <- function(title, targetPanel, result, contrast, alpha, ns){
  # create a new tabPanel for manual preprocessing
  # title: title of the tabPanel
  # targetPanel: name of the targetPanel under which the tabPanel should be created
  # result: result of the significance analysis
  # contrast: comparisons used for the significance analysis
  # alpha: significance level
  # ns: namespace function

  print("create_new_tab")
  print(title)
  print(result)
  # paste together the strings to print
  # total number of genes compared
  total_genes <- length(rownames(result))
  resume <- list()
  resume[[1]] <- paste("Total number of genes compared: ", total_genes)
  # number of NA values in pvalue
  resume[[2]] <- paste("Number of NA values in pvalue: ", sum(is.na(result$pvalue)))
  # number of genes with significant p-value
  resume[[3]] <- paste(
    "Number of genes with significant p-value: ",
    length(which(result$padj < alpha)),
    ", ",
    round(length(which(result$padj < alpha))/total_genes*100, 2),
    "%"
  )
  # number of significant genes without correction
  resume[[4]] <- paste(
    "Number of significant genes without correction: ",
    length(which(result$pvalue < alpha)),
    ", ",
    round(length(which(result$pvalue < alpha))/total_genes*100, 2),
    "%"
  )
  # TODO: add translation of genes and add them to dataframe for table

  # create a new tabPanel
  appendTab(
    inputId = targetPanel,
    tabPanel(
      title = title,
      # summary of the results
      h4(paste("Summary of the results comparing ", contrast[1], " and ", contrast[2])),
      htmlOutput(outputId = ns(paste(contrast[1], contrast[2], "summary", sep = "_")), container = pre),
      # create table with results, that allows filtering
      DT::dataTableOutput(outputId = ns(paste(contrast[1], contrast[2], "table", sep = "_")))
    )
  )
  # server part of tabPanel
  # print the summary of the results
  output[[ns(paste(contrast[1], contrast[2], "summary", sep = "_"))]] <- renderText(
    paste(resume, collapse = "<br>")
  )
  output[[ns(paste(contrast[1], contrast[2], "table", sep = "_"))]] <- DT::renderDataTable({DT::datatable(
    data = result,
    extensions = 'Buttons',
    filter = 'top',
    rownames = T,
    colnames = c('Gene' = 1),
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      order = list(list(4, 'asc'), list(5, 'asc')),  # 2=padj, 1=pvalue
      dom = 'Bfrtip',
      lengthMenu = c(10, 25, 50, 100, -1),
      buttons = c('pageLength', 'copy', 'csv', 'excel')
    ),
    class = "cell-border compact stripe hover order-column"
  )})
}


create_new_tab_DESeq <- function(title, targetPanel, result, contrast, alpha, ns){
  # create a new tabPanel for DESeq2 preprocessing
  # title: title of the tabPanel
  # targetPanel: name of the targetPanel under which the tabPanel should be created
  # result: result of the significance analysis
  # contrast: comparisons used for the significance analysis
  # alpha: significance level
  # ns: namespace function

  print("create new DESeq tab")
  print(title)
  print(result)
  # paste together the strings to print
  # total number of genes compared
  total_genes <- length(rownames(result))
  resume <- list()
  resume[[1]] <- paste("Total number of genes compared: ", total_genes)
  # number of genes with significant p-value
  resume[[2]] <- paste(
    "Number of genes with significant p-value: ",
    length(which(result$padj < alpha)),
    ", ",
    round(length(which(result$padj < alpha))/total_genes*100, 2),
    "%"
  )
  # number of upregulated significant genes
  resume[[3]] <- paste(
    "Number of upregulated significant genes: ",
    length(which(result$padj < alpha & result$log2FoldChange > 0)),
    ", ",
    round(length(which(result$padj < alpha & result$log2FoldChange > 0))/total_genes*100, 2),
    "%"
  )
  # number of downregulated significant genes
  resume[[4]] <- paste(
    "Number of downregulated significant genes: ",
    length(which(result$padj < alpha & result$log2FoldChange < 0)),
    ", ",
    round(length(which(result$padj < alpha & result$log2FoldChange < 0))/total_genes*100, 2),
    "%"
  )
  # number of significant genes without correction
  resume[[5]] <- paste(
    "Number of significant genes without correction: ",
    length(which(result$pvalue < alpha)),
    ", ",
    round(length(which(result$pvalue < alpha))/total_genes*100, 2),
    "%"
  )
  # TODO: add translation of genes and add them to dataframe for table

  # create a new tabPanel
  appendTab(
    inputId = targetPanel,
    tabPanel(
      title = title,
      # summary of the results
      h4(paste("Summary of the results comparing ", contrast[1], " and ", contrast[2])),
      htmlOutput(outputId = ns(paste(contrast[1], contrast[2], "summary", sep = "_")), container = pre),
      # create table with results, that allows filtering
      DT::dataTableOutput(outputId = ns(paste(contrast[1], contrast[2], "table", sep = "_")))
    )
  )
  # server part of tabPanel
  # print the summary of the results
  output[[ns(paste(contrast[1], contrast[2], "summary", sep = "_"))]] <- renderText(
    paste(resume, collapse = "<br>")
  )
  output[[ns(paste(contrast[1], contrast[2], "table", sep = "_"))]] <- DT::renderDataTable({DT::datatable(
    data = as.data.frame(result),
    extensions = 'Buttons',
    filter = 'top',
    rownames = TRUE,
    colnames = c('Gene' = 1),
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      order = list(list(6, 'asc'), list(2, 'desc')),  # 6=padj, 2=log2FoldChange
      dom = 'Bfrtip',
      lengthMenu = c(10, 25, 50, 100, -1),
      buttons = c('pageLength', 'copy', 'csv', 'excel')
    ),
    class = "cell-border compact stripe hover order-column"
  )})
}


significance_analysis <- function(
  df, samples, contrasts, method, correction, contrast_level
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
      list(test_method = method, test_correction = correction),
      par_tmp$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]
    )){
      print("Results exists, skipping calculations.")
      sig_results[[names(contrasts)[comp_name]]] <- res_tmp$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]
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
    # fill res_tmp, par_tmp
    res_tmp$SigAna[[contrast_level]][[names(contrasts)[comp_name]]] <<- res
    par_tmp$SigAna[[contrast_level]][[names(contrasts)[comp_name]]]  <<- list(
      test_method = method,
      test_correction = correction
    )
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
  }
  if(par_tmp$PreProcessing_Procedure == "log10"){
    lfc_per_gene_log(means, log_base = 10)
  }else if(par_tmp$PreProcessing_Procedure == "ln"){
    lfc_per_gene_log(means, log_base = exp(1))
  }else{
    lfc_per_gene(means)
  }
}
