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
    rownames = FALSE,
    colnames = c('Gene' = 1),
    options = list(
      paging = TRUE,
      searching = TRUE,
      fixedColumns = TRUE,
      autoWidth = TRUE,
      ordering = TRUE,
      order = list(list(2, 'asc'), list(1, 'asc')),  # 2=padj, 1=pvalue
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


test_one_gene <- function(data, test_function, samples_selected, contrasts){
  # written for sapply function. Gets the row of the data and performs the test
  # data: row of the data
  # test_method: test method to use
  # alpha: significance level
  # return: p-value
  data <- t(data)
  # rename colname to "value"
  colnames(data) <- "value"
  # add a column with the group
  data$group <- samples_selected$combination
  # perform the test
  res_to_return <- test_function(
     data = data,
     formula = value ~ group,
     comparisons = contrasts
  )
  res_to_return
}


significance_analysis <- function(
  df, samples, contrasts, method, alpha, correction, contrast_level
){
  # perform significance analysis
  # df: dataframe with the data
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
        return(results$p.value)
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
    # create a dataframe with the results
    res <- data.frame(
      gene = rownames(df),
      pvalue = res,
      padj = p.adjust(res, method = correction),
      stringsAsFactors = FALSE
    )
    sig_results[[names(contrasts)[comp_name]]] <- res
    comp_name <- comp_name + 1
  }
  return(sig_results)
}
