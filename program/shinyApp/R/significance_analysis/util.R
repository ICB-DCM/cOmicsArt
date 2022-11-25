create_new_tab <- function(title, targetPanel, result, contrast, alpha, ns){
  print("create_new_tab")
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