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
      tabsetPanel(
        # Table
        tabPanel(
          title = "Table",
          # summary of the results
          h4(paste("Summary of the results comparing ", contrast[1], " and ", contrast[2])),
          htmlOutput(outputId = ns(paste(contrast[1], contrast[2], "summary", sep = "_")), container = pre),
          # create table with results, that allows filtering
          DT::dataTableOutput(outputId = ns(paste(contrast[1], contrast[2], "table", sep = "_")))
        ),
        tabPanel(
          title = "Volcano",
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("40%", "60%"),
            plotlyOutput(
              outputId = ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))
            ) %>% withSpinner(type = 8),
            plotlyOutput(
              outputId = ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))
            )
          ),
          hr(style = "border-top: 1px solid #000000;"),
          uiOutput(outputId = ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_"))),
          uiOutput(outputId = ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_"))),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            h5("Volcano plot padj"),
            h5("Both Volcano plots"),
            h5("Volcano plot pvalue")
          ) %>% helper(type = "markdown", content = "SampleCorr_Downloads"),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            actionButton(
              inputId = ns("only2Report_Volcano"),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns("only2Report_Volcano_both"),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns("only2Report_Volcano_raw"),
              label = "Send only to Report",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            downloadButton(
              outputId = ns("getR_Code_Volcano"),
              label = "Get underlying R code and data",
              icon = icon("code")
            ),
            NULL,
            # downloadButton(
            #   outputId = ns("getR_Code_Volcano_both"),
            #   label = "Get underlying R code and data",
            #   icon = icon("code")
            # ),
            downloadButton(
              outputId = ns("getR_Code_Volcano_raw"),
              label = "Get underlying R code and data",
              icon = icon("code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            downloadButton(
              outputId = ns("SavePlot_Volcano"),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns("SavePlot_Volcano_both"),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns("SavePlot_Volcano_raw"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_both"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_raw"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            )
          )
        )
      )
    )
  )
  # server part of tabPanel
  # reactive values
  sig_ana_reactive <- reactiveValues(
    th_psig = NULL,
    th_lfc = NULL
  )
  # print the summary of the results into the table
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
      order = list(list(4, 'asc'), list(5, 'asc')),  # 4=padj, 5=pvalue
      dom = 'Bfrtip',
      lengthMenu = c(10, 25, 50, 100, -1),
      buttons = c('pageLength', 'copy', 'csv', 'excel')
    ),
    class = "cell-border compact stripe hover order-column"
  )})

  psig_th <- ns(paste(contrast[1], contrast[2], "psig_th", sep = "_"))
  lfc_th <- ns(paste(contrast[1], contrast[2], "lfc_th", sep = "_"))
  output[[ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_"))]] <- renderUI({
    numericInput(
      inputId = ns(paste(contrast[1], contrast[2], "psig_th", sep = "_")),
      label = "adj. p-value threshold",
      min=0,
      max=0.1,
      step=0.01,
      value = 0.05
      )
  })
  output[[ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_"))]] <- renderUI({
    numericInput(
      inputId = lfc_th,
      label = "Log FC threshold (both sides!)",
      min = 0,
      max = 10,
      step = 0.1,
      value = 1.0
      )
  })
  toPlotVolcano <- reactive({
    list(
      input[[psig_th]],
      input[[lfc_th]]
    )
  })
  observeEvent(toPlotVolcano(), {
    req(input[[psig_th]], input[[lfc_th]])
    # workaround, as somehow the input values dont show up unless we change it in the shiny
    # TODO: fix this (@Lea?)
    sig_ana_reactive$th_psig <- input[[psig_th]]
    sig_ana_reactive$th_lfc <- input[[lfc_th]]
    # plot volcano plot
    data4Volcano <- result
    data4Volcano$probename <- rownames(data4Volcano)
    data4Volcano$threshold <- ifelse(data4Volcano$padj>sig_ana_reactive$th_psig,"non-significant","significant")
    data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>sig_ana_reactive$th_psig,"non-significant","significant")
    data4Volcano$threshold_fc <- ifelse(
      data4Volcano$log2FoldChange>sig_ana_reactive$th_lfc,
      "up-regulated",
      ifelse(
        data4Volcano$log2FoldChange<(-sig_ana_reactive$th_lfc),
        "down-regulated", " "
      )
    )
    data4Volcano$combined <- paste0(data4Volcano$threshold," + ",data4Volcano$threshold_fc)
    data4Volcano$combined_raw <- paste0(data4Volcano$threshold_raw," + ",data4Volcano$threshold_fc)
    colorScheme2 <- c("#cf0e5bCD", "#0e5bcfCD", "#939596CD","#cf0e5b1A", "#0e5bcf1A", "#9395961A")
    names(colorScheme2) <- c(
      "significant + up-regulated", "significant + down-regulated", "significant +  ",
      "non-significant + up-regulated", "non-significant + down-regulated", "non-significant +  "
    )

    # remove NA values
    sig_ana_reactive$data4Volcano <- data4Volcano[complete.cases(data4Volcano),]

    sig_ana_reactive$VolcanoPlot <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=probename)
    ) +
      geom_point(aes(
        x = log2FoldChange,
        y = -log10(padj),
        colour = combined
      )) +
      geom_hline(
        yintercept = -log10(sig_ana_reactive$th_psig),
        color="lightgrey"
        ) +
      geom_vline(
        xintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
        color="lightgrey"
        ) +
      scale_color_manual(values=colorScheme2, name="") +
      xlab("Log FoldChange") +
      ylab("-log10(p_adj-value)") +
      theme(legend.position = "none") +
      ggtitle(label="Corrected p-Values")
    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot,
      legendgroup="color"
    )})
    sig_ana_reactive$VolcanoPlot_raw <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=probename)
    ) +
      geom_point(aes(
          x = log2FoldChange,
          y = -log10(pvalue),
          colour = combined_raw)) +
      geom_hline(
          yintercept = -log10(sig_ana_reactive$th_psig),
          color="lightgrey"
      ) +
      geom_vline(
          xintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
          color="lightgrey"
      ) +
      scale_color_manual(values=colorScheme2, name="") +
      xlab("Log FoldChange") +
      ylab("-log10(p-value)") +
      ggtitle(label="Uncorrected p-Values")
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot_raw,
      legendgroup="color"
    )})
  })

  # downloadhandlers
  observeEvent(input[[ns("only2Report_Volcano")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
  })
  observeEvent(input[[ns("only2Report_Volcano_raw")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
    log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
  })
  observeEvent(input[[ns("only2Report_Volcano_both")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
  })
  output[[ns("SavePlot_Volcano")]] <- downloadHandler(
    filename = function() { paste("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot,
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
      })
    })
  output[[ns("SavePlot_Volcano_raw")]] <- downloadHandler(
    filename = function() { paste("raw_VOLCANO",Sys.time(),input[[ns("file_ext_Volcano_raw")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot_raw,
        device = gsub("\\.","",input[[ns("file_ext_Volcano_raw")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
      })
    })
  output[[ns("SavePlot_Volcano_both")]] <- downloadHandler(
    filename = function() { paste0("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
      })
    })

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
      tabsetPanel(
        # Table
        tabPanel(
          title = "Table",
          # summary of the results
          h4(paste("Summary of the results comparing ", contrast[1], " and ", contrast[2])),
          htmlOutput(outputId = ns(paste(contrast[1], contrast[2], "summary", sep = "_")), container = pre),
          # create table with results, that allows filtering
          DT::dataTableOutput(outputId = ns(paste(contrast[1], contrast[2], "table", sep = "_")))
        ),
        tabPanel(
          title = "Volcano",
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("40%", "60%"),
            plotlyOutput(
              outputId = ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))
            ) %>% withSpinner(type = 8),
            plotlyOutput(
              outputId = ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))
            )
          ),
          hr(style = "border-top: 1px solid #000000;"),
          uiOutput(outputId = ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_"))),
          uiOutput(outputId = ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_"))),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            h5("Volcano plot padj"),
            h5("Both Volcano plots"),
            h5("Volcano plot pvalue")
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            actionButton(
              inputId = ns("only2Report_Volcano"),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns("only2Report_Volcano_both"),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns("only2Report_Volcano_raw"),
              label = "Send only to Report",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            downloadButton(
              outputId = ns("getR_Code_Volcano"),
              label = "Get underlying R code and data",
              icon = icon("code")
            ),
            downloadButton(
              outputId = ns("getR_Code_Volcano_both"),
              label = "Get underlying R code and data",
              icon = icon("code")
            ),
            downloadButton(
              outputId = ns("getR_Code_Volcano_raw"),
              label = "Get underlying R code and data",
              icon = icon("code")
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            downloadButton(
              outputId = ns("SavePlot_Volcano"),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns("SavePlot_Volcano_both"),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns("SavePlot_Volcano_raw"),
              label = "Save plot",
              class = "btn-info"
            )
          ),
          splitLayout(
            style = "border: 1px solid silver:",
            cellWidths = c("35%","35%", "30%"),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_both"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_raw"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf"),
              selected = ".png"
            )
          )
        )
      )
    )
  )
  # server part of tabPanel
  # reactive values
  sig_ana_reactive <- reactiveValues(
    th_psig = NULL,
    th_lfc = NULL
  )
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

  psig_th <- ns(paste(contrast[1], contrast[2], "psig_th", sep = "_"))
  lfc_th <- ns(paste(contrast[1], contrast[2], "lfc_th", sep = "_"))
  output[[ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_"))]] <- renderUI({
    numericInput(
      inputId = ns(paste(contrast[1], contrast[2], "psig_th", sep = "_")),
      label = "adj. p-value threshold",
      min=0,
      max=0.1,
      step=0.01,
      value = 0.05
      )
  })
  output[[ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_"))]] <- renderUI({
    numericInput(
      inputId = lfc_th,
      label = "Log FC threshold (both sides!)",
      min = 0,
      max = 10,
      step = 0.1,
      value = 1.0
      )
  })
  toPlotVolcano <- reactive({
    list(
      input[[psig_th]],
      input[[lfc_th]]
    )
  })
  observeEvent(toPlotVolcano(), {
    req(input[[psig_th]], input[[lfc_th]])
    sig_ana_reactive$th_psig <- input[[psig_th]]
    sig_ana_reactive$th_lfc <- input[[lfc_th]]
    # plot volcano plot
    data4Volcano <- as.data.frame(result)
    data4Volcano$probename <- rownames(data4Volcano)
    data4Volcano$threshold <- ifelse(data4Volcano$padj>sig_ana_reactive$th_psig,"non-significant","significant")
    data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>sig_ana_reactive$th_psig,"non-significant","significant")
    data4Volcano$threshold_fc <- ifelse(
      data4Volcano$log2FoldChange>sig_ana_reactive$th_lfc,
      "up-regulated",
      ifelse(
        data4Volcano$log2FoldChange<(-sig_ana_reactive$th_lfc),
        "down-regulated", " "
      )
    )
    data4Volcano$combined <- paste0(data4Volcano$threshold," + ",data4Volcano$threshold_fc)
    data4Volcano$combined_raw <- paste0(data4Volcano$threshold_raw," + ",data4Volcano$threshold_fc)
    colorScheme2 <- c("#cf0e5bCD", "#0e5bcfCD", "#939596CD","#cf0e5b1A", "#0e5bcf1A", "#9395961A")
    names(colorScheme2) <- c(
      "significant + up-regulated", "significant + down-regulated", "significant +  ",
      "non-significant + up-regulated", "non-significant + down-regulated", "non-significant +  "
    )

    # remove NA values
    sig_ana_reactive$data4Volcano <- data4Volcano[complete.cases(data4Volcano),]

    sig_ana_reactive$VolcanoPlot <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=probename)
    ) +
      geom_point(aes(
        x = log2FoldChange,
        y = -log10(padj),
        colour = combined
      )) +
      geom_hline(
        yintercept = -log10(sig_ana_reactive$th_psig),
        color="lightgrey"
        ) +
      geom_vline(
        xintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
        color="lightgrey"
        ) +
      scale_color_manual(values=colorScheme2, name="") +
      xlab("Log FoldChange") +
      ylab("-log10(p_adj-value)") +
      theme(legend.position = "none") +
      ggtitle(label="Corrected p-Values")
    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot,
      legendgroup="color"
    )})
    sig_ana_reactive$VolcanoPlot_raw <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=probename)
    ) +
      geom_point(aes(
          x = log2FoldChange,
          y = -log10(pvalue),
          colour = combined_raw)) +
      geom_hline(
          yintercept = -log10(sig_ana_reactive$th_psig),
          color="lightgrey"
      ) +
      geom_vline(
          xintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
          color="lightgrey"
      ) +
      scale_color_manual(values=colorScheme2, name="") +
      xlab("Log FoldChange") +
      ylab("-log10(p-value)") +
      ggtitle(label="Uncorrected p-Values")
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot_raw,
      legendgroup="color"
    )})
  })

  # downloadhandlers
  observeEvent(input[[ns("only2Report_Volcano")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
  })
  observeEvent(input[[ns("only2Report_Volcano_raw")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
    log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
  })
  observeEvent(input[[ns("only2Report_Volcano_both")]],{
    log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
  })
  output[[ns("SavePlot_Volcano")]] <- downloadHandler(
    filename = function() { paste("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot,
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
      })
    })
  output[[ns("SavePlot_Volcano_raw")]] <- downloadHandler(
    filename = function() { paste("raw_VOLCANO",Sys.time(),input[[ns("file_ext_Volcano_raw")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot_raw,
        device = gsub("\\.","",input[[ns("file_ext_Volcano_raw")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
      })
    })
  output[[ns("SavePlot_Volcano_both")]] <- downloadHandler(
    filename = function() { paste0("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast)
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast)
      })
    })
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


log_messages_volcano<- function(plot, table, contrast){
  notificationID <- showNotification("Saving...",duration = 0)

  tmp_filename <- paste0(
    getwd(),"/www/",paste(paste0("VOLCANO_", Sys.time(), ".png"))
    )

  ggsave(tmp_filename, plot=plot, device = "png")

  # Add Log Messages
  fun_LogIt(message = "## VOLCANO")
  fun_LogIt(message = paste(
    "**VOLCANO** - Underlying Volcano Comparison:", contrast[2],"vs", contrast[2]
  ))
  fun_LogIt(message = paste0("**VOLCANO** - ![VOLCANO](",tmp_filename,")"))

  fun_LogIt(message = paste0(
    "**VOLCANO** - The top 10 diff Expressed are the following (sorted by adj. p.val)"
  ))
  fun_LogIt(message = paste0(
    "**VOLCANO** - \n",knitr::kable(head(table[order(table$padj, table$pvalue),],10),format = "html")
  ))

  removeNotification(notificationID)
  showNotification("Saved!",type = "message", duration = 1)
}
