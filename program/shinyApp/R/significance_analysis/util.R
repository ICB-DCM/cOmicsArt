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


create_new_tab <- function(title, targetPanel, result, contrast, alpha, ns, preprocess_method, value){
  # call create_new_tab based on preprocess_method used
  # preprocess_method: preprocess_method used
  # for other parameters see create_new_tab_*
  if (preprocess_method == "vst_DESeq"){
      create_new_tab_DESeq(title, targetPanel, result, contrast, alpha, ns, value)
  }
  else{
      create_new_tab_manual(title, targetPanel, result, contrast, alpha, ns, value)
  }
}


create_new_tab_manual <- function(title, targetPanel, result, contrast, alpha, ns, value){
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
      value = value,
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
          plotlyOutput(
            outputId = ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))
          ),
          plotlyOutput(
            outputId = ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            id = ns("aligned_row"),  # Assign an ID to apply CSS
            class = "align-bottom",
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_")))),
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_")))),
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip_ui", sep = "_")))),
            column(3,
             checkboxInput(
                ns(paste(contrast[1], contrast[2], "show_legend_adj", sep = "_")),
                "Show Legend Corrected",
                value = TRUE
             ),
             checkboxInput(
                ns(paste(contrast[1], contrast[2], "show_legend_raw", sep = "_")),
                "Show Legend Uncorrected",
                value = TRUE
             ),
            )
          ),
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
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_")),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_")),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_")),
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
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano", sep = "_")),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_both", sep = "_")),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_raw", sep = "_")),
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
  
  result <- addStars(result)
  
  brks_log2FC_neg <- seq(min(result$log2FoldChange) -1, 0, length.out = 100) # -1 for towning down color match
  brks_log2FC_pos <- seq(0, max(result$log2FoldChange) +1 , length.out = 100) # +a for towning down color match
  brks <- c(brks_log2FC_neg, brks_log2FC_pos)
  clrs <- colorRampPalette(c("#0e5bcfCD","#fafafa","#cf0e5bCD"))(length(brks) + 1)
  
  brks_padj_sig <- seq(0, par_tmp[[session$token]]$SigAna$significance_level, length.out = 10)
  brks_padj_unsig <- seq(par_tmp[[session$token]]$SigAna$significance_level,1, length.out = 10)
  brks_padj <- c(brks_padj_sig, brks_padj_unsig)
  clrs_padj <- colorRampPalette(c("#ffce78","#fafafa","#fafafa"))(length(brks_padj) + 1)
  
  output[[ns(paste(contrast[1], contrast[2], "table", sep = "_"))]] <- 
    DT::renderDataTable({
      DT::datatable(
        data = result,
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
          order = list(list(7, 'asc'), list(3, 'desc')),  # 6=padj, 2=log2FoldChange
          dom = 'Bfrtip',
          lengthMenu = c(10, 25, 50, 100, -1),
          buttons = list(
            list(extend = 'colvis', text = 'Show/Hide Columns'),
            'pageLength', 'copy', 'csv', 'excel'),
          columnDefs = list(
            list(searchable = FALSE, targets = which(colnames(result) == "sig_level")),  # Disable filter for "sig_level"
            list(visible = FALSE, targets = c(2, 4, 5, 6))  # Hide specific columns initially 
          )
        ),
        escape = F,
        class = "cell-border compact stripe hover order-column"
      ) %>%
        formatStyle(
          c("log2FoldChange"), 
          backgroundColor = styleInterval(brks, clrs)
        ) %>%
        formatStyle(
          c("padj","pvalue"),
          backgroundColor = styleInterval(brks_padj, clrs_padj)
        ) %>%
        formatSignif(
          c("log2FoldChange","baseMean","lfcSE","stat","pvalue","padj"),
          digits = 4,
          interval = 3,
          dec.mark = getOption("OutDec"),
          zero.print = NULL,
          rows = NULL
        )
    })

  psig_th <- ns(paste(contrast[1], contrast[2], "psig_th", sep = "_"))
  lfc_th <- ns(paste(contrast[1], contrast[2], "lfc_th", sep = "_"))
  Volcano_anno_tooltip <- ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip", sep = "_"))
  show_legend_adj <- ns(paste(contrast[1], contrast[2], "show_legend_adj", sep = "_"))
  show_legend_raw <- ns(paste(contrast[1], contrast[2], "show_legend_raw", sep = "_"))
  
  output[[ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip_ui", sep = "_"))]] <- renderUI({
    selectInput(
    inputId = Volcano_anno_tooltip,
    label = "Select the anno to be shown at tooltip",
    choices = colnames(rowData(res_tmp[[session$token]]$data)),
    selected = colnames(rowData(res_tmp[[session$token]]$data))[1],
    multiple = F
    )
  })
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
      input[[lfc_th]],
      input[[Volcano_anno_tooltip]]
    )
  })
  observeEvent(input[[show_legend_adj]], {
    print("Show legend adjusted")
    plotlyProxy(paste(contrast[1], contrast[2], "Volcano", sep = "_")) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[show_legend_adj]]))
  })
  observeEvent(input[[show_legend_raw]], {
    print("Show legend raw")
    plotlyProxy(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_")) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[show_legend_raw]]))
  })
  observeEvent(toPlotVolcano(), {
    print("Plot volcano")
    req(input[[psig_th]], input[[lfc_th]], input[[Volcano_anno_tooltip]])
    waiter <- Waiter$new(
      id=ns(paste(contrast[1], contrast[2], "test", sep = "_")),
      html = LOADING_SCREEN,
      color="#70BF4F47",
      hide_on_render=FALSE
    )
    waiter$show()
    on.exit({
      waiter$hide()
    })
    # workaround, as somehow the input values dont show up unless we change it in the shiny
    # TODO: fix this (@Lea?)
    sig_ana_reactive$th_psig <- input[[psig_th]]
    sig_ana_reactive$th_lfc <- input[[lfc_th]]
    sig_ana_reactive$Volcano_anno_tooltip <- input[[Volcano_anno_tooltip]]
    # plot volcano plot
    data4Volcano <- result
    data4Volcano$probename <- rownames(data4Volcano)
    data4Volcano$threshold <- ifelse(data4Volcano$padj>sig_ana_reactive$th_psig,"Non-Sig","Sig")
    data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>sig_ana_reactive$th_psig,"Non-Sig","Sig")
    data4Volcano$threshold_fc <- ifelse(
      data4Volcano$log2FoldChange>sig_ana_reactive$th_lfc,
      "Up",
      ifelse(
        data4Volcano$log2FoldChange<(-sig_ana_reactive$th_lfc),
        "Down", " "
      )
    )

    data4Volcano$combined <- ifelse(
      data4Volcano$threshold_fc == " ",
      data4Volcano$threshold,
      paste(data4Volcano$threshold, data4Volcano$threshold_fc, sep = " + ")
    )

    data4Volcano$combined_raw <- ifelse(
      data4Volcano$threshold_fc == " ",
      data4Volcano$threshold_raw,
      paste(data4Volcano$threshold_raw, data4Volcano$threshold_fc, sep = " + ")
    )
    colorScheme2 <- c("#cf0e5bCD", "#0e5bcfCD", "#939596CD","#cf0e5b1A", "#0e5bcf1A", "#9395961A")
    names(colorScheme2) <- c(
      "Sig + Up", "Sig + Down", "Sig",
      "Non-Sig + Up", "Non-Sig + Down", "Non-Sig"
    )


    # remove NA values
    sig_ana_reactive$data4Volcano <- data4Volcano[complete.cases(data4Volcano),]
    sig_ana_reactive$data4Volcano$chosenAnno <- rowData(res_tmp[[session$token]]$data)[rownames(sig_ana_reactive$data4Volcano),input[[Volcano_anno_tooltip]]]
    sig_ana_reactive$VolcanoPlot <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=chosenAnno)
    ) +
      geom_point(aes(
        x = -log10(padj),
        y = log2FoldChange,
        colour = combined
      )) +
      geom_vline(
        xintercept = -log10(sig_ana_reactive$th_psig),
        color="lightgrey"
        ) +
      geom_hline(
        yintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
        color="lightgrey"
        ) +
      scale_color_manual(values=colorScheme2, name="") +
      ylab("Log FoldChange") +
      xlab("-log10(p_adj-value)") +
      ggtitle(label="Corrected p-Values") +
      CUSTOM_THEME


    sig_ana_reactive$VolcanoPlot_raw <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=chosenAnno)
    ) +
      geom_point(aes(
          x = -log10(pvalue),
          y = log2FoldChange,
          colour = combined_raw)) +
      geom_vline(
          xintercept = -log10(sig_ana_reactive$th_psig),
          color="lightgrey"
      ) +
      geom_hline(
          yintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
          color="lightgrey"
      ) +
      scale_color_manual(values=colorScheme2, name="") +
      ylab("Log FoldChange") +
      xlab("-log10(p-value)") +
      ggtitle(label="Uncorrected p-Values") +
      CUSTOM_THEME
    
    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot,
      tooltip = ifelse(is.null(sig_ana_reactive$Volcano_anno_tooltip),"all","chosenAnno"),
      legendgroup = "color"
    ) %>% layout(
      showlegend = TRUE
    )})
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot_raw,
      tooltip = ifelse(is.null(sig_ana_reactive$Volcano_anno_tooltip),"all","chosenAnno"),
      legendgroup = "color"
    ) %>% layout(
      showlegend = TRUE
    )})
  })

  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]], {
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_val", sep = "_")]])){
          req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]],{
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_both_val", sep = "_")]])){
        req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_both_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
                           sig_ana_reactive$data4Volcano, contrast, file_path)
      #log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]],{
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_raw_val", sep = "_")]])){
        req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_raw_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  output[[ns("getR_Code_Volcano")]] <- downloadHandler(
      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
        par_tmp[[session$token]]$SigAna$contrast <<- contrast

        envList <- list(
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        if(par_tmp[[session$token]]$PreProcessing_Procedure == "vst_DESeq"){
          if (useBatch) {
            envList$dds <- res_tmp[[session$token]]$DESeq_obj_batch_corrected
          } else {
            envList$dds <- res_tmp[[session$token]]$DESeq_obj
          }
        }
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)

        write(
          getPlotCode(22),
          file.path(temp_directory, "Code.R")
        )
        saveRDS(envList, file.path(temp_directory, "Data.RDS"))

        #TODO
        # Needs an extra sourcing to have in correct env - potential fix sourcing module specific functions within module
        # instead of sourcing all - or having them all globablly source (like general utils)
        source("R/significance_analysis/util.R")
        source("R/SourceAll.R")

        save.function.from.env(wanted = c("significance_analysis",
                                          "filter_significant_result",
                                          "getLFC",
                                          "map_intersects_for_highlight",
                                          "prepare_upset_plot",
                                          "filter_rna"),
                              file = file.path(temp_directory, "utils.R"))
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        waiter$hide()
      },
      contentType = "application/zip"
    )

    output[[ns("getR_Code_Volcano_raw")]] <- downloadHandler(

      filename = function(){
        paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
      },
      content = function(file){
        waiter <- Waiter$new(
          html = LOADING_SCREEN,
          color = "#3897F147",
          hide_on_render = FALSE
        )
        waiter$show()
        tmp <- getUserReactiveValues(input)
        par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
        par_tmp[[session$token]]$SigAna$contrast <<- contrast

        envList <- list(
          res_tmp = res_tmp[[session$token]],
          par_tmp = par_tmp[[session$token]]
        )
        useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
        if(par_tmp[[session$token]]$PreProcessing_Procedure == "vst_DESeq"){
          if (useBatch) {
            envList$dds <- res_tmp[[session$token]]$DESeq_obj_batch_corrected
          } else {
            envList$dds <- res_tmp[[session$token]]$DESeq_obj
          }
        }
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)

        write(
          getPlotCode(23),
          file.path(temp_directory, "Code.R")
        )
        saveRDS(envList, file.path(temp_directory, "Data.RDS"))

        #TODO
        # Needs an extra sourcing to have in correct env - potential fix sourcing module specific functions within module
        # instead of sourcing all - or having them all globablly source (like general utils)
        source("R/significance_analysis/util.R")
        source("R/SourceAll.R")

        save.function.from.env(wanted = c("significance_analysis",
                                          "filter_significant_result",
                                          "getLFC",
                                          "map_intersects_for_highlight",
                                          "prepare_upset_plot",
                                          "filter_rna"),
                               file = file.path(temp_directory, "utils.R"))
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        waiter$hide()
      },
      contentType = "application/zip"
    )

  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano", sep = "_"))]] <- downloadHandler(
    filename = function() {paste0("VOLCANO_", Sys.time(), input[[ns("file_ext_Volcano")]])},
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot,
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast, file_path)
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))
      })
    })
  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_raw", sep = "_"))]] <- downloadHandler(
    filename = function() { paste("raw_VOLCANO",Sys.time(),input[[ns("file_ext_Volcano_raw")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot_raw,
        device = gsub("\\.","",input[[ns("file_ext_Volcano_raw")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))
      })
    })
  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_both", sep = "_"))]] <- downloadHandler(
    filename = function() { paste0("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
                             sig_ana_reactive$data4Volcano, contrast, file_path)
#        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))
      })
    })

}


create_new_tab_DESeq <- function(title, targetPanel, result, contrast, alpha, ns, value){
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
      value = value,
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
          plotlyOutput(
            outputId = ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))
          ),
          plotlyOutput(
            outputId = ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))
          ),
          hr(style = "border-top: 1px solid #000000;"),
          fluidRow(
            id = ns("aligned_row"),  # Assign an ID to apply CSS
            class = "align-bottom",
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "psig_th_ui", sep = "_")))),
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "lfc_th_ui", sep = "_")))),
            column(3, uiOutput(outputId = ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip_ui", sep = "_")))),
            column(3,
             checkboxInput(
                ns(paste(contrast[1], contrast[2], "show_legend_adj", sep = "_")),
                "Show Legend Corrected",
                value = TRUE
             ),
             checkboxInput(
                ns(paste(contrast[1], contrast[2], "show_legend_raw", sep = "_")),
                "Show Legend Uncorrected",
                value = TRUE
             ),
            )
          ),
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
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_")),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_")),
              label = "Send only to Report",
              class = "btn-info"
            ),
            actionButton(
              inputId = ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_")),
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
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano", sep = "_")),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_both", sep = "_")),
              label = "Save plot",
              class = "btn-info"
            ),
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_raw", sep = "_")),
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
    th_lfc = NULL,
    Volcano_anno_tooltip = NULL
  )
  # print the summary of the results
  output[[ns(paste(contrast[1], contrast[2], "summary", sep = "_"))]] <- renderText(
    paste(resume, collapse = "<br>")
  )

  result <- addStars(result)

  brks_log2FC_neg <- seq(min(result$log2FoldChange) -1, 0, length.out = 100) # -1 for towning down color match
  brks_log2FC_pos <- seq(0, max(result$log2FoldChange) +1 , length.out = 100) # +a for towning down color match
  brks <- c(brks_log2FC_neg, brks_log2FC_pos)
  clrs <- colorRampPalette(c("#0e5bcfCD","#fafafa","#cf0e5bCD"))(length(brks) + 1)

  brks_padj_sig <- seq(0, par_tmp[[session$token]]$SigAna$significance_level, length.out = 10)
  brks_padj_unsig <- seq(par_tmp[[session$token]]$SigAna$significance_level,1, length.out = 10)
  brks_padj <- c(brks_padj_sig, brks_padj_unsig)
  clrs_padj <- colorRampPalette(c("#ffce78","#fafafa","#fafafa"))(length(brks_padj) + 1)
  
  output[[ns(paste(contrast[1], contrast[2], "table", sep = "_"))]] <- 
    DT::renderDataTable({
      DT::datatable(
        data = result,
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
          order = list(list(7, 'asc'), list(3, 'desc')),  # 6=padj, 2=log2FoldChange
          dom = 'Bfrtip',
          lengthMenu = c(10, 25, 50, 100, -1),
          buttons = list(
            list(extend = 'colvis', text = 'Show/Hide Columns'),
            'pageLength', 'copy', 'csv', 'excel'),
          columnDefs = list(
            list(searchable = FALSE, targets = which(colnames(result) == "sig_level")),  # Disable filter for "sig_level"
            list(visible = FALSE, targets = c(2, 4, 5, 6))  # Hide specific columns initially 
          )
        ),
        escape = F,
        class = "cell-border compact stripe hover order-column"
      ) %>%
        formatStyle(
          c("log2FoldChange"), 
          backgroundColor = styleInterval(brks, clrs)
          ) %>%
        formatStyle(
          c("padj","pvalue"),
          backgroundColor = styleInterval(brks_padj, clrs_padj)
          ) %>%
        formatSignif(
          c("log2FoldChange","baseMean","lfcSE","stat","pvalue","padj"),
          digits = 4,
          interval = 3,
          dec.mark = getOption("OutDec"),
          zero.print = NULL,
          rows = NULL
        )
      })

  psig_th <- ns(paste(contrast[1], contrast[2], "psig_th", sep = "_"))
  lfc_th <- ns(paste(contrast[1], contrast[2], "lfc_th", sep = "_"))
  Volcano_anno_tooltip <- ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip", sep = "_"))
  show_legend_adj <- ns(paste(contrast[1], contrast[2], "show_legend_adj", sep = "_"))
  show_legend_raw <- ns(paste(contrast[1], contrast[2], "show_legend_raw", sep = "_"))

  output[[ns(paste(contrast[1], contrast[2], "Volcano_anno_tooltip_ui", sep = "_"))]] <- renderUI({
    selectInput(
      inputId = Volcano_anno_tooltip,
      label = "Select the anno to be shown at tooltip",
      choices = colnames(rowData(res_tmp[[session$token]]$data)),
      selected = colnames(rowData(res_tmp[[session$token]]$data))[1],
      multiple = F
    )
  })
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
  observeEvent(input[[show_legend_adj]], {
    print("Show legend adjusted")
    plotlyProxy(paste(contrast[1], contrast[2], "Volcano", sep = "_")) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[show_legend_adj]]))
  })
  observeEvent(input[[show_legend_raw]], {
    print("Show legend raw")
    plotlyProxy(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_")) %>%
      plotlyProxyInvoke(method = "relayout", list(showlegend = input[[show_legend_raw]]))
  })
  observeEvent(toPlotVolcano(), {
    req(input[[psig_th]], input[[lfc_th]])
    waiter <- Waiter$new(
      id=ns(paste(contrast[1], contrast[2], "test", sep = "_")),
      html = LOADING_SCREEN,
      color="#70BF4F47",
      hide_on_render=FALSE
    )
    waiter$show()
    on.exit({
      waiter$hide()
    })
    # work
    sig_ana_reactive$th_psig <- input[[psig_th]]
    sig_ana_reactive$th_lfc <- input[[lfc_th]]
    sig_ana_reactive$annotip <- input[[Volcano_anno_tooltip]]
    # plot volcano plot
    data4Volcano <- as.data.frame(result)
    data4Volcano$probename <- rownames(data4Volcano)
    data4Volcano$threshold <- ifelse(data4Volcano$padj>sig_ana_reactive$th_psig,"Non-Sig","significant")
    data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>sig_ana_reactive$th_psig,"Non-Sig","significant")
    data4Volcano$threshold_fc <- ifelse(
      data4Volcano$log2FoldChange>sig_ana_reactive$th_lfc,
      "Up",
      ifelse(
        data4Volcano$log2FoldChange<(-sig_ana_reactive$th_lfc),
        "Down", " "
      )
    )
    data4Volcano$combined <- paste0(data4Volcano$threshold," + ",data4Volcano$threshold_fc)
    data4Volcano$combined_raw <- paste0(data4Volcano$threshold_raw," + ",data4Volcano$threshold_fc)
    colorScheme2 <- c("#cf0e5bCD", "#0e5bcfCD", "#939596CD","#cf0e5b1A", "#0e5bcf1A", "#9395961A")
    names(colorScheme2) <- c(
      "significant + Up", "significant + Down", "significant +  ",
      "Non-Sig + Up", "Non-Sig + Down", "Non-Sig +  "
    )

    # remove NA values
    sig_ana_reactive$data4Volcano <- data4Volcano[complete.cases(data4Volcano),]
    sig_ana_reactive$data4Volcano$chosenAnno <- rowData(res_tmp[[session$token]]$data)[rownames(sig_ana_reactive$data4Volcano),input[[Volcano_anno_tooltip]]]
    
    sig_ana_reactive$VolcanoPlot <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=chosenAnno)
    ) +
      geom_point(aes(
        x = -log10(padj),
        y = log2FoldChange,
        colour = combined
      )) +
      geom_vline(
        xintercept = -log10(sig_ana_reactive$th_psig),
        color="lightgrey"
        ) +
      geom_hline(
        yintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
        color="lightgrey"
        ) +
      scale_color_manual(values=colorScheme2, name="") +
      ylab("Log FoldChange") +
      xlab("-log10(p_adj-value)") +
      ggtitle(label="Corrected p-Values") +
      CUSTOM_THEME


    sig_ana_reactive$VolcanoPlot_raw <- ggplot(
      sig_ana_reactive$data4Volcano,
      aes(label=chosenAnno)
    ) +
      geom_point(aes(
          x = -log10(pvalue),
          y = log2FoldChange,
          colour = combined_raw)) +
      geom_vline(
          xintercept = -log10(sig_ana_reactive$th_psig),
          color="lightgrey"
      ) +
      geom_hline(
          yintercept = c(-sig_ana_reactive$th_lfc,sig_ana_reactive$th_lfc),
          color="lightgrey"
      ) +
      scale_color_manual(values=colorScheme2, name="") +
      ylab("Log FoldChange") +
      xlab("-log10(p-value)") +
      ggtitle(label="Uncorrected p-Values") +
      CUSTOM_THEME
    
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot_raw,
      tooltip = ifelse(is.null(sig_ana_reactive$Volcano_anno_tooltip),"all","chosenAnno"),
      legendgroup = "color"
    ) %>% layout(
      showlegend = TRUE
    )})

    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({ggplotly(
      sig_ana_reactive$VolcanoPlot  + theme(legend.position = "none"),
      tooltip = ifelse(is.null(sig_ana_reactive$Volcano_anno_tooltip),"all","chosenAnno"),
      legendgroup="color"
    ) %>% layout(
      showlegend = TRUE
    )})
  })

  # downloadhandlers
  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]], {
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_val", sep = "_")]])){
          req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(sig_ana_reactive$VolcanoPlot, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]],{
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_both_val", sep = "_")]])){
        req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_both", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_both_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
                           sig_ana_reactive$data4Volcano, contrast, file_path)
      #log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  session$userData[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]] <- observeEvent(
    input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]],{
      if(!is.null(session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_raw_val", sep = "_")]])){
        req(input[[ns(paste(contrast[1], contrast[2], "only2Report_Volcano_raw", sep = "_"))]] > session$userData[[paste(contrast[1], contrast[2], "only2Report_Volcano_raw_val", sep = "_")]])
      }
      fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
      fun_LogIt(message = "### Info")
      log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
      fun_LogIt(message = "### Publication Snippet")
      fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                         params = par_tmp[[session$token]]))
    }
  )

  output[[ns("getR_Code_Volcano")]] <- downloadHandler(
    filename = function(){
      paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
    },
    content = function(file){
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color = "#3897F147",
        hide_on_render = FALSE
      )
      waiter$show()
      tmp <- getUserReactiveValues(input)
      par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
      par_tmp[[session$token]]$SigAna$contrast <<- contrast

      envList <- list(
        res_tmp = res_tmp[[session$token]],
        par_tmp = par_tmp[[session$token]]
      )
      useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
      if(par_tmp[[session$token]]$PreProcessing_Procedure == "vst_DESeq"){
        if (useBatch) {
          envList$dds <- res_tmp[[session$token]]$DESeq_obj_batch_corrected
        } else {
          envList$dds <- res_tmp[[session$token]]$DESeq_obj
        }
      }
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      write(
        getPlotCode(22),
        file.path(temp_directory, "Code.R")
      )
      saveRDS(envList, file.path(temp_directory, "Data.RDS"))

      #TODO
      # Needs an extra sourcing to have in correct env - potential fix sourcing module specific functions within module
      # instead of sourcing all - or having them all globablly source (like general utils)
      source("R/significance_analysis/util.R")
      source("R/SourceAll.R")

      save.function.from.env(wanted = c("significance_analysis",
                                        "filter_significant_result",
                                        "getLFC",
                                        "map_intersects_for_highlight",
                                        "prepare_upset_plot",
                                        "filter_rna"),
                            file = file.path(temp_directory, "utils.R"))
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      waiter$hide()
    },
    contentType = "application/zip"
  )

  output[[ns("getR_Code_Volcano_raw")]] <- downloadHandler(

    filename = function(){
      paste0("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip")
    },
    content = function(file){
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color = "#3897F147",
        hide_on_render = FALSE
      )
      waiter$show()
      tmp <- getUserReactiveValues(input)
      par_tmp[[session$token]]$SigAna[names(tmp)] <<- tmp
      par_tmp[[session$token]]$SigAna$contrast <<- contrast

      envList <- list(
        res_tmp = res_tmp[[session$token]],
        par_tmp = par_tmp[[session$token]]
      )
      useBatch <- ifelse(par_tmp[[session$token]]$BatchColumn != "NULL" && input$UseBatch == "Yes",T,F)
      if(par_tmp[[session$token]]$PreProcessing_Procedure == "vst_DESeq"){
        if (useBatch) {
          envList$dds <- res_tmp[[session$token]]$DESeq_obj_batch_corrected
        } else {
          envList$dds <- res_tmp[[session$token]]$DESeq_obj
        }
      }
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)

      write(
        getPlotCode(23),
        file.path(temp_directory, "Code.R")
      )
      saveRDS(envList, file.path(temp_directory, "Data.RDS"))

      #TODO
      # Needs an extra sourcing to have in correct env - potential fix sourcing module specific functions within module
      # instead of sourcing all - or having them all globablly source (like general utils)
      source("R/significance_analysis/util.R")
      source("R/SourceAll.R")

      save.function.from.env(wanted = c("significance_analysis",
                                        "filter_significant_result",
                                        "getLFC",
                                        "map_intersects_for_highlight",
                                        "prepare_upset_plot",
                                        "filter_rna"),
                             file = file.path(temp_directory, "utils.R"))
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      waiter$hide()
    },
    contentType = "application/zip"
  )

  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano", sep = "_"))]] <- downloadHandler(
    filename = function() { paste("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot,
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(
          sig_ana_reactive$VolcanoPlot,
          sig_ana_reactive$data4Volcano, contrast, file_path
        )
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))
      })
    })
  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_raw", sep = "_"))]] <- downloadHandler(
    filename = function() { paste("raw_VOLCANO",Sys.time(),input[[ns("file_ext_Volcano_raw")]],sep="") },
    content = function(file){
      ggsave(
        filename = file,
        plot = sig_ana_reactive$VolcanoPlot_raw,
        device = gsub("\\.","",input[[ns("file_ext_Volcano_raw")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))

      })
    })
  output[[ns(paste(contrast[1], contrast[2], "SavePlot_Volcano_both", sep = "_"))]] <- downloadHandler(
    filename = function() { paste0("VOLCANO_",Sys.time(),input[[ns("file_ext_Volcano")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano")]])
        )
      on.exit({
        fun_LogIt(message = "## Differential analysis - Volcano {.tabset .tabset-fade}")
        fun_LogIt(message = "### Info")
        log_messages_volcano(gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot)
                             , sig_ana_reactive$data4Volcano, contrast, file_path)
        #log_messages_volcano(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$data4Volcano, contrast, file_path)
        fun_LogIt(message = "### Publication Snippet")
        fun_LogIt(message = snippet_SigAna(data = res_tmp[[session$token]],
                                           params = par_tmp[[session$token]]))
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
