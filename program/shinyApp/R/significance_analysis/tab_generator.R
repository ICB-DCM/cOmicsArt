# Tab generation of the significance analysis
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
  resume[[1]] <- paste("Total number of entities compared: ", total_genes)
  # number of NA values in pvalue
  resume[[2]] <- paste("Number of NA values in pvalue: ", sum(is.na(result$pvalue)))
  # number of genes with significant p-value
  resume[[3]] <- paste(
    "Number of entities with significant p-value: ",
    length(which(result$padj < alpha)),
    ", ",
    round(length(which(result$padj < alpha))/total_genes*100, 2),
    "%"
  )
  # number of significant genes without correction
  resume[[4]] <- paste(
    "Number of significant entities without correction: ",
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
          p("Note that the sig_level column will not show icons if downloaded"),
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
              outputId = ns(paste(contrast[1], contrast[2], "getR_Code_Volcano", sep = "_")),
              label = "Get underlying R code and data",
              icon = icon("code")
            ),
            NULL,
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "getR_Code_Volcano_raw", sep = "_")),
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
              choices = c(".png", ".tiff", ".pdf", ".svg"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_both"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf", ".svg"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_raw"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf", ".svg"),
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
  result <- addStars(result, alpha)

  brks_log2FC_neg <- seq(min(result$log2FoldChange, na.rm = T) -1 , 0, length.out = 100) # -1 for towning down color match
  brks_log2FC_pos <- seq(0, max(result$log2FoldChange, na.rm = T) +1 , length.out = 100) # +a for towning down color match
  brks <- c(brks_log2FC_neg, brks_log2FC_pos)
  clrs <- colorRampPalette(c("#0e5bcfCD","#fafafa","#cf0e5bCD"))(length(brks) + 1)

  brks_padj_sig <- seq(0, alpha, length.out = 10)
  brks_padj_unsig <- seq(alpha,1, length.out = 10)
  brks_padj <- c(brks_padj_sig, brks_padj_unsig)
  clrs_padj <- colorRampPalette(c("#ffce78","#fafafa","#fafafa"))(length(brks_padj) + 1)

  output[[ns(paste(contrast[1], contrast[2], "table", sep = "_"))]] <-
    DT::renderDataTable({
      DT::datatable(
        data = result,
        extensions = 'Buttons',
        filter = 'top',
        rownames = TRUE,
        colnames = c('Entitie' = 1),
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          order = list(list(5, 'asc'), list(6, 'desc')),
          dom = 'Bfrtip',
          lengthMenu = c(10, 25, 50, 100, -1),
          buttons = list(
            list(extend = 'colvis', text = 'Show/Hide Columns'),
            'pageLength',
            'copy',
            list(extend = 'csv', filename = 'significance_table'),
            list(extend = 'excel', filename = 'significance_table')
            ),
          columnDefs = list(
            list(searchable = FALSE, targets = which(colnames(result) == "sig_level")),  # Disable filter for "sig_level"
            list(visible = FALSE, targets = c(3, 4))  # Hide specific columns initially
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
          c("log2FoldChange","baseMean","stat","pvalue","padj"), # lfcStat only avail for DESeq
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

    # Ensure required inputs are available
    req(input[[psig_th]], input[[lfc_th]], input[[Volcano_anno_tooltip]])

    # Create a loading screen using a Waiter
    waiter <- Waiter$new(
      id = ns(paste(contrast[1], contrast[2], "test", sep = "_")),
      html = LOADING_SCREEN,
      color = "#70BF4F47",
      hide_on_render = FALSE
    )
    waiter$show()
    on.exit(waiter$hide())

    # Read input values locally
    th_psig       <- input[[psig_th]]
    th_lfc        <- input[[lfc_th]]
    anno_col_name <- input[[Volcano_anno_tooltip]]

    # Assume 'result' is defined elsewhere (e.g. from your analysis)
    # and 'res_tmp[[session$token]]$data' contains rowData with annotation information.
    anno_vector <- rowData(res_tmp[[session$token]]$data)[, anno_col_name]

    # Generate the volcano plots using the helper function.
    volcano_obj <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = FALSE)
    volcano_obj_raw <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = TRUE)
    sig_ana_reactive$data4Volcano <- volcano_obj$data_volcano
    sig_ana_reactive$VolcanoPlot <- volcano_obj$volcano_plt
    sig_ana_reactive$VolcanoPlot_raw <- volcano_obj_raw$volcano_plt

    par_tmp[[session$token]]$SigAna$th_psig <<- th_psig
    par_tmp[[session$token]]$SigAna$th_lfc <<- th_lfc
    par_tmp[[session$token]]$SigAna$anno_vector <<- anno_vector

    # Render the corrected volcano plot as a Plotly object
    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({
      ggplotly(volcano_obj$volcano_plt,
               tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")) %>%
        layout(showlegend = TRUE)
    })

    # Render the uncorrected volcano plot as a Plotly object
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({
      ggplotly(volcano_obj_raw$volcano_plt,
               tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")) %>%
        layout(showlegend = TRUE)
    })
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

  output[[ns(paste(contrast[1], contrast[2], "getR_Code_Volcano", sep = "_"))]] <- downloadHandler(
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
        par_tmp[[session$token]]$SigAna$comp <<- title
        par_tmp[[session$token]]$SigAna$raw <<- FALSE
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        write(
          create_workflow_script(
            pipeline_info = VOLCANO_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "SigAna",
            path_to_util = file.path(temp_directory, "util.R")
          ),
          file.path(temp_directory, "Code.R")
        )
        saveRDS(envList, file.path(temp_directory, "Data.rds"))
        zip::zip(
          zipfile = file,
          files = dir(temp_directory),
          root = temp_directory
        )
        waiter$hide()
      },
      contentType = "application/zip"
    )

    output[[ns(paste(contrast[1], contrast[2], "getR_Code_Volcano_raw", sep = "_"))]] <- downloadHandler(

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
        par_tmp[[session$token]]$SigAna$comp <<- title
        par_tmp[[session$token]]$SigAna$raw <<- TRUE
        envList <- list(
          par_tmp = par_tmp[[session$token]]
        )
        temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
        dir.create(temp_directory)
        # save csv files
        save_summarized_experiment(
          res_tmp[[session$token]]$data_original,
          temp_directory
        )
        write(
          create_workflow_script(
            pipeline_info = VOLCANO_PIPELINE,
            par = par_tmp[[session$token]],
            par_mem = "SigAna",
            path_to_util = file.path(temp_directory, "util.R")
          ),
          file.path(temp_directory, "Code.R")
        )
        saveRDS(envList, file.path(temp_directory, "Data.rds"))
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
    filename = function() {paste0("VOLCANO_", format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input[[ns("file_ext_Volcano")]])},
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
    filename = function() { paste("raw_VOLCANO",format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"),input[[ns("file_ext_Volcano_raw")]],sep="") },
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
    filename = function() { paste0("VOLCANO_",format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"),input[[ns("file_ext_Volcano_both")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano_both")]])
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
  resume[[1]] <- paste("Total number of entities compared: ", total_genes)
  # number of genes with significant p-value
  resume[[2]] <- paste(
    "Number of entities with significant p-value: ",
    length(which(result$padj < alpha)),
    ", ",
    round(length(which(result$padj < alpha))/total_genes*100, 2),
    "%"
  )
  # number of upregulated significant genes
  resume[[3]] <- paste(
    "Number of upregulated significant entities: ",
    length(which(result$padj < alpha & result$log2FoldChange > 0)),
    ", ",
    round(length(which(result$padj < alpha & result$log2FoldChange > 0))/total_genes*100, 2),
    "%"
  )
  # number of downregulated significant genes
  resume[[4]] <- paste(
    "Number of downregulated significant entities: ",
    length(which(result$padj < alpha & result$log2FoldChange < 0)),
    ", ",
    round(length(which(result$padj < alpha & result$log2FoldChange < 0))/total_genes*100, 2),
    "%"
  )
  # number of significant genes without correction
  resume[[5]] <- paste(
    "Number of significant entities without correction: ",
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
          p("Note that the sig_level column will not show icons if downloaded"),
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
              outputId = ns(paste(contrast[1], contrast[2], "getR_Code_Volcano", sep = "_")),
              label = "Get underlying R code and data",
              icon = icon("code")
            ),
            NULL,
            downloadButton(
              outputId = ns(paste(contrast[1], contrast[2], "getR_Code_Volcano_raw", sep = "_")),
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
              choices = c(".png", ".tiff", ".pdf", ".svg"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_both"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf", ".svg"),
              selected = ".png"
            ),
            radioGroupButtons(
              inputId = ns("file_ext_Volcano_raw"),
              label = "File Type:",
              choices = c(".png", ".tiff", ".pdf", ".svg"),
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

  brks_log2FC_neg <- seq(min(result$log2FoldChange, na.rm = T) -1, 0, length.out = 100) # -1 for towning down color match
  brks_log2FC_pos <- seq(0, max(result$log2FoldChange, na.rm = T) +1 , length.out = 100) # +a for towning down color match
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
            'pageLength', 'copy',
            list(extend = 'csv', text = 'CSV', filename = 'significance_table'),
            list(extend = 'excel', text = 'xlsx', filename = 'significance_table')
          ),
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
    print("Plot volcano")

    # Ensure required inputs are available
    req(input[[psig_th]], input[[lfc_th]], input[[Volcano_anno_tooltip]])

    # Create a loading screen using a Waiter
    waiter <- Waiter$new(
      id = ns(paste(contrast[1], contrast[2], "test", sep = "_")),
      html = LOADING_SCREEN,
      color = "#70BF4F47",
      hide_on_render = FALSE
    )
    waiter$show()
    on.exit(waiter$hide())

    # Read input values locally
    th_psig       <- input[[psig_th]]
    th_lfc        <- input[[lfc_th]]
    anno_col_name <- input[[Volcano_anno_tooltip]]

    # Assume 'result' is defined elsewhere (e.g. from your analysis)
    # and 'res_tmp[[session$token]]$data' contains rowData with annotation information.
    anno_vector <- rowData(res_tmp[[session$token]]$data)[, anno_col_name]

    # Generate the volcano plots using the helper function.
    volcano_obj <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = TRUE)
    volcano_obj_raw <- volcano_plot(result, th_psig, th_lfc, anno_vector, raw = FALSE)
    sig_ana_reactive$data4Volcano <- volcano_obj$data

    # Render the corrected volcano plot as a Plotly object
    output[[ns(paste(contrast[1], contrast[2], "Volcano", sep = "_"))]] <- renderPlotly({
      ggplotly(volcano_obj$volcano_plt,
               tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")) %>%
        layout(showlegend = TRUE)
    })

    # Render the uncorrected volcano plot as a Plotly object
    output[[ns(paste(contrast[1], contrast[2], "Volcano_praw", sep = "_"))]] <- renderPlotly({
      ggplotly(volcano_obj_raw$volcano_plt,
               tooltip = ifelse(is.null(anno_col_name), "all", "chosenAnno")) %>%
        layout(showlegend = TRUE)
    })
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

  output[[ns(paste(contrast[1], contrast[2], "getR_Code_Volcano", sep = "_"))]] <- downloadHandler(
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
      par_tmp[[session$token]]$SigAna$comp <<- title
      par_tmp[[session$token]]$SigAna$raw <<- FALSE
      envList <- list(
        par_tmp = par_tmp[[session$token]]
      )
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      # save csv files
      save_summarized_experiment(
        res_tmp[[session$token]]$data_original,
        temp_directory
      )
      write(
        create_workflow_script(
          pipeline_info = VOLCANO_PIPELINE,
          par = par_tmp[[session$token]],
          par_mem = "SigAna",
          path_to_util = file.path(temp_directory, "util.R")
        ),
        file.path(temp_directory, "Code.R")
      )
      saveRDS(envList, file.path(temp_directory, "Data.rds"))
      zip::zip(
        zipfile = file,
        files = dir(temp_directory),
        root = temp_directory
      )
      waiter$hide()
    },
    contentType = "application/zip"
  )

  output[[ns(paste(contrast[1], contrast[2], "getR_Code_Volcano_raw", sep = "_"))]] <- downloadHandler(

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
      par_tmp[[session$token]]$SigAna$comp <<- title
      par_tmp[[session$token]]$SigAna$raw <<- TRUE
      envList <- list(
        par_tmp = par_tmp[[session$token]]
      )
      temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
      dir.create(temp_directory)
      # save csv files
      save_summarized_experiment(
        res_tmp[[session$token]]$data_original,
        temp_directory
      )
      write(
        create_workflow_script(
          pipeline_info = VOLCANO_PIPELINE,
          par = par_tmp[[session$token]],
          par_mem = "SigAna",
          path_to_util = file.path(temp_directory, "util.R")
        ),
        file.path(temp_directory, "Code.R")
      )
      saveRDS(envList, file.path(temp_directory, "Data.rds"))
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
    filename = function() { paste("VOLCANO_",format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"),input[[ns("file_ext_Volcano")]],sep="") },
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
    filename = function() { paste("raw_VOLCANO",format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"),input[[ns("file_ext_Volcano_raw")]],sep="") },
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
    filename = function() { paste0("VOLCANO_",format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"),input[[ns("file_ext_Volcano_both")]]) },
    content = function(file){
      ggsave(
        filename = file,
        plot = gridExtra::arrangeGrob(sig_ana_reactive$VolcanoPlot_raw, sig_ana_reactive$VolcanoPlot),
        device = gsub("\\.","",input[[ns("file_ext_Volcano_both")]])
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
