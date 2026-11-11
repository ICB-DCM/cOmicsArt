# Server of one comparison of a Differential Analysis run

#' Module server of one comparison's results tab
#'
#' A run creates one per comparison and the next run destroys it with
#' session$destroy(id) (its observers, outputs and inputs go with it).
#'
#' @param id Module id (unique per run)
#' @param contrast Character vector c(group1, group2)
#' @param result Data frame with the comparison's results (not reactive)
#' @param alpha Numeric significance threshold of the run
#' @param deseq Logical, whether DESeq2 made the result
#' @param data The SummarizedExperiment the run used (volcano annotation)
#' @param run_params The run's parameters (parameters$SigAna in the R code)
#' @param code_context function giving the Dataset's code_context() for the
#'   R code
#' @return A reactive: the volcano's settings, list(th_psig, th_lfc, anno)
results_tab_server <- function(id, contrast, result, alpha, deseq,
                               data, run_params, code_context) {
  moduleServer(id, function(input, output, session) {
    result <- as.data.frame(result)

    output$summary <- renderText({
      summary_text <- if (deseq) {
        generate_deseq_summary(result, alpha)
      } else {
        generate_manual_summary(result, alpha)
      }
      paste(summary_text, collapse = "<br>")
    })
    output$table <- comparison_table(result, alpha, deseq)

    # the volcano's settings and both plots
    volcano <- reactive({
      req(input$psig_th, input$lfc_th, input$anno)
      anno_vector <- rowData(data)[, input$anno]
      names(anno_vector) <- rownames(data)
      plot <- function(raw) volcano_plot(result, input$psig_th, input$lfc_th, anno_vector, raw)$volcano_plt
      with_loading(list(
        th_psig = input$psig_th,
        th_lfc = input$lfc_th,
        anno = input$anno,
        anno_vector = anno_vector,
        adj = plot(raw = FALSE),
        raw = plot(raw = TRUE)
      ), color = "#70BF4F47")
    })
    # the plot of a VOLCANO_ITEMS item
    item_plot <- function(item) switch(item,
      Volcano = volcano()$adj,
      VolcanoRaw = volcano()$raw,
      VolcanoBoth = gridExtra::arrangeGrob(volcano()$raw, volcano()$adj)
    )
    # the run's parameters with this comparison's volcano under the names
    # the volcano template reads (comp, raw, th_psig, th_lfc, anno_vector)
    item_params <- function(item) {
      v <- volcano()
      c(run_params, list(
        # the name performSigAnalysis() gives this comparison's result
        comp = paste(contrast, collapse = ":"),
        raw = VOLCANO_ITEMS[[item]]$raw,
        th_psig = v$th_psig, th_lfc = v$th_lfc, anno_vector = v$anno_vector
      ))
    }

    volcano_output <- function(item, legend) renderPlotly({
      create_clipboard_plotly(
        gg_plot = item_plot(item), plot_id = item, tooltip = "chosenAnno"
      ) %>% plotly::layout(showlegend = input[[legend]] %||% TRUE)
    })
    output$Volcano <- volcano_output("Volcano", "show_legend")
    output$VolcanoRaw <- volcano_output("VolcanoRaw", "show_legend_raw")

    for (item in names(VOLCANO_ITEMS)) local({
      item <- item
      report_button <- paste0("only2Report_", item)
      observeEvent(input[[report_button]], {
        plot_report(
          session, "Differential analysis - Volcano", "VOLCANO_",
          save = function(file) ggsave(file, plot = item_plot(item), device = "png"),
          info = paste("**VOLCANO** - Underlying Volcano Comparison:", contrast[1], "vs", contrast[2]),
          snippet = snippet_SigAna(data = NULL, params = list(SigAna = item_params(item)))
        )
      })
      output[[paste0("SavePlot_", item)]] <- plot_download(
        "Differential", item, ext = function() input[[paste0("file_ext_", item)]],
        save = function(file, device) ggsave(file, plot = item_plot(item), device = device),
        report_button = session$ns(report_button)
      )
      if (VOLCANO_ITEMS[[item]]$single) {
        output[[paste0("getR_Code_", item)]] <- rcode_download("Differential", item, function(file) {
          params <- item_params(item)
          rcode_zip(file, code_context(), VOLCANO_PIPELINE, "SigAna", params, params$data_stage)
        })
      }
    })

    reactive(volcano()[c("th_psig", "th_lfc", "anno")])
  })
}

#' The comparison's results table: significance stars, coloured fold changes
#' and p-values, client side so its exports have every row (bug 08)
#'
#' @param result Data frame with the comparison's results
#' @param alpha Numeric significance threshold
#' @param deseq Logical, whether DESeq2 made the result
#' @return a renderDataTable
comparison_table <- function(result, alpha, deseq) {
  result <- addStars(result, alpha)
  brks_lfc <- c(
    seq(min(result$log2FoldChange, na.rm = TRUE) - 1, 0, length.out = 100),
    seq(0, max(result$log2FoldChange, na.rm = TRUE) + 1, length.out = 100)
  )
  clrs_lfc <- colorRampPalette(c("#0e5bcfCD", "#fafafa", "#cf0e5bCD"))(length(brks_lfc) + 1)
  # significant p-values highlighted
  brks_padj <- c(seq(0, alpha, length.out = 10), seq(alpha, 1, length.out = 10))
  clrs_padj <- colorRampPalette(c("#ffce78", "#fafafa", "#fafafa"))(length(brks_padj) + 1)
  # DESeq2's columns differ from the own tests'
  hidden_cols <- if (deseq) c(2, 4, 5, 6) else c(3, 4)
  sort_order <- if (deseq) list(list(7, "asc"), list(3, "desc")) else list(list(5, "asc"), list(6, "desc"))
  format_cols <- c("log2FoldChange", "baseMean", if (deseq) "lfcSE", "stat", "pvalue", "padj")

  DT::renderDataTable({
    results_datatable(
      result,
      download_name("Differential", NULL, "Table"),
      options = list(
        fixedColumns = TRUE,
        order = sort_order,
        columnDefs = list(
          list(searchable = FALSE, targets = which(colnames(result) == "sig_level")),
          list(visible = FALSE, targets = hidden_cols)
        )
      ),
      rownames = TRUE,
      colnames = c("Entitie" = 1),
      escape = FALSE
    ) %>%
      formatStyle("log2FoldChange", backgroundColor = styleInterval(brks_lfc, clrs_lfc)) %>%
      formatStyle(c("padj", "pvalue"), backgroundColor = styleInterval(brks_padj, clrs_padj)) %>%
      formatSignif(
        format_cols, digits = 4, interval = 3, dec.mark = getOption("OutDec"),
        zero.print = NULL, rows = NULL
      )
  }, server = FALSE)
}
