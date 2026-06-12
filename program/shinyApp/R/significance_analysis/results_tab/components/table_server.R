# Table Server Logic for Significance Analysis Results

#' Server logic for results table
#'
#' Handles rendering and styling of the DataTable showing differential
#' expression results.
#'
#' @param id Module ID (unused, kept for signature compatibility)
#' @param result Reactive containing analysis results data frame
#' @param alpha Numeric significance threshold
#' @param preprocess_method Character indicating "vst_DESeq" or "manual"
#' @param ids List of IDs from create_contrast_ids()
#' @param output Shiny output object from parent session
#' @param input Shiny input object from parent session
#' @return None (sets up server outputs)
#' @export
table_server <- function(id, result, alpha, preprocess_method, ids, output, input) {
  # Note: No moduleServer here because IDs are already namespaced from parent
  # Using moduleServer would create double-namespacing and break rendering

  # Process result data upfront - add significance stars
  # (Original code did this once, not as a reactive)
  processed_result <- addStars(result, alpha)

  # Calculate color scales upfront for table styling
  # (Original code calculated these once when tab was created)
  brks_log2FC_neg <- seq(min(processed_result$log2FoldChange, na.rm = TRUE) - 1, 0, length.out = 100)
  brks_log2FC_pos <- seq(0, max(processed_result$log2FoldChange, na.rm = TRUE) + 1, length.out = 100)
  brks_lfc <- c(brks_log2FC_neg, brks_log2FC_pos)
  clrs_lfc <- colorRampPalette(c("#0e5bcfCD", "#fafafa", "#cf0e5bCD"))(length(brks_lfc) + 1)

  # P-value color scale (highlight significant values)
  brks_padj_sig <- seq(0, alpha, length.out = 10)
  brks_padj_unsig <- seq(alpha, 1, length.out = 10)
  brks_padj <- c(brks_padj_sig, brks_padj_unsig)
  clrs_padj <- colorRampPalette(c("#ffce78", "#fafafa", "#fafafa"))(length(brks_padj) + 1)

  # Hidden columns based on preprocessing method
  hidden_cols <- if(preprocess_method == "vst_DESeq") {
    c(2, 4, 5, 6)  # Hide baseMean, lfcSE, stat columns for DESeq
  } else {
    c(3, 4)  # Hide fewer columns for manual
  }

  # Sorting order based on preprocessing method
  sort_order <- if(preprocess_method == "vst_DESeq") {
    list(list(7, 'asc'), list(3, 'desc'))  # Sort by padj, then log2FC
  } else {
    list(list(5, 'asc'), list(6, 'desc'))  # Different column positions for manual
  }

  # Render DataTable
  output[[ids$table]] <- DT::renderDataTable({
    DT::datatable(
      data = processed_result,
      extensions = 'Buttons',
      filter = 'top',
      rownames = TRUE,
      colnames = c('Entitie' = 1),
      options = create_datatable_options(hidden_cols, sort_order, processed_result),
      escape = FALSE,
      class = "cell-border compact stripe hover order-column"
    ) %>%
      formatStyle("log2FoldChange",
        backgroundColor = styleInterval(brks_lfc, clrs_lfc)) %>%
      formatStyle(c("padj", "pvalue"),
        backgroundColor = styleInterval(brks_padj, clrs_padj)) %>%
      formatSignif(
        get_format_columns(preprocess_method),
        digits = 4,
        interval = 3,
        dec.mark = getOption("OutDec"),
        zero.print = NULL,
        rows = NULL
      )
  })
}

#' Create DataTable options configuration
#'
#' @param hidden_cols Numeric vector of column indices to hide
#' @param sort_order List specifying initial sort order
#' @return List of DataTable options
#' @export
create_datatable_options <- function(hidden_cols, sort_order, result) {
  list(
    paging = TRUE,
    searching = TRUE,
    fixedColumns = TRUE,
    autoWidth = TRUE,
    ordering = TRUE,
    order = sort_order,
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
      list(searchable = FALSE, targets = which(colnames(result) == "sig_level")),
      list(visible = FALSE, targets = hidden_cols)
    )
  )
}

#' Get columns to format based on preprocessing method
#'
#' @param preprocess_method Character indicating "vst_DESeq" or "manual"
#' @return Character vector of column names to format
#' @export
get_format_columns <- function(preprocess_method) {
  if(preprocess_method == "vst_DESeq") {
    c("log2FoldChange", "baseMean", "lfcSE", "stat", "pvalue", "padj")
  } else {
    c("log2FoldChange", "baseMean", "stat", "pvalue", "padj")
  }
}
