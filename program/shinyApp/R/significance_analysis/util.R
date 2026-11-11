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
  data,              # the SummarizedExperiment to test (non-DESeq2)
  dds,               # the DESeq2 object to test (vst_DESeq), else NULL
  preprocessing,     # the Pre-processing procedure, for the fold changes
  comparisons,       # character vector of comparisons ("Treatment:Control")
  compare_within,    # the name of the sample annotation column used for contrasts
  test_method,       # the testing method (for non-DESeq2)
  padjust_method     # the correction method (e.g., "BH")
) {
  # One compare_groups() per comparison, named by the comparison. With a
  # DESeq2 object DESeq2 tests and `data` is not used.
  sig_results <- lapply(comparisons, function(comparison) {
    groups <- strsplit(comparison, ":")[[1]]
    tryCatch(
      compare_groups(
        data, compare_within, treat = groups[1], ctrl = groups[2],
        test = test_method, correction = padjust_method,
        preprocessing = preprocessing, dds = dds
      ),
      error = function(e) stop(paste0("Error in the comparison ", comparison, ":<br>", conditionMessage(e)))
    )
  })
  names(sig_results) <- comparisons
  sig_results
}


compare_groups <- function(
  data, group_col, treat, ctrl, test = "Welch-Test", correction = "BH",
  preprocessing, dds = NULL
){
  # Compares the treatment group `treat` with the control group `ctrl` of the
  # sample annotation `group_col`, feature by feature. Used for fold changes
  # and p-values, so that they are computed one way.
  # data: a SummarizedExperiment; its first assay is tested
  # test: "Welch-Test", "T-Test" (equal variances) or "Wilcoxon rank sum
  #   test", all unpaired
  # correction: a p.adjust() method, or "none"
  # preprocessing: the Pre-processing procedure, for the fold change (getLFC())
  # dds: the DESeq2 object (vst_DESeq), else NULL; with one, DESeq2's Wald
  #   test and fold change, and `group_col` must be in its design
  # Returns one row per feature (rownames) with baseMean (DESeq2: the mean of
  # all samples; else the control's mean), log2FoldChange, stat, pvalue and
  # padj (DESeq2 adds lfcSE); a test that fails for a feature gives NA.
  if (!is.null(dds)) {
    if (!group_col %in% all.vars(DESeq2::design(dds))) {
      stop(sprintf(
        "\"%s\" is not in the DESeq2 design. Add it to the DESeq2 design in Pre-processing.",
        group_col
      ))
    }
    return(DESeq2::results(dds, contrast = c(group_col, treat, ctrl), pAdjustMethod = correction))
  }
  test_function <- switch(test,
    "Welch-Test" = function(x, y) t.test(x, y, var.equal = FALSE),
    "T-Test" = function(x, y) t.test(x, y, var.equal = TRUE),
    "Wilcoxon rank sum test" = wilcox.test,
    stop("Unknown test: ", test)
  )
  groups <- SummarizedExperiment::colData(data)[[group_col]]
  for (group in c(treat, ctrl)) {
    if (!any(groups %in% group)) {
      stop(sprintf("No samples of \"%s\" in the sample annotation \"%s\".", group, group_col))
    }
  }
  values <- as.matrix(SummarizedExperiment::assay(data))
  treat_values <- values[, groups %in% treat, drop = FALSE]
  ctrl_values <- values[, groups %in% ctrl, drop = FALSE]
  tests <- vapply(seq_len(nrow(values)), function(i) {
    tryCatch({
      result <- test_function(treat_values[i, ], ctrl_values[i, ])
      c(result$statistic[[1]], result$p.value)
    }, error = function(e) {
      message("No test for ", rownames(values)[i], ": ", conditionMessage(e))
      c(NA_real_, NA_real_)
    })
  }, numeric(2))
  means <- data.frame(
    baseMean = rowMeans(ctrl_values, na.rm = TRUE),
    treatMean = rowMeans(treat_values, na.rm = TRUE),
    row.names = rownames(values)
  )
  data.frame(
    baseMean = means$baseMean,
    log2FoldChange = getLFC(means, preprocessing),
    stat = tests[1, ],
    pvalue = tests[2, ],
    padj = p.adjust(tests[2, ], method = correction),
    row.names = rownames(values)
  )
}


# What compare_groups() runs with its defaults, for the modules that use them
# without a test picker: DESeq2 on a DESeq2 Dataset (`deseq`), else Welch
comparison_method <- function(deseq){
  if (deseq) "DESeq2 Wald test, Benjamini-Hochberg adjusted" else "Welch t-test, Benjamini-Hochberg adjusted"
}

# The line under such a module's comparison pickers
comparison_hint <- function(deseq){
  helpText(paste0(
    "Fold changes and p-values: ", comparison_method(deseq),
    ". For another test, use Differential Analysis."
  ))
}


prepare_upset_plot <- function(res2plot){
  # Prepare a Significance analysis result for Upset plotting and for intersection
  # download.
  overlap_list <- UpSetR::fromList(res2plot)
  names <- c()
  if(length(res2plot) > 0){
    for(i in seq_along(res2plot)){
      names <- append(names, res2plot[[i]])
    }
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


getLFC <- function(means, preprocessing){
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
  if(preprocessing == "log10"){
    lfc_per_gene_log(means, log_base = 10)
  }else if(preprocessing == "ln"){
    lfc_per_gene_log(means, log_base = exp(1))
  }else{
    lfc_per_gene(means)
  }
}


addStars <- function(result, alpha){ # assumes padj column present
  result <- as.data.frame(result) %>%
    mutate(sig_level = case_when(
      padj < 0.0001 ~ as.character(HTML(paste0(icon("star", lib = "font-awesome"),
                                               icon("star", lib = "font-awesome"),
                                               icon("star", lib = "font-awesome")))),  # Three stars for padj < 0.0001
      padj < 0.001 ~ as.character(HTML(paste0(icon("star", lib = "font-awesome"),
                                              icon("star", lib = "font-awesome")))),  # Two stars for padj < 0.001
      padj < alpha ~ as.character(icon("star", lib = "font-awesome")),  # One star if below significance threshold
      padj >= alpha ~ as.character(icon("minus", lib = "font-awesome"))  # Default case
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
        "Note: Although you chose 'all' to visualize, only the first 2 comparisons are shown to avoid<br>",
        "unwanted computational overhead (you got more than 4 comparisons). Please choose precisely<br>",
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
        "<br>You tried to look at adjusted pvalues.\nYou might want to look at raw pvalues (CAUTION!) or change the significance level."
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
      intersect_names = intersect_names,
      overlap_list = overlap_list
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

volcano_plot <- function(
  result, th_psig, th_lfc, anno_vector, raw = FALSE
) {
  # Plot a volcano plot for the given result.
  # Parameters:
  # result: a data frame with the results of a significance analysis
  # th_psig: the threshold where to consider a p-value significant
  # th_lfc: the threshold for the log2 fold change
  # anno_vector: a named vector with annotations for each gene (needed for plotly)
  # raw: whether to use raw p-values or adjusted p-values (default: FALSE)

  # Prepare data for plotting
  data_volcano <- result
  data_volcano$probename <- rownames(data_volcano)

  # Compute significance thresholds and fold-change categories
  data_volcano$threshold      <- ifelse(data_volcano$padj > th_psig, "Non-Sig", "Sig")
  data_volcano$threshold_raw  <- ifelse(data_volcano$pvalue > th_psig, "Non-Sig", "Sig")
  data_volcano$threshold_fc   <- ifelse(data_volcano$log2FoldChange > th_lfc, "Up",
                           ifelse(data_volcano$log2FoldChange < -th_lfc, "Down", " "))
  data_volcano$combined       <- ifelse(data_volcano$threshold_fc == " ", data_volcano$threshold,
                           paste(data_volcano$threshold, data_volcano$threshold_fc, sep = " + "))
  data_volcano$combined_raw   <- ifelse(data_volcano$threshold_fc == " ", data_volcano$threshold_raw,
                           paste(data_volcano$threshold_raw, data_volcano$threshold_fc, sep = " + "))

  # Remove any rows with missing values
  data_volcano <- data_volcano[complete.cases(data_volcano), ]

  # Annotate points using the provided annotation vector
  data_volcano$chosenAnno <- anno_vector[rownames(data_volcano)]

  # Define the color scheme (names must match the levels produced above)
  colorScheme2 <- c(
    "Sig + Up"      = "#cf0e5bCD",
    "Sig + Down"    = "#0e5bcfCD",
    "Sig"           = "#939596CD",
    "Non-Sig + Up"  = "#cf0e5b1A",
    "Non-Sig + Down"= "#0e5bcf1A",
    "Non-Sig"       = "#9395961A"
  )

  # Create the volcano plot
  volcano_plt <- ggplot(data_volcano, aes(label = chosenAnno)) +
    geom_point(aes(
      x = if (raw) -log10(pvalue) else -log10(padj),
      y = log2FoldChange,
      colour = if(raw) combined_raw else combined
    )) +
    geom_vline(xintercept = -log10(th_psig), color = "lightgrey") +
    geom_hline(yintercept = c(-th_lfc, th_lfc), color = "lightgrey") +
    scale_color_manual(values = colorScheme2, name = "") +
    ylab("Log FoldChange") +
    xlab(paste0("-log10(", ifelse(raw, "p-value)", "adj. p-value)"))) +
    ggtitle(paste0(ifelse(raw, "Unc", "C"),"orrected p-Values")) +
    CUSTOM_THEME

  return(list(volcano_plt = volcano_plt, data_volcano = data_volcano))
}
