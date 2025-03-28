# Stores function infos for all functions in the pipeline needed for script generation.
# The general structure is a list with the following elements:
#   - foo: function, the function object
#   - name: character, the name of the function
#   - input_mapping: list, the input mapping that are non identical
#   - output_mapping: list, the output mapping
#   - output_name: (optional) character, the name of the output if output_mapping is empty
#   - to_util: logical, if the function is a utility function
#   - plot_name: (optional) character, the name of the plot if the function is a plot

# --- Data Selection ---
select_data_info <<- list(
  foo = select_data,
  name = "select_data",
  input_mapping = list(
    data = "data_orig"
  ),
  output_mapping = list(
    data = "data",
    samples_selected = "samples_selected",
    rows_selected = "rows_selected"
  ),
  to_util = TRUE
)

# --- Preprocessing ---
preprocessing_info <<- list(
  foo = preprocessing,
  name = "preprocessing",
  input_mapping = list(
    data = "data"
  ),
  output_mapping = list(
    data = "data"
  ),
  output_name = "res_preprocess",
  to_util = TRUE,
  additional_foos = list(
    `%||%` = `%||%`,
    deseq_processing = deseq_processing,
    prefiltering_user = prefiltering_user,
    simple_center_scaling = simple_center_scaling,
    scaling_normalisation = scaling_normalisation,
    ln_normalisation = ln_normalisation,
    tmm_proccessing = tmm_proccessing,
    limma_voom_processing = limma_voom_processing
  )
)
violin_plot_raw_info <<- list(
  foo = violin_plot,
  name = "violin_plot",
  input_mapping = list(
    data = "data_orig[rows_selected, samples_selected, drop = FALSE]"
  ),
  to_util = TRUE,
  output_name = "violin_plot_raw"
)
violin_plot_info <<- list(
  foo = violin_plot,
  name = "violin_plot",
  input_mapping = list(
    data = "data"
  ),
  to_util = FALSE,
  plot_name = "plot2return"
)
plot_mean_sd_plot_info <<- list(
  foo = function(data){
    mean_and_sd_plot <- vsn::meanSdPlot(as.matrix(assay(data)), plot=FALSE)$gg +
      CUSTOM_THEME +
      ggtitle("Mean and SD per entity")
    return(mean_and_sd_plot)
  },
  name = "plot_mean_sd_plot",
  input_mapping = list(
    data = "data"
  ),
  to_util = FALSE,
  plot_name = "mean_and_sd_plot"
)

# --- Sample Correlation ---
get_sample_correlation_info <<- list(
  foo = get_sample_correlation,
  name = "get_sample_correlation",
  input_mapping = list(
    data = "data"
  ),
  output_name = "cormat",
  to_util = TRUE
)
custom_sample_annotation_info <<- list(
  foo = custom_sample_annotation,
  name = "custom_sample_annotation",
  input_mapping = list(
      data = "data"
  ),
  additional_foos = list(
    "assign_colors_SampleCorr" = assign_colors_SampleCorr
  ),
  to_util = FALSE
)
custom_heatmap_info <<- list(
  foo = custom_heatmap,
  name = "custom_heatmap",
  input_mapping = list(
      cormat = "cormat"
  ),
  to_util = FALSE,
  plot_name = "heatmap_plot + row_anno"
)

# --- Single Gene Visualisation ---
get_single_gene_data_info <<- list(
  foo = get_single_gene_data,
  name = "get_single_gene_data",
  input_mapping = list(
      data = "data"
  ),
  output_name = "gene_data",
  to_util = TRUE
)
single_gene_boxplot_info <<- list(
  foo = single_gene_boxplot,
  name = "single_gene_boxplot",
  input_mapping = list(
      gene_data = "gene_data"
  ),
  to_util = FALSE,
  plot_name = "boxplot_plot",
  additional_foos = list(
      "maybe_add_test" = maybe_add_test
  )
)

# --- Heatmap ---
entitieSelection_info <<- list(
  foo = entitieSelection,
  name = "entitieSelection",
  input_mapping = list(
    data = "data"
  ),
  output_name = "selected_data",
  to_util = TRUE
)
custom_rowAnnotation_info <<- list(
  foo = custom_rowAnnotation,
  name = "custom_rowAnnotation",
  input_mapping = list(
      data = "data",
      row_selected = "rownames(selected_data)"
  ),
  output_name = "row_annotation",
  to_util = TRUE
)
custom_colAnnotation_info <<- list(
  foo = custom_colAnnotation,
  name = "custom_colAnnotation",
  input_mapping = list(
      data = "data"
  ),
  output_name = "col_annotation",
  to_util = TRUE
)
plot_heatmap_info <<- list(
  foo = plot_heatmap,
  name = "plot_heatmap",
  input_mapping = list(
      selected_data = "selected_data",
      row_annotation = "row_annotation",
      col_annotation = "col_annotation"
  ),
  to_util = FALSE,
  plot_name = "heatmap_plot"
)

# --- Enrichment Analysis ---
get_gene_set_choice_info <<- list(
  foo = get_gene_set_choice,
  name = "get_gene_set_choice",
  input_mapping = list(
      data = "data"
  ),
  output_name = "gene_set_choice",
  to_util = TRUE,
  additional_foos = list(
      getLFCs = getLFCs
  )
)
check_annotation_enrichment_analysis_info <<- list(
  foo = check_annotation_enrichment_analysis,
  name = "check_annotation_enrichment_analysis",
  input_mapping = list(
      data = "data"
  ),
  output_mapping = list(
      new_data = "data"
  ),
  output_name = "anno_results",
  to_util = TRUE
)
translate_genes_ea_info <<- list(
  foo = translate_genes_ea,
  name = "translate_genes_ea",
  input_mapping = list(
      data = "data",
      annotation_results = "anno_results"
  ),
  output_name = "data",
  to_util = TRUE
)
translate_genes_oa_info <<- list(
  foo = translate_genes_oa,
  name = "translate_genes_oa",
  input_mapping = list(
      annotation_results = "anno_results",
      geneSetChoice = "gene_set_choice",
      geneSet2Enrich = "ora_gene_set_type",
      data = "data"
  ),
  output_name = "gene_set_choice",
  to_util = TRUE
)
gene_set_enrichment_info <<- list(
  foo = gene_set_enrichment,
  name = "gene_set_enrichment",
  input_mapping = list(
      data = "data",
      geneSetChoice = "gene_set_choice"
  ),
  output_name = "enrichment_results",
  to_util = TRUE
)
over_representation_analysis_info <<- list(
  foo = over_representation_analysis,
  name = "over_representation_analysis",
  input_mapping = list(
      data = "data",
      geneSetChoice = "gene_set_choice"
  ),
  output_name = "enrichment_results",
  to_util = TRUE
)
plot_enrichment_results_info <<- list(
  foo = function(enrichment_results, enrich_set){
    enrichment_dotplot <- clusterProfiler::dotplot(
      enrichment_results[[paste0("EnrichmentRes_", enrich_set)]]
    ) + CUSTOM_THEME
    return(enrichment_dotplot)
  },
  name = "plot_enrichment_results",
  input_mapping = list(
      enrichment_results = "enrichment_results"
  ),
  to_util = FALSE,
  plot_name = "enrichment_dotplot"
)

# --- Principal Component Analysis ---
get_pca_info <<- list(
  foo = get_pca,
  name = "get_pca",
  input_mapping = list(
      data = "data"
  ),
  output_name = "pca_res",
  output_mapping = list(
      pca = "pca",
      pcaData = "pcaData",
      percentVar = "percentVar"
  ),
  to_util = TRUE
)
plot_pca_info <<- list(
  foo = plot_pca,
  name = "plot_pca",
  input_mapping = list(
    pca = "pca",
    pcaData = "pcaData",
    percentVar = "percentVar",
    data = "data"
  ),
  to_util = FALSE,
  plot_name = "pca_plot",
  additional_foos = list(
    prepare_coloring_pca = prepare_coloring_pca,
    pca_loadings = pca_loadings,
    pca_ellipses = pca_ellipses
  )
)
plot_pca_loadings_info <<- list(
  foo = plot_pca_loadings,
  name = "plot_pca_loadings",
  input_mapping = list(
    pca = "pca",
    data = "data"
  ),
  to_util = FALSE,
  plot_name = "loadings_plot"
)
plot_scree_pca_info <<- list(
  foo = plot_scree_pca,
  name = "plot_scree_pca",
  input_mapping = list(
    pca = "pca",
    percentVar = "percentVar"
  ),
  to_util = FALSE,
  plot_name = "scree_plot"
)
plot_loadings_matrix_info <<- list(
  foo = plot_loadings_matrix,
  name = "plot_loadings_matrix",
  input_mapping = list(
    pca = "pca",
    data = "data"
  ),
  to_util = FALSE,
  plot_name = "loadings_matrix"
)

# --- Significance Analysis ---
performSigAnalysis_info <<- list(
  foo = performSigAnalysis,
  name = "performSigAnalysis",
  input_mapping = list(
    data = "list(data = data, DESeq_obj = res_preprocess$DESeq_obj %||% NULL)",
    preprocessing = "preprocessing_procedure"
  ),
  output_name = "sig_results",
  to_util = TRUE,
  additional_foos = list(
      significance_analysis = significance_analysis
  )
)
plot_significant_results_info <<- list(
  foo = plot_significant_results,
  name = "plot_significant_results",
  input_mapping = list(
    sig_results = "sig_results"
  ),
  to_util = TRUE,
  output_mapping = list(
    plot = "sig_ana_plot"
  ),
  additional_foos = list(
      prepare_upset_plot = prepare_upset_plot,
      filter_significant_result = filter_significant_result,
      getLFCs = getLFCs
  )
)
volcano_plot_info <<- list(
  foo = volcano_plot,
  name = "volcano_plot",
  input_mapping = list(
    result = "sig_results[[parameters$SigAna$comp]]"
  ),
  to_util = FALSE,
  plot_name = "volcano_plt"
)