# Stores the plot generators (lists of function infos) for the pipeline.
# ATTENTION: The order of the functions in the pipeline is important!

# --- Preprocessing ---
VIOLIN_PLOT_PIPELINE <<- list(violin_plot_info)

# --- Sample Correlation ---
SAMPLE_CORRELATION_PIPELINE <<- list(
  get_sample_correlation_info,
  custom_sample_annotation_info,
  custom_heatmap_info
)

# --- Single Gene Visualisation ---
SINGLE_GENE_VISUALISATION_PIPELINE <<- list(
  get_single_gene_data_info,
  single_gene_boxplot_info
)

# --- Heatmap ---
HEATMAP_PIPELINE <<- list(
  entitieSelection_info,
  custom_rowAnnotation_info,
  custom_colAnnotation_info,
  plot_heatmap_info
)

# --- Enrichment Analysis ---
OA_PIPELINE <<- list(
  get_gene_set_choice_info,
  check_annotation_enrichment_analysis_info,
  over_representation_analysis_info,
  plot_enrichment_results_info
)
EA_PIPELINE <<- list(
  get_gene_set_choice_info,
  check_annotation_enrichment_analysis_info,
  gene_set_enrichment_info,
  plot_enrichment_results_info
)

# --- Principal Component Analysis ---
PCA_PIPELINE <<- list(
  get_pca_info,
  plot_pca_info
)
PCA_LOADINGS_PIPELINE <<- list(
  get_pca_info,
  plot_pca_loadings_info
)
SCREE_PLOT_PIPELINE <<- list(
  get_pca_info,
  plot_scree_pca_info
)
LOADINGS_MATRIX_PIPELINE <<- list(
  get_pca_info,
  plot_loadings_matrix_info
)

# --- Significance Analysis ---
UPSET_PLOT_PIPELINE <<- list(
  performSigAnalysis_info,
  plot_significant_results_info
)
VOLCANO_PIPELINE <<- list(
  performSigAnalysis_info,
  volcano_plot_info
)