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