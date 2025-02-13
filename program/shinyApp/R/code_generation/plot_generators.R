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