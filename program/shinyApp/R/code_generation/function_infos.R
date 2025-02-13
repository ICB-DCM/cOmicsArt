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
  to_util = TRUE,
  additional_foos = list(
    deseq_processing = "deseq_processing",
    prefiltering = "prefiltering",
    simple_center_scaling = "simple_center_scaling",
    scaling_normalisation = "scaling_normalisation",
    ln_normalisation = "ln_normalisation"
  )
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
      data = "data",
      title = "title",
      correlation_method = "correlation_method"
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