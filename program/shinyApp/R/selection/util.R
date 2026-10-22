# Plain functions behind the Selection controls. They take the raw
# SummarizedExperiment of the Upload and the Selection inputs, never
# `input`, `output`, `session_data` or `session_params`.

# Annotation columns the user can select on; the first one is the default.
# side: "entities" (rowData) or "samples" (colData)
selection_annotation_columns <- function(data, side){
  annotation <- if (side == "entities") rowData(data) else colData(data)
  colnames(annotation)
}

# Values offered for the entity selection on one annotation column.
# A numeric column only offers "all" (selection is categorical only).
entity_choices <- function(data, column){
  values <- rowData(data)[, column]
  if (is.numeric(values)) {
    return(list(numeric = TRUE, values = "all"))
  }
  list(
    numeric = FALSE,
    values = c("all", unique(unlist(strsplit(as.character(values), "|", fixed = TRUE))))
  )
}

# Values offered for the sample selection on one annotation column.
sample_choices <- function(data, column){
  c("all", unique(colData(data)[, column]))
}

# The Selection for the given inputs (any of them may be missing):
# row_selection, sample_selection, row_type, sample_type.
# Returns the selected object, the chosen values (selected_rows,
# selected_samples, the arguments of select_data()), the resolved entity and
# sample IDs (rows_selected, samples_selected) and the annotation columns.
resolve_selection <- function(data, inputs){
  row_selection <- inputs$row_selection %||% "all"
  sample_selection <- inputs$sample_selection %||% "all"
  row_type <- inputs$row_type %||% selection_annotation_columns(data, "entities")[1]
  sample_type <- inputs$sample_type %||% selection_annotation_columns(data, "samples")[1]
  res_select <- select_data(
    data = data,
    selected_rows = row_selection,
    selected_samples = sample_selection,
    row_type = row_type,
    sample_type = sample_type
  )
  list(
    data = res_select$data,
    selected_rows = row_selection,
    selected_samples = sample_selection,
    rows_selected = res_select$rows_selected,
    samples_selected = res_select$samples_selected,
    row_type = row_type,
    sample_type = sample_type
  )
}

# The Selection as stored in session_params. The R-code download passes the
# chosen values to select_data(); Pre-processing reads the resolved IDs.
selection_params <- function(selection){
  list(
    selected_rows = selection$selected_rows,
    selected_samples = selection$selected_samples,
    rows_selected = selection$rows_selected,
    samples_selected = selection$samples_selected,
    row_type = selection$row_type,
    sample_type = selection$sample_type
  )
}
