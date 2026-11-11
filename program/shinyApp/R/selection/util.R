# Plain functions behind the Selection controls. They take the raw
# SummarizedExperiment of the Upload and the Selection inputs, never
# `input` or `output`.

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
# Returns list(data, params): the selected object, and the Selection as the
# Dataset's selection() returns it and the R code reads it:
#   row_values, sample_values: the chosen values (select_data()'s arguments)
#   row_type, sample_type: the annotation columns they were chosen on
#   row_ids, sample_ids: the entities and samples they resolved to on this
#     Upload, so two Selections are the same only on the same data
resolve_selection <- function(data, inputs){
  row_values <- inputs$row_selection %||% "all"
  sample_values <- inputs$sample_selection %||% "all"
  row_type <- inputs$row_type %||% selection_annotation_columns(data, "entities")[1]
  sample_type <- inputs$sample_type %||% selection_annotation_columns(data, "samples")[1]
  res_select <- select_data(
    data = data,
    row_values = row_values,
    sample_values = sample_values,
    row_type = row_type,
    sample_type = sample_type
  )
  list(
    data = res_select$data,
    params = list(
      row_values = row_values,
      sample_values = sample_values,
      row_ids = res_select$row_ids,
      sample_ids = res_select$sample_ids,
      row_type = row_type,
      sample_type = sample_type
    )
  )
}
