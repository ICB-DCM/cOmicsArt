# tests for the Upload helpers in R/upload/util.R
app_www <- function(...) file.path(test_path("..", "..", ".."), "www", ...)

airway <- list(
  matrix = app_www("airway-read-counts-LS.csv"),
  sample_table = app_www("airway-sample-sheet-LS.csv"),
  row_annotation = app_www("airway-entitie_description-LS.csv")
)
metadata_sheet <- test_path("..", "fixtures", "airway-metadata-sheet.xlsx")

# write tables like the visual inspection does, return the three paths
write_tables <- function(matrix, sample_table, annotation_rows){
  paths <- list(
    matrix = tempfile(fileext = ".csv"),
    sample_table = tempfile(fileext = ".csv"),
    row_annotation = tempfile(fileext = ".csv")
  )
  write.csv(matrix, paths$matrix, row.names = TRUE)
  write.csv(sample_table, paths$sample_table, row.names = TRUE)
  write.csv(annotation_rows, paths$row_annotation, row.names = TRUE)
  paths
}

# precompiled RDS holding the given object
write_rds <- function(object){
  path <- tempfile(fileext = ".RDS")
  saveRDS(object, path)
  path
}

small_se <- function(counts){
  SummarizedExperiment::SummarizedExperiment(
    assays = list(raw = counts),
    rowData = data.frame(id = rownames(counts), row.names = rownames(counts)),
    colData = data.frame(group = c("a", "b"), row.names = colnames(counts))
  )
}
small_counts <- function(values = c(1, 2, 3, 4)){
  matrix(values, nrow = 2, dimnames = list(c("g1", "g2"), c("s1", "s2")))
}

# Inputs complete? ----
test_that("upload_inputs_complete accepts each source with its inputs", {
  file <- list(datapath = "x")
  file_inputs <- list(matrix = file, sample_table = file, row_annotation = file)
  expect_true(upload_inputs_complete("file_input", file_inputs))
  expect_false(upload_inputs_complete("file_input", file_inputs[1:2]))
  expect_true(upload_inputs_complete("precompiled", list(precompiled = file)))
  expect_false(upload_inputs_complete("precompiled", file_inputs))
  expect_true(upload_inputs_complete("testdata", list()))
  expect_true(upload_inputs_complete("VI_data", list()))
})

test_that("upload_inputs_complete accepts the Metadata source with its own inputs", {
  file <- list(datapath = "x")
  metadata_inputs <- list(
    metadata_matrix = file, metadata_sheet = file, metadata_row_annotation = file
  )
  expect_true(upload_inputs_complete("metadata", metadata_inputs))
  expect_false(upload_inputs_complete("metadata", metadata_inputs[1:2]))
  # the File Input's files don't make a Metadata upload complete, and vice versa
  file_inputs <- list(matrix = file, sample_table = file, row_annotation = file)
  expect_false(upload_inputs_complete("metadata", file_inputs))
  expect_false(upload_inputs_complete("file_input", metadata_inputs))
})

# Read an Upload ----
test_that("read_upload returns a SummarizedExperiment for each source", {
  expect_dims <- function(upload, dims){
    expect_s4_class(upload$se, "SummarizedExperiment")
    expect_equal(dim(upload$se), dims)
    expect_false(upload$read_failed)
    expect_null(upload$error_modal)
  }
  expect_dims(read_upload("file_input", "Transcriptomics", airway), c(33469, 8))
  expect_dims(
    read_upload("VI_data", "Transcriptomics", airway, changed_during_vi = FALSE),
    c(33469, 8)
  )
  expect_dims(
    read_upload(
      "VI_data", "Transcriptomics", list(corrected = airway), changed_during_vi = TRUE
    ),
    c(33469, 8)
  )
  expect_dims(
    read_upload(
      "precompiled", "Transcriptomics",
      list(precompiled = app_www("Transcriptomics_only_precompiled-LS.RDS"))
    ),
    c(33469, 8)
  )
  test_data <- list(
    Transcriptomics = c(33469, 8), Lipidomics = c(660, 28), Metabolomics = c(108, 35)
  )
  for (omic in names(test_data)){
    expect_dims(
      read_upload("testdata", omic, test_data_dir = app_www()),
      test_data[[omic]]
    )
  }
})

test_that("read_upload reads the Metadata source from its own inputs", {
  upload <- read_upload(
    "metadata", "Transcriptomics",
    list(
      # the File Input's files must not be read
      matrix = "missing-file-input-matrix.csv",
      row_annotation = "missing-file-input-annotation.csv",
      metadata_matrix = airway$matrix,
      metadata_sheet = metadata_sheet,
      metadata_row_annotation = airway$row_annotation
    )
  )
  expect_s4_class(upload$se, "SummarizedExperiment")
  expect_equal(dim(upload$se), c(33469, 8))
  expect_false(upload$metadata_read_failed)
})

test_that("read_upload reports a Metadata sheet whose samples are not in the matrix", {
  counts <- small_counts()
  colnames(counts) <- c("other1", "other2")
  paths <- write_tables(
    counts, data.frame(), data.frame(id = c("a", "b"), row.names = c("g1", "g2"))
  )
  upload <- read_upload(
    "metadata", "Transcriptomics",
    list(
      metadata_matrix = paths$matrix,
      metadata_sheet = metadata_sheet,
      metadata_row_annotation = paths$row_annotation
    )
  )
  expect_null(upload$se)
  expect_true(upload$metadata_read_failed)
  expect_match(
    tail(upload$messages, 1),
    "Your Sample Names from the Metadata Sheet and from your Matrix do not match"
  )
})

test_that("read_upload adds a dummy column to a one-column entity annotation", {
  annotation <- data.frame(symbol = c("A", "B"), row.names = c("g1", "g2"))
  paths <- write_tables(
    small_counts(), data.frame(group = c("a", "b"), row.names = c("s1", "s2")), annotation
  )
  upload <- read_upload("file_input", "Transcriptomics", paths)
  expect_equal(
    colnames(SummarizedExperiment::rowData(upload$se)), c("symbol", "origRownames")
  )
  expect_equal(SummarizedExperiment::rowData(upload$se)$origRownames, c("g1", "g2"))
})

test_that("read_upload reports a File Input that cannot be read", {
  paths <- airway
  paths$matrix <- tempfile(fileext = ".txt")
  writeLines("not a table", paths$matrix)
  upload <- read_upload("file_input", "Transcriptomics", paths)
  expect_null(upload$se)
  expect_true(upload$read_failed)
  expect_equal(upload$error_modal, "Uploading via file input failed")
  # the read error is shown first, then overwritten by the SummarizedExperiment error
  expect_match(upload$messages[1], "Upload failed, please check your input.")
  expect_match(tail(upload$messages, 1), "could not be put into a SummarizedExperiment")
})

test_that("read_upload rejects a precompiled list with two SummarizedExperiments", {
  se <- small_se(small_counts())
  upload <- read_upload(
    "precompiled", "Transcriptomics", list(precompiled = write_rds(list(a = se, b = se)))
  )
  expect_null(upload$se)
  # the Upload goes on and fails building the SummarizedExperiment, whose message is shown
  expect_match(upload$messages[1], "more than one Summarized Experiment")
  expect_match(tail(upload$messages, 1), "could not be put into a SummarizedExperiment")
})

test_that("read_upload takes the one SummarizedExperiment out of a precompiled list", {
  se <- small_se(small_counts())
  upload <- read_upload(
    "precompiled", "Lipidomics", list(precompiled = write_rds(list(other = 1, se = se)))
  )
  expect_equal(dim(upload$se), c(2, 2))
  expect_length(upload$messages, 0)
})

test_that("read_upload rejects a precompiled object that is no SummarizedExperiment", {
  upload <- read_upload(
    "precompiled", "Transcriptomics", list(precompiled = write_rds(data.frame(a = 1)))
  )
  expect_null(upload$se)
  expect_null(upload$error_modal)
  expect_match(upload$messages, "Is the provided input Summarized Experiment object?")
})

test_that("read_upload warns about missing, negative and non-integer assay values", {
  read_counts <- function(values){
    read_upload(
      "precompiled", "Transcriptomics",
      list(precompiled = write_rds(small_se(small_counts(values))))
    )
  }
  missing <- read_counts(c(1, NA, 3, -4.5))
  expect_s4_class(missing$se, "SummarizedExperiment")
  expect_length(missing$messages, 1)
  expect_match(missing$messages, "contains missing values")

  negative <- read_counts(c(1, -2, 3, 4))
  expect_length(negative$messages, 1)
  expect_match(negative$messages, "contains negative values")

  non_integer <- read_counts(c(1, 2.5, 3, 4))
  expect_length(non_integer$messages, 1)
  expect_match(non_integer$messages, "contains non-integer values")

  # both: the non-integer message is written last, so it is the one shown
  both <- read_counts(c(1, -2.5, 3, 4))
  expect_match(both$messages[1], "negative")
  expect_match(both$messages[2], "non-integer")
})

test_that("read_upload turns a data.table assay into a data.frame with row names", {
  se <- small_se(small_counts())
  SummarizedExperiment::assay(se, withDimnames = FALSE) <-
    data.table::data.table(s1 = c(1, 2), s2 = c(3, 4))
  upload <- read_upload("precompiled", "Transcriptomics", list(precompiled = write_rds(se)))
  raw_assay <- SummarizedExperiment::assay(upload$se, withDimnames = FALSE)
  expect_s3_class(raw_assay, "data.frame")
  expect_false(inherits(raw_assay, "data.table"))
  expect_equal(rownames(raw_assay), c("g1", "g2"))
})

test_that("fill_na_row_annotation replaces NA annotation values by row names", {
  se <- small_se(small_counts())
  SummarizedExperiment::rowData(se)$symbol <- c("A", NA)
  filled <- fill_na_row_annotation(se)
  expect_equal(SummarizedExperiment::rowData(filled)$symbol, c("A", "g2"))
  expect_equal(SummarizedExperiment::rowData(filled)$id, c("g1", "g2"))
})

test_that("dataset_start_copy drops single-value annotation columns before the NA fill", {
  se <- SummarizedExperiment::SummarizedExperiment(
    assays = list(raw = small_counts()),
    rowData = data.frame(
      symbol = c("A", NA), constant = c("x", "x"), empty = c(NA, NA),
      row.names = c("g1", "g2")
    ),
    colData = data.frame(
      group = c("a", "b"), batch = c("1", "1"), row.names = c("s1", "s2")
    )
  )
  data <- dataset_start_copy(se)
  # an all-NA column has a single value, so it is dropped, not filled
  expect_equal(colnames(SummarizedExperiment::rowData(data)), "symbol")
  expect_equal(SummarizedExperiment::rowData(data)$symbol, c("A", "g2"))
  expect_equal(colnames(SummarizedExperiment::colData(data)), "group")
  expect_equal(dim(data), dim(se))
})

# Inspect files ----
test_that("inspect_upload_files passes on the airway files", {
  inspection <- inspect_upload_files(
    airway$matrix, airway$row_annotation, sample_table_path = airway$sample_table
  )
  expect_true(inspection$passed)
  expect_null(inspection$error)
  expect_equal(dim(inspection$matrix), c(33469, 8))
  expect_equal(dim(inspection$sample_table), c(8, 9))
  expect_equal(dim(inspection$annotation_rows), c(33469, 6))
  checks <- inspection$checks
  expect_true(checks$csv_readable)
  expect_true(checks$numeric)
  expect_true(checks$row_names_match)
  expect_true(checks$sample_names_match)
  expect_true(checks$syntactic_names)
  expect_true(checks$no_na_matrix)
  expect_true(checks$no_na_sample_table)
  # the airway annotation has NAs, which is not crucial
  expect_false(checks$no_na_annotation)
  expect_true(length(inspection$annotation_na_columns) > 0)
})

test_that("inspect_upload_files reads the sample table from a metadata workbook", {
  inspection <- inspect_upload_files(
    airway$matrix, airway$row_annotation, metadata_path = metadata_sheet
  )
  expect_equal(nrow(inspection$sample_table), 8)
  expect_true(inspection$checks$sample_names_match)
})

test_that("inspect_upload_files fails with the right check on broken files", {
  matrix <- read.csv(airway$matrix, row.names = 1)[1:5, ]
  sample_table <- read.csv(airway$sample_table, row.names = 1)
  annotation_rows <- read.csv(airway$row_annotation, row.names = 1)[1:5, ]

  non_numeric <- matrix
  non_numeric$SRR1039508 <- "a"
  paths <- write_tables(non_numeric, sample_table, annotation_rows)
  inspection <- inspect_upload_files(
    paths$matrix, paths$row_annotation, sample_table_path = paths$sample_table
  )
  expect_false(inspection$passed)
  expect_false(inspection$checks$numeric)
  expect_equal(inspection$non_numeric_columns, "SRR1039508")
  expect_true(inspection$checks$sample_names_match)

  mismatched <- sample_table
  rownames(mismatched)[1] <- "other_sample"
  paths <- write_tables(matrix, mismatched, annotation_rows)
  inspection <- inspect_upload_files(
    paths$matrix, paths$row_annotation, sample_table_path = paths$sample_table
  )
  expect_false(inspection$passed)
  expect_false(inspection$checks$sample_names_match)
  expect_true(inspection$checks$numeric)

  invalid <- matrix
  colnames(invalid) <- paste0("1_", colnames(invalid))
  invalid_samples <- sample_table
  rownames(invalid_samples) <- colnames(invalid)
  paths <- write_tables(invalid, invalid_samples, annotation_rows)
  inspection <- inspect_upload_files(
    paths$matrix, paths$row_annotation, sample_table_path = paths$sample_table
  )
  expect_false(inspection$passed)
  expect_false(inspection$checks$syntactic_names)
})

test_that("inspect_upload_files flags a file that is not a csv", {
  path <- tempfile(fileext = ".txt")
  writeLines(c("a b", "x 1 2"), path)
  inspection <- inspect_upload_files(
    path, airway$row_annotation, sample_table_path = airway$sample_table
  )
  expect_false(inspection$checks$csv_readable)
  expect_false(inspection$passed)
  expect_equal(inspection$matrix_message, "<font color=\"#ab020a\"><b>Not a real csv file!</b></font>")
})

test_that("inspect_upload_files returns the read error of the sample table", {
  path <- tempfile(fileext = ".txt")
  writeLines("a,b", path)
  inspection <- inspect_upload_files(
    airway$matrix, airway$row_annotation, sample_table_path = path
  )
  expect_match(conditionMessage(inspection$error), "Unsupported file type")
  expect_equal(nrow(inspection$matrix), 33469)
  expect_null(inspection$sample_table)
})

# Correct names ----
test_that("correct_upload_names makes names syntactic and summarises the changes", {
  matrix <- data.frame(
    `1_a` = c(1, 2), `2_b` = c(3, 4),
    row.names = c("1gene", "gene two"), check.names = FALSE
  )
  sample_table <- data.frame(group = c("x", "y"), row.names = c("1_a", "2_b"))
  annotation_rows <- data.frame(symbol = c("A", "B"), row.names = c("1gene", "gene two"))

  corrected <- correct_upload_names(matrix, sample_table, annotation_rows)
  expect_equal(rownames(corrected$matrix), c("entite_1gene", "gene.two"))
  expect_equal(colnames(corrected$matrix), c("sample_1_a", "sample_2_b"))
  expect_equal(rownames(corrected$sample_table), c("sample_1_a", "sample_2_b"))
  expect_equal(rownames(corrected$annotation_rows), c("entite_1gene", "gene.two"))
  expect_equal(corrected$annotation_rows$original_rownames, c("1gene", "gene two"))
  expect_equal(make.names(colnames(corrected$matrix)), colnames(corrected$matrix))

  expect_named(
    corrected$changes, c("matrix_rows", "matrix_columns", "sample_table", "annotation_rows")
  )
  expect_match(corrected$changes[["matrix_rows"]], "Matrix: Number of rownames changed")
  expect_match(corrected$changes[["matrix_columns"]], "Matrix: Number of colnames changed")
  expect_match(corrected$changes[["sample_table"]], "Sample Table: Number of rownames changed")
  expect_match(corrected$changes[["annotation_rows"]], "Entitie Table: Number of rownames changed")
})

test_that("correct_upload_names leaves valid names alone", {
  matrix <- data.frame(a = c(1, 2), b = c(3, 4), row.names = c("g1", "g2"))
  sample_table <- data.frame(group = c("x", "y"), row.names = c("a", "b"))
  annotation_rows <- data.frame(symbol = c("A", "B"), row.names = c("g1", "g2"))
  corrected <- correct_upload_names(matrix, sample_table, annotation_rows)
  expect_equal(corrected$matrix, matrix)
  expect_equal(corrected$sample_table, sample_table)
  expect_equal(corrected$annotation_rows, annotation_rows)
  expect_true(all(corrected$changes == ""))
})
