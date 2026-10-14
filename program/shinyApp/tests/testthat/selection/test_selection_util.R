# tests for the Selection helpers in R/selection/util.R and select_data()
app_www <- function(...) file.path(test_path("..", "..", ".."), "www", ...)

# Lipidomics test data: 660 lipids x 28 samples
lipids <- readRDS(app_www("Lipidomics_only_precompiled-LS.RDS"))$Lipidomics_SumExp

small_se <- function(row_data, col_data){
  SummarizedExperiment::SummarizedExperiment(
    assays = list(raw = matrix(
      seq_len(nrow(row_data) * nrow(col_data)), nrow = nrow(row_data),
      dimnames = list(rownames(row_data), rownames(col_data))
    )),
    rowData = row_data,
    colData = col_data
  )
}
# 4 entities with a "|"-separated pathway column and a numeric column,
# 3 samples in 2 groups
small <- small_se(
  data.frame(
    pathway = c("a|b", "b", "c", "a"), mass = c(1.5, 2, 3, 4),
    row.names = paste0("e", 1:4)
  ),
  data.frame(group = c("x", "y", "x"), batch = c("1", "1", "2"), row.names = paste0("s", 1:3))
)

test_that("annotation columns to select on, first one is the default", {
  expect_equal(selection_annotation_columns(small, "entities"), c("pathway", "mass"))
  expect_equal(selection_annotation_columns(small, "samples"), c("group", "batch"))
  expect_equal(selection_annotation_columns(lipids, "samples"), c("diet", "ID"))
})

test_that("entity choices split '|' values and offer 'all' and High Values+IQR", {
  choices <- entity_choices(small, "pathway")
  expect_false(choices$numeric)
  expect_equal(choices$values, c("High Values+IQR", "all", "a", "b", "c"))
})

test_that("a numeric entity column only offers 'all'", {
  choices <- entity_choices(small, "mass")
  expect_true(choices$numeric)
  expect_equal(choices$values, "all")
})

test_that("sample choices are 'all' and the column's values", {
  expect_equal(sample_choices(small, "group"), c("all", "x", "y"))
  expect_equal(
    sample_choices(lipids, "diet"),
    c("all", unique(SummarizedExperiment::colData(lipids)$diet))
  )
})

test_that("resolve_selection fills today's defaults for missing inputs", {
  res <- resolve_selection(small, list())
  expect_equal(dim(res$data), c(4, 3))
  expect_equal(res$row_type, "pathway")
  expect_equal(res$sample_type, "group")
  expect_equal(res$propensity, 1)
  # the resolved IDs replace the chosen values
  expect_equal(res$selected_rows, paste0("e", 1:4))
  expect_equal(res$selected_samples, paste0("s", 1:3))
})

test_that("resolve_selection passes the chosen values to select_data", {
  res <- resolve_selection(small, list(
    row_selection = "b", row_type = "pathway",
    sample_selection = "1", sample_type = "batch",
    propensity = 0.85
  ))
  # exact match on the annotation value: "a|b" is not "b"
  expect_equal(res$selected_rows, "e2")
  expect_equal(res$selected_samples, c("s1", "s2"))
  expect_equal(dim(res$data), c(1, 2))
  expect_equal(res$propensity, 0.85)
})

test_that("select_data keeps everything for 'all'", {
  res <- select_data(lipids)
  expect_equal(dim(res$data), c(660, 28))
  res <- select_data(lipids, selected_samples = c("all", "CD_CD_CD"), sample_type = "diet")
  expect_equal(ncol(res$data), 28)
})

test_that("select_data selects samples and entities by annotation", {
  samples <- select_data(
    lipids, selected_samples = c("CD_CD_HFD", "HFD_HFD_CD"), sample_type = "diet"
  )
  expect_equal(dim(samples$data), c(660, 12))
  expect_setequal(unique(samples$data$diet), c("CD_CD_HFD", "HFD_HFD_CD"))
  rows <- select_data(
    lipids, selected_rows = c("TACG_sat", "PETA", "DACG"), row_type = "CLASS"
  )
  expect_equal(dim(rows$data), c(297, 28))
  expect_equal(rows$rows_selected, rownames(rows$data))
  both <- select_data(
    lipids,
    selected_samples = c("CD_CD_HFD", "HFD_HFD_CD"), sample_type = "diet",
    selected_rows = c("TACG_sat", "PETA", "DACG"), row_type = "CLASS"
  )
  expect_equal(dim(both$data), c(297, 12))
  expect_equal(both$samples_selected, colnames(both$data))
})

test_that("known bug (issue 08): as many chosen values as samples counts as 'all'", {
  # s1 is "x", s2 is "y" and "z" matches nothing, so only s1 should be kept;
  # but 2 chosen values for 2 samples are taken as "all"
  two <- small[, c("s1", "s2")]
  res <- select_data(two, selected_samples = c("x", "z"), sample_type = "group")
  expect_equal(res$samples_selected, c("s1", "s2"))
})

test_that("known bug (issue 06): High Values+IQR fails", {
  expect_error(
    select_data(lipids, selected_rows = "High Values+IQR", propensity = 0.85),
    "row_selection"
  )
})
