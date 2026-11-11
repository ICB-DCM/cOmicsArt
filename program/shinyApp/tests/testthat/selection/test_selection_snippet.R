# The publication snippet of the report names the Selection that was made
lipids <- test_data("Lipidomics")

# snippet_dataInput() as server.R calls it on confirm
snippet_for <- function(inputs, organism = NULL){
  snippet_dataInput(
    raw = lipids,
    selection = selection_params(resolve_selection(lipids, inputs)),
    omic_type = "Lipidomics",
    organism = organism
  )
}

test_that("the snippet says no selection was made for 'all'", {
  snippet <- snippet_for(list())
  expect_match(snippet, "No sample selection was performed.", fixed = TRUE)
  expect_match(snippet, "No entity selection was performed.", fixed = TRUE)
})

test_that("the snippet names the annotation column and values of a sample subset", {
  snippet <- snippet_for(list(
    sample_type = "diet", sample_selection = c("CD_CD_HFD", "HFD_HFD_CD")
  ))
  expect_match(
    snippet, "All samples with diet being CD_CD_HFD, HFD_HFD_CD were selected.",
    fixed = TRUE
  )
  expect_match(snippet, "No entity selection was performed.", fixed = TRUE)
})

test_that("the snippet names the annotation column and values of an entity subset", {
  snippet <- snippet_for(list(
    row_type = "CLASS", row_selection = c("TACG_sat", "PETA")
  ))
  expect_match(snippet, "No sample selection was performed.", fixed = TRUE)
  expect_match(
    snippet, "All entities with CLASS being TACG_sat, PETA were selected.",
    fixed = TRUE
  )
})

test_that("the snippet names the organism of an added gene annotation", {
  snippet <- snippet_for(list(), organism = "Human genes (GRCh38.p14)")
  expect_match(snippet, "Gene annotation was added using the Human genes (GRCh38.p14) mart", fixed = TRUE)
})

test_that("the snippet mentions no gene annotation without an organism", {
  expect_no_match(snippet_for(list()), "Gene annotation", fixed = TRUE)
})
