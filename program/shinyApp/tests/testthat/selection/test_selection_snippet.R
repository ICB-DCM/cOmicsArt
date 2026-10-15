# The publication snippet of the report names the Selection that was made
lipids <- readRDS(
  file.path(test_path("..", "..", ".."), "www", "Lipidomics_only_precompiled-LS.RDS")
)$Lipidomics_SumExp

# snippet_dataInput() as the bridge calls it: session_params hold what
# selection_params() returns, plus the Upload's parameters
snippet_for <- function(inputs){
  params <- c(
    selection_params(resolve_selection(lipids, inputs)),
    list(omic_type = "Lipidomics", addedGeneAnno = FALSE)
  )
  snippet_dataInput(data = list(data_original = lipids), params = params)
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
