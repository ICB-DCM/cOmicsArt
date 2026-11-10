# the PCA publication snippets read the module's parameters (issue 32)
PARAMS <- list(PCA = list(
  scale_data = TRUE, show_loadings = TRUE,
  n_top = 7, n_bottom = 3, cutoff = 0.25, n_pcs = 5, x_axis = "PC2"
))

test_that("the PCA snippet says whether the data was scaled and whether loadings were added", {
  expect_match(snippet_PCA(NULL, PARAMS), "performed on the centered and scaled data", fixed = TRUE)
  expect_match(snippet_PCA(NULL, PARAMS), "top 5 loadings", fixed = TRUE)
  unscaled <- list(PCA = modifyList(PARAMS$PCA, list(scale_data = FALSE, show_loadings = FALSE)))
  expect_match(snippet_PCA(NULL, unscaled), "performed on the centered data", fixed = TRUE)
  expect_no_match(snippet_PCA(NULL, unscaled), "loadings", fixed = TRUE)
})

test_that("the Loadings snippet names the numbers of top and bottom loadings", {
  expect_match(snippet_PCAloadings(NULL, PARAMS), "The top 7 positive loadings and the top 3 negative", fixed = TRUE)
})

test_that("the Loadings matrix snippet names the cutoff and the number of PCs", {
  snippet <- snippet_PCAloadingsMatrix(NULL, PARAMS)
  expect_match(snippet, "higher than 0.25 into account for the first 5 PCs.", fixed = TRUE)
})

test_that("the scree snippet needs no parameters", {
  expect_match(snippet_PCAscree(NULL, PARAMS), "scree plot", fixed = TRUE)
})
