# tests for download_name() in R/utils/download_name.R (issue #640)
TIMESTAMP <- "\\d{4}-\\d{2}-\\d{2}_\\d{2}-\\d{2}-\\d{2}"

test_that("a plot download is named after its module, item and time", {
  expect_match(download_name("PCA", ".png", "Scree"), paste0("^cOmicsArt_PCA_Scree_", TIMESTAMP, "\\.png$"))
})

test_that("the main plot of a module has no item", {
  expect_match(download_name("Heatmap", ".pdf"), paste0("^cOmicsArt_Heatmap_", TIMESTAMP, "\\.pdf$"))
})

test_that("the extension works with and without the dot", {
  expect_match(download_name("Upload", "csv", "DataMatrix"), "_DataMatrix_[0-9_-]+\\.csv$")
  expect_match(download_name("Upload", ".csv", "DataMatrix"), "_DataMatrix_[0-9_-]+\\.csv$")
})

test_that("R code is a zip with RCode after the item", {
  expect_match(download_name("PCA", "rcode", "Scree"), paste0("^cOmicsArt_PCA_Scree_RCode_", TIMESTAMP, "\\.zip$"))
  expect_match(download_name("SingleGene", "rcode"), paste0("^cOmicsArt_SingleGene_RCode_", TIMESTAMP, "\\.zip$"))
})

test_that("an unknown module fails", {
  expect_error(download_name("Volcano", ".png"))
})

test_that("an item with an underscore or a space fails", {
  expect_error(download_name("Enrichment", ".png", "GO_BP"), "letters and digits")
  expect_error(download_name("ML", ".png", "k means"), "letters and digits")
})
