# Shared by the Pre-processing tests; testthat loads it before them
app_dir <- normalizePath(test_path("..", "..", ".."))
app_www <- function(...) file.path(app_dir, "www", ...)
# the SummarizedExperiment of the test data for `omic`
test_data <- function(omic){
  rds <- readRDS(app_www(paste0(omic, "_only_precompiled-LS.RDS")))
  rds[[grep("SumExp", names(rds))[1]]]
}
