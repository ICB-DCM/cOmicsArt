# test for reading in uploaded files (see issue #564)
test_that("read_file handles file extensions case-insensitive", {
  df <- data.frame(sample1 = c(1, 2), sample2 = c(3, 4), row.names = c("gene1", "gene2"))
  for (ext in c(".csv", ".CSV", ".Csv")){
    path <- tempfile(fileext = ext)
    write.csv(df, path)
    res <- read_file(path, check.names = T)
    expect_equal(rownames(res), rownames(df))
    expect_equal(colnames(res), colnames(df))
    expect_equal(res$sample1, df$sample1)
    unlink(path)
  }
})

test_that("read_file errors on unsupported file types instead of failing silently", {
  path <- tempfile(fileext = ".txt")
  writeLines("a,b\n1,2", path)
  expect_error(read_file(path), "Unsupported file type")
  unlink(path)
})
