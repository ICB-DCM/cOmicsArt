# The session folder www/<token>/ that server.R creates at session start
app_dir <- normalizePath(test_path("..", "..", ".."))

test_that("the session folder is deleted when the session ends (issue 24)", {
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  session_dir <- NULL
  testServer(shinyAppDir("."), {
    session_dir <<- file.path("www", session$token)
    expect_true(file.exists(file.path(session_dir, "Report.md")))
  })
  expect_false(dir.exists(session_dir))
})

test_that("a session start leaves other sessions' folders alone (issue 24)", {
  withr::local_dir(app_dir)
  withr::local_options(shiny.testmode = TRUE)
  other <- file.path("www", "other_session_token")
  dir.create(other)
  on.exit(unlink(other, recursive = TRUE), add = TRUE)
  testServer(shinyAppDir("."), {
    expect_true(dir.exists(other))
  })
})
