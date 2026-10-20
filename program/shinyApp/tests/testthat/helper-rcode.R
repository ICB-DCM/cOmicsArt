# Helpers for the R_code tests (test-rcode-*.R): run the "Get R code"
# download of a tab and check it draws a figure that is not empty.

# Seconds one downloaded script may run (it loads all its packages first).
RCODE_TIMEOUT <- as.numeric(Sys.getenv("RCODE_TIMEOUT", 600))

# Runs in a fresh R process inside the unzipped download. Every top-level
# expression gets its own PNG device, so whatever it draws (a printed visible
# value or a base plot) lands in its own files. Returns one record per
# expression that drew something: its PNG files, the class of its visible
# value and, for a ggplot, the number of data rows over all layers.
rcode_subprocess <- function(dir) {
  # the script finds its files through RStudio; answer like RStudio would
  utils::assignInNamespace(
    "getActiveDocumentContext",
    function() list(path = file.path(dir, "Code.R")),
    "rstudioapi"
  )
  env <- new.env(parent = globalenv())
  exprs <- parse(file.path(dir, "Code.R"), keep.source = FALSE)
  figures <- list()
  for (i in seq_along(exprs)) {
    prefix <- sprintf("figure-%03d-", i)
    grDevices::png(file.path(dir, paste0(prefix, "%02d.png")), width = 1000, height = 800)
    res <- tryCatch(
      withVisible(eval(exprs[[i]], env)),
      finally = grDevices::dev.off()
    )
    # printing needs its own device as the expression's device is closed
    if (res$visible && !is.null(res$value)) {
      grDevices::png(file.path(dir, paste0(prefix, "%02d.png")), width = 1000, height = 800)
      tryCatch(print(res$value), finally = grDevices::dev.off())
    }
    pngs <- list.files(dir, pattern = paste0("^", prefix), full.names = TRUE)
    if (length(pngs) == 0) next
    value <- if (res$visible) res$value
    n_data <- NA_integer_
    if (inherits(value, "ggplot")) {
      n_data <- sum(vapply(
        seq_along(value$layers),
        function(l) nrow(ggplot2::layer_data(value, l)),
        integer(1)
      ))
    }
    figures[[length(figures) + 1]] <- list(png = pngs, class = class(value), n_data = n_data)
  }
  figures
}

# Unzip a downloaded R-code zip and run its Code.R in a fresh R process.
# Returns list(error = message or NULL, figures = list of records, see above).
run_rcode <- function(zip, timeout = RCODE_TIMEOUT) {
  dir <- tempfile("rcode-")
  dir.create(dir)
  utils::unzip(zip, exdir = dir)
  tryCatch(
    list(
      error = NULL,
      figures = callr::r(
        rcode_subprocess, args = list(dir = dir), wd = dir, timeout = timeout
      )
    ),
    error = function(e) {
      msg <- conditionMessage(e)
      parent <- if (is.null(e$parent)) "" else conditionMessage(e$parent)
      if (!grepl(parent, msg, fixed = TRUE)) msg <- paste(msg, parent)
      stderr <- if (is.null(e$stderr)) character() else strsplit(e$stderr, "\n")[[1]]
      list(error = paste(c(msg, utils::tail(stderr, 20)), collapse = "\n"), figures = list())
    }
  )
}

# The R code in `zip` runs without error and every figure it shows is not
# empty: more than one colour, and data in every ggplot. One expectation per
# call, its message names the first problem.
expect_rcode_figure <- function(zip, label) {
  res <- NULL
  problem <- if (!isTRUE(!is.na(zip) && file.exists(zip))) {
    "R code not downloaded"
  } else {
    res <- run_rcode(zip)
    rcode_problem(res)
  }
  expect(is.null(problem), paste0(label, ": ", problem))
  invisible(res)
}

# The first problem with a run_rcode() result, or NULL.
rcode_problem <- function(res) {
  if (!is.null(res$error)) return(paste0("R code fails:\n", res$error))
  if (length(res$figures) == 0) return("R code shows no figure")
  for (i in seq_along(res$figures)) {
    f <- res$figures[[i]]
    colours <- vapply(f$png, function(p) length(unique(as.vector(png::readPNG(p)))), integer(1))
    if (any(colours <= 1) || isTRUE(f$n_data == 0)) {
      return(sprintf("figure %d (%s) is empty", i, paste(f$class, collapse = "/")))
    }
  }
  NULL
}

# R-code downloads of a tab besides the one visit_tab() checks, by tab name:
# `open` (optional) is clicked first to show the plot's sub-tab.
RCODE_EXTRA <- list(
  "Pre-processing" = list(
    "mean and SD plot" = list(rcode = "#preprocessing-rcode_mean_sd")
  ),
  "PCA" = list(
    "Loadings" = list(
      open = "#PCA_main_panel_div a[data-value='PCA_Loadings']",
      rcode = "#PCA-getR_Code_Loadings"
    ),
    "Loadings matrix" = list(
      open = "#PCA_main_panel_div a[data-value='PCA_Loadings_matrix']",
      rcode = "#PCA-getR_Code_Loadings_matrix"
    ),
    "Scree plot" = list(
      open = "#PCA_main_panel_div a[data-value='Scree_Plot']",
      rcode = "#PCA-getR_Code_Scree_Plot"
    )
  ),
  "Differential Analysis" = list(
    # visit_tab() leaves the Volcano sub-tab of the first comparison open
    "uncorrected Volcano" = list(
      rcode = "#Significance_div [id$='_getR_Code_Volcano_raw']"
    )
  )
)

# Run `tab` with defaults (see visit_tab()) and check the R code of its main
# plot and of every RCODE_EXTRA plot.
expect_tab_rcode <- function(app, tab) {
  expect_rcode_figure(visit_tab(app, tab), tab)
  expect_extra_rcode(app, tab)
}

# Check the R code of every RCODE_EXTRA plot of `tab`, which is already run.
expect_extra_rcode <- function(app, tab) {
  for (name in names(RCODE_EXTRA[[tab]])) {
    extra <- RCODE_EXTRA[[tab]][[name]]
    if (!is.null(extra$open)) {
      app$run_js(sprintf("document.querySelector(\"%s\")?.click()", extra$open))
      app$wait_for_idle(duration = 1000, timeout = TIMEOUT)
    }
    expect_rcode_figure(expect_download(app, extra$rcode), paste0(tab, ", ", name))
  }
}
