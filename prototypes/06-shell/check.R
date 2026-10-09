# PROTOTYPE 06 -- headless check that add / remove / destroy work (shiny::testServer).
# Run from the repo root:  Rscript prototypes/06-shell/check.R   (needs shiny >= 1.14 on the lib path)
library(shiny)
app <- source("prototypes/06-shell/app.R")$value # defines UPLOADS, MODULES, ... globally

ok <- function(cond, msg) { cat(if (cond) "PASS" else "FAIL", msg, "\n"); if (!cond) quit(status = 1) }

testServer(app, {
  sc <- session$userData$scopes
  live <- function() Filter(function(i) is.na(sc[[i]]$destroyed), ls(sc))

  ok(length(live()) == 0, "no scopes at start")

  session$setInputs(upload = names(UPLOADS)[1], ds_omic = "Transcriptomics", ds_name = "bad name!")
  session$setInputs(add = 1)
  ok(length(registry()) == 0, "invalid name rejected")

  session$setInputs(ds_name = "Transcriptomics_1", add = 2)
  ok(length(live()) == 1 + nrow(MODULES), "add creates 1 Dataset scope + 9 module scopes")
  ok(all(startsWith(setdiff(live(), "ds_1"), "ds_1-")), "module scopes nested under ds_1")
  session$setInputs(ds_name = "Transcriptomics_1", add = 3)
  ok(length(registry()) == 1, "duplicate name rejected")

  session$setInputs(seed = 1) # adds 3 more -> 4
  ok(length(live()) == 4 * (1 + nrow(MODULES)), "seed adds 3 Datasets (4 total)")
  ok(names(registry())[2] == "ds_2" && registry()[["ds_2"]]$name == "Transcriptomics_2",
     "default name Transcriptomics_2")

  # module inputs exist before removal
  session$setInputs(`ds_1-pca-n` = 30, `ds_1-pca-run` = 1)
  session$setInputs(ping = 1)
  ok(sc[["ds_1-pca"]]$pings == 1, "ping observer fires in ds_1-pca before removal")

  # remove ds_1 (confirm modal -> remove_ok)
  session$setInputs(remove_req = "ds_1"); session$setInputs(remove_ok = 1)
  ok(registry()[["ds_1"]]$status == "removed", "registry keeps ds_1 marked removed")
  ok(all(!is.na(vapply(grep("^ds_1(-|$)", ls(sc), value = TRUE),
                       function(i) as.numeric(sc[[i]]$destroyed), 0))),
     "onDestroy ran for ds_1 and all 9 nested module scopes")
  ok(!any(startsWith(names(reactiveValuesToList(input)), "ds_1-")), "ds_1-* inputs removed")

  session$setInputs(ping = 2)
  ok(sc[["ds_1-pca"]]$pings == 1, "destroyed observer no longer fires on ping")
  ok(sc[["ds_2-pca"]]$pings == 2, "other Datasets' observers still fire")

  # soft cap: 3 live, add 2 more fine, 6th asks for confirmation
  session$setInputs(ds_omic = "Lipidomics", ds_name = "Lip_a", add = 4)
  session$setInputs(ds_name = "Lip_b", add = 5)
  ok(length(live()) / (1 + nrow(MODULES)) == 5, "5 live Datasets")
  session$setInputs(ds_name = "Lip_c", add = 6)
  ok(length(Filter(function(d) d$status == "live", registry())) == 5, "6th blocked by soft-cap modal")
  session$setInputs(add_anyway = 1)
  ok(length(Filter(function(d) d$status == "live", registry())) == 6, "'Add anyway' exceeds the soft cap")
  ok("ds_7" %in% names(registry()), "ids are never reused (ds_1 not recycled)")
})
cat("all checks passed\n")
