# PROTOTYPE -- throwaway. Ticket 06 (MultiOmics shell). No real analysis.
# Question: how should the Upload | Datasets | MultiOmics shell look and behave,
# with Datasets created dynamically and torn down via session$destroy()?
# Variants are chosen with URL params (bottom bar): ?layout=&badge=&colour=&removed=
# Run: see README.md (needs shiny >= 1.14.0).

library(shiny)
`%||%` <- function(a, b) if (is.null(a)) b else a
stopifnot(packageVersion("shiny") >= "1.14.0")

SOFT_CAP <- 5 # configurable soft cap on Datasets (memory, not destroy, is the reason)

OMIC_TYPES <- c("Transcriptomics", "Lipidomics", "Metabolomics")
UPLOADS <- c( # dummy Uploads (stand-ins for file / precompiled / test data)
  "airway-read-counts-LS.csv (test data)" = "Transcriptomics",
  "Lipidomics_only_precompiled-LS.RDS" = "Lipidomics",
  "Metabolomics_only_precompiled-LS.RDS" = "Metabolomics"
)
# module tabs inside a Dataset; ids are the module namespaces, labels match the
# real app's tab data-values so the existing module colours apply
MODULES <- data.frame(
  id = c("selection", "preprocessing", "sample_corr", "pca", "ml", "differential",
         "heatmap", "single_gene", "enrichment"),
  label = c("Data selection", "Pre-processing", "Sample Correlation", "PCA", "ML",
            "Differential Analysis", "Heatmap", "Single Gene Visualisations",
            "Enrichment Analysis"),
  colour = c("#70BF4F", "#3897F1", "#A208BA", "#FD8D33", "#7a7e80", "#FFD335",
             "#70BF4F", "#3897F1", "#A208BA"),
  stringsAsFactors = FALSE
)
# per-Dataset colours, picked to not collide with the module colours above
DS_COLOURS <- c("#EC0014", "#00897B", "#3F51B5", "#EF0089", "#6D4C41", "#455A64", "#9E9D24")

VARIANTS <- list(
  layout = c(nested = "Datasets tab > Dataset tabs > modules",
             flat = "Each Dataset is a top-level tab",
             picker = "Datasets tab with a Dataset picker"),
  badge = c(header = "Badge strip above module tabs",
            sidebar = "Badge at top of every module sidebar"),
  colour = c(stripe = "Dataset colour as stripe/dot",
             frame = "Dataset colour frames the whole Dataset"),
  removed = c(gone = "Removed Dataset disappears (toast)",
              tombstone = "Removed Dataset leaves a greyed tombstone tab")
)

badge <- function(ds, removed = FALSE) {
  span(class = paste("ds-badge", if (removed) "ds-removed"),
       style = sprintf("--ds:%s", ds$colour),
       span(class = "ds-dot"), strong(ds$name), span(class = "ds-omic", ds$omic))
}
tab_title <- function(ds, removed = FALSE) {
  span(class = paste("ds-tab-title", if (removed) "ds-removed"),
       style = sprintf("--ds:%s", ds$colour), span(class = "ds-dot"), ds$name)
}

# one root-level event for all remove buttons (avoids duplicate ids per module)
remove_button <- function(ds, class = NULL) {
  tags$button(class = paste("btn btn-default btn-xs", class), "Remove Dataset",
              onclick = sprintf("Shiny.setInputValue('remove_req', '%s', {priority: 'event'})", ds$id))
}

# ---------------------------------------------------------------- dummy module
module_ui <- function(id, label, colour, ds, v) {
  ns <- NS(id)
  tabPanel(
    label, value = label,
    sidebarLayout(
      sidebarPanel(
        style = sprintf("background-color:%s47", colour),
        if (v$badge == "sidebar") div(class = "sidebar-badge", badge(ds),
                                      remove_button(ds)),
        h4(label),
        sliderInput(ns("n"), "Dummy parameter", 10, 200, 50),
        actionButton(ns("run"), "Do analysis")
      ),
      mainPanel(plotOutput(ns("plot"), height = 250), verbatimTextOutput(ns("info")))
    )
  )
}

# every scope reports into session$userData$scopes (a plain env that outlives
# the scopes) so the debug panel can show whether destroy really stopped them
track <- function(session, kind) {
  ns <- sub("-$", "", session$ns(""))
  sc <- session$userData$scopes
  sc[[ns]] <- list(kind = kind, created = Sys.time(), heartbeats = 0L, pings = 0L,
                   destroyed = NA)
  session$onDestroy(function() {
    s <- sc[[ns]]; s$destroyed <- Sys.time(); sc[[ns]] <- s
  })
  # heartbeat timer + ping observer: both must stop once the scope is destroyed
  observe({
    invalidateLater(1000)
    s <- sc[[ns]]; s$heartbeats <- s$heartbeats + 1L; sc[[ns]] <- s
  })
  observeEvent(session$userData$ping(), ignoreInit = TRUE, {
    s <- sc[[ns]]; s$pings <- s$pings + 1L; sc[[ns]] <- s
  })
  ns
}

module_server <- function(id, label, ds) {
  force(label)
  moduleServer(id, function(input, output, session) {
    ns <- track(session, "module")
    res <- eventReactive(input$run, ignoreNULL = FALSE, {
      set.seed(input$run); matrix(rnorm(2 * input$n), ncol = 2)
    })
    output$plot <- renderPlot({
      plot(res(), pch = 19, col = ds$colour, main = paste(ds$name, "-", label),
           xlab = "", ylab = "")
    })
    output$info <- renderText(sprintf("module scope: %s\nreads Dataset handle '%s' (%s)",
                                      ns, ds$name, ds$omic))
  })
}

# ------------------------------------------------------------- Dataset scope
dataset_ui <- function(ds, v) {
  ns <- NS(ds$id)
  modules <- lapply(seq_len(nrow(MODULES)), function(i)
    module_ui(ns(MODULES$id[i]), MODULES$label[i], MODULES$colour[i], ds, v))
  div(
    class = paste("ds-container", paste0("colour-", v$colour)),
    style = sprintf("--ds:%s", ds$colour),
    if (v$badge == "header") div(
      class = "ds-header", badge(ds),
      span(class = "ds-meta", sprintf("from %s · scope id %s", ds$upload, ds$id)),
      remove_button(ds, "pull-right")
    ),
    div(class = "ds-modules", do.call(tabsetPanel, c(list(id = ns("modules")), modules)))
  )
}

dataset_server <- function(ds) {
  moduleServer(ds$id, function(input, output, session) {
    track(session, "dataset")
    for (i in seq_len(nrow(MODULES))) module_server(MODULES$id[i], MODULES$label[i], ds)
  })
}

# ----------------------------------------------------------------------- UI
ui <- function(request) {
  q <- parseQueryString(request$QUERY_STRING %||% "")
  v <- lapply(setNames(names(VARIANTS), names(VARIANTS)), function(k)
    if (!is.null(q[[k]]) && q[[k]] %in% names(VARIANTS[[k]])) q[[k]] else names(VARIANTS[[k]])[1])

  upload_tab <- tabPanel(
    "Upload", value = "Upload",
    sidebarLayout(
      sidebarPanel(
        h4("1. Upload (dummy)"),
        selectInput("upload", "Upload", names(UPLOADS)),
        helpText("Visual inspection and gene annotation would happen here."),
        hr(),
        h4("2. Add as Dataset"),
        textInput("ds_name", "Dataset name ([A-Za-z0-9_-])"),
        selectInput("ds_omic", "Omic type (fixed once added)", OMIC_TYPES),
        actionButton("add", "Add as Dataset", class = "btn-primary"),
        hr(),
        actionButton("seed", "Demo: add 3 Datasets", class = "btn-xs")
      ),
      mainPanel(h4("Datasets in this session"), uiOutput("ds_list"))
    )
  )
  datasets_tab <- tabPanel(
    "Datasets", value = "Datasets",
    if (v$layout == "picker") uiOutput("picker"),
    uiOutput("no_ds"),
    tabsetPanel(id = "ds_tabs", type = if (v$layout == "picker") "hidden" else "tabs")
  )
  multi_tab <- tabPanel(
    "MultiOmics", value = "MultiOmics",
    sidebarLayout(
      sidebarPanel(h4("Cross-Dataset correlation (dummy)"),
                   uiOutput("mo_choices"), actionButton("mo_run", "Run")),
      mainPanel(uiOutput("mo_result"))
    )
  )
  tabs <- if (v$layout == "flat") list(upload_tab, multi_tab)
          else list(upload_tab, datasets_tab, multi_tab)

  bar <- div(class = "variant-bar",
    strong("PROTOTYPE 06"),
    lapply(names(VARIANTS), function(k)
      tags$label(k, tags$select(
        class = "variant", `data-key` = k,
        lapply(names(VARIANTS[[k]]), function(o)
          tags$option(value = o, selected = if (o == v[[k]]) NA, VARIANTS[[k]][[o]]))))),
    span(class = "text-muted", "(switching reloads: state is lost)"))

  fluidPage(
    tags$head(tags$link(rel = "stylesheet", href = "proto.css"), tags$script(HTML("
      $(document).on('change', 'select.variant', function() {
        var p = new URLSearchParams(location.search);
        $('select.variant').each(function() { p.set($(this).data('key'), $(this).val()); });
        location.search = p.toString();
      });"))),
    titlePanel("cOmicsArt -- MultiOmics shell prototype"),
    fluidRow(
      column(9, do.call(tabsetPanel, c(list(id = "top"), tabs))),
      column(3, div(class = "debug", h4("Debug: module scopes"),
                    actionButton("ping", "Ping all scopes", class = "btn-xs"),
                    checkboxInput("show_destroyed", "show destroyed scopes", TRUE),
                    uiOutput("debug")))
    ),
    bar
  )
}

# ------------------------------------------------------------------- server
server <- function(input, output, session) {
  q <- isolate(parseQueryString(session$clientData$url_search))
  v <- lapply(setNames(names(VARIANTS), names(VARIANTS)), function(k)
    if (!is.null(q[[k]]) && q[[k]] %in% names(VARIANTS[[k]])) q[[k]] else names(VARIANTS[[k]])[1])

  session$userData$scopes <- new.env()
  session$userData$ping <- reactiveVal(0)
  observeEvent(input$ping, session$userData$ping(input$ping))

  # registry: lives at the root, outside every Dataset scope, so it survives destroy
  registry <- reactiveVal(list())
  next_k <- 1L
  colour_i <- 0L
  live <- reactive(Filter(function(d) d$status == "live", registry()))
  ds_tabset <- if (v$layout == "flat") "top" else "ds_tabs"

  hideTab("top", "MultiOmics")
  observe(if (length(live()) >= 2) showTab("top", "MultiOmics") else hideTab("top", "MultiOmics"))
  output$no_ds <- renderUI(if (!length(registry()))
    div(class = "text-muted", style = "padding:20px",
        "No Dataset yet. Use \"Add as Dataset\" on the Upload tab."))

  # default name like Transcriptomics_2
  default_name <- function(omic) {
    taken <- vapply(registry(), `[[`, "", "name")
    n <- sum(vapply(registry(), function(d) d$omic == omic, TRUE)) + 1L
    while (paste0(omic, "_", n) %in% taken) n <- n + 1L
    paste0(omic, "_", n)
  }
  observeEvent(input$upload, updateSelectInput(session, "ds_omic", selected = UPLOADS[[input$upload]]))
  observe(updateTextInput(session, "ds_name", value = default_name(input$ds_omic)))

  add_dataset <- function(name, omic, upload) {
    id <- paste0("ds_", next_k); next_k <<- next_k + 1L # ids are never reused
    colour_i <<- colour_i + 1L
    ds <- list(id = id, name = name, omic = omic, upload = upload, status = "live",
               colour = DS_COLOURS[(colour_i - 1L) %% length(DS_COLOURS) + 1L])
    tab <- tabPanel(tab_title(ds), value = id, dataset_ui(ds, v))
    if (v$layout == "flat") insertTab("top", tab, target = "MultiOmics", position = "before", select = TRUE)
    else {
      insertTab("ds_tabs", tab, select = TRUE)
      updateTabsetPanel(session, "top", selected = "Datasets")
    }
    dataset_server(ds)
    registry(c(registry(), setNames(list(ds), id)))
  }

  try_add <- function(name, omic, upload, force = FALSE) {
    if (!grepl("^[A-Za-z0-9_-]+$", name))
      return(showNotification("Name may only contain A-Z a-z 0-9 _ -", type = "error"))
    if (name %in% vapply(registry(), `[[`, "", "name"))
      return(showNotification(sprintf("A Dataset called '%s' already exists", name), type = "error"))
    if (!force && length(live()) >= SOFT_CAP) {
      pending_add <<- list(name, omic, upload)
      return(showModal(modalDialog(
        title = "Soft cap reached",
        sprintf("You already have %d Datasets (soft cap %d). Each one keeps its data and results in memory, which may slow the app down.",
                length(live()), SOFT_CAP),
        footer = tagList(modalButton("Cancel"), actionButton("add_anyway", "Add anyway")))))
    }
    add_dataset(name, omic, upload)
  }
  pending_add <- NULL
  observeEvent(input$add, try_add(trimws(input$ds_name), input$ds_omic, input$upload))
  observeEvent(input$add_anyway, { removeModal(); do.call(add_dataset, pending_add) })
  observeEvent(input$seed, for (u in names(UPLOADS)) try_add(default_name(UPLOADS[[u]]), UPLOADS[[u]], u))

  # ---- remove: confirm, removeTab, session$destroy(id), keep the registry entry
  to_remove <- NULL
  observeEvent(input$remove_req, {
    id <- input$remove_req
    req(identical(registry()[[id]]$status, "live"))
    to_remove <<- id
    ds <- registry()[[id]]
    showModal(modalDialog(
      title = "Remove Dataset?", badge(ds),
      p(style = "margin-top:10px", "Its Selection, preprocessing and all analyses are discarded.",
        "MultiOmics results that used it are kept but marked."),
      footer = tagList(modalButton("Cancel"), actionButton("remove_ok", "Remove", class = "btn-danger"))))
  })
  observeEvent(input$remove_ok, {
    removeModal()
    id <- to_remove; reg <- registry(); ds <- reg[[id]]
    if (v$removed == "tombstone") {
      ghost <- tabPanel(tab_title(ds, removed = TRUE), value = paste0(id, "_removed"),
        div(class = "tombstone", badge(ds, removed = TRUE),
            p(sprintf("Removed at %s. Its %d module scopes were destroyed; nothing here is live.",
                      format(Sys.time(), "%H:%M:%S"), nrow(MODULES) + 1)),
            actionButton(paste0("close_", id), "Close this tab", class = "btn-xs")))
      insertTab(ds_tabset, ghost, target = id, position = "after", select = TRUE)
      local({ gid <- id
        observeEvent(input[[paste0("close_", gid)]], once = TRUE,
                     removeTab(ds_tabset, paste0(gid, "_removed")))
      })
    }
    removeTab(ds_tabset, id)
    session$destroy(id) # tears down ds_k and every nested module scope
    ds$status <- "removed"; reg[[id]] <- ds; registry(reg)
    if (v$removed == "gone")
      showNotification(sprintf("Removed %s (%s): %d module scopes destroyed",
                               ds$name, ds$omic, nrow(MODULES) + 1), type = "message")
  })

  # ---- Upload-side list and picker
  output$ds_list <- renderUI({
    reg <- registry()
    if (!length(reg)) return(p(class = "text-muted", "none yet"))
    tags$ul(class = "list-unstyled", lapply(reg, function(d) tags$li(
      badge(d, removed = d$status != "live"), span(class = "ds-meta", d$upload),
      if (d$status != "live") em(" removed"))))
  })
  output$picker <- renderUI({
    l <- live()
    if (!length(l)) return(NULL)
    div(class = "ds-picker", "Dataset:",
        radioButtons("pick", NULL, inline = TRUE, selected = isolate(input$pick),
                     choiceNames = unname(lapply(l, tab_title)), choiceValues = names(l)))
  })
  observeEvent(input$pick, updateTabsetPanel(session, "ds_tabs", selected = input$pick))
  observeEvent(live(), if (v$layout == "picker" && length(live()))
    updateRadioButtons(session, "pick", selected = tail(names(live()), 1)))

  # ---- MultiOmics: reads only through the registry
  output$mo_choices <- renderUI({
    l <- live()
    checkboxGroupInput("mo_ds", "Datasets", selected = names(l),
                       choiceNames = unname(lapply(l, badge)), choiceValues = names(l))
  })
  mo <- eventReactive(input$mo_run, {
    req(length(input$mo_ds) >= 2)
    list(ids = input$mo_ds, at = Sys.time())
  })
  output$mo_result <- renderUI({
    r <- mo(); reg <- registry()
    gone <- Filter(function(i) reg[[i]]$status != "live", r$ids)
    tagList(
      if (length(gone)) div(class = "alert alert-warning",
        "This result used a Dataset that has since been removed: ",
        lapply(gone, function(i) badge(reg[[i]], removed = TRUE))),
      p("Correlation computed at", format(r$at, "%H:%M:%S"), "from:",
        lapply(r$ids, function(i) badge(reg[[i]], removed = reg[[i]]$status != "live"))),
      renderPlot(image(matrix(runif(100), 10), main = "dummy cross-Dataset correlation"))
    )
  })

  # ---- debug panel: live module scopes, heartbeats, pings, inputs
  output$debug <- renderUI({
    invalidateLater(1000)
    sc <- session$userData$scopes
    ids <- sort(ls(sc))
    inp <- names(isolate(reactiveValuesToList(input)))
    rows <- lapply(ids, function(i) {
      s <- sc[[i]]
      dead <- !is.na(s$destroyed)
      if (dead && !isTRUE(input$show_destroyed)) return(NULL)
      tags$tr(class = if (dead) "dead" else if (s$kind == "dataset") "ds-row",
        tags$td(i), tags$td(if (dead) "destroyed" else "live"),
        tags$td(s$heartbeats), tags$td(s$pings),
        tags$td(sum(startsWith(inp, paste0(i, "-")))))
    })
    n_live <- sum(vapply(ids, function(i) is.na(sc[[i]]$destroyed), TRUE))
    tagList(
      p(sprintf("shiny %s · %d live scopes · %d destroyed · soft cap %d",
                packageVersion("shiny"), n_live, length(ids) - n_live, SOFT_CAP)),
      p(class = "text-muted small", "heartbeat = invalidateLater(1000) observer; pings = observeEvent on a root reactiveVal;",
        "inputs = input names with the scope's prefix. Destroyed rows must stay frozen and show 0 inputs."),
      tags$table(class = "table table-condensed small",
        tags$tr(tags$th("scope"), tags$th("state"), tags$th("beats"), tags$th("pings"), tags$th("inputs")),
        rows)
    )
  })
}

shinyApp(ui, server)
