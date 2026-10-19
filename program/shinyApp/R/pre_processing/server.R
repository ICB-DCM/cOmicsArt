### Pre-processing module server
# Runs the Pre-processing on the Selection and shows, saves and reports it.
# Arguments:
#   raw: reactive, the Upload's raw SummarizedExperiment
#   omic_type: reactive, the Upload's omic type
#   selected: reactive, the Selection (resolve_selection()): its data is
#     what a run pre-processes, the controls and the R code read
#   selection_confirmed: reactive, changes when the Selection is confirmed;
#     hides the plots until the next run
#   upload_done: reactive, set after each successful Upload
# Returns reactives:
#   result: the last successful run, NULL before; set once at its end, so a
#     change is the done event. A list of run_preprocessing()'s data,
#     data_batch_corrected, DESeq_obj and DESeq_obj_batch_corrected (DESeq
#     objects for vst_DESeq only), with
#       params: the options, named like the arguments of preprocessing(),
#         with batch_column and violin_color
#       selection: the Selection it pre-processed (resolve_selection())
#       omic_type: the omic type it ran for
#       run: the number of the run, so that a repeated run is a change
#   failed: counts up after each run that failed (not for the normality test)
#   start_discovering: the "Start Discovering" button
preprocessing_server <- function(id, raw, omic_type, selected, selection_confirmed, upload_done){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    file_path <- paste0("/www/", session$token, "/")

    result <- reactiveVal(NULL)
    console_output <- reactiveVal(NULL)
    warnings <- reactiveVal(NULL)
    failed <- reactiveVal(NULL)
    count_up <- function(counter){
      counter((isolate(counter()) %||% 0) + 1)
    }
    # the parameters of the last run with its Selection, for the R code
    rcode_params <- reactive({
      req(result())
      c(
        selection_params(result()$selection),
        list(omic_type = result()$omic_type),
        result()$params
      )
    })

    react_violin_plot <- reactiveVal(NULL)
    react_violin_plot_raw <- reactiveVal(NULL)
    react_mean_sd_plot <- reactiveVal(NULL)
    able_to_plot <- reactiveVal(FALSE)

    shinyjs::hideElement(id = "data_summary")
    # the Selection's data the controls offer choices for: as of the last
    # Upload or confirmed Selection, not while it is being changed
    controls_data <- reactive({
      upload_done()
      selection_confirmed()
      isolate(selected()$data)
    })
    observeEvent(selection_confirmed(), {
      able_to_plot(FALSE)
    })

## UI section ----
    # Dynamically update second dropdown based on Processing Type
    output$dynamic_options_ui <- renderUI({
      req(input$processing_type)  # Ensure input exists
      choices <- procedure_choices(input$processing_type)
      tagList(
      if(choices$filtering){
        div(style = "margin-left: 15px;",
            selectInput(
              inputId = ns("PreProcessing_Procedure_filtering"),
              label = "Choose Filtering",
              choices = c("global Filtering" = "filterOnly",
                          "sample-wise Filtering" = "filterPerSample"),
              selected = NULL,
              multiple = FALSE
            )
        )
      },
      div(style = "margin-left: 15px;",
        selectInput(
          inputId = ns("PreProcessing_Procedure"),
          label = "Choose Processing Option",
          choices = choices$procedures,
          selected = NULL,
          multiple = FALSE
        )
      )
      )
    })
    # Show additional numeric input if "Omic-specific filtering" is chosen
    output$additional_inputs_filter_ui <- renderUI({
      req(input$PreProcessing_Procedure)
      filter <- filter_inputs(
        input$PreProcessing_Procedure, input$PreProcessing_Procedure_filtering,
        omic_type(), ncol(controls_data())
      )
      if (is.null(filter)) return(NULL)
      if (filter$kind == "global") {
        tagList(
          div(style = "margin-left: 30px;",
            numericInput(
              inputId = ns("filter_threshold"),
              label = "Specifcy the minimum sum of counts/concentration accross all samples for an entitie to be kept in the analysis.",
              value = filter$threshold,
              min = 0
            )
          )
        )
      } else {
        tagList(
          div(style = "margin-left: 30px;",
          numericInput(
              inputId = ns("filter_threshold_samplewise"),
              label = "Specifcy theFiltering threshold of counts/concentration for an entitie",
              value = filter$threshold
            ),
          numericInput(
              inputId = ns("filter_samplesize"),
              label = "Number of Samples that need to pass the Filtering threshold",
              value = filter$samplesize,
              min = 1,
              max = filter$max_samplesize,
              step = 1
            ),
          helpText(paste0("Note: This number should not exceed ", filter$max_samplesize, "(the number of samples in your data set)")
          )
          )
        )
      }
    })

    # Update the batch effect UI based on the available columns
    output$batch_effect_ui <- renderUI({
      req(upload_done())
      selectInput(
        inputId = ns("BatchEffect_Column"),
        label = "[optional] Select Batch Effect Column",
        choices = batch_column_choices(
          raw(), input$PreProcessing_Procedure, input$DESeq_formula_sub
        ),
        selected = "NULL"
      )
    })
    output$formula_sub_ui <- renderUI({
      req(upload_done())
      req(input$PreProcessing_Procedure)
      design <- design_inputs(controls_data(), input$PreProcessing_Procedure)
      if (is.null(design)) return(NULL)
      if(design$kind == "deseq"){
        div(style = "margin-left: 30px;",
          selectInput(
            inputId = ns("DESeq_formula_sub"),
            label = paste0(
              "Choose factors to account for ",
              "(App might crash if your factor has only 1 sample per level)"
            ),
            choices = design$choices,
            multiple = T,
            selected = design$selected
          )) %>% helper(type = "markdown", content = "PreProcessing_DESeq")
      }else{
        div(style = "margin-left: 30px;",
          tagList(
            # add a radiobutton whether one wants an intercept or not
            radioButtons(
              inputId = ns("limma_intercept"),
              label = "Include Intercept?",
              choices = c("Yes" = "TRUE", "No" = "FALSE"),
              selected = "TRUE"
            ),
          selectInput(
            inputId = ns("limma_formula"),
            label = paste0(
              "Choose the design formula for limma voom"
            ),
            choices = design$choices,
            multiple = T,
            selected = design$selected
          ) %>% helper(type = "markdown", content = "PreProcessing_voom")
          )
        )
      }
    })

    output$violin_plot_color_ui <- renderUI({
      req(selected())
      selectInput(
        inputId = ns("violin_color"),
        label = "Color the violin plot by:",
        choices = c(colnames(colData(raw()))),
        selected = c(colnames(colData(raw())))[1],
        multiple = F
      )
    })

    # the summary of the last run, or why it failed
    summary_text <- reactiveVal("Press 'Get-Preprocessing' to start!")
    output$Statisitcs_Data <- renderText(summary_text())

## Plots ----
    output$raw_violin_plot <- renderPlot({
      req(able_to_plot())
      violin_plot(result()$selection$data, violin_color = isolate(input$violin_color))
    })
    output$preprocessed_violin_plot <- renderPlot({
      req(able_to_plot())
      violin_plot(result()$data, violin_color = input$violin_color)
    })
    output$mean_sd_plot <- renderPlot({
      req(able_to_plot())
      req(react_mean_sd_plot())
      react_mean_sd_plot()
    })

## Console output ----
    hasConsoleOutput <- reactive({
      length(console_output()) > 0 || length(warnings()) > 0
    })
    observe({
      if(hasConsoleOutput()) {
        shinyjs::show("console_toggle_button")
      } else {
        shinyjs::hide("console_toggle_button")
      }
    })
    observeEvent(input$console_toggle_button, {
      shinyjs::toggle(id = "console_toggle")
    })
    output$consoleOutputDisplay <- renderUI({
      # Only show if we have output
      if (!hasConsoleOutput()) return(NULL)
      tagList(
        div(
          class = "console-output-container",
          if (length(console_output()) > 0) {
            tagList(
              h4("Console Output:"),
              tags$pre(paste(console_output(), collapse = "\n"))
            )
          },
          if (length(warnings()) > 0) {
            tagList(
              h4("Warnings and Messages:"),
              tags$pre(paste(warnings(), collapse = "\n"))
            )
          }
        )
      )
    })

## Preprocessing ----
    observeEvent(input$Do_preprocessing, {
      # only enter this when you actually click data
      req(input$Do_preprocessing > 0)
      shinyjs::showElement(id = "data_summary")
      waiter <- Waiter$new(
        html = LOADING_SCREEN,
        color = "#3897F147",
        hide_on_render = FALSE
      )
      waiter$show()
      on.exit(waiter$hide())
      print("Starting the Preprocessing")
      # ---- Options, named like the arguments of preprocessing() ----
      options <- list(
        preprocessing_procedure = input$PreProcessing_Procedure,
        preprocessing_filtering = input$PreProcessing_Procedure_filtering,
        deseq_factors = input$DESeq_formula_sub,
        filter_threshold = input$filter_threshold,
        filter_threshold_samplewise = input$filter_threshold_samplewise,
        filter_samplesize = input$filter_samplesize,
        limma_intercept = input$limma_intercept,
        limma_formula = input$limma_formula
      )
      batch_column <- input$BatchEffect_Column %||% "NULL"
      selection <- selected()
      selected_data <- selection$data

      # returns NULL for `res`
      preprocessing_failed <- function(message, additional_text = NULL){
        error_modal(message, additional_text = additional_text)
        summary_text(ERROR_PREPROC)
        count_up(failed)
        NULL
      }
      res <- tryCatch(
        run_preprocessing(
          data = selected_data,
          omic_type = omic_type(),
          options = options,
          batch_column = batch_column
        ),
        normality_error = function(e){
          summary_text(ERROR_NORM_TEST)
          NULL
        },
        batch_error = function(e){
          preprocessing_failed(conditionMessage(e), additional_text = ifelse(
            options$preprocessing_procedure == "vst_DESeq", ERROR_BATCH_DESEQ, ERROR_BATCH_CORR
          ))
        },
        error = function(e) preprocessing_failed(conditionMessage(e))
      )
      if (is.null(res)) return()
      data <- res$data
      run_params <- c(options, list(batch_column = batch_column, violin_color = input$violin_color))

      # everything that can fail before anything is shown or returned
      summary <- preprocessing_summary(
        dim(raw()), data, options$preprocessing_procedure, res$warnings
      )
      violin <- violin_plot(data = data, violin_color = input$violin_color)
      violin_raw <- violin_plot(data = selected_data, violin_color = input$violin_color)
      mean_and_obj <- vsn::meanSdPlot(as.matrix(assay(data)), plot=FALSE)
      mean_sd <- mean_and_obj$gg + CUSTOM_THEME + ggtitle("Mean and SD per entity")

## Log preprocessing ----
      fun_LogIt(session, "## Pre Processing {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      for (line in preprocessing_log_lines(run_params, dim(data))) {
        fun_LogIt(session, message = line)
      }
      fun_LogIt(session, message = "### Publication Snippet")
      fun_LogIt(session, message = snippet_preprocessing(
        data = list(data = data),
        params = run_params
      ))
      fun_LogIt(session, message = "<br>")

      console_output(res$console_output)
      warnings(res$warnings)
      summary_text(summary)
      react_violin_plot(violin)
      react_violin_plot_raw(violin_raw)
      react_mean_sd_plot(mean_sd)
      able_to_plot(TRUE)
      result(c(
        res[c("data", "data_batch_corrected", "DESeq_obj", "DESeq_obj_batch_corrected")],
        list(
          params = run_params,
          selection = selection,
          omic_type = omic_type(),
          run = input$Do_preprocessing
        )
      ))
      # set the warning as toast
      show_toast(
        title = "Attention",
        text = HTML(create_warning_preproc(
          options$preprocessing_procedure, res$n_processed, nrow(data)
        )),
        position = "top",
        timer = 2500,
        timerProgressBar = T
      )
    })

## Preprocessing save, Report and Code Snippet ----
    # the raw and the pre-processed violin plot side by side
    combined_violin_plot <- function(){
      raw_plot <- react_violin_plot_raw() +
        ggtitle("Count distribution per sample - raw") + theme(legend.position = "none")

      preprocessed_plot <- react_violin_plot() +
        ggtitle("Count distribution per sample - preprocessed")

      # Arrange the plots side by side with more space for the right plot
      grid.arrange(
        raw_plot,
        preprocessed_plot,
        ncol = 2,
        widths = c(1, 1.3)
      )
    }
    # Code.R, Data.rds and the data as csv files for `pipeline_info`
    rcode_download <- function(pipeline_info){
      downloadHandler(
        filename = function() {
          paste0("cOmicsArt_Rcode2Reproduce_", Sys.Date(), ".zip")
        },
        content = function(file) {
          waiter <- Waiter$new(
            html = LOADING_SCREEN,
            color = "#3897F147",
            hide_on_render = FALSE
          )
          waiter$show()
          envList <- list(
            par_tmp = rcode_params()
          )
          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)
          # save csv files
          save_summarized_experiment(
            raw(),
            temp_directory
          )

          write(
            create_workflow_script(
              pipeline_info = pipeline_info,
              par = rcode_params(),
              path_to_util = file.path(temp_directory, "util.R")
            ),
            file.path(temp_directory, "Code.R")
          )

          saveRDS(envList, file.path(temp_directory, "Data.rds"))

          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
          waiter$hide()
        },
        contentType = "application/zip"
      )
    }

    # "Save plot" (`save_id`) and "Send only to Report" (`report_id`) for
    # `plot()`: saved as `file_stem`_<time> with the extension chosen in
    # `ext_id`; saving also sends it to the Report. The Report gets `heading`,
    # the line `info()` and the plot, titled `title`.
    plot_downloads <- function(save_id, report_id, ext_id, file_stem, plot, heading, info, title){
      save_plot <- function(file){
        ggsave(
          filename = file,
          plot = plot(),
          width = 16,  # Increase the width of the figure
          height = 8,  # Adjust height if necessary
          units = "in",
          device = gsub("\\.","",input[[ext_id]])
        )
      }
      output[[save_id]] <- downloadHandler(
        filename = function() {
          paste0(file_stem, "_", format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input[[ext_id]])
        },
        content = function(file) {
          save_plot(file)
          on.exit({shinyjs::click(ns(report_id))})
        }
      )
      observeEvent(input[[report_id]],{
        notificationID <- showNotification("Saving to Report...",duration = 0)
        tmp_filename <- paste0(
          getwd(),
          file_path,
          paste0(file_stem, "_", format(Sys.time(), "%Y_%m_%d_%H_%M_%S"), input[[ext_id]])
        )
        save_plot(tmp_filename)
        fun_LogIt(session, message = paste0("## ", heading, "{.tabset .tabset-fade}"))
        fun_LogIt(session, message = "### Info")
        fun_LogIt(session, message = info())
        fun_LogIt(session,
          message = paste0("**PreProcess** - ![", title, "](",tmp_filename,")")
        )
        if(isTruthy(input$NotesPreprocessedData) & !(isEmpty(input$NotesPreprocessedData))){
          fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesPreprocessedData)))
        }
        # no publication snippet as thats already in the log
        removeNotification(notificationID)
        showNotification("Report Saved!",type = "message", duration = 1)
      })
    }

    plot_downloads(
      save_id = "SavePlot_Preprocess",
      report_id = "only2Report_Preprocess",
      ext_id = "file_ext_Preprocess",
      file_stem = "Preprocessing_ViolinPlot",
      plot = combined_violin_plot,
      heading = "PreProcessing Violin Plot",
      info = function() paste0("**PreProcess** - The Samples were plotted after: ",input$violin_color),
      title = "Violin Plot"
    )
    output$getR_Code_Preprocess <- rcode_download(VIOLIN_PLOT_PIPELINE)

    plot_downloads(
      save_id = "SavePlot_mean_sd_plot",
      report_id = "only2Report_mean_sd_plot",
      ext_id = "file_type_mean_sd_plot",
      file_stem = "Preprocessing_mean_sd_plot",
      plot = react_mean_sd_plot,
      heading = "PreProcessing Mean and SD Plot",
      info = function() "The means of the preprocessed data are plotted agains their standard deviation to check for heteroskedasticity.",
      title = "Mean and SD Plot"
    )
    output$getR_Code_mean_sd_plot <- rcode_download(MEAN_SD_PLOT_PIPELINE)

    list(
      result = reactive(result()),
      failed = reactive(failed()),
      start_discovering = reactive(input$GoToStartAnalysis)
    )
  })
}
