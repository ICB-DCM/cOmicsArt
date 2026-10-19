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
#   processed, batch_corrected: the processed and batch-corrected objects
#   deseq, deseq_batch_corrected: the DESeq objects (vst_DESeq only)
#   params: the options of the last run, named like the arguments of
#     preprocessing(), with batch_column and violin_color
#   started: changes on every "Get Pre-Processing"
#   done: counts up after each successful run
#   failed: counts up after each run that failed (not for the normality test)
#   start_discovering: the "Start Discovering" button
preprocessing_server <- function(id, raw, omic_type, selected, selection_confirmed, upload_done){
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    file_path <- paste0("/www/", session$token, "/")

    processed <- reactiveVal(NULL)
    batch_corrected <- reactiveVal(NULL)
    deseq <- reactiveVal(NULL)
    deseq_batch_corrected <- reactiveVal(NULL)
    params <- reactiveVal(NULL)
    # the Selection and the parameters of the last run, for the R code
    raw_selected <- reactiveVal(NULL)
    rcode_params <- reactiveVal(NULL)
    console_output <- reactiveVal(NULL)
    warnings <- reactiveVal(NULL)
    done <- reactiveVal(NULL)
    failed <- reactiveVal(NULL)
    count_up <- function(counter){
      counter((isolate(counter()) %||% 0) + 1)
    }

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

    output$Statisitcs_Data <- renderText({
      "Press 'Get-Preprocessing' to start!"
    })

## Plots ----
    output$raw_violin_plot <- renderPlot({
      req(able_to_plot())
      violin_plot(raw_selected(), violin_color = isolate(input$violin_color))
    })
    output$preprocessed_violin_plot <- renderPlot({
      req(able_to_plot())
      violin_plot(processed(), violin_color = input$violin_color)
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

      preprocessing_failed <- function(message, additional_text = NULL){
        error_modal(message, additional_text = additional_text)
        output$Statisitcs_Data <- renderText({ERROR_PREPROC})
        count_up(failed)
        waiter$hide()
        req(FALSE)
      }
      res <- tryCatch(
        run_preprocessing(
          data = selected_data,
          omic_type = omic_type(),
          options = options,
          batch_column = batch_column
        ),
        normality_error = function(e){
          output$Statisitcs_Data <- renderText({ERROR_NORM_TEST})
          waiter$hide()
          req(FALSE)
        },
        batch_error = function(e){
          preprocessing_failed(conditionMessage(e), additional_text = ifelse(
            options$preprocessing_procedure == "vst_DESeq", ERROR_BATCH_DESEQ, ERROR_BATCH_CORR
          ))
        },
        error = function(e) preprocessing_failed(conditionMessage(e))
      )
      data <- res$data
      run_params <- c(options, list(batch_column = batch_column, violin_color = input$violin_color))
      params(run_params)
      rcode_params(c(selection_params(selection), list(omic_type = omic_type()), run_params))
      raw_selected(selected_data)
      batch_corrected(res$data_batch_corrected)
      deseq(res$DESeq_obj)
      deseq_batch_corrected(res$DESeq_obj_batch_corrected)
      console_output(res$console_output)
      warnings(res$warnings)
      processed(data)

      output$Statisitcs_Data <- renderText(preprocessing_summary(
        dim(raw()), data, options$preprocessing_procedure, res$warnings
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
      react_violin_plot(violin_plot(
        data = data,
        violin_color = input$violin_color
      ))
      react_violin_plot_raw(violin_plot(
        data = selected_data,
        violin_color = input$violin_color
      ))
      mean_and_obj <- vsn::meanSdPlot(as.matrix(assay(data)), plot=FALSE)
      react_mean_sd_plot(mean_and_obj$gg + CUSTOM_THEME + ggtitle("Mean and SD per entity"))
      able_to_plot(TRUE)

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

      count_up(done)
      waiter$hide()
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

    output$SavePlot_Preprocess <- downloadHandler(
      filename = function() {
        paste0("Preprocessing_ViolinPlot_", format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input$file_ext_Preprocess)
      },
      content = function(file) {
        # Save the combined plot
        ggsave(
          filename = file,
          plot = combined_violin_plot(),
          width = 16,  # Increase the width of the figure
          height = 8,  # Adjust height if necessary
          units = "in",
          device = gsub("\\.","",input$file_ext_Preprocess)
        )
        on.exit({shinyjs::click("only2Report_Preprocess")})
      }
    )

    observeEvent(input$only2Report_Preprocess,{
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(),
        file_path,
        paste0(
          "Preprocessing_ViolinPlot_",
          format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
          input$file_ext_Preprocess
        )
      )
      ggsave(
        filename = tmp_filename,
        plot = combined_violin_plot(),
        width = 16,  # Increase the width of the figure
        height = 8,  # Adjust height if necessary
        units = "in",
        device = gsub("\\.","",input$file_ext_Preprocess)
      )
      fun_LogIt(session, message = "## PreProcessing Violin Plot{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = paste0("**PreProcess** - The Samples were plotted after: ",input$violin_color))
      fun_LogIt(session,
        message = paste0("**PreProcess** - ![Violin Plot](",tmp_filename,")")
      )
      if(isTruthy(input$NotesPreprocessedData) & !(isEmpty(input$NotesPreprocessedData))){
        fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesPreprocessedData)))
      }
      # no publication snippet as thats already in the log
      removeNotification(notificationID)
      showNotification("Report Saved!",type = "message", duration = 1)
    })

    output$getR_Code_Preprocess <- rcode_download(VIOLIN_PLOT_PIPELINE)

    output$SavePlot_mean_sd_plot <- downloadHandler(
      filename = function() {
        paste0("Preprocessing_mean_sd_plot_", format(Sys.time(), "(%d.%m.%Y)_(%H;%M;%S)"), input$file_type_mean_sd_plot)
      },
      content = function(file) {
        ggsave(
          filename = file,
          plot = react_mean_sd_plot(),
          width = 16,  # Increase the width of the figure
          height = 8,  # Adjust height if necessary
          units = "in",
          device = gsub("\\.","",input$file_type_mean_sd_plot)
        )
        on.exit({shinyjs::click("only2Report_mean_sd_plot")})
      }
    )

    observeEvent(input$only2Report_mean_sd_plot,{
      notificationID <- showNotification("Saving to Report...",duration = 0)
      tmp_filename <- paste0(
        getwd(),
        file_path,
        paste0(
          "Preprocessing_mean_sd_plot_",
          format(Sys.time(), "%Y_%m_%d_%H_%M_%S"),
          input$file_type_mean_sd_plot
        )
      )
      ggsave(
        filename = tmp_filename,
        plot = react_mean_sd_plot(),
        width = 16,  # Increase the width of the figure
        height = 8,  # Adjust height if necessary
        units = "in",
        device = gsub("\\.","",input$file_type_mean_sd_plot)
      )
      fun_LogIt(session, message = "## PreProcessing Mean and SD Plot{.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session, message = "The means of the preprocessed data are plotted agains their standard deviation to check for heteroskedasticity.")
      fun_LogIt(session,
        message = paste0("**PreProcess** - ![Mean and SD Plot](",tmp_filename,")")
      )
      if(isTruthy(input$NotesPreprocessedData) & !(isEmpty(input$NotesPreprocessedData))){
        fun_LogIt(session, message = add_notes_report(shiny::markdown(input$NotesPreprocessedData)))
      }
      # no publication snippet as thats already in the log
      removeNotification(notificationID)
      showNotification("Saved!",type = "message", duration = 1)
    })

    output$getR_Code_mean_sd_plot <- rcode_download(MEAN_SD_PLOT_PIPELINE)

    list(
      processed = reactive(processed()),
      batch_corrected = reactive(batch_corrected()),
      deseq = reactive(deseq()),
      deseq_batch_corrected = reactive(deseq_batch_corrected()),
      params = reactive(params()),
      started = reactive(input$Do_preprocessing),
      done = reactive(done()),
      failed = reactive(failed()),
      start_discovering = reactive(input$GoToStartAnalysis)
    )
  })
}
