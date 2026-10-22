### Upload module server
# Everything that brings data into a session: the four sources, visual
# inspection, gene annotation, the status box and the Upload's report lines.
# Returns reactives:
#   done: "DataUploadSuccesful" after each successful Upload (passed to the
#     analysis modules as data_input_shiny)
#   raw: the raw SummarizedExperiment, also after gene annotation
#   dataset_copy: the Dataset's starting copy of the last Upload
#   annotated: counts up after each gene annotation
#   cleared: counts up when the File Input is cleared
#   source, omic_type, organism, added_gene_anno: about the current Upload
upload_server <- function(id){
  moduleServer(id, function(input, output, session){
    ns <- session$ns

    # Counts up on every "Upload new data" / "Upload test data" / "Use the Data"
    commit <- reactiveVal(NULL)
    uploaded_from <- reactiveVal(NULL)
    omic_type <- reactiveVal(NULL)
    raw <- reactiveVal(NULL)
    dataset_copy <- reactiveVal(NULL)
    organism <- reactiveVal(NULL)
    added_gene_anno <- reactiveVal(FALSE)
    annotated <- reactiveVal(NULL)
    cleared <- reactiveVal(NULL)
    vi <- reactiveValues(
      passed = FALSE,                   # Visual inspection passed
      changed = FALSE,                  # Data changed during VI
      tables = NULL,                    # Tables offered for column-name correction
      checks = NULL                     # Check results kept after correction
    )
    data_changed_since_VI <- reactiveVal(TRUE)  # Track if data has changed since last VI

    start_upload <- function(source){
      uploaded_from(source)
      count_up(commit)
    }
    reset_file_input <- function(){
      reset("data_matrix1")
      reset("data_sample_anno1")
      reset("data_row_anno1")
      session$sendCustomMessage(type = "resetValue", message = ns("data_matrix1"))
      session$sendCustomMessage(type = "resetValue", message = ns("data_sample_anno1"))
      session$sendCustomMessage(type = "resetValue", message = ns("data_row_anno1"))
    }
    # The file inputs of all sources, for upload_inputs_complete()
    upload_files <- function(){
      list(
        matrix = input$data_matrix1,
        sample_table = input$data_sample_anno1,
        row_annotation = input$data_row_anno1,
        precompiled = input$data_preDone,
        metadata_matrix = input$data_matrix_metadata,
        metadata_sheet = input$metadataInput,
        metadata_row_annotation = input$data_row_anno_metadata
      )
    }

    # Reset file input
    observeEvent(input$clear_FileInput, {
      reset_file_input()
      count_up(cleared)
      output$debug <- renderText({
        "<b>Cleared data, hence there is nothing to upload. Please select your data or use the Testdata</b><br>Make sure to click on  'Upload new data' before proceeding to pre-processing"
      })
    })

    # Track file input changes to know when to clear VI outputs
    observeEvent(input$data_matrix1, {
      data_changed_since_VI(TRUE)
    })
    observeEvent(input$data_sample_anno1, {
      data_changed_since_VI(TRUE)
    })
    observeEvent(input$metadataInput, {
      data_changed_since_VI(TRUE)
    })
    observeEvent(input$data_row_anno1, {
      data_changed_since_VI(TRUE)
    })

    output$SaveInputAsList <- downloadHandler(
     filename = function() {
       paste0(omic_type(), "_only_precompiled", " ", Sys.time(), ".RDS") },
      content = function(file){
        # TODO Q: What to save here? only original enough?
        saveRDS(
          object = raw(),
          file = file
        )
      }
    )

## Gene annotation ----
    observeEvent(omic_type(),{
      output$AddGeneSymbols_ui <- NULL
      output$AddGeneSymbols_organism_ui <- NULL

      if(omic_type() == "Transcriptomics"){
        output$AddGeneSymbols_ui <- renderUI({
          actionButton(
            inputId = ns("AddGeneSymbols"),
            label = "Add Gene Annotation"
          )
        })
        output$AddGeneSymbols_organism_ui <- renderUI({selectInput(
          inputId = ns("AddGeneSymbols_organism"),
          label = "Which Organisms?",
          # choices = listDatasets(useEnsembl(biomart = "genes"))[,"description"],
          choices = c("Mouse genes (GRCm39)", "Human genes (GRCh38.p14)"),
          selected = "Mouse genes (GRCm39)"
        )})
      }
    })

    # Show or hide the gene annotation options based on the button click
    observeEvent(input$geneAnno_toggle_button, {
      shinyjs::toggle(id = "geneAnno_toggle")  # Toggle the div on button click
    })

    observeEvent(input$AddGeneSymbols, {
      req(data_input_shiny())
      req(raw())
      annotation_result <- detect_annotation(raw())
      annotation_name <- annotation_result$AnnoType
      column_name <- annotation_result$AnnoCol

      showModal(modalDialog(
        title = "Gene Annotation",
        HTML(paste0(
          "We tried to find an appropriate annotation and ",
          if (is.null(annotation_name)) {

            "found nothing."
          } else {
            paste0("might have found <strong>", annotation_name, "</strong> in <strong>", column_name, "</strong>. Is that correct?")
          }
        )),
        fluidRow(
          column(6, selectInput(
            inputId = ns("annotation_name"),
            label = "Gene Annotation",
            choices = c("entrezgene_id", "ensembl_gene_id", "Symbol"),
            selected = annotation_name
          )),
          column(6, selectInput(
            inputId = ns("annotation_colname"),
            label = "Column Names",
            choices = colnames(rowData(raw())),
            selected = column_name
          ))
        ),
        footer = tagList(
          modalButton("Close"),
          actionButton(
            inputId = ns("do_annotation"),
            label = "Do annotation"
          )
        )
      ))
    })

    observeEvent(input$do_annotation, {
      annotated_se <- raw()
      # Added gene annotation if asked for
      if(input$AddGeneSymbols & omic_type() == "Transcriptomics") {
        if(input$AddGeneSymbols_organism == "Human genes (GRCh38.p14)"){
          ensembl_slot <- "hsapiens_gene_ensembl"
        }else{
          ensembl_slot <- "mmusculus_gene_ensembl"
        }

        ensembl <- loadedVersion[[ensembl_slot]]$ensmbl
        out <- tryCatch(
          getBM(
            attributes = c("ensembl_gene_id", "gene_biotype", "external_gene_name", "entrezgene_id"),
            filter = input$annotation_name,
            values = rowData(annotated_se)[,input$annotation_colname],
            mart = ensembl
          ),
          error = function(e){
            print(paste("Gene annotation failed:", conditionMessage(e)))
            NULL
          }
        )
        if(is.null(out)){
          # Ensembl could not be reached or refused the query: keep the data as it is
          output$debug <- renderText({"<font color=\"#ab020a\"><b>Gene annotation failed: Ensembl could not be reached. No annotation was added, please try again later.</b></font>"})
          removeModal()
          return()
        }
        fun_LogIt(session,
          message = "**DataInput** - Gene Annotation (SYMBOL and gene type) was added"
        )
        fun_LogIt(session,
          message = paste0("**DataInput** - chosen Organism: ", input$AddGeneSymbols_organism)
        )

        output$debug <- renderText({"<font color=\"#00851d\"><b>Added gene annotation</b></font>"})
        # Align the rows based on matching annotation
        match_indices <- match(
          rowData(annotated_se)[,input$annotation_colname], out[,input$annotation_name]
        )
        matched_out <- out[match_indices, ]

        if (all(is.na(matched_out$ensembl_gene_id))) {
          # Most likely wrong organism used
          output$debug <- renderText({"<font color=\"#ab020a\"><b>You have most likely chosen the wrong organism! No annotation was added</b></font>"})
        } else {
          # Initialize new columns in the rowData with NA
          rowData(annotated_se)$ensembl_gene_id <- NA
          rowData(annotated_se)$gene_biotype <- NA
          rowData(annotated_se)$external_gene_name <- NA
          rowData(annotated_se)$entrezgene_id <- NA

          # Update rowData with matched information
          matched_rows <- !is.na(match_indices)
          rowData(annotated_se)$ensembl_gene_id[matched_rows] <- matched_out$ensembl_gene_id[matched_rows]
          rowData(annotated_se)$gene_biotype[matched_rows] <- matched_out$gene_biotype[matched_rows]
          rowData(annotated_se)$external_gene_name[matched_rows] <- matched_out$external_gene_name[matched_rows]
          rowData(annotated_se)$entrezgene_id[matched_rows] <- matched_out$entrezgene_id[matched_rows]
        }
      }

      # edit annotation columns such that if na is present in the row annotation,
      # the na gets replaced by the rowname
      raw(fill_na_row_annotation(annotated_se))

      added_gene_anno(TRUE)
      organism(input$AddGeneSymbols_organism)
      count_up(annotated)
      removeModal()
    })

    # Observer to toggle visibility of the download button and helper
    observe({
      if (is.null(uploaded_from())) {
        shinyjs::hide("SaveInputAsRDS")
      }
      req(uploaded_from())
      if (uploaded_from() == "metadata" || uploaded_from() == "file_input") {
        shinyjs::show("SaveInputAsRDS")
      } else {
        shinyjs::hide("SaveInputAsRDS")
      }
    })

    # Show gene annotation and saving after a successful Upload
    observe({
      req(data_input_shiny() == "DataUploadSuccesful")
      shinyjs::showElement("upload_done")
    })

## Upload visual inspection ----
    snippetYes <- "<font color=\"#00851d\"><b>Yes</b></font>"
    snippetNo <-  "<font color=\"#ab020a\"><b>No</b></font>"
    observeEvent(input$inspect_data, {
      # Clear outputs only if data has changed since last inspection
      if(data_changed_since_VI()) {
        output$DataMatrix_VI <- DT::renderDataTable({DT::datatable(data = data.frame())})
        output$SampleMatrix_VI <- DT::renderDataTable({DT::datatable(data = data.frame())})
        output$EntitieMatrix_VI <- DT::renderDataTable({DT::datatable(data = data.frame())})
        output$DataMatrix_VI_Info <- renderText({""})
        output$SampleMatrix_VI_Info <- renderText({""})
        output$EntitieMatrix_VI_Info <- renderText({""})
        output$OverallChecks <- renderText({""})
      }

      showModal(modalDialog(
        title = "Upload Visual Inspection",
        helpText("If you have uploaded your data, you might want to visually check the tables to confirm the correct data format. If you notice irregualarities you will need to correct the input data - this cannot be done in cOmicsArt, See the help on how your data is expected."),
        br(),
        actionButton(
          inputId = ns("DoVisualDataInspection"),
          label = "Upload data for visual inspection"
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput(ns("DataMatrix_VI")),
          htmlOutput(outputId = ns("DataMatrix_VI_Info"), container = pre)
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput(ns("SampleMatrix_VI")),
          htmlOutput(outputId = ns("SampleMatrix_VI_Info"), container = pre)
        ),
        splitLayout(
          style = "border: 1px solid silver:", cellWidths = c("70%", "30%"),
          DT::dataTableOutput(ns("EntitieMatrix_VI")),
          htmlOutput(outputId = ns("EntitieMatrix_VI_Info"), container = pre)
        ),
        includeMarkdown("helpfiles/VI_checks.md"),
        htmlOutput(outputId = ns("OverallChecks"), container = pre),
        easyClose = T,
        footer = tagList(
          actionButton(ns("usingVIdata"), "Use the Data"),
          actionButton(ns("CloseVI"),"Close")
          ),
        size = "l", # large modal
        class = "custom-modal" # custom class for this modal
      ))
    })
    observeEvent(input$DoVisualDataInspection, {
      tryCatch({
      if(isTruthy(input$data_preDone)){
        output$DataMatrix_VI_Info <- renderText({
          "Visual Inspection only for primary data, not for precompiled set possible!"
        })
        req(F)
      }
      if(!(isTruthy(input$data_matrix1) &
           (isTruthy(input$data_sample_anno1)|isTruthy(input$metadataInput)) &
           isTruthy(input$data_row_anno1))){
        output$OverallChecks <- renderText(
          "<font color=\"#ab020a\"><b>The Upload has failed completely, or you haven't uploaded anything yet. Need to uploade all three matrices!</b></font>"
          )
      } else {
        inspection <- inspect_upload_files(
          matrix_path = input$data_matrix1$datapath,
          row_annotation_path = input$data_row_anno1$datapath,
          sample_table_path = if(isTruthy(input$data_sample_anno1)) input$data_sample_anno1$datapath,
          metadata_path = if(isTruthy(input$metadataInput)) input$metadataInput$datapath
        )
        if(!is.null(inspection$matrix_message)){
          output$OverallChecks <- renderText(inspection$matrix_message)
        }
        Matrix <- inspection$matrix

        tryCatch(
          expr = {
            withCallingHandlers(
              {
                # to envoke an error if it is not present
                if(is.null(Matrix)) stop("Matrix could not be read")
                dim(Matrix)
                # (needed sometime as DT sometimes seem to handle errors internally and does not throw an error)
                # not the cleanest but works
                output$DataMatrix_VI <- DT::renderDataTable({
                  DT::datatable(data = Matrix)
                })
              },
              warning = function(w) {
                # Optionally handle warnings here
                output$DataMatrix_VI_Info <- renderText({paste0("Warning captured: ",conditionMessage(w))})
              }
            )
          },
          error = function(e) {
            # Handle errors specifically
            output$DataMatrix_VI <- DT::renderDataTable({
              DT::datatable(data = data.frame(Error = "Invalid data for display"))
            })
          }
        )

        output$DataMatrix_VI_Info <- renderText({"Matrix:"})
        if(!is.null(inspection$sample_table)){
          sample_table <- inspection$sample_table
          output$SampleMatrix_VI <- DT::renderDataTable({DT::datatable(data = sample_table)})
          output$SampleMatrix_VI_Info <- renderText({"Sample table:"})
        }
        if(!is.null(inspection$annotation_rows)){
          annotation_rows <- inspection$annotation_rows
          output$EntitieMatrix_VI <- DT::renderDataTable({
            DT::datatable(data = annotation_rows)
          })
          output$EntitieMatrix_VI_Info <- renderText({"Entitie table:"})
        }
        if(!is.null(inspection$error)){
          stop(inspection$error)
        }

        ## Do some checking
        snippetOrangeNo <- "<font color=\"#FFA500\"><b>No</b></font> But if you use any preprocessing other than `None`, those NAs will be removed"
        # a check that failed itself (NA) shows a red No
        check_snippet <- function(passed, no = snippetNo){
          if(isTRUE(passed)) snippetYes else if(is.na(passed)) snippetNo else no
        }
        checks <- inspection$checks
        check0 <- check_snippet(checks$csv_readable)
        check1 <- check_snippet(checks$row_names_match)
        check2 <- check_snippet(checks$sample_names_match)
        check3 <- check_snippet(checks$no_na_matrix, no = snippetOrangeNo)
        check4 <- check_snippet(checks$no_na_sample_table)
        check5 <- check_snippet(checks$no_na_annotation)
        check6 <- check_snippet(checks$syntactic_names)
        check7 <- check_snippet(checks$numeric)

        vi$passed <- inspection$passed
        vi$changed <- FALSE

        if(check0 == snippetNo){
          # add help text
          check0 <- paste0(
            snippetNo,
            "\n\tMost likely: You had a xlsx and exported to csv but your excel is in ",
            "german\n\tand/or you use ',' as separators for decimal positions.\n\t",
            "Fix: change your decimal separator in Excel and re-export!"
          )
        }
        if(check5 == snippetNo){
          # Indicate columns with NA
          colsWithNa <- inspection$annotation_na_columns
          check5 <- paste0("<font color=\"#FFA500\"><b>No</b></font>",
                           "\n\tFollowing columns are *potentially* problematic: ",
                           "\n\t",paste0(colsWithNa, collapse = ", "),
                           "\n\tNa's will be replaced by rownames per default")
        }
        # ensuring we try to rescue names only if crucial checks pass
        if(check6 == snippetNo & check0 == snippetYes & check7 == snippetYes ){
          # add option to user for automatic column name correction
          showModal(modalDialog(
            title = "Column Name Correction",
            helpText("Some or all of your names are not syntactically valid. Do you want to try to correct them within cOmicsArt?"),
            footer = tagList(
              actionButton(ns("yes_correct_column_names"), "Yes"),
              modalButton("No") # Close button by default
            )
          ))
          # the tables to correct if the user clicks "Yes"
          vi$tables <- list(
            matrix = Matrix, sample_table = sample_table, annotation_rows = annotation_rows
          )

          if(check6 == "No"){
            # add help text
            check6 <- paste0(
              snippetNo,
              "\n\tA syntactically valid name consists of letters, numbers,\n\t",
              "the dot or underline characters and starts with a letter.\n\t",
              "Therefore '12345' is invalid, 'ID_12345' is valid.\n\t",
              "Remember to change the Sample ID everywhere (Matrix & Sample Table)."
            )
          }

        }
        if(check7 == snippetNo & check0 == snippetYes){
          # add help text
          propblem_columns <- inspection$non_numeric_columns
          check7 <- paste0(
            snippetNo,
            "\n\tThe data has columns with non-numeric values.",
            "\n\tFollowing ",length(propblem_columns)," columns are potentially problematic: ",
            paste0("\n\t",propblem_columns, collapse = ", ")
          )
        }

        # the checks that column-name correction does not redo
        vi$checks <- list(
          check0 = check0, check3 = check3, check4 = check4, check5 = check5, check7 = check7
        )
        output$OverallChecks <- renderText({
          sprintf(
            CHECK_TEMPLATE_VI,
            check0, check7, check1, check2, check6, check3, check4, check5
          )
        })
        # Reset flag - data has been inspected, no need to clear on next modal open
        data_changed_since_VI(FALSE)
      }
        },
      error = function(e){
        show_toast(
          title = "Data did not passed visual inspection. Please refer to the Documentation to readjust and then reupload your data accordinlgy.",
          type = "error",
          position = "top",
          timerProgressBar = FALSE,
          width = "100%"
        )
      })
    })

    # Handle "Yes" button click
    observeEvent(input$yes_correct_column_names, {
      removeModal() # Close the modal
      corrected <- correct_upload_names(
        vi$tables$matrix, vi$tables$sample_table, vi$tables$annotation_rows
      )
      vi$tables <- corrected[c("matrix", "sample_table", "annotation_rows")]

      # TODO if rownames updated put also to report

      # Write the matrices to csv's to allow reupload for later
      write.csv(corrected$matrix, file = paste0("www/",session$token,"/updatedMatrix.csv"), row.names = T)
      write.csv(corrected$sample_table, file = paste0("www/",session$token,"/updatedSampleTable.csv"), row.names = T)
      write.csv(corrected$annotation_rows, file = paste0("www/",session$token,"/updatedEntitieAnnotation.csv"), row.names = T)
      vi$changed <- TRUE

      # TODO also set flag to update matrixes upon 'upload new data' within app for this round

      showModal(modalDialog(
        title = "Download Updated Data",
          HTML(paste0("You can download your updated data for later reupload (not guaranteed to be correct!).<br>",
                 corrected$changes[["matrix_rows"]],"<br>",
                 corrected$changes[["matrix_columns"]],"<br>",
                 corrected$changes[["sample_table"]],"<br>",
                 corrected$changes[["annotation_rows"]]
                 )
          ),
        footer = tagList(
          downloadButton(
            outputId = ns("DownloadUpdatedMatrix"),
            label = "Updated Matrix"
          ),
          downloadButton(
            outputId = ns("DownloadUpdatedSampleTable"),
            label = "Updated Sample Table"
          ),
          downloadButton(
            outputId = ns("DownloadUpdatedEntitieAnnotation"),
            label = "Updated Entitie Annotation"
          ),
          actionButton(ns("close_modal_VI"), "Close") # Custom close button
        )
      ))
    })

    # Handle data download
    output$DownloadUpdatedMatrix <- downloadHandler(
      filename = function() {
        paste0("updated_dataMatrix", Sys.Date(), ".csv") # Dynamic filename
      },
      content = function(file) {
        write.csv(vi$tables$matrix, file, row.names = TRUE) # Save the reactive data with row names
      }
    )
    output$DownloadUpdatedSampleTable <- downloadHandler(
      filename = function() {
        paste0("updated_SampleTable", Sys.Date(), ".csv") # Dynamic filename
      },
      content = function(file) {
        write.csv(vi$tables$sample_table, file, row.names = TRUE) # Save the reactive data with row names
      }
    )
    output$DownloadUpdatedEntitieAnnotation <- downloadHandler(
      filename = function() {
        paste0("updated_EntitieAnno", Sys.Date(), ".csv") # Dynamic filename
      },
      content = function(file) {
        write.csv(vi$tables$annotation_rows, file, row.names = TRUE) # Save the reactive data with row names
      }
    )
    observeEvent(input$close_modal_VI, {
      removeModal()  # Close the modal
      Matrix <- vi$tables$matrix
      sample_table <- vi$tables$sample_table
      annotation_rows <- vi$tables$annotation_rows
      checks <- vi$checks
      # Redo relevant checks
      check1 <- ifelse(all(rownames(Matrix) == rownames(annotation_rows)),snippetYes,snippetNo)
      check2 <- ifelse(all(colnames(Matrix) == rownames(sample_table)),snippetYes,snippetNo)
      check6 <- ifelse(all(colnames(Matrix) == colnames(Matrix)),snippetYes,snippetNo)
      # Add any additional actions you want to perform
      shinyjs::click(id = "inspect_data")
      output$DataMatrix_VI <- DT::renderDataTable({DT::datatable(data = Matrix)})
      output$SampleMatrix_VI <- DT::renderDataTable({DT::datatable(data = sample_table)})
      output$EntitieMatrix_VI <- DT::renderDataTable({
        DT::datatable(data = annotation_rows)
      })
      showNotification("Check if now you pass all tests")
      output$OverallChecks <- renderText({
        sprintf(
          CHECK_TEMPLATE_VI,
          checks$check0, checks$check7, check1, check2, check6, checks$check3, checks$check4, checks$check5
        )
      })
      if(grepl(snippetYes,checks$check0) &
         grepl(snippetYes,check1) &
         grepl(snippetYes,check2) &
         #grepl(snippetYes,check3) &
         #grepl(snippetYes,check4) & # not crucial
         #grepl(snippetYes,check5) & # not crucial
         grepl(snippetYes,check6) &
         grepl(snippetYes,checks$check7)){
        vi$passed <- TRUE
      } else {
        vi$passed <- FALSE
      }

    })

    # Visual Inspection ends here

    observeEvent(input$usingVIdata,{
      if(vi$passed){
        removeModal()
        # take specification from file_input as only here VI applicable
        omic_type(input$omic_type_file_input)
        start_upload("VI_data")
      }else{
        show_toast(
          title = "Data did not passed visual inspection. Data is not used and needs adjustments before reuploading and usage within cOmicsArt!",
          type = "error",
          position = "top",
          timerProgressBar = FALSE,
          width = "100%"
        )
      }

    })
    observeEvent(input$CloseVI,{
      removeModal()
    })


    observeEvent(input$refresh_file_input, {
      start_upload("file_input")
    })

    observeEvent(input$refresh_precompiled, {
      start_upload("precompiled")
    })

    observeEvent(input$refresh_metadata, {
      start_upload("metadata")
    })

    observeEvent(input$EasyTestForUser,{
      start_upload("testdata")
    })


## Do Upload ----
    observeEvent(commit(),{
      req(data_input_shiny())
      fun_LogIt(session, message = "## Data Selection {.tabset .tabset-fade}")
      fun_LogIt(session, message = "### Info")
      fun_LogIt(session,
        message = paste0("**DataInput** - Uploaded Omic Type: ", omic_type())
      )
      if(!upload_inputs_complete(uploaded_from(), upload_files())){
        output$debug <- renderText("The Upload has failed, or you haven't uploaded anything yet")
      } else if (uploaded_from() == "testdata"){
        show_toast(
          title = paste(omic_type(),"Example Upload"),
          text = paste(omic_type(),"- Example upload was successful"),
          position = "top",
          timer = 1500,
          timerProgressBar = T
        )
        output$debug <- renderText({"The Test Data Set was used"})
      } else {
        show_toast(
          title = paste(omic_type(),"Data Upload"),
          text = paste(omic_type(),"- data upload was successful"),
          position = "top",
          timer = 1500,
          timerProgressBar = T
        )
        output$debug <- renderText({
          paste0(
          "<font color=\"#00851d\"><b>Upload successful</b></font>",
          "<br>",ifelse(uploaded_from()=="file_input", paste0("The following data files are uploaded to cOmicsArt:",
                                                             "<br>Data Matrix: ", input$data_matrix1$name,
                                                             "<br>Sample Annotation: ", input$data_sample_anno1$name,
                                                             "<br>Entitie Annotation: ", input$data_row_anno1$name),""),
          "<br>",ifelse(uploaded_from()=="metadata", paste0("The following data files are uploaded to cOmicsArt:",
                                                             "<br>Data Matrix: ", input$data_matrix_metadata$name,
                                                             "<br>Sample Annotation: ", input$metadataInput$name,
                                                             "<br>Entitie Annotation: ", input$data_row_anno_metadata$name),""),
          "<br>",ifelse(uploaded_from()=="precompiled", paste0("The following data files are uploaded to cOmicsArt:",
                                                             "<br>Precompiled Data: ", input$data_preDone$name),"")
          )
        })
        if(isTruthy(input$data_preDone)){
          # precomplied set used
          fun_LogIt(session, message = paste0(
            "**DataInput** - The used data was precompiled. Filename: \n\t",
            input$data_preDone$name
          ))
        } else if(uploaded_from() == "VI_data"){
          fun_LogIt(session, message = paste0(
            "**DataInput** - The used data was uploaded for visual inspection and changed within.")
          )
          }else {
          if(uploaded_from() == "metadata"){
            used_files <- list(input$data_matrix_metadata, input$metadataInput, input$data_row_anno_metadata)
          } else {
            used_files <- list(input$data_matrix1, input$data_sample_anno1, input$data_row_anno1)
          }
          fun_LogIt(session, message = paste0(
            "The following data was used: \n\t",
            used_files[[1]]$name,"\n\t",
            used_files[[2]]$name,"\n\t",
            used_files[[3]]$name
          ))

          fun_LogIt(session, message = paste0(
            "**DataInput** - The raw data dimensions are: ",
            paste0(dim(raw()),collapse = ", ")
          ))

        }
      }
    })

## create data object ----
    data_input_shiny <- eventReactive(commit(),{
      uploaded_where <- isolate(uploaded_from())
      if(uploaded_where == "VI_data") uploaded_where <- "file_input"
      omic_type(input[[paste0("omic_type_", uploaded_where)]])
      # Add check if the data upload fails due to no supply of files
      if(!upload_inputs_complete(uploaded_from(), upload_files())){
        # if it is not evaluted to TRUE
        output$debug <- renderText({
          "<font color=\"#FF0000\"><b>Upload failed, please check your input - Did you specify all files?.</b></font>"
        })
        req(FALSE)
      }
      # upload depending on where the button was clicked
      upload <- read_upload(
        source = uploaded_from(),
        omic_type = omic_type(),
        paths = list(
          matrix = input$data_matrix1$datapath,
          sample_table = input$data_sample_anno1$datapath,
          row_annotation = input$data_row_anno1$datapath,
          metadata_matrix = input$data_matrix_metadata$datapath,
          metadata_sheet = input$metadataInput$datapath,
          metadata_row_annotation = input$data_row_anno_metadata$datapath,
          precompiled = input$data_preDone$datapath,
          corrected = list(
            matrix = paste0("www/",session$token,"/updatedMatrix.csv"),
            sample_table = paste0("www/",session$token,"/updatedSampleTable.csv"),
            row_annotation = paste0("www/",session$token,"/updatedEntitieAnnotation.csv")
          )
        ),
        changed_during_vi = vi$changed
      )
      if(uploaded_from() == "testdata"){
        fun_LogIt(session,
          message = paste0("<font color=\"#FF0000\"><b>**Attention** - Test Data set used</b></font>")
        )
      }
      # only the last status message is shown
      if(length(upload$messages) > 0){
        upload_status <- tail(upload$messages, 1)
        output$debug <- renderText({upload_status})
      }
      if(upload$read_failed){
        reset_file_input()
      }
      if(upload$metadata_read_failed){
        reset("metadataInput")
        session$sendCustomMessage(type = "resetValue", message = ns("metadataInput"))
      }
      if(!is.null(upload$error_modal)){
        error_modal(list(message = upload$error_modal))
      }
      if(is.null(upload$se)){
        return(NULL)
      }

      # Make a copy, to leave original data untouched
      dataset_copy(dataset_start_copy(upload$se))
      # edit annotation columns such that if na is present in the row annotation,
      # the na gets replaced by the rowname
      raw(fill_na_row_annotation(upload$se))
      added_gene_anno(FALSE)
      return("DataUploadSuccesful")
    })

    list(
      done = data_input_shiny,
      raw = reactive(raw()),
      dataset_copy = reactive(dataset_copy()),
      annotated = reactive(annotated()),
      cleared = reactive(cleared()),
      source = reactive(uploaded_from()),
      omic_type = reactive(omic_type()),
      organism = reactive(organism()),
      added_gene_anno = reactive(added_gene_anno())
    )
  })
}
