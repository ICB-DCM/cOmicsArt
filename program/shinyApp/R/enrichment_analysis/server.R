enrichment_analysis_geneset_server <- function(id, result, scenario){
  moduleServer(
    id,
    function(input,output,session){
      # Enrichment Result Plot
      output$EnrichmentPlot<-renderPlot({clusterProfiler::dotplot(result)})
      # download R Code for further plotting
      output$getR_Code_GO <- downloadHandler(
        filename = function(){
          paste("ShinyOmics_Rcode2Reproduce_", Sys.Date(), ".zip", sep = "")
        },
        content = function(file){
          envList=list(EnrichmentRes=result)
          # assign unique name to result for saving later
          result_name <- paste("EnrichmentRes", id, sep="_")
          names(envList) = result_name

          temp_directory <- file.path(tempdir(), as.integer(Sys.time()))
          dir.create(temp_directory)

          write(getPlotCode(scenario), file.path(temp_directory, "Code.R"))

          saveRDS(envList, file.path(temp_directory, "Data.RDS"))
          zip::zip(
            zipfile = file,
            files = dir(temp_directory),
            root = temp_directory
          )
        },
        contentType = "application/zip"
      )

      # Saving Plot
      output$SavePlot_GO=downloadHandler(
        filename = function() { paste("id",Sys.time(),input$file_ext,sep="_") },

        content = function(file){
          ggsave(
            file,
            plot=clusterProfiler::dotplot(result),
            device = gsub("\\.","",input$file_ext)
          )
          # TODO : on.exit({Logging_GSEA})
        }
      )

      # result table
      output$ResultTable=DT::renderDataTable({DT::datatable(
        {result@result},
        extensions = 'Buttons',
        options = list(
          paging = TRUE,
          searching = TRUE,
          fixedColumns = TRUE,
          autoWidth = TRUE,
          ordering = TRUE,
          dom = 'Bfrtip',
          buttons = c('copy', 'csv', 'excel')
        ),
        class = "display"
      )})

      # download section
      observeEvent(input$only2Report,{
        notificationID<-showNotification("Saving...",duration = 0)
        tmp_filename=paste0(getwd(),"/www/",paste(id,Sys.time(),".png",sep="_"))
        ggsave(tmp_filename,plot=clusterProfiler::dotplot(global_Vars$KEGG_EnrichmentRes_Kegg),device = "png")
        fun_LogIt(message = paste("###", id, "ENRICHMENT", sep=" "))
        fun_LogIt(message = paste("-", id, "Enrichment was performed with a gene set of interest of size: ",length(geneSetChoice_tranlsated)))
        fun_LogIt(message = paste("- Note that ENSEMBL IDs were translated to ENTREZIDs. Original size: ",length(geneSetChoice())))
        fun_LogIt(message = paste("- Chosen Organism (needed for translation): ",input$OrganismChoice))
        # fun_LogIt(message = paste0("**KEGG ENRICHMENT** - The universe of genes was selected to be: ",global_Vars$KEGG_UniverseOfGene, " (",length(global_Vars$KEGG_universeSelected_tranlsated)," genes)"))
        # TODO: discuss with Lea -> global_Vars$KEGG_UniverseOfGene is only defined in case of ORA. Wouldn't that throw an error?
        fun_LogIt(message = paste("- The number of found enriched terms (p.adj <0.05): ",nrow(result@result[result@result$p.adjust<0.05,])))
        # TODO: discuss with Lea, what is that Log for?
        # TODO: discuss with LEA -> On exit logging the same as this one?
        # fun_LogIt(message = paste0("**KEGG ENRICHMENT** - ![KEGG ENRICHMENT](",tmp_filename,")"))
        fun_LogIt(message = paste("- The top 5 terms are the following (sorted by adj. p.val)"))
        fun_LogIt(message = knitr::kable(
          head(result@result[order(result@result$p.adjust,decreasing = T),], 5),
          format = "html"
        ))
        if(isTruthy(input$Notes) & !(isEmpty(input$Notes))){
          fun_LogIt("### Personal Notes:")
          fun_LogIt(message = input$Notes)
        }
        removeNotification(notificationID)
        showNotification("Saved!",type = "message", duration = 1)
      })
    }
  )
}

enrichment_analysis_server <- function(id, result, scenario){

  moduleServer(
    id,
    function(input,output,session){
      enrichment_analysis_geneset_server('GO', result, scenario)
    }
  )
}