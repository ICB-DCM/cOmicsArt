# Report Download module ----
# ui ----
DownloadReport_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionLink(
      inputId = ns("DownloadReport"),
      label = "Download Report (as html)"
    )
  )
}

# server ----
DownloadReport_server<-function(id){
  moduleServer(
    id,
    function(input,output,session){
      observeEvent(input$DownloadReport,{
        if(file.exists("./www/Report.md")){
          show_toast(
            title = "Generating Report....please wait",
            type = "info",
            position = "top",
            timerProgressBar = TRUE,
            width = "30%"
          )
          rmarkdown::render(
            input = "./www/Report.md",
            html_document(toc = TRUE, toc_float = T ,fig_caption = T)
          )
          showModal(modalDialog(
            tags$h4(a(href="Report.html", "Download report", download=NA, target="_blank")),
            footer=tagList(
              modalButton(label = 'Return')
            )))
        }else{
          warning("No Report File yet! Do something first")
          shinyalert(
            title = "Warning",
            text = "No Report File yet! Do something first",
            type = "warning"
          )
        }
      })
    }
  )
}
