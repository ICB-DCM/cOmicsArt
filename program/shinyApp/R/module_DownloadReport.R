# Report Download module ----
# ui ----
DownloadReport_ui <- function(id){
  ns <- NS(id)
  tagList(
    actionLink(
      inputId = ns("DownloadReport"),
      label = tagList(icon("download"), "Download Report (as html)"),
      style = "font-size: 18px; font-weight: bold;  color: black;text-decoration: underline;"
    )
  )
}

# server ----
DownloadReport_server<- function(id){
  moduleServer(
    id,
    function(input,output,session){
      observeEvent(input$DownloadReport,{
        file_path <- paste0("/www/", session$token, "/")
        if(file.exists(paste0(".", file_path, "Report.md"))){
          show_toast(
            title = "Generating Report....please wait",
            type = "info",
            position = "top",
            timerProgressBar = TRUE,
            width = "30%"
          )
          rmarkdown::render(
            input = paste0(".", file_path, "Report.md"),
            html_document(toc = TRUE, toc_float = T ,fig_caption = T)
          )
          showModal(modalDialog(
            tags$h4(a(
              href=paste0(session$token, "/Report.html"),
              "Download report",
              download=NA,
              target="_blank"
            )),
            footer=tagList(
              modalButton(label = 'Return')
            )))
        } else {
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
