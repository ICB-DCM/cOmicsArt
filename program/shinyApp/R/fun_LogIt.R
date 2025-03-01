## log function
fun_LogIt <- function(
  message = "",
  addPlot = F,
  tableSaved = F,
  Filename = NULL
){
  # sophisticated "Where to place log file"
  file_path <- paste0("www/", session$token)
  LogfileName <- paste0(file_path, "/Report.md")  # DO NOT CHANGE THE NAME!
  line <- message
  workingDir <- getwd()
  if(!is.null(Filename) & !(file.exists(paste0(workingDir,"/",Filename)))){
    warning("The file does not exist!")
    addPlot <- F
    tableSaved <- F
  }
  if(addPlot == T){
    line <- paste0(message, "\n", "![](", workingDir, "/", Filename)
  }

  if(tableSaved == T){
    # TODO: would have to check file extension..
    knitr::kable(line, "markdown") %>% cat(., file = LogfileName,append = T)
  }else{
    line <- paste0(line,"\n")
    if(file.exists(LogfileName)){
      write(line, file = LogfileName, append = TRUE)
      return(NULL)
    } else {
      ### Überschrift mit links zu versionen und textbausteinen (eventuell rein linken)
      write(
        paste0(
          "# ShinyOmics Report (",format(Sys.Date(),'%d/%m/%Y'),")",
          "\n **AppVersion: ",getCurrentVersion(updateDESCRIPTION = F),"** \n"
        ),
        file=LogfileName
      )
      write(line, file = LogfileName, append = TRUE)
      return(NULL)
    }
  }
}
