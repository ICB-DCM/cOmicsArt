## log function
fun_LogIt <- function(
  message = "",
  funMode = F,
  addPlot = F,
  tableSaved = F,
  Filename = NULL,
  jokes = JOKES
){
  # sophisticated "Where to place log file"
  LogfileName <- "www/Report.md"  # DO NOT CHANGE THE NAME!
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
    if(funMode == T){
      # extend message with a sampled joke by a certain probability
      n <- sample(1:10,1)
      if(n == 6){
        randJoke <- sample(1:nrow(jokes),1)
        line <- paste0(
          line,"\t ","<span style='color:orange;'>",jokes[randJoke,"Joke"],"</span>"
        )
      }
    }
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
