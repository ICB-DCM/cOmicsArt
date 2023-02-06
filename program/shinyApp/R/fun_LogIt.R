## log function
fun_LogIt <- function(
  message = "",
  funMode = F,
  addPlot = F,
  tableSaved = F,
  Filename = NULL,
  jokes = jokesDF
){
  # sophisticated "Where to place log file"
  # how to name it ?

  LogfileName <- "www/Report.md"  # DO NOT CHANGE THE NAME!
  line <- message
  workingDir <- getwd()
  if(!is.null(Filename) & !(file.exists(paste0(workingDir,"/",Filename)))){
    warning("The file does not exist!")
    addPlot <- F
    tableSaved <- F
  }
  if(addPlot == T){
    # functioning
    # important - to capture the correct name ?! (esspcially if user changes it)
    # probably in Shiny important which order
    line <- paste0(message, "\n", "![](", workingDir, "/", Filename)
  }

  if(tableSaved == T){
    # state where the file was saved and show header
    # line=paste0(message,"\n","The corresponding file was saved and can be found [here](",workingDir,"/",Filename)
    # TODO: would have to check file extension..
    # NO LINE BREAK?!
    print("We are here to save pretty tables")
    knitr::kable(line, "markdown") %>% cat(., file = LogfileName,append = T)
  }else{
    # Add Line break
    if(funMode == T){
      # extend message with a sampled joke by a certain probability
      # should make a joke database that comes with the shiny
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
    }else{
      ### Überschrift mit links zu versionen und textbausteinen (eventuell rein linken)
      # SetUp Hea
      write(
        paste0("# ShinyOmics Report (",format(Sys.Date(),'%d/%m/%Y'),")",
               "\n **AppVersion: ",getCurrentVersion(updateDESCRIPTION = F),"** \n"),
        file=LogfileName
      )
      write(line, file = LogfileName, append = TRUE)
      return(NULL)
    }
  }
}
