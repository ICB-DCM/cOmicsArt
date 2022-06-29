## log function
fun_LogIt=function(message="",
                   funMode=F,
                   addPlot=F,
                   tableSaved=F,
                   Filename=NULL,
                   jokes=jokesDF){
  # sophisticated "Where to place log file"
  # how to name it ?
  LogfileName="www/Report.md" # DO NOT CHANGE THE NAME!
  line=message
  workingDir=getwd()
  if(!is.null(Filename) & !(file.exists(paste0(workingDir,"/",Filename)))){
    warning("The file does not exist!")
    addPlot=F
    tableSaved=F
  }
  if(addPlot==T){
    
    # functioning
    # important - to capture the correct name ?! (esspcially if user changes it)
    # probably in Shiny important which order
    line=paste0(message,"\n","![](",workingDir,"/",Filename)
  }
  if(tableSaved==T){
    # state where the file was saved and show header
    line=paste0(message,"\n","The corresponding file was saved and can be found [here](",workingDir,"/",Filename)
    # would have to check file extension.. TO DO
  }
  
  if(funMode==T){
    # extend message with a sampled joke by a certain probability
    # should make a joke database that comes with the shiny
    n=sample(1:10,1)
    if(n==6){ # ==6
      randJoke=sample(1:nrow(jokes),1)
      line=paste0(line,"\t ","<span style='color:orange;'>",jokes[randJoke,"Joke"],"</span>")
    }
   
  }
  
  
  # Add Line break
  line=paste0(line,"\n")
  if(file.exists(LogfileName)){
    write(line,file=LogfileName,append=TRUE)
  }else{
    ### Überschrift mit links zu versionen und textbausteinen (eventuell rein linken)
    # SetUp Hea
    write(paste0("# ShinyOmics Report (",format(Sys.Date(),'%d/%m/%Y'),")"),file=LogfileName)
    write(line,file=LogfileName,append=TRUE)
  }
  
}

#knitr::kable(head(jokes), format = "markdown")

# save files in directory
# [Text](Link)
# Lokale text files


###########################################################################
#
#############################################################################
# oder Pre compiled data
# File Names Loggen
# DImension matrizen

# selection chosen
# dimension (nach start journey)

# Data Selection __ hfjfkdrgiuh
# Abfrage ob, Daten schonmal ubgeloaded -> Warning für App (Warning => button "Are you sure?")

# welche preprocessing => text bausteine (upon click)
# bestimmte omics teile background filtering 
# nach preprocessing dimensions

# teste ob auf server automatische file => directory 

# PCA => save only upon plot
# save all => new button

# => save plot save alle Volcano setting
# bei tabelle " Wurde gepspeichert unter folgendem File" show header (make sure to show the header that this is the)

# save the plot heatmap

# Heatmap
# save gene 



