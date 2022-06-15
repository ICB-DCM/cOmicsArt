## log function
fun_LogIt=function(message="Hello",fileName="ShinyOmics.md",funMode=T){
  # sophisticated "Where to place log file"
  # how to name it ?
  line=message
  if(funMode){
    # exten message with a sampled joke
    # should make a joke database that comes with the shiny
    line=paste0(line,"\t *How I met your mother*")
  }
  
  if(file.exists(fileName)){
    write(line,file=fileName,append=TRUE)
  }else{
    ### Überschrift mit links zu versionen und textbausteinen (eventuell rein linken)
    write(line,file=fileName)
  }
  
  # somehow ("at the end") crawl through it an add necassary information
  # e.g. DESeq does this, this package was used, more information here (in brief blahblah)
  # we cannot forseen the end of the application
  # additional button that says populate log? or similar
  # could be automatiacally invoked at each "saving button"
  # question where and how to add ? (all add the end)-> possibility to additional file and merge them?!
  
}

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



