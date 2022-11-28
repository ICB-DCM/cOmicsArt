# Write function to insert current release absed on CHANGE log to DESCRIPTIOn
# Return current version
getCurrentVersion <- function(updateDESCRIPTION = T){
  ChangeLog <- readLines("../../CHANGELOG.md")
  # take the first hit as it is the most recent
  recentSeries <- which(grepl("series$",ChangeLog))[1]
  recentVersion <- ChangeLog[recentSeries+4]
  DESCRIPTION <- readLines("DESCRIPTION")
  DESCRIPTION_new <- gsub("Version:.*$",paste0("Version: ",recentVersion),DESCRIPTION)
  writeLines(DESCRIPTION_new,con ="DESCRIPTION" )
  
  # take the + next line to get version
  return(recentVersion)
}

