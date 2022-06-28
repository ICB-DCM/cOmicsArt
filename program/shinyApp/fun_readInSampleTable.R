# Read in Excel File
# Search for sample_section save row number
# read in again and skip first row number lines
fun_readInSampleTable=function(dataFileName){
  my_data_tmp <- as.data.frame(read_excel(dataFileName,sheet="Input"))
  RowsToSkip=which(my_data_tmp[,1]%in%"Sample-section")+1
  my_data_tmp <- as.data.frame(read_excel(dataFileName,sheet="Input",skip = RowsToSkip))
  
  # Advance: check if subsample etc are present 
  # for now remove any non complete rows
  my_data_tmp=my_data_tmp[!is.na(my_data_tmp$personal_ID),]
  
  my_data_tmp=my_data_tmp[-(which(my_data_tmp[,1]%in%"Sub-Sample Section"):nrow(my_data_tmp)),]
  
  my_data_tmp=t(my_data_tmp)
  colnames(my_data_tmp)=my_data_tmp[1,]
  my_data_tmp=as.data.frame(my_data_tmp[-1,])
 
  return(my_data_tmp) 
}



# For Future somehow implempent type check, eg if there are numerics!! 
# either by prior info (e.g. weight, days, BMI is numeric) or by going through cols and check wheter to numeric is possible?!