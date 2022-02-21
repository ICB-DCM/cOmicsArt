## RNA raw Input to count matrix
sampleAnno=as.data.frame(readxl::read_xlsx(paste0(basePath,"sample overview RNAseq melanoma HSD.xlsx"),skip = 2))
colnames(sampleAnno)[7]="global_ID" #NEEDS TO BE NAMED LIKE THIS
rownames(sampleAnno)=sampleAnno$global_ID

sampleAnno$replicate=gsub(".*_","",sampleAnno$`Stimulation/Treatment`)
sampleAnno$condition=gsub(" .*$","",sampleAnno$`Stimulation/Treatment`)
sampleAnno$Merged=paste0(sampleAnno$condition,"_",sampleAnno$replicate)
sampleAnno$condition=as.factor(sampleAnno$condition)
sampleAnno$Nr.=as.factor(sampleAnno$Nr.)

#### Read in single count files
rnaDataLoc="P2018-055-LEX_RNAseq melanoma HSD/"
allFiles=list.files(paste0(basePath,rnaDataLoc))
countData=list()

for(i in sampleAnno$global_ID){
  individualFile=allFiles[grep(i,allFiles)]
  countData[[i]]=read.table(paste0(basePath,rnaDataLoc,individualFile,"/star_out/",i,"_1.fq.gz/read_counts.txt"))
  colnames(countData[[i]])=c("global_ID",i)
  if(length(countData)>=2){
    countData_df=merge(countData_df,countData[[i]],by="global_ID")
  }else{
    countData_df=countData[[i]]
  }
}

# separate quality info from counts
qualityInfo=countData_df[1:5,]
countData_raw=countData_df[-c(1:5),]
rownames(countData_raw)=countData_raw$global_ID
countData_raw$global_ID=NULL

## Save Count Data as txt as well as sample Anno
write.csv(countData_raw,file = "../data/count_matrix_raw_HighSalt.csv")
write.csv(sampleAnno,file = "../data/sample_anno_HighSalt.csv")

