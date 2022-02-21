## read in metabolom data
# check out row annotation

rawData=as.data.frame(readxl::read_xlsx("/Volumes/lseep@uni-bonn.de/SFB1454-P14/Clivia/metabolon_melanoma HSD_clivia.XLSX",
                  sheet="(OrigScale) optimized"))
rownames(rawData)=rawData$`BIOCHEMICAL (identifier)`
rawData$`BIOCHEMICAL (identifier)`=NULL
#get annotation data from different sheet
rawData_anno=readxl::read_xlsx("/Volumes/lseep@uni-bonn.de/SFB1454-P14/Clivia/metabolon_melanoma HSD_clivia.XLSX",
                          sheet="ScaledImpData (tissue) metabolo",skip = 8)
rawData_anno_only=as.data.frame(rawData_anno[,1:14])

# assume that the samples are in the same order
sampleAnno=readxl::read_xlsx("/Volumes/lseep@uni-bonn.de/SFB1454-P14/Clivia/metabolon_melanoma HSD_clivia.XLSX",
                             sheet="ScaledImpData (tissue) metabolo",range = "N1:AA8" )
sampleAnno=as.data.frame(t(sampleAnno))
colnames(sampleAnno)=sampleAnno[1,]
sampleAnno=sampleAnno[-1,]

sampleAnno$global_ID=sampleAnno$TREATMENT
sampleAnno$global_ID[grep("NSD",sampleAnno$global_ID)]=paste0(sampleAnno$global_ID[grep("NSD",sampleAnno$global_ID)],"_",1:length(grep("NSD",sampleAnno$global_ID)))
sampleAnno$global_ID[grep("HSD",sampleAnno$global_ID)]=paste0(sampleAnno$global_ID[grep("HSD",sampleAnno$global_ID)],"_",1:length(grep("HSD",sampleAnno$global_ID)))

sampleAnno$SAMPLE_NAME=rownames(sampleAnno)
rownames(sampleAnno)=sampleAnno$global_ID

all(colnames(rawData) == rownames(sampleAnno))
###### do PCA

library(ggplot2)

plotPCA(pca_input = as.data.frame(rawData),
        shape = "TREATMENT",
        sample_table = sampleAnno,
        color="GROUP_NUMBER")

