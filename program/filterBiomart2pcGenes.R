# Mouse Build: GRCm38
# Go here to Ensembl and search for respecitve gtf file 
# should be eventually the one used for calling the genes
# hence this is guessing
biomart=read.table("~/Downloads/Mus_musculus.GRCm38.102.gtf",skip = 13,sep="\t",header = F)
extract_attributes <- function(gtf_attributes, att_of_interest){
  att <- strsplit(gtf_attributes, "; ")
  att <- gsub("\"","",unlist(att))
  if(!is.null(unlist(strsplit(att[grep(att_of_interest, att)], " ")))){
    return( unlist(strsplit(att[grep(att_of_interest, att)], " "))[2])
  }else{
    return(NA)}
}
colnames(biomart)=c("chr","source","type","start","end","score","strand","phase","attributes")
table(biomart$type)
# filter to gene - Clicia wants full annotation potetniallly looking in miRNA
biomart_gene=biomart[biomart$type=="gene",]  # transcripts ? or gene # to have splicing variants
biomart_gene$geneID<- unlist(lapply(biomart_gene$attributes, extract_attributes, "gene_id"))
biomart_gene$gene_type<- unlist(lapply(biomart_gene$attributes, extract_attributes, "gene_biotype"))
biomart_gene$gene_type=gsub(";","",biomart_gene$gene_type)
table(biomart_gene$gene_type)
fre2order=as.data.frame(table(biomart_gene$gene_type))
newOrder=order(fre2order$Freq,decreasing = T)
biomart_gene$gene_type=factor(biomart_gene$gene_type,levels = fre2order$Var1[newOrder])
library(ggplot2)
ggplot(biomart_gene,aes(y=gene_type))+
  geom_bar()+
  theme_bw()+
  theme(axis.text.x=element_text(angle = 90))

#biomart_gene=biomart_gene[biomart_gene$gene_type=="protein_coding",]

##########
# save Bimart protein coding genes as file for faste access
write.csv(biomart_gene,file = "../data/Biomart_GRCm38_GenesOnly.csv")