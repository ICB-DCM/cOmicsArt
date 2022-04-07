filter_rna <- function(rna=rna, prop = 0.5){
  #calculate the maximum of gene expression per each gene (row) and take the top
  maxGE <- apply(rna, 1, max)
  propGEmax <- quantile(maxGE, prop)
  #take the IQR of each gene and take the top genes
  IQRGE <- apply(rna, 1, IQR, na.rm=TRUE)
  propGEIQR <- quantile(IQRGE, prop)
  #selected genes/probes are the intersection of the two previous sets
  filter2 <- (intersect(which(maxGE> propGEmax), which(IQRGE> propGEIQR)))
  return(filter2)
}