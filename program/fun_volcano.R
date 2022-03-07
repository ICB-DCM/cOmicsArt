# Volcano Function
Volcano_Plot=function(data,
                      ctrl_samples_idx,
                      comparison_samples_idx,
                      p_sig_threshold=0.05,
                      LFC_threshold=2){
  df=as.matrix(data)
  ttest_raw <- function(df, grp1, grp2) {
    x = as.numeric(df[grp1])
    y = as.numeric(df[grp2])
    results = t.test(x, y)
    results$p.value
  }
  

  rawpvalue = apply(df, 1, ttest_raw, 
                    grp1 = ctrl_samples_idx, 
                    grp2 = comparison_samples_idx)
  p_adj=p.adjust(rawpvalue,method = "fdr")
  eps=10^-12
  log_df=log2(as.data.frame(df)+eps)
  
  Ctrl_mean=apply(log_df[,ctrl_samples_idx],1,mean)
  Cmp_mean=apply(log_df[,comparison_samples_idx],1,mean)
  
  FC=Ctrl_mean-Cmp_mean
  
  # Data 2 Plot
  results = cbind(FC, rawpvalue,p_adj)
  results = as.data.frame(results)
  results$probename <- rownames(results)
  results$threshold = ifelse(results$p_adj>p_sig_threshold,"non-sig","sig")
  results$threshold_fc = ifelse(results$FC>LFC_threshold|results$FC<(-LFC_threshold),"change","steady")
  results$combined = paste0(results$threshold,"_",results$threshold_fc)
  colorScheme=c("#939596","#939596","#cf0e5b","#338ed4")
  names(colorScheme)=c("non-sig_change","non-sig_steady","sig_change","sig_steady")
  plot=ggplot(results) +
    geom_point(aes(x = FC, y = -log10(p_adj), colour = combined))+
    geom_hline(yintercept=-log10(p_sig_threshold),color="lightgrey")+
    geom_vline(xintercept = c(-LFC_threshold,LFC_threshold),color="lightgrey")+ 
    scale_color_manual(values=colorScheme, name="")+
    theme_bw()
  plot
}