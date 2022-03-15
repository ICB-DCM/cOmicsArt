# Volcano Function
Volcano_Plot=function(data,
                      ctrl_samples_idx,
                      comparison_samples_idx,
                      p_sig_threshold,
                      LFC_threshold){
  df=as.data.frame(data)
  ttest_raw <- function(df, grp1, grp2) {
    x = df[grp1]
    y = df[grp2]
    x = as.numeric(x)
    y = as.numeric(y)  
    results = t.test(x, y)
    results$p.value
  }
  
  rawpvalue = apply(df, 1, ttest_raw, 
                    grp1 = ctrl_samples_idx, 
                    grp2 = comparison_samples_idx)
  
  p_adj=p.adjust(rawpvalue, method = "fdr") # is this wrong?
  #p_adj=rawpvalue
  
  #eps=10^-12
  log_df=log2(df)

  Ctrl_mean=apply(log_df[,ctrl_samples_idx],1,mean)
  Cmp_mean=apply(log_df[,comparison_samples_idx],1,mean)

  LFC=Ctrl_mean-Cmp_mean
  
  
  
  # Data 2 Plot
  results = cbind(LFC, rawpvalue,p_adj)
  results = as.data.frame(results)
  results$probename <- rownames(results)
  results$threshold = ifelse(results$p_adj>p_sig_threshold,"non-significant","significant")
  results$threshold_fc = ifelse(results$LFC>LFC_threshold|results$LFC<(-LFC_threshold),"change","steady")
  results$combined = paste0(results$threshold,"_",results$threshold_fc)
  colorScheme=c("#cf0e5b","#939596")
  names(colorScheme)=c("significant","non-significant")
  alphaScheme=c(0.8,0.1)
  names(alphaScheme)=c("change","steady")
  
  
  plot=ggplot(results,aes(label=probename)) +
    geom_point(aes(x = LFC, y = -log10(p_adj), colour = threshold,alpha=threshold_fc))+
    geom_hline(yintercept=-log10(p_sig_threshold),color="lightgrey")+
    geom_vline(xintercept = c(-LFC_threshold,LFC_threshold),color="lightgrey")+ 
    scale_color_manual(values=colorScheme, name="")+
    scale_alpha_manual(values=alphaScheme, name="")+
    xlab("Log FoldChange")+
    theme_bw()
  plot
}