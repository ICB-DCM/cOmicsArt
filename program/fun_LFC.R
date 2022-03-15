# get LFC
getLFC=function(data,
                ctrl_samples_idx,
                comparison_samples_idx){
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
  
  eps=10^-15
  log_df=log2(df+eps)
  
  Ctrl_mean=apply(log_df[,ctrl_samples_idx],1,mean)
  Cmp_mean=apply(log_df[,comparison_samples_idx],1,mean)
  
  LFC=Ctrl_mean-Cmp_mean
  
  # Data 2 Plot
  results = cbind(LFC, rawpvalue,p_adj)
  results = as.data.frame(results)
  results$probename <- rownames(results)
  
  return(results)
}