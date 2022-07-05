# get LFC
getLFC=function(data,
                ctrl_samples_idx,
                comparison_samples_idx,
                completeOutput=FALSE){
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
  
  # eps=10^-15
  # log_df=log2(df+eps)
  # 
  # Ctrl_mean=apply(log_df[,ctrl_samples_idx],1,mean)
  # Cmp_mean=apply(log_df[,comparison_samples_idx],1,mean)
  # 
  # LFC=Ctrl_mean-Cmp_mean
  
  Ctrl_mean=apply(df[,ctrl_samples_idx],1,mean)
  Cmp_mean=apply(df[,comparison_samples_idx],1,mean)
  
  FC=Cmp_mean/Ctrl_mean
  
  LFC=log2(FC)
  
  # Data 2 Plot
  results = cbind(LFC, rawpvalue,p_adj)
  results = as.data.frame(results)
  results$probename <- rownames(results)
  
  if(completeOutput){
    # report results table + inital values that where used to calculate (mostly
    # for sainity checks)
    colnames(df)[ctrl_samples_idx]=paste0(colnames(df)[ctrl_samples_idx],"_ctrl")
    colnames(df)[comparison_samples_idx]=paste0(colnames(df)[comparison_samples_idx],"_cmp")
    results=cbind(results,df[rownames(df),])
  }
  
  return(results)
}