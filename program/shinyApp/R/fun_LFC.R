# get LFC
getLFCs <- function(
  data,
  ctrl_samples_idx,
  comparison_samples_idx,
  completeOutput = FALSE
){
  df <- as.data.frame(data)
  # Todo by @Lea: discuss and finalize how to handle this. constant row are not removed but small noise is added should in here a check if all 0 rows?
  ttest_raw <- function(df, grp1, grp2) {
    x <- df[grp1]
    y <- df[grp2]
    x <- as.numeric(x)
    y <- as.numeric(y)
    results <- t.test(x, y)
    return(results$p.value)
  }
  #remove constant rows
  removedAsConst_1 <- which(apply(df[,ctrl_samples_idx],1,sd) < 1e-6)
  df[removedAsConst_1,ctrl_samples_idx] <- df[removedAsConst_1,ctrl_samples_idx] + t(apply(df[removedAsConst_1,ctrl_samples_idx],1,function(x){
    rnorm(
      n = length(x),
      mean = 0,
      sd=0.0000001
    )}))
  
  removedAsConst_2 <- which(apply(df[,comparison_samples_idx],1,sd) < 1e-6)
  df[removedAsConst_2,comparison_samples_idx] <- df[removedAsConst_2,comparison_samples_idx] + t(apply(df[removedAsConst_2,comparison_samples_idx],1,function(x){
    rnorm(
      n = length(x),
      mean = 0,
      sd=0.0000001
    )}))
  
  
  rawpvalue <- apply(df, 1, ttest_raw, grp1 = ctrl_samples_idx, grp2 = comparison_samples_idx)
  
  p_adj <- p.adjust(rawpvalue, method = "fdr")
  
  Ctrl_mean <- apply(df[,ctrl_samples_idx],1,mean)
  Cmp_mean <- apply(df[,comparison_samples_idx],1,mean)
  
  FC <- Cmp_mean/Ctrl_mean
  
  LFC <- log2(FC)
  
  # Data 2 Plot
  results <- cbind(LFC, rawpvalue,p_adj)
  results <- as.data.frame(results)
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