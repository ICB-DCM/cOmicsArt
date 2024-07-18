# get LFC
getLFCs <- function(
  data,
  ctrl_samples_idx,
  comparison_samples_idx,
  completeOutput = FALSE
){
  df <- as.data.frame(data)
  # constant rows result in NA p-values
  ttest_raw <- function(df, grp1, grp2) {
    x <- df[grp1]
    y <- df[grp2]
    x <- as.numeric(x)
    y <- as.numeric(y)
    results <- NULL
    tryCatch({
      results <- t.test(y, x)
      results <- list(p.value = results$p.value, statistic = unname(results$statistic))
    },
      
    error = function(e) {
        results <- list(p.value = NA, statistic = NA)
        }
    )
    
    if(is.null(results)){
      results <- list(p.value = NA, statistic = NA)
    }

    return(unlist(results))
  }
  
  rawpvalue_stat <- apply(df, 1, ttest_raw, grp1 = ctrl_samples_idx, grp2 = comparison_samples_idx)
  
  p_adj <- p.adjust(rawpvalue_stat["p.value",], method = "fdr")
  Ctrl_mean <- apply(df[,ctrl_samples_idx],1,mean)
  Cmp_mean <- apply(df[,comparison_samples_idx],1,mean)
  # check if any of those 0
  # put them to NA =< if they are signficiant but have 0 mean fc cannot be computed but potentially really interesting
  Ctrl_mean[which(Ctrl_mean == 0)] <- NA
  Cmp_mean[which(Cmp_mean == 0)] <- NA
  
  FC <- Cmp_mean/Ctrl_mean
  LFC <- log2(FC)
  
  # Data 2 Plot
  results <- t(rbind(rawpvalue_stat,LFC,p_adj))
  results <- as.data.frame(results)
  results$probename <- rownames(results)
  if(completeOutput){
    # report results table + inital values that where used to calculate (mostly
    # for sainity checks)
    colnames(df)[ctrl_samples_idx] <- paste0(colnames(df)[ctrl_samples_idx],"_ctrl")
    colnames(df)[comparison_samples_idx] <- paste0(colnames(df)[comparison_samples_idx],"_cmp")
    results <- cbind(results,df[rownames(df),])
  }
  return(results)
}
