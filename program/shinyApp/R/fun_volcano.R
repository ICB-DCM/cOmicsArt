# Volcano Function
Volcano_Plot <- function(
  data,
  ctrl_samples_idx,
  comparison_samples_idx,
  p_sig_threshold,
  LFC_threshold,
  correction_test_method,
  method,
  annotation_add=NULL,
  annoData=NULL,
  alreadyLogged=F
){
  df <- as.data.frame(data)
  if(method == "T-Test"){
    # TODO test for Varianz Homogenität (Levene Test) - 
    # intermediate Lösung könnte sein einfach standard abweichungen der Gruppen anzugeben
    # User hinweisen diese zu untersuchen!
    test_function <- function(...) t.test(..., var.equal = TRUE)
    
  }else if(method == "Welch-Test"){
    test_function <- function(...) t.test(..., var.equal = FALSE)
  }
  else{
    test_function <- wilcox.test
  }
  
  # ttest_raw <- function(df, grp1, grp2) {
  #   x <- df[grp1]
  #   y <- df[grp2]
  #   x <- as.numeric(x)
  #   y <- as.numeric(y)
  #   results <- t.test(x, y)
  #   results$p.value
  # }

  ttest_raw <- function(df, grp1, grp2) {
    x <- df[grp1]
    y <- df[grp2]
    x <- as.numeric(x)
    y <- as.numeric(y)
    tryCatch(
      {
        results <- test_function(x, y)
        return(results$p.value)
      },
      error = function(e) {
        cat(
          "Error in gene ", df["gene"], ":\n  ",
          as.character(e), "  Values are:\n",
          "   x: ", paste(x),
          "\n   y: ", paste(y), "\n",
          "NA will be returned instead. \n",
          sep = ""
        )
        return(NA)
      }
    )
  }
  #remove constant rows for control and comparisons separately
  df <- df[(apply(df[,ctrl_samples_idx], 1, sd) > 0),]
  df <- df[(apply(df[,comparison_samples_idx], 1, sd) > 0),]

  print(paste0("Number of rows removed due to being group-wise constant: ",dim(as.data.frame(data))[1]-dim(df)[1]))
  
  rawpvalue <- apply(
    df, 1, ttest_raw, grp1 = ctrl_samples_idx, grp2 = comparison_samples_idx
  )
  
  p_adj <- p.adjust(rawpvalue, method = PADJUST_METHOD[[correction_test_method]])
  
  Ctrl_mean <- apply(df[,ctrl_samples_idx],1,mean)
  # check whether there are 0's in Ctrl mean
  if(any(Ctrl_mean < 0)){
    warning("NAs will be produced due to impossible division by 0")
  }
  
  Cmp_mean <- apply(df[,comparison_samples_idx], 1, mean)
  
  FC <- Cmp_mean/Ctrl_mean
  
  
  if(alreadyLogged){
    LFC <- FC
  }else{
    LFC <- log2(FC)
  }

  # Data 2 Plot
  results <- cbind(LFC, rawpvalue,p_adj)
  results <- as.data.frame(results)
  results$probename <- rownames(results)
  results$threshold <- ifelse(results$p_adj>p_sig_threshold,"non-significant","significant")
  results$threshold_fc <- ifelse(results$LFC>LFC_threshold|results$LFC<(-LFC_threshold),"change"," ")
  results$combined <- paste0(results$threshold,"_",results$threshold_fc)
  colorScheme <- c("#cf0e5b","#939596")
  names(colorScheme) <- c("significant","non-significant")
  alphaScheme <- c(0.8,0.1)
  names(alphaScheme) <- c("change","steady")
  
  # add annotation data based on user Input (ergo annotation_add)
  results$annotation_add <- annoData[rownames(results),annotation_add]
  
  return(results)
}