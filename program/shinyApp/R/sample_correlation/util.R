assign_colors_SampleCorr <- function(annotation_df){
  # more advanced colors
  # Identify how many anno colors it is asked for (max 3 atm)
  # check the levels if more than 8 go for rainbow
  # more divergent palletes
  palletteOrder <- c("Paired","Pastel2","Dark2")
  anno_colors <- list()
  for (i in 1:(ncol(annotation_df))) {
    if (i > 3) {
      break
    }
    if (length(unique(annotation_df[,i])) == 2){
      colors_tmp <- c("navy","orange")
      names(colors_tmp) <- unique(annotation_df[,i])
      anno_colors[[colnames(annotation_df)[i]]] <- colors_tmp
    } else if (length(unique(annotation_df[,i])) <= 8) {
      colors_tmp <- RColorBrewer::brewer.pal(
        n = length(unique(annotation_df[,i])),
        name = palletteOrder[i]
      )
      names(colors_tmp) <- unique(annotation_df[,i])
      anno_colors[[colnames(annotation_df)[i]]] <- colors_tmp
    }
  }
  return(anno_colors)
}