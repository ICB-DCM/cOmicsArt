
getPlotCode <- function(numberOfScenario) {
  PreProcessing_Procedure <- par_tmp[[session$token]]$PreProcessing_Procedure
  row_selection <- par_tmp[[session$token]]$row_selection
  col_selection <- par_tmp[[session$token]]$sample_selection
  omic_type <- par_tmp[[session$token]]$omic_type

  #TODO  change all data download to par_tmp and res_tmp
 # Selection ----
  if(any(row_selection == "all")){
    stringSelection <- 'selected <- rownames(rowData(res_tmp$data_original))
    '
  }else{
    if(!(length(row_selection) == 1 & any(row_selection == "High Values+IQR"))){
    stringSelection <- 'selected <- c()
selected <- unique(
  c(selected,rownames(rowData(res_tmp$data_original))[
  which(rowData(res_tmp$data_original)
  [,par_tmp$providedRowAnnotationTypes]%in%par_tmp$row_selection)]
    )
  )
  '
  }
  if(any(row_selection == "High Values+IQR") ){
    stringSelection <- 'toKeep <- filter_rna(
    rna = assay(res_tmp$data_original),
    prop = par_tmp$propensityChoiceUser
    )
    filteredIQR_Expr <- assay(res_tmp$data_original)[toKeep,]
    '
    if(length(row_selection) == 1){
      stringSelection <- paste0(stringSelection,
                                'selected <- rownames(filteredIQR_Expr)')
    }else{
      stringSelection <- paste0(stringSelection,
                                'selected <- intersect(
                                selected,
                                rownames(filteredIQR_Expr)
                                )')
    }
  }
  }

  if(any(col_selection == "all")){
    stringSelection <- paste0(stringSelection,"\n",
                              'samples_selected <- colnames(assay(res_tmp$data_original))
                              tmp_data_selected <- res_tmp$data_original[selected,samples_selected]
                              ')
  }else{
    stringSelection <- paste0(stringSelection,
                              'samples_selected <- c(
                                samples_selected,
                                rownames(colData(res_tmp$data_original))[which(
                                colData(res_tmp$data_original)[,par_tmp$providedSampleAnnotationTypes] %in% par_tmp$sample_selection
                              )]
                              )
                              tmp_data_selected <- res_tmp$data_original[selected,samples_selected]
                              ')
  }
 # Preprocessing ----

  if(PreProcessing_Procedure != "none"){
    if(PreProcessing_Procedure == "filterOnly"){
      if(omic_type == "Transcriptomics"){
        stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]'
      }
      if(omic_type == "Metabolomics"){
        stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(apply(assay(tmp_data_selected),1,median)!=0),]'
      }
      prequel_stringPreProcessing <- c("")
    }else{
      if(omic_type == "Transcriptomics"){
        prequel_stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]'
      }
      if(omic_type == "Metabolomics"){
        prequel_stringPreProcessing <- 'res_tmp$data <- tmp_data_selected[which(apply(assay(tmp_data_selected),1,median)!=0),]'
      }
    }
    if(PreProcessing_Procedure == "simpleCenterScaling"){
        stringPreProcessing <- 'processedData <- as.data.frame(t(
    scale(
      x = as.data.frame(t(as.data.frame(assay(res_tmp$data)))),
      scale = T,
      center = T
      )
    )
    )
    assay(res_tmp$data) <- as.data.frame(processedData)
    '
      }
    if(PreProcessing_Procedure == "vst_DESeq"){
        stringPreProcessing <- 'dds <- DESeq2::DESeqDataSetFromMatrix(
          countData = assay(res_tmp$data),
          colData = colData(res_tmp$data),
          design = as.formula(par_tmp$DESeq_formula)
        )
      de_seq_result <- DESeq2::DESeq(dds)
      res_tmp$DESeq_obj <- de_seq_result
      dds_vst <- vst(
      object = de_seq_result,
      blind = TRUE
      )
      assay(res_tmp$data) <- as.data.frame(assay(dds_vst))
      '
      }
    if(PreProcessing_Procedure == "Scaling_0_1"){
      stringPreProcessing <- 'processedData <- as.data.frame(t(
      apply(assay(res_tmp$data),1,function(x){
      (x - min(x))/(max(x) - min(x))
      })
      ))
      assay(res_tmp$data) <- as.data.frame(processedData)
      '
    }
    if(PreProcessing_Procedure == "ln"){
      stringPreProcessing <- 'processedData <- as.data.frame(log(
        as.data.frame(assay(res_tmp$data))
      ))
      assay(res_tmp$data) <- as.data.frame(processedData)
      '
    }
    if(PreProcessing_Procedure == "log10"){
      stringPreProcessing <- 'processedData <- as.data.frame(assay(res_tmp$data))
      if(any(processedData==0)){
        processedData <- as.data.frame(log10(
        processedData + 1)
       )
      assay(res_tmp$data) <- as.data.frame(processedData)
      }'
    }
    
    if(PreProcessing_Procedure == "pareto_scaling"){
      stringPreProcessing <- 'processedData <- as.data.frame(assay(res_tmp$data))
      centered <- as.data.frame(t(
        apply(processedData, 1, function(x){x - mean(x)})
      ))
      pareto.matrix <- as.data.frame(t(
        apply(centered, 1, function(x){x/sqrt(sd(x))})
      ))
      
      assay(res_tmp$data) <- as.data.frame(pareto.matrix)
      '
    }
    if(par_tmp[[session$token]]['BatchColumn'] != "NULL" & PreProcessing_Procedure != "vst_DESeq"){
      string_batchCorrection <- 'res_tmp$data_batch_corrected <- res_tmp$data
        assay(res_tmp$data_batch_corrected) <- sva::ComBat(
        dat = assay(res_tmp$data_batch_corrected),
        batch = as.factor(colData(res_tmp$data_batch_corrected)[,par_tmp["BatchColumn"]])
      )
      '
      # copy string to a new one and replace all orccurences of res_tmp$data with res_tmp$data_batch_corrected
      stringPreProcessing_batch <- stringPreProcessing
      stringPreProcessing_batch <- gsub("res_tmp$data","res_tmp$data_batch_corrected",stringPreProcessing_batch)
      string_batchCorrection <- paste0(prequel_stringPreProcessing,"\n", string_batchCorrection)
    } else if (par_tmp[[session$token]]['BatchColumn'] != "NULL" & PreProcessing_Procedure == "vst_DESeq") {
      stringPreProcessing_batch <- stringPreProcessing
      stringPreProcessing_batch <- gsub("res_tmp$data","res_tmp$data_batch_corrected",stringPreProcessing_batch)
      stringPreProcessing_batch <- gsub("par_tmp$DESeq_formula","par_tmp$DESeq_formula_batch",stringPreProcessing_batch)
      string_batchCorrection <- paste0(prequel_stringPreProcessing,"\n", string_batchCorrection)
    } else {
        string_batchCorrection <- ''
    }
    stringPreProcessing <- paste0(prequel_stringPreProcessing,"\n", string_batchCorrection, "\n", stringPreProcessing)
    if (par_tmp[[session$token]]['BatchColumn'] != "NULL") {
      stringPreProcessing <- paste0(
        stringPreProcessing, "\n",
        "# uncomment this line to use batch corrected data\n# res_tmp$data <- res_tmp$data_batch_corrected\n"
      )
    }
  }else{
    stringPreProcessing <- ''
  }
    

## Plot Code ----
  ## PCA ----
  if(numberOfScenario >= 1 & numberOfScenario < 9){
    # Calculate all necessary intermediate data sets
    prequel_stringtosave <- '
    
     pca <- prcomp(
            x = as.data.frame(t(assay(res_tmp$data))),
            center = T,
            scale. = FALSE
          )
    
    pcaData <- data.frame(pca$x,colData(res_tmp$data))
# Annotation (important for plotly)
    if(!any(colnames(pcaData) == "global_ID")){
    pcaData$global_ID <- rownames(pcaData)
    }
    if(!is.null(par_tmp$PCA$PCA_anno_tooltip)){
    adj2colname <- gsub(" ",".",par_tmp$PCA$PCA_anno_tooltip)
    pcaData$chosenAnno <- pcaData[,adj2colname]
    }else{
    pcaData$chosenAnno <- pcaData$global_ID
  }
# Scree Plot calculations
var_explained_df <- data.frame(
    PC = paste0("PC",1:ncol(pca$x)),
    var_explained = (pca$sdev)^2/sum((pca$sdev)^2)
)
var_explained_df$Var <- paste0(round(var_explained_df$var_explained,4)*100,"%")
var_explained_df$PC <- factor(var_explained_df$PC,levels = paste0("PC",1:ncol(pca$x)))
percentVar <- round(100 * var_explained_df$var_explained, digits = 1)
names(percentVar)<- var_explained_df$PC

# Loadings calculations
LoadingsDF <- data.frame(
  entitie = rownames(pca$rotation),
  Loading = pca$rotation[,par_tmp$PCA$x_axis_selection]
)
LoadingsDF <- LoadingsDF[order(LoadingsDF$Loading,decreasing = T),]
LoadingsDF <- rbind(
LoadingsDF[nrow(LoadingsDF):(nrow(LoadingsDF) - par_tmp$PCA$bottomSlider),],
  LoadingsDF[par_tmp$PCA$topSlider:1,]
)
LoadingsDF$entitie <- factor(LoadingsDF$entitie,levels = rownames(LoadingsDF))
if(!is.null(par_tmp$PCA$EntitieAnno_Loadings)){
  LoadingsDF$entitie=factor(
  make.unique(as.character(rowData(res_tmp$data)[rownames(LoadingsDF),par_tmp$PCA$EntitieAnno_Loadings])),
  levels = make.unique(as.character(rowData(res_tmp$data)[rownames(LoadingsDF),par_tmp$PCA$EntitieAnno_Loadings]))
  )
}

if(is.null(par_tmp$PCA$nPCAs_to_look_at)){
  df_loadings <- data.frame(
  entity = row.names(pca$rotation),
  pca$rotation[, 1:2]
  )
}else{
  df_loadings <- data.frame(
    entity = row.names(pca$rotation),
    pca$rotation[, 1:par_tmp$PCA$nPCAs_to_look_at]
  )
}

df_loadings_filtered <- as.matrix(df_loadings[,-1]) >= abs(par_tmp$PCA$filterValue)
entitiesToInclude <- apply(df_loadings_filtered, 1, any)

df_loadings <- df_loadings[entitiesToInclude,] %>%
  tidyr::gather(key = "PC", value = "loading", -entity)

if(!is.null(par_tmp$PCA$EntitieAnno_Loadings_matrix)){
  df_loadings$chosenAnno <- factor(
    make.unique(as.character(rowData(res_tmp$data)[unique(df_loadings$entity),par_tmp$PCA$EntitieAnno_Loadings_matrix])),
    levels = make.unique(as.character(rowData(res_tmp$data)[unique(df_loadings$entity),par_tmp$PCA$EntitieAnno_Loadings_matrix]))
  )
}else{
  df_loadings$chosenAnno <- df_loadings$entity
}
'
    
    if (numberOfScenario == 1) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = par_tmp$PCA$coloring_options,values=par_tmp$PCA$colorTheme)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 2) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                         y = pcaData[,par_tmp$PCA$y_axis_selection],
                                         color=pcaData[,par_tmp$PCA$coloring_options],
                                         label=global_ID,
                                         global_ID=global_ID,
                                         chosenAnno=chosenAnno)) +
           geom_point(size =3)+
           scale_color_discrete(name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 3) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=par_tmp$PCA$colorTheme,
                       name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)'
    }
    if (numberOfScenario == 4) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(name = par_tmp$PCA$coloring_options,values=par_tmp$PCA$colorTheme)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
      
      
    }
    if (numberOfScenario == 5) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$PCA$x_axis_selection],
                                               y = pcaData[,par_tmp$PCA$PCA$y_axis_selection],
                                               color=pcaData[,par_tmp$PCA$PCA$coloring_options],
                                               label=global_ID,
                                               global_ID=global_ID,
                                               chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_discrete(name = par_tmp$PCA$coloring_options)+
    xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
    ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
    coord_fixed()+
    theme_classic()+
    theme(aspect.ratio = 1)+
    ggtitle(par_tmp$PCA$customTitle)+
    geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                                                      aes(x=0, y=0, xend=v1, yend=v2),
                                                      arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                                                      #linetype="solid",
                                                      #alpha=0.5,
                                                      color="#ab0521")'
    }
    if (numberOfScenario == 6) {
      stringtosave = 'pca_plot <- ggplot(pcaData, aes(x = pcaData[,par_tmp$PCA$x_axis_selection],
                                  y = pcaData[,par_tmp$PCA$y_axis_selection],
                                  color=pcaData[,par_tmp$PCA$coloring_options],
                                  label=global_ID,
                                  global_ID=global_ID,
                                  chosenAnno=chosenAnno)) +
    geom_point(size =3)+
    scale_color_manual(values=par_tmp$PCA$colorTheme,
                       name = par_tmp$PCA$coloring_options)+
      xlab(paste0(names(percentVar[par_tmp$PCA$x_axis_selection]),": ",percentVar[par_tmp$PCA$x_axis_selection], "% variance")) +
      ylab(paste0(names(percentVar[par_tmp$PCA$y_axis_selection]),": ", percentVar[par_tmp$PCA$y_axis_selection], "% variance")) +
      coord_fixed()+
      theme_classic()+
      theme(aspect.ratio = 1)+
      ggtitle(par_tmp$PCA$customTitle)+geom_segment(data=df_out_r[which(df_out_r$feature!=""),],
                 aes(x=0, y=0, xend=v1, yend=v2),
                 arrow=arrow(type="closed",unit(0.01, "inches"),ends = "both"),
                 #linetype="solid",
                 #alpha=0.5,
                 color="#ab0521")'
    }
### Scree 
    if (numberOfScenario == 7) {
      stringtosave = 'scree_plot=ggplot(var_explained_df,aes(x=PC,y=var_explained, group=1))+
                                  geom_point(size=4,aes(label=Var))+
                                  geom_line()+
                                  ylab("Variance explained")+
                                  theme_bw()+
                                  ggtitle("Scree-Plot for shown PCA")'
    }
### Loadings single
    if (numberOfScenario == 8) {
      stringtosave = 'plotOut=ggplot(LoadingsDF,aes(x = Loading,y = entitie)) +
      geom_col(aes(fill = Loading)) +
      scale_y_discrete(
        breaks = LoadingsDF$entitie,
        labels = stringr::str_wrap(gsub("\\\\.[0-9].*$","",LoadingsDF$entitie),20)) +
      scale_fill_gradient2(low = "#277d6a",mid = "white",high = "orange") +
      ylab(ifelse(is.null(par_tmp$PCA$EntitieAnno_Loadings),"",par_tmp$PCA$EntitieAnno_Loadings)) +
      xlab(paste0("Loadings: ",par_tmp$PCA$x_axis_selection)) +
      theme_bw(base_size = 15)'
    }
### Loadings matrix
  if (numberOfScenario == 8.1) {
    stringtosave =  'LoadingsMatrix_plot <- ggplot(df_loadings,
    aes(x = PC,y = chosenAnno,fill = loading)) +
    geom_raster() +
    scale_fill_gradientn(
    colors = c("#277d6a", "white", "orange"),
    limits = c(-max(df_loadings$loading),max(df_loadings$loading))
    ) +
    labs(x = "PCs", y = "entity", fill = "Loading") +
    theme_bw(base_size = 15)'
  }

  stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave,"\n","lapply(ls(pattern='plot'), get)")
    
  }

  
  ## Heatmap ----
if(numberOfScenario >= 10  & numberOfScenario <= 11){
  prequel_stringtosave <- '
colorTheme <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c","#fdbf6f", "#ff7f00", "#fb9a99", "#e31a1c")
paletteLength <- 25
myColor_fill <- colorRampPalette(c("blue", "white", "firebrick"))(paletteLength)
  
# select and caluculate Heatmap input depending on users input - 
# check par_tmp$Heatmap for selected options or change accrodingly to what you desire
mycolors <- list()
if(length(par_tmp$Heatmap$anno_options) == 1 & !("None" %in% par_tmp$Heatmap$anno_options)){
  if(length(unique(colData(res_tmp$data)[,par_tmp$Heatmap$anno_options])) <= 8){
    names(colorTheme) <- unique(colData(res_tmp$data)[,par_tmp$Heatmap$anno_options])
    colorTheme <- colorTheme[!is.na(names(colorTheme))]
    mycolors[[par_tmp$Heatmap$anno_options]] <- colorTheme
  }
}

        
# Do PreSelection of input to Heatmap to show
if(par_tmp$Heatmap$row_selection_options != "all"){
  # Note entitieSelection and getLFCs is a custom function - source code in utils.R
  data2plot <- entitieSelection(
    res_tmp$data,
    type = par_tmp$Heatmap$row_selection_options,
    TopK2Show = par_tmp$Heatmap$TopK,
    TopKOrder = par_tmp$Heatmap$TopK_order,
    additionalInput_row_anno = par_tmp$Heatmap$anno_options_heatmap,
    additionalInput_row_anno_factor = par_tmp$Heatmap$row_anno_options_heatmap,
    additionalInput_sample_annotation_types = par_tmp$Heatmap$sample_annotation_types_cmp_heatmap,
    additionalInput_ctrl_idx = par_tmp$Heatmap$Groups2Compare_ref_heatmap,
    additionalInput_cmp_idx = par_tmp$Heatmap$Groups2Compare_treat_heatmap,
    psig_threhsold = par_tmp$Heatmap$psig_threhsold_heatmap
  )
} else {
  data2plot <- assay(res_tmp$data)
}
        
doThis_flag <- T
if(is.null(data2plot)){
  print("Nothing is left,e.g. no significant Terms or TopK is used but no inherent order of the data")
  heatmap_plot <- NULL
  doThis_flag <- F
}
'

  if(numberOfScenario == 11){
    stringtosave <- '
annotation_col <- NA
annotation_row <- NA
if(!("None" %in% par_tmp$Heatmap$anno_options)){
  annotation_col <- colData(data)[, par_tmp$Heatmap$anno_options, drop = F]
  annotation_col <- as.data.frame(annotation_col)
}
if(!("None" %in% par_tmp$Heatmap$anno_options)){
  annotation_row <- rowData(data)[, par_tmp$Heatmap$row_anno_options, drop = F]
  annotation_row <- as.data.frame(annotation_row)
}

# Potential Clustering
cluster_rows <- FALSE
cluster_cols <- FALSE
if(par_tmp$Heatmap$cluster_rows){
  cluster_rows <- hclust(dist(data2plot), method = "complete")
}
if(par_tmp$Heatmap$cluster_cols){
  cluster_cols <- hclust(dist(t(data2plot)), method = "complete")
}
heatmap_data <- as.matrix(data2plot)
max_val <- max(abs(heatmap_data), na.rm = T)
if (par_tmp$Heatmap$rowWiseScaled | max_val == Inf | max_val == -Inf) {
    breakings <- NA
} else {
    breakings <- seq(-max_val, max_val, length.out = 101)
}
heatmap_plot <- pheatmap(
    heatmap_data,
    main = "Heatmap",
    show_rownames = ifelse(nrow(data2plot) <= par_tmp$Heatmap$row_label_no, TRUE, FALSE),
    labels_row = rowData(res_tmp$data)[rownames(data2plot), par_tmp$Heatmap$row_label_options],
    show_colnames = TRUE,
    cluster_cols = cluster_cols,
    cluster_rows = cluster_rows,
    scale = ifelse(par_tmp$Heatmap$rowWiseScaled, "row", "none"),
    annotation_col = annotation_col,
    annotation_row = annotation_row,
    silent = F,
    breaks = breakings
)'
  }
stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave)
}



## Single Gene Visualisation ----
if(numberOfScenario %in% c(12,13)){
  if(par_tmp[[session$token]]$SingleEntVis$type_of_data_gene == "preprocessed"){
    prequel_stringtosave <- '#get IDX to data
idx_selected <- which(par_tmp$SingleEntVis$Select_Gene == rowData(res_tmp$data)[,par_tmp$SingleEntVis$Select_GeneAnno])
GeneData <- as.data.frame(t(as.data.frame(assay(res_tmp$data))[idx_selected,,drop=F]))
GeneData$anno <- colData(res_tmp$data)[,par_tmp$SingleEntVis$accross_condition]
if(length(idx_selected)>1){
  # summarise the data
  GeneData_medians <- rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
  GeneData <- GeneData[,ncol(GeneData),drop=F]
  GeneData$rowMedian <- GeneData_medians
  GeneData <- GeneData[,c("rowMedian","anno")]
}
GeneData$anno <- as.factor(GeneData$anno)
    '
  }else if(par_tmp[[session$token]]$SingleEntVis$type_of_data_gene == "raw" ){
    prequel_stringtosave <- '#get IDX to data
idx_selected <- which(par_tmp$SingleEntVis$Select_Gene == rowData(res_tmp$data_original)[,par_tmp$SingleEntVis$Select_GeneAnno])
GeneData <- as.data.frame(t(assay(res_tmp$data_original)[idx_selected,,drop=F]))
GeneData$anno <- colData(res_tmp$data_original)[,par_tmp$SingleEntVis$accross_condition]
# select to selection of processed data
annoToSelect=unique(c(colData(res_tmp$data)[,par_tmp$SingleEntVis$accross_condition]))
GeneData = subset(GeneData, anno %in% annoToSelect)
if(length(idx_selected)>1){
  # summarise the data
  GeneData_medians <- rowMedians(as.matrix(GeneData[,-ncol(GeneData)]))
  GeneData <- GeneData[,ncol(GeneData),drop=F]
  GeneData$rowMedian <- GeneData_medians
  GeneData <- GeneData[,c("rowMedian","anno")]
  }
GeneData$anno <- as.factor(GeneData$anno)
    '
  }

  if (numberOfScenario == 12) {
    stringtosave = '# GeneData now contains the same as res_tmp$SingleEntVis
P_boxplots <- ggplot(GeneData, 
  aes(y=GeneData[,colnames(GeneData)[-ncol(GeneData)]],
      x=anno,
      fill=anno))+
  geom_boxplot()+ # unable if less then 4 samples in all groups to get the same plot as in the App
  geom_point(shape = 21,size=5)+
  scale_fill_brewer(palette="RdBu")+
  xlab(par_tmp$SingleEntVis$Select_Gene)+
  ylab(par_tmp$SingleEntVis$type_of_data_gene)+
  theme_bw()+
  geom_hline(yintercept = mean(GeneData[,colnames(GeneData)[-ncol(GeneData)]]), linetype = 2)+ # Add horizontal line at base mean
  #stat_compare_means(method = "anova")+        # Add global annova p-value
  stat_compare_means(comparisons = par_tmp$SingleEntVis$chooseComparisons_list,
                     method = par_tmp$SingleEntVis$testMethod,
                     label = "p.signif",
                     hide.ns = F)'
  }
  if (numberOfScenario == 13) {
    stringtosave = '# GeneData now contains the same as res_tmp$SingleEntVis
P_boxplots <- ggplot(res_tmp$SingleEntVis, 
  aes(y=res_tmp$SingleEntVis[,colnames(res_tmp$SingleEntVis)[-ncol(res_tmp$SingleEntVis)]],
      x=anno,
      fill=anno))+
  geom_boxplot()+# unable if less then 4 samples in all groups to get the same plot as in the App
  geom_point(shape = 21,size=5)+
  scale_fill_brewer(palette="RdBu")+
  xlab(par_tmp$SingleEntVis$Select_Gene)+
  ylab(par_tmp$SingleEntVis$type_of_data_gene)+
  theme_bw()'
  }
  stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave,"\n","lapply(ls(pattern='boxplots'), get)")
}
 
  ## TODO ensure this remains working with new output from Enrichment, needs a potential update!
  if(numberOfScenario == 14){
    stringtosave = 'KEGG_Plot_GSE=clusterProfiler::dotplot(EnrichmentRes_Kegg,split=".sign") +
                      facet_grid(.~.sign)'
  }
  if(numberOfScenario==15){
    stringtosave = 'KEGG_Plot_ORA=clusterProfiler::dotplot(EnrichmentRes_Kegg)'
  }
  if(numberOfScenario==16){
    stringtosave='GO_Plot=clusterProfiler::dotplot(EnrichmentRes_GO)'
  }
  if(numberOfScenario == 17){
    stringtosave='REACTOME_Plot=clusterProfiler::dotplot(EnrichmentRes_RACTOME)'
  }

## Sample Correlation plot ----
  if(numberOfScenario == 18){
    stringtosave <- 'annotationDF <- colData(res_tmp$data)[,par_tmp$SampleCorr$SampleAnnotationChoice,drop = F]
cormat <- cor(
  x = as.matrix(assay(res_tmp$data)),
  method = par_tmp$SampleCorr$corrMethod
)

SampleCorrelationPlot <- pheatmap(
mat = cormat, #res_tmp$SampleCorr
annotation_row = par_tmp$SampleCorr$annotationDF,
main = par_tmp$SampleCorr$customTitleSampleCorrelation,
annotation_colors = par_tmp$SampleCorr$anno_colors
)'
  }

## Significance Analysis -----
if(numberOfScenario >= 20 & numberOfScenario < 24){
  # Calculate all necessary intermediate data sets
  prequel_stringtosave <- '
  # Test correction list
PADJUST_METHOD <- list(
  "None" = "none",
  "Bonferroni" = "bonferroni",
  "Benjamini-Hochberg" = "BH",
  "Benjamini Yekutieli" = "BY",
  "Holm" = "holm",
  "Hommel" = "hommel",
  "Hochberg" = "hochberg",
  "FDR" = "BH"
)
# get the results
res2plot <- list()

if(par_tmp$PreProcessing_Procedure == "vst_DESeq"){
  dds <- res_tmp$DESeq_obj
  
  # rewind the comparisons again
  newList <- par_tmp$SigAna$comparisons
  contrasts <- vector("list", length(par_tmp$SigAna$comparisons))
  for (i in 1:length(newList)) {
    contrasts[[i]] <- unlist(strsplit(x = par_tmp$SigAna$comparisons[i],split = ":"))
  }

  # get the results for each contrast and put it all in a big results object
  sig_results <- list()
  for (i in 1:length(contrasts)) {
    if(identical(
      list(test_method = "Wald", test_correction = PADJUST_METHOD[[par_tmp$SigAna$test_correction]]),
      par_tmp$SigAna[[par_tmp$SigAna$sample_annotation_types_cmp]][[par_tmp$SigAna$comparisons[i]]]
    )){
      print("Results exists, skipping calculations.")
      sig_results[[par_tmp$SigAna$comparisons[i]]] <- res_tmp$SigAna[[par_tmp$SigAna$sample_annotation_types_cmp]][[par_tmp$SigAna$comparisons[i]]]
      next
    }
    sig_results[[par_tmp$SigAna$comparisons[i]]] <- DESeq2::results(
      dds,
      contrast = c(
        par_tmp$SigAna$sample_annotation_types_cmp,
        contrasts[[i]][1],
        contrasts[[i]][2]
      ),
      pAdjustMethod = PADJUST_METHOD[[par_tmp$SigAna$test_correction]]
    )
    # fill in res_tmp, par_tmp
    res_tmp$SigAna[[par_tmp$SigAna$sample_annotation_types_cmp]][[par_tmp$SigAna$comparisons[i]]] <- sig_results[[par_tmp$SigAna$comparisons[i]]]
    par_tmp$SigAna[[par_tmp$SigAna$sample_annotation_types_cmp]][[par_tmp$SigAna$comparisons[i]]] <- list(
      test_method = "Wald",
      test_correction = PADJUST_METHOD[[par_tmp$SigAna$test_correction]]
    )
  }
  }else{  
    # all other methods require manual testing
    # rewind the comparisons again
    newList <- par_tmp$SigAna$comparisons
    contrasts <- vector("list", length(par_tmp$SigAna$comparisons))
    contrasts_all <- list()
    for (i in 1:length(newList)) {
      contrasts[[i]] <- unlist(strsplit(x = par_tmp$SigAna$comparisons[i],split = ":"))
      contrasts_all <- append(contrasts_all, contrasts[[i]])
    }
    # make all contrasts unique
    contrasts_all <- unique(unlist(contrasts_all))
    # name the contrasts with the comparison names
    names(contrasts) <- par_tmp$SigAna$comparisons
    # get names of columns we want to choose:
    index_comparisons <- which(
      colData(res_tmp$data)[,par_tmp$SigAna$sample_annotation_types_cmp] %in% contrasts_all
    )
    samples_selected <- colData(res_tmp$data)[index_comparisons,]
    # get the data
    data_selected <- as.matrix(assay(res_tmp$data))[,index_comparisons]
    sig_results <- significance_analysis(
      df = as.data.frame(data_selected),
      samples = as.data.frame(samples_selected),
      contrasts = contrasts,
      method = par_tmp$SigAna$test_method,
      correction = PADJUST_METHOD[[par_tmp$SigAna$test_correction]],
      contrast_level = par_tmp$SigAna$sample_annotation_types_cmp
    )
  }
  
  if(any(par_tmp$SigAna$comparisons_to_visualize == "all")){
          # show all comparisons if no more than 4
          if(length(par_tmp$SigAna$comparisons)<5){
            chosenVizSet <- par_tmp$SigAna$comparisons
          }else{

            chosenVizSet <-  par_tmp$SigAna$comparisons[c(1,2)]
            print("Note: Although you choose all to visualize only first 2 comparisons are shown to avoid unwanted computational overhead, 
            as you got more than 4 comparisons. Please choose precisely the comparisons for visualisation.")
          }
        }else{
          chosenVizSet <- par_tmp$SigAna$comparisons_to_visualize
        }
        for (i in 1:length(chosenVizSet)) {
          to_add_tmp <- rownames(
            filter_significant_result(
              result = sig_results[[chosenVizSet[i]]],
              alpha = par_tmp$SigAna$significance_level,
              filter_type = par_tmp$SigAna$sig_to_look_at
            )
          )
          # only add if the result is not empty
          if(length(to_add_tmp) > 0){
            res2plot[[chosenVizSet[i]]] <- to_add_tmp
          }
        }
        # check that you have more than one comparison
        if(length(res2plot) <= 1){
          print("You either have no significant results or only significant results in one comparison.")
          # if current plots to llok at are adjusted pvalues, suggest to look at raw pvalues
            if(par_tmp$SigAna$sig_to_look_at == "Significant"){
                print("You tried to look at adjusted pvalues.\nYou might want to look at raw pvalues (CAUTION!) or change the significance level.")
            }
        }
  
  '

  ### Venn Diagram ----
    if(numberOfScenario == 20){
      stringtosave <- '          
      Venn_plot <- ggvenn::ggvenn(
            res2plot, 
            fill_color=c("#44af69", "#f8333c", "#fcab10", "#2b9eb3"),
            set_name_size = 3
          )
      '
    }

  ### UpSet Plot ----
  if(numberOfScenario == 21){
    stringtosave <- '
              overlap_list <- prepare_upset_plot(res2plot=res2plot)
          Upset_plot <- ComplexUpset::upset(
            overlap_list,
            colnames(overlap_list),
            themes=list(default=theme())
          )
          intersect_names <-  ggplot_build(
            Upset_plot
          )$layout$panel_params[[1]]$x$get_labels()
    
          # if we want to change the highlighting
    if(!is.null(par_tmp$SigAna$intersection_high)){
        querie_names_all <- map_intersects_for_highlight(
          highlights=intersect_names,
          plot=Upset_plot,
          overlap_list=overlap_list
        )
        querie_names <- map_intersects_for_highlight(
          highlights=par_tmp$SigAna$intersection_high,
          plot=Upset_plot,
          overlap_list=overlap_list
        )
        queries <- vector("list", length(querie_names_all))
        for(i_querie in seq_along(intersect_names)){
          if(intersect_names[[i_querie]] %in% par_tmp$SigAna$intersection_high){
            queries[[i_querie]] <- upset_query(
              intersect=colnames(overlap_list)[querie_names_all[[i_querie]]],
              color="red",
              fill="red"
            )
          }else{
            queries[[i_querie]] <- upset_query(
              intersect=colnames(overlap_list)[querie_names_all[[i_querie]]],
              color="#595959",
    fill="#595959"
    )
}
}
Upset_plot <- ComplexUpset::upset(
  overlap_list,
  colnames(overlap_list),
  themes=list(default=theme()),
  queries=queries
)
}
 
    '
  }
  
  ### Volcano ----
  # option of both unnecessary 
  if(numberOfScenario >= 22 & numberOfScenario <= 23 ){
    stringtosave_1 <- '
# plot volcano plot
data4Volcano <- sig_results[[chosenVizSet[i]]]
par_name <- gsub(":","_",chosenVizSet[i])
data4Volcano$probename <- rownames(data4Volcano)
data4Volcano$threshold <- ifelse(data4Volcano$padj>par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")],"non-significant","significant")
data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")],"non-significant","significant")
data4Volcano$threshold_fc <- ifelse(
  data4Volcano$log2FoldChange>par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")],
  "up-regulated",
  ifelse(
    data4Volcano$log2FoldChange<(-1*as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")])),
    "down-regulated", " "
  )
)
data4Volcano$combined <- paste0(data4Volcano$threshold," + ",data4Volcano$threshold_fc)
data4Volcano$combined_raw <- paste0(data4Volcano$threshold_raw," + ",data4Volcano$threshold_fc)
colorScheme2 <- c("#cf0e5bCD", "#0e5bcfCD", "#939596CD","#cf0e5b1A", "#0e5bcf1A", "#9395961A")
names(colorScheme2) <- c(
  "significant + up-regulated", "significant + down-regulated", "significant +  ",
  "non-significant + up-regulated", "non-significant + down-regulated", "non-significant +  "
)

# remove NA values
data4Volcano <- data4Volcano[complete.cases(data4Volcano),]

'
    if(numberOfScenario == 22){
      stringtosave_2 <- 'Volcano_plot <- ggplot(
  data4Volcano,
  aes(label=probename)
) +
  geom_point(aes(
    x = log2FoldChange,
    y = -log10(padj),
    colour = combined
  )) +
  geom_hline(
    yintercept = -1*(log10(as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")]))),
    color="lightgrey"
    ) +
  geom_vline(
    xintercept = c((-1*as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")])),as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")])),
    color="lightgrey"
    ) +
  scale_color_manual(values=colorScheme2, name="") +
  xlab("Log FoldChange") +
  ylab("-log10(p_adj-value)") +
  theme(legend.position = "none") +
  theme_bw()+
  ggtitle(label="Corrected p-Values")'
    }
    if(numberOfScenario == 23){
      stringtosave_2 <- 'Volcano_plot <- ggplot(
  data4Volcano,
  aes(label=probename)
) +
  geom_point(aes(
      x = log2FoldChange,
      y = -log10(pvalue),
      colour = combined_raw)) +
  geom_hline(
      yintercept = -1*(log10(as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")]))),
      color="lightgrey"
  ) +
  geom_vline(
      xintercept = c((-1*as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")])),as.numeric(par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")])),
      color="lightgrey"
  ) +
  scale_color_manual(values=colorScheme2, name="") +
  xlab("Log FoldChange") +
  ylab("-log10(p-value)") +
  ggtitle(label="Uncorrected p-Values")
  '
    }
    stringtosave <- paste0(stringtosave_1,"\n",stringtosave_2)
  }
  
  stringtosave <- paste0(prequel_stringtosave,"\n",stringtosave,"\n","lapply(ls(pattern='plot'), get)")
  
}


## Enrichment Analysis ----
if(numberOfScenario >= 14 & numberOfScenario <= 15){
  if(numberOfScenario == 14){
    stringtosave_1 <- '
    # if you want to upload a different set of genes than uploaded to the App
    # uncomment the following lines
    # you can also manually change the inputs to geneSetChoice

    #   Tmp <- read.csv(par_tmp$Enrichment$UploadedGeneSet$datapath, header = F)
    #   # check take first column as a character vector
    #   geneSetChoice_tmp <- Tmp$V1
    #   # Check if they start with "ENS.."
    #   if(!length(which(grepl("ENS.*",geneSetChoice_tmp) == TRUE)) == length(geneSetChoice_tmp)){
    #     print("wrong data!")
    #     print("Check your input format, should be only gene names ENSMBL-IDs")
    #     geneSetChoice_tmp <- geneSetChoice
    #    }else{
    #      geneSetChoice <- geneSetChoice_tmp
    #    }
    
    geneSetChoice <- par_tmp$Enrichment$tmp_genes

    '
    stringtosave_3 <- '
    if(anno_results$can_start == FALSE){
        tmp_genes <- translate_genes_oa(
        annotation_results = anno_results,
        input = par_tmp$Enrichment,
        geneSetChoice = geneSetChoice,
        geneSet2Enrich = par_tmp$Enrichment$GeneSet2Enrich,
        data = res_tmp$data)
    anno_results$can_start <- TRUE
    geneSetChoice <- tmp_genes
     }

      enrichment_results <- over_representation_analysis(
        par_tmp$Enrichment,
        par_tmp$organism,
        geneSetChoice,
        res_tmp$data,
        par_tmp$Enrichment$enrichments2do,
        par_tmp$Enrichment$test_correction
      )
    
    '
    
  }
  
  if(numberOfScenario == 15){
    stringtosave_1 <- '
    if(par_tmp$Enrichment$ValueToAttach == "LFC" | par_tmp$Enrichment$ValueToAttach == "LFC_abs"){

        #get LFC
        ctrl_samples_idx <- which(colData(res_tmp$data)[,par_tmp$Enrichment$sample_annotation_types_cmp_GSEA] %in% par_tmp$Enrichment$Groups2Compare_ref_GSEA)
        comparison_samples_idx <- which(colData(res_tmp$data)[,par_tmp$Enrichment$sample_annotation_types_cmp_GSEA] %in% par_tmp$Enrichment$Groups2Compare_treat_GSEA)

            Data2Plot <- getLFCs(
              assays(res_tmp$data)$raw,
              ctrl_samples_idx,
              comparison_samples_idx
            )

            Data2Plot_tmp <- Data2Plot
            if(par_tmp$Enrichment$ValueToAttach == "LFC"){
              geneSetChoice_tmp <- Data2Plot_tmp$LFC
            }
            else if(par_tmp$Enrichment$ValueToAttach == "LFC_abs"){
              geneSetChoice_tmp <- abs(Data2Plot_tmp$LFC)
            }

            if(length(geneSetChoice_tmp) < 1){
              print("Nothing significant!")
              geneSetChoice_tmp <- NULL
            }else{
              names(geneSetChoice_tmp) <- Data2Plot_tmp$probename
            }
    geneSetChoice <- geneSetChoice_tmp
          }
    '
    stringtosave_3 <- '
if(anno_results$can_start == FALSE){
  res_tmp$data <- translate_genes_ea(
    data = res_tmp$data,
    annotation_results = anno_results,
    input = par_tmp$Enrichment
  )
  anno_results$can_start <- TRUE
}
enrichment_results <- gene_set_enrichment(
  par_tmp$organism,
  par_tmp$Enrichment$tmp_genes,
  res_tmp$data,
  par_tmp$Enrichment$enrichments2do,
  par_tmp$Enrichment$test_correction,
  par_tmp$Enrichment$sample_annotation_types_cmp_GSEA,
  par_tmp$Enrichment$Groups2Compare_ref_GSEA,
  par_tmp$Enrichment$Groups2Compare_treat_GSEA,
  par_tmp$Enrichment$ValueToAttach
)
    
'
  }
  
  
  stringtosave_2 <- '# Check whether the necessary annotation is available
anno_results <- check_annotation_enrichment_analysis(res_tmp$data)
res_tmp$data <- anno_results$new_data

if(anno_results$no_ann){
  print("No valid annotation type was detected in your row annotation. Please indicate the type of annotation with which you uploaded your genes.")
  print("Should be one of ENSEMBL, ENTREZID, SYMBOL (was selected within App")
  anno_results$base_annotation <- par_tmp$Enrichment$AnnotationSelection
  anno_results$can_start = FALSE

}
'

  stringtosave_4 <- 
  'plot_list <- list()
for(i in names(enrichment_results)){
  if (is.null(enrichment_results[[i]]) | i == "geneSetChoice"){
    print(i)
    print(enrichment_results[[i]])
    next
  }
  print(enrichment_results[[i]])
  plot_list[[i]] <- clusterProfiler::dotplot(enrichment_results[[i]])+ggtitle(i)
}
'

  stringtosave <- paste0(
    stringtosave_1,"\n",
    stringtosave_2,"\n",
    stringtosave_3,"\n",
    stringtosave_4,"\n",
    "lapply(ls(pattern='plot'), get)"
  )
}
### Overrepresentation ----

###


  if(numberOfScenario == 0){
    stringtosave <- '# No_code_yet'
  }
  
  return(paste0(CODE_DOWNLOAD_PREFACE,
                "\n",
                stringSelection,
                "\n",
                stringPreProcessing,
                "\n",
                stringtosave))
}
