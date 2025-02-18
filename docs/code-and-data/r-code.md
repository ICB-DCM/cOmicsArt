---
title: R code
layout: default
parent: Code and Data download
nav_order: 2
---

# R code

This page contains an explanation of the R code structure when downloading R code and 
data to reproduce/alter the analysis. We will use the original code of [the volcano plot example](code-and-data/examples.md).

## 1. Package loading and installation

As a first step the code loads the necessary packages for the analysis. If the 
packages is not installed, the code will install it. The code will look like this:

```r
check_and_install_package <- function(package_name) {
  for(package in package_name){
  # Check if the package is installed
  if (!requireNamespace(package, quietly = TRUE)) {
    # If not installed, install the package
    BiocManager::install(package)
  }
  }
}

check_and_install_package("SummarizedExperiment")
library("SummarizedExperiment") #tested with: source Bioconductor, v.1.28.0
check_and_install_package("rstudioapi")
library("rstudioapi") #tested with: source CRAN, v.0.16.0
check_and_install_package("ggplot2")
library("ggplot2") #tested with: source CRAN, v.3.5.1
check_and_install_package("DESeq2")
library("DESeq2") #tested with: source Bioconductor, v.1.38.3
```

With this we ensure that the packages are installed and loaded and the code will run as intended.

## 2. Data loading

The code will then try to detect the directoy of the file and based on that load the 
data. Notice that this works if:
1. You have just unzipped the folder and **did not** move the files to another directory.
2. You kept the **original file names**.
3. You work in **RStudio**.

Adjustments to these might render the code unable to load the data. In this case you 
can manually insert the file paths of the rds object and the utility file. The code
will look like this:

```r
MANUALLY <- FALSE # change to TRUE, if you want to set the paths manually

if(MANUALLY){
  # Adjust the path to the data file
  envList <- readRDS('path/to/Data.rds')
  # Adjust the path to the utils.R file
  source('path/to/utils.R')
  print('Path manually set')
}else{
  # if you get an error try to set paths manually
  # remember to set MANUALLY <- TRUE
  direcoty_of_files <- dirname(rstudioapi::getSourceEditorContext()$path)
  envList <- readRDS(paste0(direcoty_of_files,'/','Data.rds'))
  if('utils.R' %in% list.files(direcoty_of_files)){
    source(file.path(direcoty_of_files,'utils.R'))
  }
  print('Path automatically set')
}

# Set Environment ----
list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if loadedversion present, make it global
if(exists('loadedVersion')){
  assign('loadedVersion',loadedVersion,envir = globalenv())
}
```

## 3. Data Selection

This one is rather straightforward. To adjust it you can just adjst `selected` and 
`samples_selected` to your needs. The code will look like this:

```r
# Data Selection ----
selected <- rownames(rowData(res_tmp$data_original))
samples_selected <- colnames(assay(res_tmp$data_original))

tmp_data_selected <- res_tmp$data_original[selected,samples_selected]
```

## 4. Preprocessing

The preprocessing is set up as used in the application. Adjustments to this part might 
be more intricate, e.g. adjusting the statistical model in DESeq2. The code will look 
like this (here depending on the preprocessing method):

```r
res_tmp$data <- tmp_data_selected[which(rowSums(assay(tmp_data_selected)) > 10),]
dds <- DESeq2::DESeqDataSetFromMatrix(
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
```

Up until this point the code will look the same for all different kinds of analysis 
you can download the code for. The following steps will be highly specific to the 
analysis for which the code was downloaded. If you want to combine multiple analysis 
steps you may copy and paste the these last parts of multiple analyses below the first 4 
steps altogether.

## 5. Analysis

This is the core of the downloaded code. Here the analysis is performed. Adjusting 
these parts is possible with some knowledge of R and the specific analysis. The code 
for e.g. a statistical analysis will look like this:

```r
# get the results
res2plot <- list()

if(par_tmp$preprocessing_procedure == "vst_DESeq"){
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
```

An example how to adjust this, for example changing the comparison method is shown in 
[the volcano plot example](examples.md) in the last step.

## 6. Visualization

After the analysis is done, the results are visualized. There are two visualization 
types in cOmicsArt, using ggplot2 and using pheatmap. These parts can be adjusted 
following their respective documentation. Also, there exists gallerys and other useful resources, where one can screen for what they search for visually and get the particular code snippets:
- [R Gallery](ggplot2 https://r-graph-gallery.com/ggplot2-package.html)
- [Top 50 ggplot visualisaitions](https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html?utm_content=cmp-true)
- [ggplot extensions](https://exts.ggplot2.tidyverse.org/gallery/)
- [Pheatmp function](https://r-charts.com/correlation/pheatmap/?utm_content=cmp-true)

From own experience, we can also recommend LLMs such as ChatGPT to help to adjust the plotting commands. The code for a typical ggplot2 visualization 
will look like this:

```r
# plot volcano plot
contrast <- paste0(par_tmp$SigAna$contrast[[1]], ":", par_tmp$SigAna$contrast[[2]])
data4Volcano <- sig_results[[contrast]]
par_name <- gsub(":","_",contrast)
significance_threshold <- par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_psig_th")]
lfc_threshold <- par_tmp$SigAna[paste0("SignificanceAnalysis-",par_name,"_lfc_th")]
data4Volcano$probename <- rownames(data4Volcano)
data4Volcano$threshold <- ifelse(data4Volcano$padj>significance_threshold,"non-significant","significant")
data4Volcano$threshold_raw <- ifelse(data4Volcano$pvalue>significance_threshold,"non-significant","significant")
data4Volcano$threshold_fc <- ifelse(
  data4Volcano$log2FoldChange>lfc_threshold,
  "up-regulated",
  ifelse(
    data4Volcano$log2FoldChange<(-1*as.numeric(lfc_threshold)),
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


Volcano_plot <- ggplot(
  data4Volcano,
  aes(label=probename)
) +
  geom_point(aes(
    x = log2FoldChange,
    y = -log10(padj),
    colour = combined
  )) +
  geom_hline(
    yintercept = -1*(log10(as.numeric(significance_threshold))),
    color="lightgrey"
    ) +
  geom_vline(
    xintercept = c((-1*as.numeric(lfc_threshold)),as.numeric(lfc_threshold)),
    color="lightgrey"
    ) +
  scale_color_manual(values=colorScheme2, name="") +
  xlab("Log FoldChange") +
  ylab("-log10(p_adj-value)") +
  theme(legend.position = "none") +
  CUSTOM_THEME +
  ggtitle(label="Corrected p-Values")
lapply(ls(pattern='plot'), get)
```

## Additional Information

The code also contains a lot of comments intended to help you understand the code. 
These comments are marked with `#`. Additionally, some upper case variables are used, 
these are constants that are used in subsequent parts of the code. They help in making 
code more readable and easier to adjust. A good example is the `CUSTOM_THEME` variable 
that is used in the ggplot2 visualization. This variable is defined as:

```r
# Setting default options
CUSTOM_THEME <- theme_bw(base_size = 15) + 
  theme(
    axis.title = element_text(size = 15),        # Axis labels
    axis.text = element_text(size = 15),         # Axis tick labels
    legend.text = element_text(size = 15),       # Legend text
    legend.title = element_text(size = 15),      # Legend title
    plot.title = element_text(size = 17, face = 'bold')  # Plot title
  )
```