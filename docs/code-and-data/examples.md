---
title: Examples
layout: default
parent: Code and Data download
nav_order: 3
---

# Examples

Here are some examples of how to use the data and code provided in this repository, 
also highlighting how to easily adjust the code to alter the workflow.

## Example 1: Volcano Plots

In this example, we will create a volcano plot using the example dataset.

### Recreate Plot in App and Download Code

To recreate this example **within** cOmicsArt, use the following steps:

1. Start the Application (locally or [online](https://shiny.iaas.uni-bonn.de/cOmicsArt/))
2. In the `Data Selection`, use the `Testdata`
3. We want to use all the data, so we will not filter the data, directly clicking `"Start 
   the Journey"`
4. Select `DESeq2` as the pre-processing method with `condition` as the main factor
5. In the `Significance Analysis`, run the significance analysis for `trt:untrt`, Significance 
   level: `0.05` and test-correction: `Benjamini-Hochberg`
6. Select now the `trt:untrt` tab, in the the `Volcano` tab
7. Download the data and code by clicking on `Get underlying R code and data` under 
   `Volcanot plot padj`

Anything not mentioned here can be left as default. Below you can find a slide show of 
the steps to follow:

<!-- Container for the slideshow -->
<div style="display: flex; align-items: center; justify-content: center;">
  <!-- Left arrow -->
  <span id="prev" style="font-size: 2em; cursor: pointer; margin-right: 10px;">&#8592;</span>

  <!-- Image element -->
  <img id="slideshow" src="/cOmicsArt/assets/images/" width="600px">

  <!-- Right arrow -->
  <span id="next" style="font-size: 2em; cursor: pointer; margin-left: 10px;">&#8594;</span>
</div>

<!-- Subtitle element -->
<p id="subtitle" style="text-align: center;">Test 1</p>

<script>
var images = [
    {src: "/cOmicsArt/assets/images/cOmicsCat.png", subtitle: "Test 1"},
    {src: "/cOmicsArt/assets/images/cOmicsGiraffe.png", subtitle: "Test 2"}
];
var currentIndex = 0;

function updateSlideshow(index) {
    document.getElementById("slideshow").src = images[index].src;
    document.getElementById("subtitle").textContent = images[index].subtitle;
}

document.getElementById("prev").onclick = function() {
    currentIndex = (currentIndex - 1 + images.length) % images.length;
    updateSlideshow(currentIndex);
};

document.getElementById("next").onclick = function() {
    currentIndex = (currentIndex + 1) % images.length;
    updateSlideshow(currentIndex);
};
</script>

### Downloaded R Code

If successful, you should have downloaded a folder with the files `Data.RDS`, `utils.
R` and `Code.R`. The `Data.RDS` file contains the data used in the analysis, a more 
detailed description of the data can be found in the in [Data object](data.md). The 
`utils.R` file contains the functions used in the analysis, while the `Code.R` file 
contains the code to reproduce the volcano plot along with the data processing steps.

The R code is shown below:

<div style="width: 90%; height: 50vh; margin: 0 auto; overflow-y: scroll; border: 1px solid #ccc; padding: 10px; box-sizing: border-box;">

```r
# ShinyOmics R Code Download
# Load necassary packages ----
# Note that you do not need to install packages everytime you run the script
# The following will check whether the package is installed and if not, installs it
# We provide the version and repo of the package that was used in the project
# in case the you run into problems try to install the specifc version

# This command is requried only once per R installation. (uncomment if needed)
# install.packages("BiocManager", repos = "https://cloud.r-project.org")
# BiocManager::install(version = "3.16")
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


# Load the data ----
# The following will try to detect the directory of the file and load the data
# this is succesfull if
# - you have just unzipped the folder and did not move the files separately to different locations
# - you kept the original filenames
# - you work in RStudio

# If the requisites are not met you will have to adjust the path to the data file
# and your utils.R file (if present) manually

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

# if you want to combine multiple plots use the `with` notation instead e.g.
# plot <- with(envList, {ggplot(..)+geom_point()})

# Setting default options
CUSTOM_THEME <- theme_bw(base_size = 15) + 
  theme(
    axis.title = element_text(size = 15),        # Axis labels
    axis.text = element_text(size = 15),         # Axis tick labels
    legend.text = element_text(size = 15),       # Legend text
    legend.title = element_text(size = 15),      # Legend title
    plot.title = element_text(size = 17, face = 'bold')  # Plot title
  )

# Happy Adjusting! :)
# Data Selection ----
selected <- rownames(rowData(res_tmp$data_original))
    
samples_selected <- colnames(assay(res_tmp$data_original))
tmp_data_selected <- res_tmp$data_original[selected,samples_selected]
# Data Preprocessing ----
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
</div>

A detailed explanation of the code structure can be found [here](r-code.md). We will 
concentrate on specific parts of the code to understand the workflow and alter it in 
different ways.

### Running the Original Code

Running the unaltered code will produce the following plot:

![The original plot reproduced from the shiny application](/cOmicsArt/assets/images/volcano_plot_original.pdf)

### Altering the Theme

As a first step, we can change the overall theme of the volcano plot. As the volcano 
plot is created using `ggplot2`, we can use the `theme` function to alter the 
appearance by just replacing the original `CUSTOM_THEME` object with the altered one. 
In the code `CUTSOM_THEME` can be found in lines 71-79. Below you can see the 
original and altered theme side by side:

<div style="display: flex; align-items: center; justify-content: center;">

<!-- Original Theme -->
<div style="width: 45%; padding: 10px;">
<h3 style="text-align: center;">Original Theme</h3>
<div style="background-color: #f9f9f9; border: 1px solid #ccc; height: 500px; overflow-y: auto;">
      <pre><code>
# Setting default options
CUSTOM_THEME <- theme_bw(base_size = 15) + 
  theme(
    # Axis labels
    axis.title = element_text(size = 15),
    # Axis tick labels
    axis.text = element_text(size = 15),
    # Legend text
    legend.text = element_text(size = 15),
    # Legend title
    legend.title = element_text(size = 15),
    # Plot title
    plot.title = element_text(size = 17, face = 'bold')
  )
      </code></pre>
    </div>
  </div>

  <!-- Arrow in between -->
  <div style="width: 10%; text-align: center; font-size: 2em;">
    &#8594;
  </div>

  <!-- Altered Theme -->
  <div style="width: 45%; padding: 10px;">
    <h3 style="text-align: center;">Altered Theme</h3>
    <div style="background-color: #f9f9f9; border: 1px solid #ccc; height: 500px; overflow-y: auto;">
      <pre><code>
# Setting altered theme options
CUSTOM_THEME <- theme_dark(base_size = 18) + 
  theme(
    # Axis labels with italic font
    axis.title = element_text(size = 18, face = 'italic'),
    # Axis tick labels
    axis.text = element_text(size = 15),
    # Legend text
    legend.text = element_text(size = 15),
    # Legend title
    legend.title = element_text(size = 15),
    # Plot title with red color
    plot.title = element_text(size = 17, face = 'bold', color = 'red'),
    # Darker panel background
    panel.background = element_rect(fill = 'gray20')
  )
      </code></pre>
    </div>
  </div>

</div>

We can save the plot easily as pdf, png or svg using:

```r
ggsave(
  filename = paste0("Volcano_plot",".pdf"),
  plot = Volcano_plot,
  width = 10,
  height = 5
)
```

The altered volcano plot will look like this:

![Changed Theme with a dark background and red title.](/cOmicsArt/assets/images/volcano_plot_theme.pdf)

A more extensive list of possible manipulations with ggplot2 can we found in [their 
website](https://ggplot2.tidyverse.org).

### Altering Thresholds

In many cases, we have to adjust the pvalue threshold or the log2 fold change 
threshold, deteriming the significance of the data points. In the code, the thresholds 
are defined in lines 194 + 195. They originally use the vales stored from the 
application, but we can insert our own values.

<div style="display: flex; align-items: center; justify-content: center;">

  <!-- Original Thresholds -->
  <div style="width: 45%; padding: 10px;">
    <h4 style="text-align: center;">Original Thresholds</h4>
    <div style="background-color: #f9f9f9; height: 200px; border: 1px solid #ccc; 
overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
significance_threshold <- par_tmp$SigAna[paste0("SignificanceAnalysis-", par_name, "_psig_th")]
lfc_threshold <- par_tmp$SigAna[paste0("SignificanceAnalysis-", par_name, "_lfc_th")]
      </code></pre>
    </div>
  </div>

  <!-- Arrow in between -->
  <div style="width: 10%; text-align: center; font-size: 2em;">
    &#8594;
  </div>

  <!-- Altered Thresholds -->
  <div style="width: 45%; padding: 10px;">
    <h4 style="text-align: center;">Altered Thresholds</h4>
    <div style="background-color: #f9f9f9; height: 200px; border: 1px solid #ccc; 
overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
significance_threshold <- 0.01
lfc_threshold <- 2.5
      </code></pre>
    </div>
  </div>

</div>

After changing the thresholds, the plot will look like this:

![Changed thresholds to 0.01 and 2.5 for pvalue and log2 fold change, respectively.](/cOmicsArt/assets/images/volcano_plot_thresholds.pdf)

### Altering the Comparison

In a troughout analysis, we never only want to compare a single contrast. We can 
adjust the code to change the contrast we compare. In the most straightforward case, 
we will flip the treatment and control groups. We will keep the adjusted thresholds 
and thus only have to take care of the `par_tmp$SigAna$comparisons` and 
`par_tmp$SigAna$contrast`. We overwrite both after loading the RDS file.

<div style="display: flex; align-items: center; justify-content: center;">

  <!-- Original Code -->
  <div style="width: 45%; padding: 10px;">
    <h4 style="text-align: center;">Original Thresholds</h4>
    <div style="background-color: #f9f9f9; height: 275px; border: 1px solid #ccc; 
overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if loadedversion present, make it global
if(exists('loadedVersion')){
  assign('loadedVersion',loadedVersion,envir = globalenv())
}
      </code></pre>
    </div>
  </div>

  <!-- Arrow in between -->
  <div style="width: 10%; text-align: center; font-size: 2em;">
    &#8594;
  </div>

  <!-- Altered Comparison -->
  <div style="width: 45%; padding: 10px;">
    <h4 style="text-align: center;">Altered Thresholds</h4>
    <div style="background-color: #f9f9f9; height: 275px; border: 1px solid #ccc; 
overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
# Set Environment ----
list2env(envList,envir = globalenv()) 
# loads the varaibles directly into global env
# if loadedversion present, make it global
if(exists('loadedVersion')){
  assign('loadedVersion',loadedVersion,envir = globalenv())
}
par_tmp$SigAna$comparisons <- "untrt:trt"
par_tmp$SigAna$contrast <- c("untrt", "trt")
      </code></pre>
    </div>
  </div>

</div>

The altered plot will look like a mirrored version of the threshold adjusted plot:

![Changed comparison to untrt:trt.](/cOmicsArt/assets/images/volcano_plot_comparison.pdf)

### Conclusions

There are of course many more things one can adjust in the plot theme, the threshholds 
and or the comparison. The code is structured in a way that makes it easy to adjust 
the code and rerun the analysis. With this example, we have learned how to adjust the 
code to our needs and individualize the workflow.