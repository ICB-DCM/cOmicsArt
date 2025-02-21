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
check_and_install_packages <- function(package_names) {
  # Check if the packages are installed and install them if not
  for(package in package_names){
    # Check if the package is installed
    if (!requireNamespace(package, quietly = TRUE)) {
      # If not installed, install the package
      BiocManager::install(package)
    }
  }
}
# Load all necessary libraries
check_and_install_packages(c('generics', 'rstudioapi', 'SummarizedExperiment', 'ggplot2'))

library(generics)
library(rstudioapi)
library(SummarizedExperiment)
library(ggplot2)
```

With this we ensure that the packages are installed and loaded and the code will run as intended.

## 2. Data loading

The code will then try to detect the directoy of the file and based on that load the 
data. Notice that this works if:
1. You have just unzipped the folder and **did not** move the files to another directory.
2. You kept the **original file names**.
3. You work in **RStudio**.

Adjustments to these might render the code unable to load the data. In this case you 
can manually insert the file paths of the rds object, utility files and .csv files. The 
code will look like this:

```r
# --- Load the data and environment ---
# Define the path to the csv- and rds-files. If this fails, set the path manually.
file_path <- rstudioapi::getActiveDocumentContext()$path
file_dir <- dirname(file_path)

# Load the used parameters and utility functions
source(file.path(file_dir, 'util.R'))
envList <- readRDS(file.path(file_dir, 'Data.rds'))
parameters <- envList$par_tmp
data_matrix <- read.csv(file.path(file_dir, 'data_matrix.csv'), row.names = 1)
sample_annotation <- read.csv(file.path(file_dir, 'sample_annotation.csv'), row.names = 1)
row_annotation <- read.csv(file.path(file_dir, 'row_annotation.csv'), row.names = 1)
data_orig <- SummarizedExperiment(
  assays = list(raw = as.matrix(data_matrix)),
  colData = sample_annotation,
  rowData = row_annotation[rownames(data_matrix),,drop=F]
)
```

## 3. Data Selection

This one is rather straightforward. To adjust it you can simply adjust any of the 
defined parameters.

```r
# --- Data Selection ---
# Assign variables from parameter list or default values
selected_samples <- parameters$selected_samples %||% "all"
sample_type <- parameters$sample_type %||% NULL
selected_rows <- parameters$selected_rows %||% "all"
row_type <- parameters$row_type %||% NULL
propensity <- parameters$propensity %||% 1
# tmp_res is used as intermediate as return value is a list
tmp_res <- select_data(
  data = data_orig,
  selected_samples = selected_samples,
  sample_type = sample_type,
  selected_rows = selected_rows,
  row_type = row_type,
  propensity = propensity
)
data <- tmp_res$data
samples_selected <- tmp_res$samples_selected
rows_selected <- tmp_res$rows_selected
```

This code already reveals a structure within this code:
- Any functionality has its own function and will be called with all parameters of 
  this function assigned explicitly.
- Before the call to this function, all parameters will be assigned from the stored 
  parameters as you had set them in the application.
- This way, any changes can easily be made on the parameters and for each function 
  separately.

## 4. Preprocessing

The preprocessing is set up as used in the application. The amount of variables is 
explained by the different choices (e.g. DESeq2 or limma). While this may seem 
overkill at the start, it allows you to even change the preprocessing method without 
having to rewrite the code. The code will look like this:

```r
# --- Preprocessing ---
# Assign variables from parameter list or default values
omic_type <- parameters$omic_type
preprocessing_procedure <- parameters$preprocessing_procedure
# Parameters with default values, some might not be used.
preprocessing_filtering <- parameters$preprocessing_filtering %||% NULL
deseq_factors <- parameters$deseq_factors %||% NULL
filter_threshold <- parameters$filter_threshold %||% 10
filter_threshold_samplewise <- parameters$filter_threshold_samplewise %||% NULL
filter_samplesize <- parameters$filter_samplesize %||% NULL
limma_intercept <- parameters$limma_intercept %||% NULL
limma_formula <- parameters$limma_formula %||% NULL


res_preprocess <- preprocessing(
  data = data,
  omic_type = omic_type,
  preprocessing_procedure = preprocessing_procedure,
  preprocessing_filtering = preprocessing_filtering,
  deseq_factors = deseq_factors,
  filter_threshold = filter_threshold,
  filter_threshold_samplewise = filter_threshold_samplewise,
  filter_samplesize = filter_samplesize,
  limma_intercept = limma_intercept,
  limma_formula = limma_formula
)
data <- res_preprocess$data
```

Up until this point the code will look the same for all different kinds of analysis 
you can download the code for. The following steps will be highly specific to the 
analysis for which the code was downloaded. If you want to combine multiple analysis 
steps you may copy and paste the these last parts of multiple analyses below the first 4 
steps altogether.

## 5. Further Analysis

This is the core of the downloaded code. Here the analysis is performed. Adjusting 
these parts is made easy by the structure of the code. It again assigns all variables 
then performs a function call. The code will look like this:

```r
# Assign variables from parameter list or default values
scale_data <- parameters$PCA$scale_data
sample_types <- parameters$PCA$sample_types
sample_selection <- parameters$PCA$sample_selection

pca_res <- get_pca(
  data = data,
  scale_data = scale_data,
  sample_types = sample_types,
  sample_selection = sample_selection
)
pca <- pca_res$pca
pcaData <- pca_res$pcaData
percentVar <- pca_res$percentVar
```

An example how to adjust this, is shown in 
[the pca plot example](examples.md) in the last step.

## 6. Visualization

After the analysis is done, the results are visualized. This part of the code is not 
outsourced to a function, but directly written in the code, as it is usually the part 
that is most adjusted. Still, the parameters are assigned beforehand, allowing changes 
with relative ease. There are two visualization types in cOmicsArt, using ggplot2 and using ComplexHeatmap. These parts can be adjusted 
following their respective documentation. Also, there exists gallerys and other useful resources, where one can screen for what they search for visually and get the particular code snippets:
- [R Gallery](ggplot2 https://r-graph-gallery.com/ggplot2-package.html)
- [Top 50 ggplot visualisaitions](https://r-statistics.co/Top50-Ggplot2-Visualizations-MasterList-R-Code.html?utm_content=cmp-true)
- [ggplot extensions](https://exts.ggplot2.tidyverse.org/gallery/)
- [Pheatmp function](https://r-charts.com/correlation/pheatmap/?utm_content=cmp-true)

From own experience, we can also recommend LLMs such as ChatGPT to help to adjust the plotting commands. The code for a typical ggplot2 visualization 
will look like this:

```r
# Function plot_pca
# Assign variables from parameter list or default values
x_axis <- parameters$PCA$x_axis
y_axis <- parameters$PCA$y_axis
color_by <- parameters$PCA$color_by
title <- parameters$PCA$title
show_loadings <- parameters$PCA$show_loadings
plot_ellipses <- parameters$PCA$plot_ellipses
entitie_anno <- parameters$PCA$entitie_anno
tooltip_var <- parameters$PCA$tooltip_var


# Plot the PCA plot using the principal components chosen in x_axis and y_axis.
# Parameters:
#   pca: PCA object, generated from the data with prcomp
#   pcaData: data.frame, data with the PCA data
#   percentVar: numeric, percentage of variance explained by each PC
#   x_axis: str, name of the column in pcaData to use as x-axis, "PC1" or other PCs
#   y_axis: str, name of the column in pcaData to use as y-axis, "PC2" or other PCs
#   color_by: str, name of the sample data to group the samples by
#   title: str, title of the plot
#   show_loadings: bool, whether to show the loadings on top of the PCA plot
#   entitie_anno: str, what to name the loadings after, part of the rowData
#   tooltip_var: str, name of the column in pcaData to use as tooltip, Only useful
#     when using ggplotly to make the plot interactive. For this wrap the final plot
#     in ggplotly(final_plot).
# Returns:
#   ggplot object, PCA plot

coloring <- prepare_coloring_pca(pcaData, color_by)
color_theme <- coloring$color_theme
pcaData <- coloring$pcaData
if(!is.null(tooltip_var)){
  adj2colname <- gsub(" ",".",tooltip_var)
  pcaData$chosenAnno <- pcaData[,adj2colname]
} else{
  pcaData$chosenAnno <- pcaData$global_ID
}
# Plotting routine
pca_plot <- ggplot(
    pcaData,
    mapping = aes(
      x = pcaData[,x_axis],
      y = pcaData[,y_axis],
      color = pcaData[,color_by],
      label = global_ID,
      global_ID = global_ID,
      chosenAnno = chosenAnno
    )
  ) +
  geom_point(size = 3) +
  pca_ellipses(plot_ellipses) +
  scale_color_manual(
    values = color_theme,
    name = color_by
  ) +
  xlab(paste0(
    names(percentVar[x_axis]),
    ": ",
    round(percentVar[x_axis] * 100, 1),
    "% variance"
  )) +
  ylab(paste0(
    names(percentVar[y_axis]),
    ": ",
    round(percentVar[y_axis] * 100, 1),
    "% variance"
  )) +
  coord_fixed() +
  CUSTOM_THEME +
  theme(aspect.ratio = 1) +
  ggtitle(title) +
  pca_loadings(pca, x_axis, y_axis, show_loadings, entitie_anno, data)
pca_plot
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

Lastly, the code outsources some functions to a utility file. This file is loaded at 
the beginning via

```r
source(file.path(file_dir, 'util.R'))
```

This was done to keep the code clean and structured. The utility file contains all
functions that are used in the code. As this file is passed within the zip-file, 
advanced users can adjust the functions in this file to their needs.