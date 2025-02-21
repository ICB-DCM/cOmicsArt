---
title: Examples
layout: default
parent: Code and Data download
nav_order: 3
---

# Examples

Here are some examples of how to use the data and code provided in this repository, 
also highlighting how to easily adjust the code to alter the workflow.

## Example 1: PCA Plots

In this example, we will create a pca plot using the example dataset.

### Recreate Plot in App and Download Code

To recreate this example **within** cOmicsArt, use the following steps:

0. Start the Application (locally or [online](https://shiny.iaas.uni-bonn.de/cOmicsArt/))
1. In the `Data Selection`, click the `Testdata`-tab, select `Transcriptomics`, and 
 click `"Upload test data"`
2. We want to use all the data, so we will not filter the data. Hence, directly click 
   `"Go to Preprocessing"`
3. We want to use `DESeq2` as the pre-processing method. Thus choose `Omic-Specific` 
   as the **Processing Type**, verify that `DESeq2` is selected as the **Preprocessing 
   Option**, and click `"Get Pre-Preprocessing"`
4. In the `PCA`-tab, set `Plot Ellispes` to **No** and just click `"Get PCA Plot"`
5. Download the data and code by clicking on `Get underlying R code and data`

Anything not mentioned here can be left as default. Below you can find a slide show of 
the steps to follow:

<!-- Container for the slideshow -->
<div style="display: flex; align-items: center; justify-content: center;">
  <!-- Left arrow -->
  <span id="prev" style="font-size: 2em; cursor: pointer; margin-right: 10px;">&#8592;</span>

  <!-- Image element -->
  <img id="slideshow" src="/cOmicsArt/assets/images/Slideshow1.png" width="600px" style="transition: transform 0.3s ease;">

  <!-- Right arrow -->
  <span id="next" style="font-size: 2em; cursor: pointer; margin-left: 10px;">&#8594;</span>
</div>

<!-- Subtitle element -->
<p id="subtitle" style="text-align: center;">Slideshow</p>

<style>
  /* Enlarge image on hover */
  #slideshow:hover {
    transform: scale(1.5); /* Enlarge the image by 1.5 times */
  }
</style>

<script>
var images = [
    {src: "/cOmicsArt/assets/images/Slideshow1.png", subtitle: "1. Select Testdata, 1.2 Select Transcriptomics, 1.3 Upload test data, 2. Go to Preprocessing"},
    {src: "/cOmicsArt/assets/images/Slideshow2.png", subtitle: "3.1 Select Omic-Specific, 3.2 Select DEseq2, 3.3 Get Preprocessing"},
    {src: "/cOmicsArt/assets/images/Slideshow3.png", subtitle: "4.0 Select PCA tab, 4.1 Get PCA, 5. Download Code"},
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

If successful, you should have downloaded a folder with the following files (after 
unzipping):
- `Code.R` contains the main code to reproduce the plot
- `Data.rds` contains the parameters used in the application
- `util.R` contains additional functions used in the code
- `.csv`-files with the data used in the analysis

As we will change it, the code of `Code.R` is shown below:

<div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">

  <!-- Code block with copy button -->
  <div style="width: 90%; padding: 2px; margin-bottom: 10px;">
    <div style="background-color: #f9f9f9; border: 1px solid #ccc; height: 800px; overflow-y: auto; padding: 10px;">
      <pre><code id="code-block">
# Load all necessary libraries
library(rstudioapi)
library(SummarizedExperiment)
library(ggplot2)
library(stats)
library(generics)
library(dplyr)
library(grid)


# Define Constants necessary for the script
# Custom theme for ggplot2. Any non ggplot is adjusted to closely match this theme.
CUSTOM_THEME <<- theme_bw(base_size = 15) +  theme(
  axis.title = element_text(size = 15),        # Axis labels
  axis.text = element_text(size = 15),         # Axis tick labels
  legend.text = element_text(size = 15),       # Legend text
  legend.title = element_text(size = 15),      # Legend title
  plot.title = element_text(size = 17, face = 'bold')  # Plot title
)

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



# --- Data Selection ---
# Assign variables from parameter list or default values

# Parameters with default values, some might not be used.
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
      </code></pre>
    </div>
  </div>
</div>

A detailed explanation of the code structure can be found [here](r-code.md). We will 
concentrate on specific parts of the code to understand the workflow and alter it in 
different ways.

### Running the Original Code

Running the unaltered code will produce the following plot:

![The original plot reproduced from the shiny application](/cOmicsArt/assets/images/pca_plot_original_example1.png)

### Altering the Theme

As a first step, we can change the overall theme of the volcano plot. As the volcano 
plot is created using `ggplot2`, we can use the `theme` function to alter the 
appearance by just replacing the original `CUSTOM_THEME` object with the altered one. 
In the code `CUSTOM_THEME` can be found in lines 13-19. Below you can see the 
original and altered theme side by side:

<div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">

  <!-- Original Theme -->
  <div style="width: 90%; padding: 10px; margin-bottom: 0px;">
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
  <div style="width: 90%; text-align: center; font-size: 2em; margin-bottom: 20px;">
    &#8595;
  </div>

  <!-- Altered Theme -->
  <div style="width: 90%; padding: 10px;">
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
  filename = paste0("pca_plot",".pdf"),
  plot = pca_plot,
  width = 10,
  height = 5
)
```

The altered pca plot will look like this:

![Changed Theme with a dark background and red title.](/cOmicsArt/assets/images/volcano_plot_theme.pdf)

A more extensive list of possible manipulations with ggplot2 can we found in [their 
website](https://ggplot2.tidyverse.org).

### Changes in Analysis

We can also change values for getting the pca. These, as lines 104-106 show are mainly 
data selection or scaling the data. Thus let us change the `scale_data` Parameter from 
its loaded value to `FALSE`:

<div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">

  <!-- Original Thresholds -->
  <div style="width: 90%; padding: 10px; margin-bottom: 5px;">
    <h4 style="text-align: center;">Original Parameters</h4>
    <div style="background-color: #f9f9f9; height: 150px; border: 1px solid #ccc; overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
scale_data <- parameters$PCA$scale_data
sample_types <- parameters$PCA$sample_types
sample_selection <- parameters$PCA$sample_selection
      </code></pre>
    </div>
  </div>

  <!-- Arrow in between -->
  <div style="width: 90%; text-align: center; font-size: 2em; margin-bottom: 20px;">
    &#8595;
  </div>

  <!-- Altered Thresholds -->
  <div style="width: 90%; padding: 10px;">
    <h4 style="text-align: center;">Altered Scaling</h4>
    <div style="background-color: #f9f9f9; height: 200px; border: 1px solid #ccc; overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
scale_data <- FALSE
sample_types <- parameters$PCA$sample_types
sample_selection <- parameters$PCA$sample_selection
      </code></pre>
    </div>
  </div>

</div>

After changing the thresholds, the plot will look like this:

![Non scaled pca plot.](/cOmicsArt/assets/images/pca_unscaled.png)

### Altering the Plotting Code

We can also change parameters for the plotting. They are gathered in lines 122-129. 
Let us change the `show_loadings` parameter to show us the loadings, and also color 
the data by `cell` instead of `condition`.

<div style="display: flex; flex-direction: column; align-items: center; justify-content: center;">

  <!-- Original Code -->
  <div style="width: 90%; padding: 10px; margin-bottom: 20px;">
    <h4 style="text-align: center;">Original Code</h4>
    <div style="background-color: #f9f9f9; height: 200px; border: 1px solid #ccc; overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
x_axis <- parameters$PCA$x_axis
y_axis <- parameters$PCA$y_axis
color_by <- parameters$PCA$color_by
title <- parameters$PCA$title
show_loadings <- parameters$PCA$show_loadings
plot_ellipses <- parameters$PCA$plot_ellipses
entitie_anno <- parameters$PCA$entitie_anno
tooltip_var <- parameters$PCA$tooltip_var
      </code></pre>
    </div>
  </div>

  <!-- Arrow in between -->
  <div style="width: 90%; text-align: center; font-size: 2em; margin-bottom: 20px;">
    &#8595;
  </div>

  <!-- Altered Comparison -->
  <div style="width: 90%; padding: 10px;">
    <h4 style="text-align: center;">Altered Code</h4>
    <div style="background-color: #f9f9f9; height: 200px; border: 1px solid #ccc; overflow-x: auto; overflow-y: auto; white-space: nowrap;">
      <pre><code>
x_axis <- parameters$PCA$x_axis
y_axis <- parameters$PCA$y_axis
color_by <- "cell"
title <- parameters$PCA$title
show_loadings <- TRUE
plot_ellipses <- parameters$PCA$plot_ellipses
entitie_anno <- parameters$PCA$entitie_anno
tooltip_var <- parameters$PCA$tooltip_var
      </code></pre>
    </div>
  </div>
</div>


The altered plot will look like a mirrored version of the threshold adjusted plot:

![Changed comparison to untrt:trt.](/cOmicsArt/assets/images/pca_loadings_and_cellcolor.png)

### Conclusions

There are of course many more things one can adjust in the plot theme, the threshholds 
and or the comparison. The code is structured in a way that makes it easy to adjust 
the code and rerun the analysis. With this example, we showed how to adjust the 
code to our needs and individualize the workflow.