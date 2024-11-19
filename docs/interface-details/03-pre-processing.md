---
title: "Pre-processing"
layout: default
parent: "Interface Details"
nav_order: 3
---

# Pre-processing

The Pre-processing tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you choose the preprocessing procedure and potential batch effects. 
The options (and steps) of preprocessing are as follows:

---

### Pre-processing Procedure

**Step 1: General Data Cleaning**

- Constant entities across all samples are removed from the dataset.
- Rows with all-zero values are removed to improve data quality.

**Step 2: Data Filtering (Optional)**

- If the selected preprocessing procedure is not `None`, additional filtering steps are 
  applied based on the data type (Transcriptomics or Metabolomics).
- Low-abundance entities are filtered out based on specified criteria.

**Step 3: Data Transformation**

- **No pre-processing**
  - No transformation is applied to the data.
  - Step 2 is skipped.
  - This option is recommended for data that has already been normalized and transformed.

- **Omic-specific filtering of low abundance**
  - No transformation is applied to the data.
  - Data is only filtered based on Step 2.
  - This option is recommended for data that has already been normalized and 
    transformed or if you want to use raw data.

- **DESeq2 pre-processing (including variance stabilising transformation)**
  - For transcriptomics data, DESeq2 is used for normalization and VST transformation.
  - The formula for analysis is determined based on user-specified factors (`additional 
    option`).
  - DESeq2 performs a negative binomial test to estimate the variance and stabilize the data.
  - The variance-stabilized data is then used for downstream analysis.
  - **Notes:** 
    - This option is **only suggested** for transcriptomics data.
    - Internally a `DESeq object` is created, which is used for the downstream 
      analysis. The vst transformation is used for visualizations such as the PCA.
    - The formulas supported are only simple ones for now. For a more complex 
      analysis, we suggest to write your own script.

- **centering to 0 and scaling**
  - The data is centered and scaled.
  - Centering involves subtracting the mean of each entity, and scaling involves dividing by the standard deviation.
  - This procedure ensures that each entity has a mean of 0 and a standard deviation of 1.

- **scaling values to be within 0 and 1**
  - The data is scaled to fit within the range of 0 to 1.
  - Each entity's values are transformed proportionally to ensure a consistent scale.

- **Natural Logarithm (ln):**
  - The natural logarithm of each data point is calculated.
  - This transformation is particularly useful for data that exhibits exponential growth.

- **Logarithm Base 10 (log10):**
  - The base-10 logarithm of each data point is calculated.
  - Special consideration is given to handling zero values to avoid undefined 
    results: If any zero values are present, +1 is added to all values before applying
    the logarithm.
    
- **Logarithm Base 2 (log2):**
  - The base-2 logarithm of each data point is calculated.
  - Special consideration is given to handling zero values to avoid undefined 
    results: If any zero values are present, +1 is added to all values before applying
    the logarithm.
    
- **Pareto Scaling:**
  - Pareto scaling emphasizes the importance of small values by dividing each data point by the square root of its standard deviation.
  - This method is suitable for datasets with a wide range of values.

---

### Select Batch Effect Column

Choose a batch effect if applicable. Possible choices are taken from the sample 
annotation columns. **Important**: This step is **optional**, as sometimes no batches need 
to be accounted for. Additionally, in the very first run, batch effects might not be 
know, thus using no batch effects and looking at correlations and PCA plots can help 
to identify them. To learn more about batch effects - how they appear and how to 
identify them you can start by checking out [this website ](https://bigomics.ch/blog/the-tricky-problem-of-batch-effects-in-biological-data/)

---

-   **Get Pre-Processing**: Clicking this button will apply the selected pre-processing procedure to the data.

-   **Color the violin plot by**: Choose a variable to color the violin plots. The options are taken from the sample annotation columns.

## Main Panel 💡

The main panel displays the results of the pre-processing. Here are some key points:

-   **General statistics to the input data**: Displays general statistics about the input data, such as dimensions.

-   **Violin Plot**: Shows the count distribution per sample before and after pre-processing.

### Download Options 📂

-   **Save Violin Plot**: You can save the violin plot in different file formats such as PNG, TIFF, and PDF. You also have the option to download the underlying R code and data.

### Other Notes 📌

-   **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
-   **Pre-processing Interpretation**: Observing the pre-processed data can provide insights into how different pre-processing procedures affect the data. Adjusting the pre-processing parameters can help in optimizing the data for downstream analyses.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform significance analysis on your data? → Go to [Significance Analysis](05-significance-analysis.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---
