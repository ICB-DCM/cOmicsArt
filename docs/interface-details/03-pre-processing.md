---
title: "Pre-processing"
layout: default
parent: "Interface Details"
nav_order: 3
---

# Pre-processing

The Pre-processing tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you have the following options:


- **Pre-Processing Procedures**: You can select from various pre-processing procedures.
  - Options: none, filterOnly, vst_DESeq, simpleCenterScaling, Scaling_0_1, log10, log2, pareto_scaling, ln
  - vst_DESeq also requires the selection of a design formula.

-   **Select Batch Effect Column**: Choose a batch effect if applicable. Possible 
    choices are taken from the sample annotation columns. **Important**: This step is 
    optional, as sometimes no batches need to be accounted for. Additionally, in the 
    very first run, batch effects might not be know, thus using no batch effects and 
    looking at correlations and PCA plots can help to identify them. To learn more about batch effects - how they appear and how to identify them you can start by checking out [this website ](https://bigomics.ch/blog/the-tricky-problem-of-batch-effects-in-biological-data/)

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
