---
title: "Heatmap"
layout: default
parent: Interface Details
nav_order: 7
---

# Heatmap

The Heatmap tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

Within the side panel, you have multiple options, which depend on the selected option within
'Select Entities to show'. Here are the general options:

- **Use batch corrected data?**: Allows you to choose whether to use batch corrected data.
  - Options: Yes, No. Note, this option only shows up at the top if you have done batch correction
  within the [Pre-procesing tab](03-pre-processing.md) tab

- **Select Entities to show**: Allows you to select which entities to show in the heatmap.
  - Options: all, Select based on Annotation, Top K. Below you can find further information. Note, that you will get a warning if you want to visualise more than 100 entries to prevent unintentional long loading times.
  

### Conditional Options for "Top K"

Often, you don't want to visualize the entire set of entities you have but only the top entities, for example, the top 100 entities with the highest LogFoldChange
Hence you need to specifcy the ordering critera and the 'k' (number of entities) you want to visualize. 

- **Order based on**: Select the criterion for ordering the top entities.
  - Options: LogFoldChange, absolute LogFoldChange, LogFoldChange and Significant, absolute LogFoldChange and Significant

- **Choose number of top entities to show**: Select the number of top entities to show.

- **Choose reference of log2 FoldChange**: Select the reference group for log2 fold change.

- **Choose treatment group of log2 FoldChange**: Select the treatment group for log2 fold change.

- **adj. p-value threshold**: Set the adjusted p-value threshold. Only entities with an adjusted p-value below this threshold will be considered. Hence, if your `k` is greater than the number of significant entities, you will get less than `k` entities.

### Conditional Options for "Select based on Annotation"

- **Choose the variable to select the rows after**: Select the variable to use for row selection. Displayed here are all options (specifically column names) you provided within the row annotation.

- **Which entities to use?**: Select the specific entities to include in the heatmap. The default option is `all`. Note that you can choose multiple items.

### Action Button

- **Get Heatmap**: Clicking this button will generate the heatmap in the main panel based on the selected parameters.

### Aesthetics Options

- **Choose the variable to color the samples after**: Select the variable for coloring the samples. The coloring options are populated based on the sample annotation provided initially. Often, you want to examine whether the given sample labels coincide with the unsupervised clustering of the heatmap.

- **Choose the variable to color the rows after**: Select the variable for coloring the rows. The coloring options are populated based on the row annotation provided initially. This can help identify patterns within the heatmap that are similar to the coloring of the samples.

- **Row/Column Clustering?**: Enable or disable row/column clustering.

- **row-wise scaling?**: Enable or disable row-wise scaling - note that this is z-scaling. Useful to see changes 
  within one row more clearly.

## Main Panel 💡

The main panel displays the heatmap and additional options. Here are some key points:

- **Heatmap Plot**: Displays the generated heatmap.

- **Choose the label of rows**: Select the label for rows to be shown in the plot.

- **Threshold upon which explicit labels are shown**: Set the threshold for showing 
  explicit row labels. If more than the specified number of rows are shown, the labels 
  will be hidden.

- **Save genes shown in Heatmap for OA with Enrichment Analysis tab**: Sends the list of genes displayed in the 
  heatmap for further usage in the [Enrichment Analysis](09-enrichment-analysis.md). Hence, if one wants to send a specific set to the over-represententation analysis (OA) tab, this is the way to go.

- **Download Options**: The visualization can be downloaded directly in common formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R code and data. For more information, check out [Interface Details](../interface-details.md).

- **Notes**: Add personal notes regarding the heatmap.

### Other Notes 📌

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
- **Interpretation**: The heatmap can provide insights into the relationships between different entities based on the selected annotations and clustering options. Adjusting the selection and aesthetic parameters can help in identifying the most relevant patterns.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Discover the pre-processing options available? → Go to [Pre-processing](03-pre-processing.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform significance analysis on your data? → Go to [Significance Analysis](05-significance-analysis.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---
