---
title: "Heatmap"
layout: default
parent: Interface Details
nav_order: 6
---

# Heatmap

The Heatmap tab is divided into two main sections: the side panel and the main panel.

## Side Panel

In the side panel, you have the following options:

- **Use batch corrected data?**: Allows you to choose whether to use batch corrected data.
  - Options: Yes, No

- **Select Entities to show**: Allows you to select which entities to show in the heatmap.
  - Options: all, Select based on Annotation, Top K

### Conditional Options for "Top K"

- **Order based on**: Select the criterion for ordering the top entities.
  - Options: LogFoldChange, absolute LogFoldChange, LogFoldChange and Significant, absolute LogFoldChange and Significant

- **Choose number of top entities to show**: Select the number of top entities to show.

- **Choose reference of log2 FoldChange**: Select the reference group for log2 fold change.

- **Choose treatment group of log2 FoldChange**: Select the treatment group for log2 fold change.

- **adj. p-value threshold**: Set the adjusted p-value threshold.

### Conditional Options for "Select based on Annotation"

- **Choose the variable to select the rows after**: Select the variable to use for row selection.

- **Which entities to use?**: Select the specific entities to include in the heatmap.

### Action Button

- **Get Heatmap**: Clicking this button will generate the heatmap in the main panel based on the selected parameters.

### Aesthetics Options

- **Choose the variable to color the samples after**: Select the variable for coloring the samples.

- **Choose the variable to color the rows after**: Select the variable for coloring the rows.

- **Choose the label of rows**: Select the label for rows to be shown in the plot.

- **Row/Column Clustering?**: Enable or disable row/column clustering.

- **row-wise scaling?**: Enable or disable row-wise scaling. Useful to see changes 
  within one row more clearly.

## Main Panel

The main panel displays the heatmap and additional options. Here are some key points:

- **Heatmap Plot**: Displays the generated heatmap.

- **Threshold upon which explicit labels are shown**: Set the threshold for showing 
  explicit row labels. If more than the specified number of rows are shown, the labels 
  will be hidden.

- **Save genes shown in Heatmap as list**: Save the list of genes displayed in the 
  heatmap for further usage in the [Enrichment Analysis](enrichment-analysis.md).

- **Download Options**: The visualization can be downloaded directly in common formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R code and data. For more information, check out [Interface Details](../interface-details.md).

- **Notes**: Add personal notes regarding the heatmap.

### Other Notes

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!

- **Interpretation**: The heatmap can provide insights into the relationships between different entities based on the selected annotations and clustering options. Adjusting the selection and aesthetic parameters can help in identifying the most relevant patterns.
