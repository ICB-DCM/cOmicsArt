---
title: "Significance Analysis"
layout: default
parent: Interface Details
nav_order: 5
---

# Significance Analysis

The Significance Analysis tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you have the following options:

- **Choose groups to compare**: This allows you to select the groups you want to compare.
- **Select your desired comparisons**: Here you input the specific comparisons you 
  want to make. You enter the groups in the format `Treatment:Control`. Multiple 
  comparison pairs can be entered at once.
- **Test method**: You can select the statistical test method to use. You can choose 
  between [T-test](https://en.wikipedia.org/wiki/Student%27s_t-test), [Wilcoxon Rank 
  Sum Test](https://en.wikipedia.org/wiki/Mann–Whitney_U_test), and [Welch's T-test](https://en.wikipedia.org/wiki/Welch%27s_t-test). 
  If you used `vst_DESeq` as the [preprocessing method](03-pre-processing.md), DESeq2 
  will use the [Wald test](https://en.wikipedia.org/wiki/Wald_test).

- **Significance level**: This slider allows you to set the significance level for the analysis, ranging from 0.005 to 0.1.

- **Test correction**: You can choose the method for multiple testing correction.

- **Get Significance Analysis**: Clicking this button will perform the significance analysis based on the selected parameters.

## Main Panel 💡

The main panel displays the results of the significance analysis. The main panel has several tabs:
- **Result Visualization**: This tab shows similarities and differences between the groups you compared.
  - **Visualization Choices**: Allows you to customize the visualization of the results.
    - **Select your desired comparisons to visualize**: Choose which comparisons to include in the visualization.
    - **Visualization method**: Select the type of plot to use. If the number of comparisons is 
      smaller than 4, you can choose between UpSetR plot and Venn diagram. If the 
      number of comparisons is larger than 4, only the UpSetR plot is available.
    - **Type of significance to look at**: Choose between Significant and Significant 
      unadjusted.
    - **Intersections to highlight**: Specify the number of intersections to 
      highlight in the plot. (Only available for UpSetR plot)
  - **Venn Diagram**: Shows the intersections of significant results across 
    different comparisons as overlapping circles. The number within each 
    intersection is equal to the number of significant results that the circles 
    sharing the intersection have in common.
  - **UpSetR Plot**: The UpSetR plot section visualizes the intersections of significant results across different comparisons.
    - **Intersection Size**: The bars represent the number of significant results for each intersection.
    - **Set Size**: The horizontal bars show the number of significant results in each comparison group.
    - **Intersections**: The dots and lines indicate which groups are part of each intersection.
  - **Download Options**: You can save the plots in different file formats such as 
    PNG, TIFF, and PDF. You also have the option to download the underlying R code 
    and data. For more information, check out [Interface Details](../interface-details.md).
- **Comparison tabs**: For each selected comparison, a separate tab is created in 
  which the results can be viewed. They consist of a Volcano plot and a table.
  - **Table**: A sortable table, displaying pvalue, padj, log2FoldChange, and other 
    information for each entity tested. The table can be downloaded as a `.csv` or `.
    xlsx` file. The table can be sorted by clicking on the column headers.
  - **Volcano Plot**: The volcano plot section is divided into two plots:
    - **Corrected p-Values**: Shows the log fold change versus the corrected p-values.
    - **Uncorrected p-Values**: Shows the log fold change versus the uncorrected p-values.
    - Both plots simultaneously or separately can be downloaded directly in common 
      formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download 
      the underlying R code and data. For more information, check out [Interface Details](../interface-details.md).

### Other Notes 📌

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
- **Interpretation**: The plots and table can give insights into the significant 
  changes between the compared groups. Adjusting the significance level and thresholds 
  can help in identifying the most relevant results. Checking the intersections can 
  point towards commonalities between the groups.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Discover the pre-processing options available? → Go to [Pre-processing](03-pre-processing.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---
