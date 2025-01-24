---
title: "Differential Analysis"
layout: default
parent: Interface Details
nav_order: 5
---

# Differential Analysis

The Differential Analysis tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you have the following options:

### 1. Choose Groups to Compare
Select the groups from your data for which you want to perform differential analysis.

- For DESeq preprocessing, select from predefined factors.
- For other preprocessing methods, choose from available sample annotation columns.

### 2. Choose Comparisons
Select the specific pairings of groups for which you want to perform differential analysis.

- Automatically generates possible pairings based on selected groups.
- Notation is "Treatment:Control" and indicates the direction of the comparison.

### 3. Choose Test Method
Select the statistical test method for differential analysis.

- For DESeq preprocessing, a Wald test statistic is used. For more information [read here](https://en.wikipedia.org/wiki/Wald_test) or [check out the original paper](http://www.jstor.org/stable/1990256).
- For other preprocessing methods, choose from:
  - Wilcoxon rank sum test (Mann-Whitney U test). [Read more here](https://en.wikipedia.org/wiki/Mann–Whitney_U_test)
  - T-Test. [Read more here](https://en.wikipedia.org/wiki/Student%27s_t-test)
  - Welch-Test. [Read more here](https://en.wikipedia.org/wiki/Welch%27s_t-test)

### 4. Choose Significance Level
Set the desired significance level (alpha) for hypothesis testing.

- Slider input allowing selection between 0.005 and 0.1 with a default value of 0.05.

### 5. Choose Test Correction
Select the method for correcting p-values to account for multiple testing.

<div style="margin-left: 20px;">

<details>
<summary>1. None</summary>
No correction is applied to p-values. Each test is considered independently.
</details>

<details>
<summary>2. Bonferroni</summary>
Adjusts the significance level by dividing it by the number of tests. Controls the family-wise error rate.
</details>

<details>
<summary>3. Benjamini-Hochberg</summary>
Controls the false discovery rate (FDR) by adjusting p-values. Often more powerful than Bonferroni.
</details>

<details>
<summary>4. Benjamini Yekutieli</summary>
An extension of Benjamini-Hochberg for controlling the FDR under dependence. Suitable when tests are correlated.
</details>

<details>
<summary>5. Holm</summary>
A step-down method that controls the family-wise error rate. Adjusts p-values sequentially.
</details>

<details>
<summary>6. Hommel</summary>
A method that controls the family-wise error rate and is more powerful than Bonferroni. Accounts for arbitrary dependency structures.
</details>

<details>
<summary>7. Hochberg</summary>
A step-up method that controls the family-wise error rate. Adjusts p-values sequentially.
</details>

<details>
<summary>8. FDR (False Discovery Rate)</summary>
Controls the expected proportion of falsely rejected null hypotheses. More liberal than family-wise error rate methods.
</details>

For a bit more information on multiple testing correction, [read here](https://en.wikipedia.org/wiki/Multiple_comparisons_problem).

</div>

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
