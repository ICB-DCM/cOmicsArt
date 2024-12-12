---
title: "Single Gene Visualisations"
layout: default
parent: Interface Details
nav_order: 8
---


# Single Gene Visualisations

The Single Gene Visualisations tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you have the following options:

- **Choose Data to use**: You can select between raw, processed, and batch corrected data.

- **Choose the style of visualisation**: You can choose between boxplots with testing and boxplots only. Note that actual boxplots will only be displayed if there are at least 4 samples per group. Which group can be determined by the option below the horizontal line. If there are fewer than 4 samples, only dots will be displayed.

- **Select Annotation you want to select an entity from**: You can choose the annotation to select the entities from. The options are directly taken from the provided row annotation. Precisely it is the column names of the row annotation.

- **Select the Gene from the list**: In the dropdown menu, you can choose from the unique items present in the chosen annotation category. Note that you can choose an annotation that is the same for multiple items. For example, you can choose `gene_type` (if present in your row annotation) and then `protein coding`. The displayed plot will show the sum of all protein coding genes for the respective sample, grouped by what is chosen below. You can check out [our showcases](../showcases.md) for actual examples. There is also a [screen recording](../screen_recording.md) covering some examples.

- **Get Single Gene Visualisation**: Clicking this button will generate the visualisation in the main panel based on the selected options.

- **Choose the groups to show the data for**: Below the horizontal line, you can choose the groups to show the data for. The options are populated based on the sample annotation provided initially.

## Main Panel 💡

The main panel displays the single gene visualisations. Here are some key points:

- **Visualisation**: The visualisation provides a boxplot or dot plot based on the number of samples per group and the selected options. Note that you only see boxplots if you have more than 3 samples per group. If there are fewer than 4 samples, only dots will be displayed.
- **Select your desired comparisons**: Here you select which comparisons you want to test and display in the plot. Note that each test is taken as an individual test, there is no multiple testing correction done \([Why it is important](https://www.nature.com/articles/nbt1209-1135)\) when choosing more than one test. For more advanced testing please go to the [Significance analysis tab](05-significance-analysis.md)


- **Download Options**: The visualisation can be downloaded directly in common formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R code and data. For more information, check out [Interface Details](../interface-details.md).

### Other Notes 📌

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Discover the pre-processing options available? → Go to [Pre-processing](03-pre-processing.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform significance analysis on your data? → Go to [Significance Analysis](05-significance-analysis.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---
