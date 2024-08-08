---
title: "Pre-processing"
layout: default
parent: "Interface Details"
nav_order: 4
---

# Pre-processing

The Pre-processing tab is divided into two main sections: the side panel and the main panel.

## Side Panel

In the side panel, you have the following options:

- **Pre-Processing Procedures**: You can select from various pre-processing procedures.
  - Options: none, filterOnly, vst_DESeq, simpleCenterScaling, Scaling_0_1, log10, pareto_scaling, ln
  - vst_DESeq also requires the selection of a design formula.

- **Select Batch Effect Column**: Choose a batch effect if applicable. Possible 
  choices are taken from the sample annotation columns.

- **Get Pre-Processing**: Clicking this button will apply the selected pre-processing procedure to the data.

- **Color the violin plot by**: Choose a variable to color the violin plots. The options are taken from the sample annotation columns.

## Main Panel

The main panel displays the results of the pre-processing. Here are some key points:

- **General statistics to the input data**: Displays general statistics about the input data, such as dimensions.

- **Violin Plot**: Shows the count distribution per sample before and after 
  pre-processing.

### Download Options

- **Save Violin Plot**: You can save the violin plot in different file formats such as PNG, TIFF, and PDF. You also have the option to download the underlying R code and data.

### Other Notes

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
- **Pre-processing Interpretation**: Observing the pre-processed data can provide insights into how different pre-processing procedures affect the data. Adjusting the pre-processing parameters can help in optimizing the data for downstream analyses.
