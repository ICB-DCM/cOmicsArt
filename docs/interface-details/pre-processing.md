---
title: "Pre-processing"
layout: default
parent: Interface Details
nav_order: 2
---

# Pre-Processing

The Pre-Processing tab is designed to provide users with various data pre-processing options to ensure their data is ready for further analysis. This tab is detrimental in preparing the data for the actual analysis tabs.

## Side Panel

The side panel contains various pre-processing procedures. Here are the details of each option:

1. **Pre-Processing Procedures**:
    - **Options**:
      - Option `none`: No pre-processing is applied. Data remains in its original state. This option is recommened if pre-processing has already been applied to the data outside cOmicsART.
      - Option `filterOnly`: Filters out low abundant entities based on the omic type
          - Transcriptomics: filters genes with low counts less than 10 counts accross all samples 
          - Metabolomics: filters entities that have a zero median over all samples
          - Lipidomics: no filtering is applied
      - Option `vst_DESeq`: Applies variance stabilizing transformation using the DESeq2 pipeline. This option requires users to specify main and (optionally) sub factors for the DESeq design formula. Please, see below for more information regarding the DESeq formula options.
      - Option `simpleCenterScaling`: Centers the data by subtracting the mean and scales it by dividing by the standard deviation (per entitie)
      - Option `Scaling_0_1`: Scales the data to a range of 0 to 1 (per entity)
      - Option `log10`: Applies log base 10 transformation to the data. If there are zeros present, the transformation is applied as log10(data + 1).
      - Option `pareto_scaling`: Scales the data by dividing each value by the square root of the standard deviation.
      - Option `ln`: Applies natural logarithm transformation to the data.

If you are missing an option feel free to contact us or open an issue on our [GitHub repository](https://github.com/LeaSeep/OmicShiny)

2. **(needed if vst_DESeq chosen) DESeq Formula (Main Factor)**:
    Allows users to choose the main factor for the design formula in the DESeq pipeline. This is important for the `vst_DESeq` procedure and significance analysis. Note, that your options are taken from the provided sample data. A valid main factor needs to be a categorical with at least two levels. If not, an error will occur notifying you that it is impossible to determine the dispersion.

3. **(optional if vst_DESeq chosen) DESeq Formula (Sub Factors)**:
  Allows users to select additional factors to account for in the DESeq pipeline. This helps in adjusting for other variables that might affect the results. Note, that you select here multiple items from the list. In order to employ more intricate design, make sure to have read and understand [this](https://bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html#interactions), hence whether an added merged factor to you sample table is sufficient. If not you can download code+data of e.g. one of the analysis tabs. The preprocessing is included in the snippets and you can adjust taken DE-Seq design formula there to your needs. Here, you can find more details about [R code and Data download](../code-and-data.md).
    
If you got a general question about the DESeq formula or the underlying pipline, please refer to the [DESeq2 documentation](https://bioconductor.org/packages/devel/bioc/vignettes/DESeq2/inst/doc/DESeq2.html).

4. **Select Batch Effect Column**:
 Dynamically generated select input.
Users can select a column to correct for batch effects. Note, that in the first iteration one should make sure to look at the entire data without batch correction to see if there are any batch effects present. 
Note, If a batch effect column is selected and the `vst_DESeq` procedure is chosen, batch correction is integrated into the DESeq2 pipeline. Otherwise, the app uses the ComBat method from the `sva` package for batch correction (see [here](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3307112/) for details.

5. **Get Pre-processing**:
    Upon button click - chosen pre-processing is applied, returning some general information about the pre-processed data as well as dample distribution plots within the main panel.

6. **Color the violin plot by**:
  Allows users to select a column to color the sample-distribution violin plots by. Often you want to ensure not have a general shifted distribution of a single sample. If the distritbution differ per sample group this might be an indication of a batch effect or a very high impact of respective treatment (rather unlikely but possible).

## Main Panel

The main panel provides visual feedback and summary statistics of the raw and pre-processed data. It contains the following components:

1. **Statistics to the Data**:
    - **UI Element**: HTML output.
    - **Description**: Displays general statistics about the input data, including dimensions and any warnings related to the pre-processing steps.

2. **Raw Data Visualization**:
    - **UI Elements**: Plot outputs.
    - **Description**: Shows the raw data using violin plots and kernel density estimation (KDE) plots. These plots help users understand the initial state of their data.

3. **Pre-processed Data Visualization**:
    - **UI Elements**: Plot outputs.
    - **Description**: Displays the pre-processed data using violin plots and KDE plots. These plots allow users to compare the effects of the selected pre-processing procedures.

## Important Notes

- Pre-processing steps may remove entities that are constant across all samples.
- Warnings are displayed if NA values are present after pre-processing, and rows containing NAs are removed.
- Some pre-processing steps may lead to negative values, which can affect downstream analyses like log fold changes.
- Users should ensure that their batch effect column and DESeq formulas do not introduce linear dependencies, which can cause errors.

This documentation aims to provide users with a clear understanding of the pre-processing options available and how to use them effectively to prepare their data for further analysis.