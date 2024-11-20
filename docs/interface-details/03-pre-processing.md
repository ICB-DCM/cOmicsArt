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
- The formula for analysis is determined based on user-specified factors. (<span id="toggle-button" style="color: blue; cursor: pointer;" onclick="toggleInfoBox()">Learn more</span>).
  <div id="info-box" style="display: none; margin-top: 10px; padding: 10px; border: 1px solid #ccc; background-color: #f9f9f9; width: auto; max-width: 100%;">
    <h2>Understanding the Design Matrix in DESeq2</h2>
    <p>
      The design matrix in DESeq2 is a fundamental component used to specify the experimental 
      design of your RNA-seq dataset. It helps in determining the relationship between the observed 
      counts and the experimental conditions. Here's a detailed explanation:
    </p>
      
    <strong>What is a Design Matrix?</strong>
    <p>
      A design matrix is a mathematical representation that describes how the experimental conditions 
      (factors) are associated with the observed data. In the context of DESeq2, it is used to model 
      the relationship between the counts (gene expression levels) and the experimental factors 
      (conditions, treatments, etc.).
    </p>
      
    <p><strong>Choosing Factors for the Design Matrix</strong></p>
    <p>
      In DESeq2, you typically need to specify factors that explain your data.
    </p>
    <p>
      The design formula in DESeq2 is created by combining the factors. The formula is typically written in the form:
    </p>
    <pre><code>~ factor_1 + factor_2 + etc</code></pre>
    <p>
      This describes that the factors contribute to the data fitting independently from each other.
    </p>
    <p>
      For example, if your main factors are <code>treatment</code>, <code>batch</code>, and <code>sequencing_depth</code>, 
      the design formula would be:
    </p>
    <pre><code>~ treatment + batch + sequencing_depth</code></pre>
      
    <p><strong>What Does the Design Matrix Do?</strong></p>
    <p>
      The design matrix allows DESeq2 to model the counts data while considering the specified experimental design. 
      It helps in:
    </p>
    <ul>
      <li><strong>Normalization</strong>: Adjusting for differences in sequencing depth or other technical biases.</li>
      <li><strong>Variance Stabilization</strong>: Ensuring that the variance is stabilized across the range of mean values.</li>
      <li><strong>Differential Expression Analysis</strong>: Identifying genes that are differentially expressed between the levels of the main factor while controlling for other factors.</li>
    </ul>
      
    <p><strong>Example</strong></p>
    <p>
      Let's say you have an RNA-seq experiment with two conditions (Control and Treatment) and two additional factors 
      (Batch and Sequencing Depth). You want to analyze the effect of the treatment while accounting for batch effects 
      and sequencing depth. Here's how you can set it up:
    </p>
    <ol>
      <li><strong>Factors</strong>: Treatment, Batch, Sequencing Depth</li>
      <li><strong>Design Formula</strong>: <code>~ treatment + batch + sequencing_depth</code></li>
    </ol>
    <p>
      The design matrix will help DESeq2 to:
    </p>
    <ul>
      <li>Compare gene expression between Control and Treatment groups.</li>
      <li>Adjust for any variability introduced by different batches.</li>
      <li>Account for differences in sequencing depth across samples.</li>
    </ul>
      
    <p><strong>Conclusion</strong></p>
    <p>
      The design matrix in DESeq2 is crucial for accurately modeling your RNA-seq data and identifying differentially 
      expressed genes. By carefully selecting and combining the main and other factors, you can ensure that your analysis 
      accounts for all relevant variables and provides reliable results.
    </p>
  </div>
  - DESeq2 performs a negative binomial test to estimate the variance and stabilize the data.
  - The variance-stabilized data is then used for downstream analysis.
  - **Notes:**
    - This option is **only suggested** for transcriptomics data.
    - Internally a `DESeq object` is created, which is used for the downstream analysis. 
      The VST transformation is used for visualizations such as the PCA.
    - The formulas supported are only simple ones for now. For a more complex analysis, we suggest writing your own script.



<script>
  function toggleInfoBox() {
    const infoBox = document.getElementById('info-box');
    const toggleButton = document.getElementById('toggle-button');
    
    if (infoBox.style.display === 'none' || infoBox.style.display === '') {
      infoBox.style.display = 'block';
      toggleButton.textContent = 'Show less';
    } else {
      infoBox.style.display = 'none';
      toggleButton.textContent = 'Learn more';
    }
  }
</script>

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

### Understanding the Design Matrix in DESeq2

The design matrix in DESeq2 is a fundamental component used to specify the experimental design of your RNA-seq dataset. It helps in determining the relationship between the observed counts and the experimental conditions. Here's a detailed explanation:

#### What is a Design Matrix?

A design matrix is a mathematical representation that describes how the experimental conditions (factors) are associated with the observed data. In the context of DESeq2, it is used to model the relationship between the counts (gene expression levels) and the experimental factors (conditions, treatments, etc.).

#### Choosing Factors for the Design Matrix

In DESeq2, you typically need to specify factors that explain your data.

The design formula in DESeq2 is created by combining the factors. The formula is typically written in the form:
```R
~ factor_1 + factor_2 + etc
```
This describes that the factors contribute to the data fitting independently from each 
other.

For example, if your main factors are `treatment`, `batch` and `sequencing_depth`, the 
design formula would be:

```R
~ treatment + batch + sequencing_depth
```

#### What Does the Design Matrix Do?

The design matrix allows DESeq2 to model the counts data while considering the specified experimental design. It helps in:

- **Normalization**: Adjusting for differences in sequencing depth or other technical biases.
- **Variance Stabilization**: Ensuring that the variance is stabilized across the range of mean values.
- **Differential Expression Analysis**: Identifying genes that are differentially expressed between the levels of the main factor while controlling for other factors.

#### Example

Let's say you have an RNA-seq experiment with two conditions (Control and Treatment) and two additional factors (Batch and Sequencing Depth). You want to analyze the effect of the treatment while accounting for batch effects and sequencing depth. Here's how you can set it up:

1. **Factors**: Treatment, Batch, Sequencing Depth
2. **Design Formula**: `~ treatment + batch + sequencing_depth`

The design matrix will help DESeq2 to:

- Compare gene expression between Control and Treatment groups.
- Adjust for any variability introduced by different batches.
- Account for differences in sequencing depth across samples.

#### Conclusion

The design matrix in DESeq2 is crucial for accurately modeling your RNA-seq data and identifying differentially expressed genes. By carefully selecting and combining the main and other factors, you can ensure that your analysis accounts for all relevant variables and provides reliable results.