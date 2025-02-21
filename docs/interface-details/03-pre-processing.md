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

Constant entities across all samples are removed from the dataset - even if choose `No pre-processing` as preprocessing. Those entities do not contain any information and can lead to errors in the downstream analysis.

**Step 2: Choose Processing Type**

You can first choose your processing type, where we distinguish between the following options:
- Filtering
- Omic-Specific
- Log-Based
- Miscellaneous

Each processing type offers a set of options that allow you to refine the specific pre-processing steps you want to apply. If you select 'Filtering' as the processing type, it will only remove low-abundance entities based on your chosen criteria. For all other processing types, filtering must always be explicitly specified, as it serves as the initial step before any further pre-processing is applied.

**Step 3: Choose your Processing Option dependent on your pre-preocessing type**

**Type: Filtering**

*global filtering*

  - Removes low-abundance entities by setting a minimum total count accross all samples. For example, if you set the minimum total count to 10, each entities sum over all samples is calculated and if it is below 10, the entity is removed.
  
*sample-wise filtering*

  - Removes low-abundance entities by setting a minimum count per sample as well as a threshold how many samples need to have this minimum count. For example, if you set the minimum count per sample to 0 and the threshold to 5, each entity is checked if it has a count of at least 1 in at least 5 samples. If not, the entity is removed. Note, that the number of sample meeting the defined threshold should not exceed total sample size.

**Type: Omic-Specific**

You will need to specify the filtering options as well. For more information see above.

*DESeq2*
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

*limma voom*
  - Note that you need to also specify here first the filtering characteristics as first steps. limma::voom recommends to remove entities with low counts.
  -  Transforms data for linear modeling through two key steps. First, it converts raw counts into log2 counts per million (log2CPM). Then, it estimates the mean-variance relationship while accounting for the experimental design.
  -  Choose factors of interest which will be added to your design in additive fashion and decide whether to include an intercept (default: yes). If you are not sure what to choose, we suggest to include the intercept. You would choose not intercept if you expect no expression in your control group.
  - If you want to know more about 'What a desing matrix is' and how to choose the right factors, check out the information above under DESeq2.
  
*TMM*
  - Not that you need to specify here first the filtering characteristics as first steps.
  - TMM stands for Trimmed Mean of M-values. It is a normalization method that estimates the scaling factors for the library sizes. The scaling factors are used to adjust the library sizes to account for differences in sequencing depth between samples.
  
**Type: Log-based**

You have the options of log10, log2 and ln (natural logarithm). Note, that there will be 1 added to the raw measurements to avoid taking log of 0 which results in -Inf. You will also need to specify - again - the filtering options as first steps.

**Type: Miscellaneous**

Be aware that this methods might not be particular suitable for e.g. raw count data. It might be useful however if you have data that is already-pre processed to some extend and you want to use e.g. cOmicsArts visualisation tools.
You will also need to specify - again - the filtering options as first steps.

*Pareto scaling*

  - Pareto scaling is a scaling method that divides the data by the square root of the standard deviation. This method is useful when the data has a large dynamic range.
  
*Centering & Scaling*

  - Centering and scaling is a normalization method that subtracts the mean and divides by the standard deviation.
  
*Scaling 0-1*

  - Scaling to 0-1 is a normalization method that scales the data to a range between 0 and 1.
  


**[optional Step]  Select Batch Effect Column**

Choose a batch effect if applicable. Possible choices are taken from the sample 
annotation columns. **Important**: This step is **optional**, as sometimes no batches need 
to be accounted for. Additionally, in the very first run, batch effects might not be 
know, thus using no batch effects and looking at correlations and PCA plots can help 
to identify them. To learn more about batch effects - how they appear and how to 
identify them you can start by checking out [this website ](https://bigomics.ch/blog/the-tricky-problem-of-batch-effects-in-biological-data/)



**Get Pre-Processing**: Clicking this button will apply the selected pre-processing procedure to the data.

**Color the violin plot by**: Choose a variable to color the violin plots. The options are taken from the sample annotation columns.

---

## Main Panel 💡

The main panel displays the results of the pre-processing. Here are some key points:

-   **General statistics to the input data**: Displays general statistics about the input data, such as dimensions before and after pre-processing. Here you can see how many entities were removed due to the pre-processing steps. Additionally you find a summary of the entitie-wise conducted normality test. Check out the interpretation guidance for more information on this topic.

-  **Start Discovering** : Upon click you will actually not taken to 'the' analysis (as you are free to choose the order or only a single module) but will be reminded again how to navigate to the different analysis tabs. When you use cOmicsArt more regularly you can simply skip this step and directly click on the analysis tabs to switch to the respective analysis.

- **Violin Plot**: Shows the count distribution per sample before and after pre-processing. You are most likely looking for the same shapes across the samples. If one sample behaves distinctly different it might hint to you that there is something wrong with this sample. This can be further confirmed within the PCA or Sample correlation analysis. See at the bottom of this page for more information on how to interpret the pre-processing plots.

- **Mean and Standard Deviation Plot**: Shows the mean and standard deviation of the pre-processed data. This plot can help you to identify if the standard deviation is constant across the mean - one speaks of homoscedasticity. If the standard deviation increases with the mean you have heteroscedasticity. This can be a problem for some statistical tests. See at the bottom of this page for more information on how to interpret the pre-processing plots.

### Download Options 📂

-   **Save Violin Plot**: You can save the violin plot in different file formats such as PNG, TIFF, and PDF. You also have the option to download the underlying R code and data or send to plot to the report
-   **Save Mean and Standard Deviation Plot**: You can save the mean and standard deviation plot in different file formats such as PNG, TIFF, and PDF. You also have the option to download the underlying R code and data or send to plot to the report

### Interpretation Guidance 🧭
The pre-processing step is crucial as it is the basis on which all the following analyses are based. A unsuitable pre-processing may lead to draw wrong conclusions from the data. Hence you should carefully consider your options and make an informed choice. 
⚠️There is no harm in trying out different pre-processing options but should not try until you find the results you 'like'. What you should look for are the properties of your pre-processed data.

🎻 In general you want to have similar global distributions per sample which you can check with the provided violin plots. There should be no sample that behaves distinctly differently from the others. If you see such a shifted pattern that corresponds with e.g. a variable of interest (try to color the different violins by various variables). This pattern might be an actual variation of interest or can be accounted for with batch correction.

📈 The mean and standard deviation plot can help you to identify if the standard deviation is constant across the mean - one speaks of homoscedasticity. You are looking for a rather constant line. For more information on this topic, you might want to check out this [resource](https://www.statisticshowto.com/homoscedasticity/). Note that we plot the rank of the mean not the mean. If you use scaling methods you will see that the standard deviation is constant across the mean. The colored hexagonal shapes represent the number of entities at the respective rank area with respective standard deviations (will be suitably binned automatically). You are generally looking for high counts close to the fitted red line. If the line deviates from a constant behavior but for a rather low number of variables you often can accept that and still assume reasonable homoscedasticity. If you have a large number of variables that deviate from the red line you might want to consider applying a different pre-processing to your data as it is an assumption in many statistical tests.

📊Finally you can assess the displayed results of the normality tests. This can provide you an overview of which potential statistical test you can apply to your data as the t-test for example requires normal distributed data. You should be aware that the normality test is only reliable for large sample sizes which is often not the case - this means for small sample sizes the null hypothesis cannot be rejected. Hence handle this information carefully and potentially ask yourself whether you assume that the respective data is normally distributed. For further reading you might want to check out this paper: [Descriptive Statistics and Normality Tests for Statistical Data](https://pmc.ncbi.nlm.nih.gov/articles/PMC6350423/#abstract1) or other resources.

Based on the Interpretation guide and linked resources we aim to provide you with a good starting point to choose the correct pre-processing procedure for your data. Please be aware that the pre-processing is a crucial step and can have a large impact on the downstream analysis.

🔔The currently implemented options are a good starting point but might not cover all possible scenarios.

⏳If you are missing a specific option please feel free to contact us via mail or GitHub! We are happy to expand upon the user's request.


### Other Notes 📌

-   **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
-   **Pre-processing Interpretation**: Observing the pre-processed data can provide insights into how different pre-processing procedures affect the data. Adjusting the pre-processing parameters can help in optimizing the data for downstream analyses.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform differential analysis on your data? → Go to [Differential Analysis](05-significance-analysis.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---