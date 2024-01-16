## Data Preprocessing

**Step 1: General Data Cleaning**

- Constant entities across all samples are removed from the dataset.
- Rows with all-zero values are removed to improve data quality.

**Step 2: Data Filtering (Optional)**

- If the selected preprocessing procedure is "filterOnly," additional filtering steps are applied based on the data type (Transcriptomics or Metabolomics).
- Low-abundance entities are filtered out based on specified criteria.

**Step 3: Data Transformation**

- **None**
  - No transformation is applied to the data.
  - Step 2 is skipped.
  - This option is recommended for data that has already been normalized and transformed.

- **Filter Only**
  - No transformation is applied to the data.
  - Data is only filtered based on Step 2.
  - This option is recommended for data that has already been normalized and 
    transformed or if you want to use raw data.

- **Simple Center Scaling:**
  - The data is centered and scaled.
  - Centering involves subtracting the mean of each entity, and scaling involves dividing by the standard deviation.
  - This procedure ensures that each entity has a mean of 0 and a standard deviation of 1.

- **Variance Stabilizing Transformation (VST):**
  - For transcriptomics data, DESeq2 is used for normalization and VST transformation.
  - The formula for analysis is determined based on user-specified factors (`main 
    factors` and `other factors`).
  - DESeq2 performs a negative binomial test to estimate the variance and stabilize the data.
  - The variance-stabilized data is then used for downstream analysis.
  - **Notes:** 
    - This option is **only suggested** for transcriptomics data.
    - Internally a `DESeq object` is created, which is used for the downstream 
      analysis. The vst transformation is used for visualizations such as the PCA.
    - The formulas supported are only simple ones for now. For a more complex 
      analysis, we suggest to write your own script. For this, you can perform within the App your analysis (e.g. PCA) and download R Code and data. Within the provided script you then may change the DESeq formula at the appropriate position and rerun the entire code to update respective data and retrieve new analysis results.

- **Scaling 0-1:**
  - The data is scaled to fit within the range of 0 to 1.
  - Each entity's values are transformed proportionally to ensure a consistent scale.

- **Natural Logarithm (ln):**
  - The natural logarithm of each data point is calculated.
  - This transformation is particularly useful for data that exhibits exponential growth.

- **Logarithm Base 10 (log10):**
  - The base-10 logarithm of each data point is calculated.
  - Special consideration is given to handling zero values to avoid undefined results.
  - If any zero values are present, a small constant is added before applying the logarithm.

- **Pareto Scaling:**
  - Pareto scaling emphasizes the importance of small values by dividing each data point by the square root of its standard deviation.
  - This method is suitable for datasets with a wide range of values.

**Step 4: Additional Checks and Warnings**

- Checks for the presence of NA values after preprocessing.
- Display of warnings and additional information based on the preprocessing steps.

---

