## Significance Analysis Options

**1. Choose Groups to Compare:**

- **Description:**
  - Select the groups from your data for which you want to perform significance analysis.

- **Options:**
  - For DESeq preprocessing, select from predefined factors.
  - For other preprocessing methods, choose from available sample annotation columns.

**2. Choose Comparisons:**

- **Description:**
  - Select the specific pairings of groups for which you want to perform significance analysis.

- **Options:**
  - Automatically generates possible pairings based on selected groups.
  - Notation is "Treatment:Control" and indicates the direction of the comparison.

**3. Choose Test Method:**

- **Description:**
  - Select the statistical test method for significance analysis.

- **Options:**
  - For DESeq preprocessing, a Wald test statistic is used. For more information [read 
    here](https://en.wikipedia.org/wiki/Wald_test) or [check out the original paper](http://www.jstor.org/stable/1990256)
  - For other preprocessing methods, choose from:
    - Wilcoxon rank sum test (Mann-Whitney U test). [Read more here](https://en.wikipedia.org/wiki/Mann–Whitney_U_test)
    - T-Test. [Read more here](https://en.wikipedia.org/wiki/Student%27s_t-test)
    - Welch-Test. [Read more here](https://en.wikipedia.org/wiki/Welch%27s_t-test)

**4. Choose Significance Level:**

- **Description:**
  - Set the desired significance level (alpha) for hypothesis testing.

- **Options:**
  - Slider input allowing selection between 0.005 and 0.1 with a default value of 0.05.

**5. Choose Test Correction:**

- **Description:**
  - Select the method for correcting p-values to account for multiple testing.

- **Options:**
  - Choose from various correction methods:

    **1. None:**
    
    - **Description:**
      - No correction is applied to p-values.
      - Each test is considered independently.
    
    **2. Bonferroni:**
    
    - **Description:**
      - Adjusts the significance level by dividing it by the number of tests.
      - Controls the family-wise error rate.
    
    **3. Benjamini-Hochberg:**
    
    - **Description:**
      - Controls the false discovery rate (FDR) by adjusting p-values.
      - Often more powerful than Bonferroni.
    
    **4. Benjamini Yekutieli:**
    
    - **Description:**
      - An extension of Benjamini-Hochberg for controlling the FDR under dependence.
      - Suitable when tests are correlated.
    
    **5. Holm:**
    
    - **Description:**
      - A step-down method that controls the family-wise error rate.
      - Adjusts p-values sequentially.
    
    **6. Hommel:**
    
    - **Description:**
      - A method that controls the family-wise error rate and is more powerful than Bonferroni.
      - Accounts for arbitrary dependency structures.
    
    **7. Hochberg:**
    
    - **Description:**
      - A step-up method that controls the family-wise error rate.
      - Adjusts p-values sequentially.
    
    **8. FDR (False Discovery Rate):**
    
    - **Description:**
      - Controls the expected proportion of falsely rejected null hypotheses.
      - More liberal than family-wise error rate methods.

  - For a bit more information on multiple testing correction, [read here](https://en.wikipedia.org/wiki/Multiple_comparisons_problem).

