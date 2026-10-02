---
title: "ML Classification"
layout: default
parent: Interface Details
nav_order: 10
---

# ML Classification

The ML Classification tab provides machine learning methods to analyze sample groupings using both supervised and unsupervised approaches. This feature helps you discover natural patterns in your data or test whether known experimental groups are separable based on expression patterns.

## When to Use This Feature 🎯

- **Unsupervised (k-means)**: Discover if your samples naturally cluster by experimental conditions without imposing labels
- **Supervised (SVM)**: Test whether known experimental groups are separable based on expression patterns

This tab is divided into two main sections: the side panel for analysis configuration and the main panel for results visualization.

## Side Panel 📚

### 1. Analysis Type

Choose between two fundamental approaches:

- **Unsupervised**: Discovers natural groupings without using condition labels
  - Ideal when you want to see if samples naturally group by your experimental conditions


- **Supervised**: Tests separability based on known condition labels
  - Ideal when you want to assess how well conditions can be distinguished
  - Requires categorical condition labels (e.g., "treated" vs "control")

### 2. Unsupervised Settings (k-means)

When **Unsupervised** is selected:

- **Method**: Currently k-means clustering (additional methods coming in future 
  releases). For info see e.g. https://youtu.be/4b5d3muPQmA

- **Number of clusters (k)**:
  - Select how many groups to divide samples into (2-10)
  - **Tip**: Start with the number of biological conditions you have. For example:
    - 2 conditions (treated vs control) → try k=2
    - 3 timepoints → try k=3
    - Then try k±1 to see if substructure exists

- **Filter to top variable genes**:
  - Recommended for large datasets (>5000 genes)
  - Improves performance and reduces noise by selecting most informative features
  - **Number of genes**: Select how many top variable genes to use (50-5000)
    - Default: 500 genes is a good starting point
    - Decrease if running into performance issues

- **Overlay condition (optional)**:
  - Select a condition column from your sample annotation
  - Displays true condition labels as shapes on the plot
  - Helps compare discovered clusters with expected groupings
  - Select "None" to see only cluster assignments

### 3. Supervised Settings (SVM)

When **Supervised** is selected:

- **Method**: Currently SVM (Support Vector Machine) - additional methods coming in 
  future releases. For more info see e.g. https://youtu.be/efR1C6CvhmE

- **Condition column (target)**:
  - Select the categorical variable you want to predict
  - Examples: "treatment", "cell_type", "disease_status"
  - **Important**: Must be categorical, not continuous
    - ✅ Categorical: "treated" vs "control", "Type A" vs "Type B"
    - ❌ Continuous: dosage amounts, time points as numbers

- **Filter to top variable genes**:
  - Same as unsupervised settings
  - Especially recommended for SVM with high-dimensional data

## Main Panel 💡

### Unsupervised Results

After clicking **Run Clustering**, the main panel displays:

#### Clustering Plot
- **Visualization**: PCA projection showing samples colored by cluster assignment
  - X-axis: First principal component (PC1) with % variance explained
  - Y-axis: Second principal component (PC2) with % variance explained
  - Colors: Cluster assignments (automatically assigned)
  - Shapes (if overlay selected): True condition labels

- **Interpretation**:
  - **Good clustering**: Clusters match expected biological conditions, clear separation in PCA space
  - **Poor clustering**: Samples don't group by condition - may indicate:
    - Weak biological signal
    - Batch effects dominating
    - Need for different k value
    - Need for better preprocessing

#### Cluster Assignments Table
- Lists each sample with its cluster number
- If condition overlay is selected, shows both cluster and true condition
- Sortable and searchable for easy exploration

### Supervised Results

After clicking **Run Classification**, the main panel displays:

#### Training Accuracy Panel
- Shows the percentage of correctly classified samples
- **⚠️ Important Disclaimer**: This is training accuracy only (no validation)
  - Results are exploratory and need to be distinguished from true predictive performance
  - May be overly optimistic due to overfitting
  - Cross-validation coming in future release

- **Interpretation**:
  - **High accuracy (>90%)**: Conditions are highly separable - strong expression signature
  - **Medium accuracy (70-90%)**: Partial separability - most common realistic scenario
  - **Low accuracy (<70%)**: Conditions overlap in expression space - may indicate:
    - Weak biological differences
    - Need for more samples
    - Need for feature selection

#### Decision Boundary Plot
- **Visualization**: PCA projection with SVM decision boundaries
  - Background colors: Decision regions for each class
  - Points colored by true label
  - Points shaped by predicted label
  - Red circles: Misclassified samples

- **Key Features**:
  - Variance explained for PC1 and PC2
  - Clear identification of misclassifications
  - Visual assessment of class separability

#### Predictions Table
- Lists each sample with:
  - True Label: Actual condition from your annotation
  - Predicted Label: SVM prediction
  - Correct: Whether prediction matches truth
- Color-coded rows:
  - Green background: Correct predictions
  - Red background: Incorrect predictions

## Important Notes 📌

### Stochastic Algorithms

k-means and SVM may produce slightly different results each time due to random initialization:

- Results shown in app use random starting points (demonstrates natural variability)
- Downloaded R code shows how to set a `set.seed()` for exact reproducibility
- Running the same analysis twice may give slightly different results

**Example from generated code**:
```r
# To ensure reproducibility, set a random seed before running:
set.seed(123)  # Use any integer
```

### Data Requirements

- **Preprocessed data only**: ML Classification requires data from the Preprocessing tab
  - Raw counts will not work
  - Normalized and transformed data is required

- **No missing values**: Ensure data is complete before running
  - Check preprocessing output for NA warnings

- **Minimum samples**:
  - At least 3 samples required (hard requirement)
  - 10+ samples recommended for reliable results

### Limitations (Version 1)

- **No cross-validation**: Supervised methods show training accuracy only
  - Cross-validation planned for future release
  - Use results for exploration, not final conclusions

- **Continuous outcomes not supported**: Regression methods coming in future release
  - Only categorical condition columns work for supervised learning
  - Error message shown if continuous variable selected

- **Single method per type**: Additional algorithms coming soon
  - Unsupervised: Hierarchical clustering, GMM planned
  - Supervised: Random Forest, Naive Bayes planned

### Performance Warnings

- Running on >5000 genes may cause slowdowns
- Use gene filtering for large datasets
- Expect 10-30 seconds for typical analyses
- Browser may become unresponsive during computation (this is normal)

## Comparison with Other Features 🔄

Understanding when to use ML Classification versus other analysis features:

| Feature | Purpose | When to Use |
|---------|---------|-------------|
| **ML Classification** | Discover/test sample groupings | Want to know if conditions cluster naturally or are separable |
| **Sample Correlation** | Show pairwise sample relationships | Want to see correlation structure and hierarchical clustering |
| **PCA** | Visualize major variation sources | Want to see overall data structure and outliers |
| **Differential Analysis** | Find significantly different genes | Want to identify which features drive differences |

**These features are complementary** - use multiple to build a complete picture of your data!

## Practical Workflow Examples 📋

### Example 1: Exploratory Analysis

1. Run **Unsupervised (k-means)** with k = number of conditions
   - Use gene filtering with 500 genes
   - Add condition overlay to compare clusters vs. expected groups
2. If clusters match conditions → strong biological signal confirmed
3. If clusters don't match → investigate with PCA and Sample Correlation

### Example 2: Hypothesis Testing

1. Run **Supervised (SVM)** on your condition of interest
   - Filter to 500-1000 top variable genes
2. High accuracy? → conditions are separable
3. Download results and identify misclassified samples
4. Investigate misclassified samples with Single Gene Visualizations

### Example 3: Comparing Multiple k Values

1. Run k-means with k=2, k=3, k=4
2. Compare cluster sizes and assignments
3. Look for consistent groupings across k values
4. Use the one that best matches your biological hypothesis

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Prepare your data for ML? → Go to [Pre-processing](03-pre-processing.md)
- Explore sample relationships? → Go to [Sample Correlation](04-sample-correlation.md)
- Visualize overall structure? → Go to [PCA](06-pca.md)
- Find differentially expressed genes? → Go to [Differential Analysis](05-significance-analysis.md)
- Create detailed visualizations? → Go to [Heatmap](07-heatmap.md)

---

## Questions?

Check the [FAQ](../faq.md) or use the help icons (?) throughout the interface for quick guidance.
