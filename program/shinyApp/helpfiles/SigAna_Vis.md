## Visualizing Significance Analysis Results Options

***
**1. Select Comparisons to Visualize:**

- **Description:**
  - Choose specific comparisons to visualize.

- **Options:**
  - "all": Visualize results for all selected comparisons.
  - Individual comparisons based on the performed analysis. (selected in `Select you 
    desired comparisons`)

**2. Choose Visualization Method:**

- **Description:**
  - Select the method for visualizing significance analysis results.

- **Options:**
  - **UpSetR plot:**
    - Displays intersecting sets of significant features between comparisons.
    - Suitable for analyzing overlaps and unique features. Can handle multiple 
      comparisons reasonably well.

  - **Venn diagram:**
    - Easy to interpret, but available only when the number of selected comparisons is 
      less than five.
    - Illustrates overlapping features between comparisons using circles.

**3. Choose Genes to Look At:**

- **Description:**
  - Specify the type of genes to focus on within the selected comparisons.

- **Options:**
  - **For DESeq preprocessing:**
    - "Significant": Display genes with adjusted significance.
    - "Upregulated": Focus on upregulated genes.
    - "Downregulated": Focus on downregulated genes.
    - "Significant unadjusted": Display genes with unadjusted significance.

  - **For other preprocessing methods:**
    - "Significant": Display genes with adjusted significance.
    - "Significant unadjusted": Display genes with unadjusted significance.

**Note:**
- UpSetR plots provide a comprehensive view of overlapping features, while Venn diagrams offer a simplified representation.

---

