Certainly! Here's an updated description for the row selection options in the heatmap:

---

## Row Selection Options

**1. Row Selection Options:**

- **Description**
  - Choose the method(s) for selecting rows in the heatmap.

- **Options**
  - _all_: Display all rows in the heatmap.
  - _TopK_: Show the top K rows based on the specified criteria. If rows are ordered, 
    it displays the top K; otherwise, it shows all rows. **Only makes sense in 
    conjunction with "significant_LFC" or "LFC_onlySig" row selection methods.**
  - _significant_LFC_: Display rows sorted based on the significance of log fold changes. Requires specifying control and comparison groups.
  - _LFC_onlySig_: Display rows with significant log fold changes based on a specified significance threshold. Requires specifying control and comparison groups.
  - _rowAnno_based_: Customize row selection based on additional row annotation factors. Select specific factors for tailored row display.

**2. Show Log Fold Changes:**

- **Description:**
  - Decide whether to display log fold changes in the heatmap.

- **Options:**
  - Checkbox input with "Show log Fold Changes" label.
  - Toggle between displaying or hiding log fold changes.

**Note:**
- Optimize row selection options to focus on specific subsets of data, such as significant changes or custom annotation-based criteria.
- Experiment with different row selection methods to highlight patterns and features relevant to your analysis.

***
