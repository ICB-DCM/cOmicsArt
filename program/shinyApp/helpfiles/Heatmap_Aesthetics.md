## Heatmap Aesthetics Options

---
**1. Choose Variable to Color the Samples After:**

- **Description:**
  - Select the variable(s) to determine the coloration of samples in the heatmap.
  
- **Options:**
  - Dropdown menu labeled "Choose the variable to color the samples after."
  - Choices include columns (variables) from the dataset.
  - Multiple selections are possible.

**2. Choose Variable to Color the Rows After:**

- **Description:**
  - Select the variable(s) to determine the coloration of rows in the heatmap.

- **Options:**
  - Dropdown menu labeled "Choose the variable to color the rows after."
  - Choices include rows (variables) from the dataset.
  - Multiple selections are possible.

**Note:**
- Coloring the columns and rows allows for an easy detection of patterns in the data.

**3. Column/Row Clustering:**

- **Description:**
  - Enable or disable clustering for columns/rows in the heatmap.

- **Options:**
  - Checkbox labeled "Column/Row Clustering."
  - Default value is set to TRUE (enabled).

- **Note:**
  - Clustering allows to see patterns in the data. As the clustering is data-driven only and overlap with any of your defined categories (e.g., treated and untreated) indicates a data signal to able to distinguish your groups.

**6. Row-Wise Scaling:**

- **Description:**
  - Enable or disable row-wise scaling for the heatmap. Row wise scaling scales each 
    row separately to have a mean of 0 and a standard deviation of 1. This is useful 
    when the rows have different scales and you want to compare clusters in the rows. 
    However, it can also hide important differences between columns due to differences 
    in the row scales.

- **Options:**
  - Checkbox labeled "Row-Wise Scaling."
  - Default value is set to FALSE.

---

