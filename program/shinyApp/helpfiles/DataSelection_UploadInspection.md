### Data Quality Check - Visual Inspection

***

Using the `Upload visual inspection` tab, you can see the uploaded data and perform a 
data quality check.
This tab is only available if you have uploaded the necessary data in the `Data Upload 
via file input` tab and does not work for precompiled data.

**Step 1: Upload Matrices and Tables**

- If you haven't uploaded the necessary data, such as the main matrix (`data_matrix1`),
  the sample annotation or metadata (`data_sample_anno1` or `metadataInput`), and the 
  entity annotation (`data_row_anno1`), an error message will guide you. Make sure to 
  upload all three matrices beforehand for a comprehensive inspection.

**Step 2: Visualize Uploaded Data**

- The application will read and display the uploaded data, including the main matrix, 
  sample table, and entity table. You can visually inspect these tables using 
  interactive data tables. This helps in making sure, that `cOmicsArt` has read the 
  data correctly and that the data is in the expected format.

**Step 3: Data Quality Checks**

- The application performs various checks on the uploaded data to ensure its quality.
  - It verifies if the main matrix is a valid CSV file.
  - It checks if the row names of the matrix match those of the entity table.
  - It ensures that the column names of the matrix match the row names of the sample table.
  - Checks are conducted for the presence of missing values in the matrices and tables.

**Step 4: Display Overall Checks**

- The results of the checks are displayed in a formatted text section, summarizing the overall data quality. Each check is marked as "Yes" (passed) or "No" (failed), and additional information is provided if any issues are detected.

**Note:**
- The application dynamically adjusts based on the uploaded data and guides you through the visual inspection process. If any checks fail, helpful suggestions or information are presented to assist you in resolving potential issues.

---

