### Data Quality Check - Visual Inspection

***
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/02-selection.html#file-input-%EF%B8%8F" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

Using the `Upload visual inspection` tab, you can see the uploaded data and perform a 
data quality check.
This tab is only available if you have uploaded the necessary data in the `Data Upload 
via file input` tab and does not work for precompiled data.

**Step 1: Upload Matrices and Tables**

- If you haven't uploaded the necessary data, such as the main matrix,
  the sample annotation or metadata, and the entity annotation, an 
  error message will be shown in the first entry. Make sure to upload all three matrices 
  beforehand for a comprehensive inspection.

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

---

