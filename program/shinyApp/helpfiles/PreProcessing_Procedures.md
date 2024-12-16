## Data Preprocessing
***  
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/03-pre-processing.html" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

### Key Steps:
1. **Cleaning:**  
   - Remove constant entities and all-zero rows.  

2. **Filtering**  
   - Filter low-abundance entities (based on data type).  

3. **Transformation Options:**  
   - **None/Filter Only:** Skip or apply filtering only, suitable for preprocessed data.  
   - **Scaling:** Normalize data via Center Scaling, Pareto Scaling, or scaling (0-1).  
   - **Logarithmic Transformations:** Apply `natural logarithm`, `log10`, or `log2` 
     (handles zero values by adding +1).  
   - **DESeq2 pre-processing:** For transcriptomics includes variance stabilizing transformation.  

--- 