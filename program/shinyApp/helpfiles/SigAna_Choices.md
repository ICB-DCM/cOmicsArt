## Significance Analysis Options
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/05-significance-analysis.html#main-panel-" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

1. **Factor to group samples**  
   - Choose the factor from among your sample annotation columns to group samples by - these are precisely the column names from your uploaded sample data
     for the analysis (e.g., "Condition").

2. **Comparisons**  
   - Choose specific group pairings (e.g., "Treatment:Control") for analysis. The 
     options will be all combinations of unique values in the chosen factor.

3. **Test Method**  
   - Options depend on preprocessing:
     - **DESeq:** Wald test.  
     - **Other methods:** Wilcoxon, T-Test, or Welch-Test.

4. **Significance Level**  
   - Set alpha (default: 0.05) via a slider (range: 0.005–0.1).

5. **Test Correction**  
   - Adjust p-values for multiple testing using methods like Bonferroni, Benjamini-Hochberg, or FDR. 

---
