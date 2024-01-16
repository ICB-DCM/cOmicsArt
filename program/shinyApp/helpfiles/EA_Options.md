## Enrichment Analysis Options

---
In cOmicsArt you can do either a gene set enrichment analysis or an over-representation analysis. The options for both analyses are described below.
For more details read here on
[Gene Set Enrichment Analysis](https://www.pnas.org/doi/abs/10.1073/pnas.0506580102) 
and/or [Over-Representation Analysis](https://doi.org/10.1093/bioinformatics/bth456).

You can choose between the two analyses by selecting either of the two below `Choose 
type of Analysis.`

<h3>Gene Set Enrichment Options</h3> 
<details>
<summary>Click here</summary>
<br>

**1. Specify Organism:**
   - Choose the organism for which the enrichment analysis will be performed.
   - Dropdown menu labeled "Specify your current organism."
   - Choices include "hsa" (Human) or "mmu" (Mouse).

**2. Choose Metric for Gene Sorting (GeneSetEnrichment):**
   - Select the metric to sort genes after performing Gene Set Enrichment.
   - Dropdown menu labeled "Select the metric to sort the genes after."
   - Choices include "LFC_abs" (absolute Log Fold Change) or "LFC" (Log Fold Change).
   - Single selection only.

**3. Choose Comparison to sort by LFC:**
   - Select annotation type and groups to calculate LFC and sort accordingly.
   - Dropdown menus labeled "Choose type for LFC-based ordering," "Choose reference of log2 FoldChange," and "Choose treatment group of log2 FoldChange."
   - Choices are based on sample annotations from the dataset.

**4. Choose Gene Sets for Enrichment:**
   - Select the gene sets for which enrichment analysis will be performed.
   - Dropdown menu labeled "Choose sets to do enrichment for."
   - Choices include various gene set collections like KEGG, GO, Hallmarks, etc.
   - See the help icon next to the dropdown menu for more details on the sets.
   - Multiple selections allowed.

**5. Test Correction Method:**
   - Choose the test correction method for the enrichment analysis. For more details 
     on the correction methods, click on the help icon at `Significance Analysis`.
   - Dropdown menu labeled "Test correction."
   - Choices include "None," "Bonferroni," "Benjamini-Hochberg," "Benjamini Yekutieli," "Holm," "Hommel," "Hochberg," and "FDR."
   - Single selection only.
   
</details>

<h3>Over-Representation Analysis Options</h3>
<details>
<summary>Click here</summary>
<br>

**1. Specify Organism:**
   - Choose the organism for which the enrichment analysis will be performed.
   - Dropdown menu labeled "Specify your current organism."
   - Choices include "hsa" (Human) or "mmu" (Mouse).

**2. Choose Gene Set for Over-Representation Analysis:**
   - Select the gene sets for which enrichment analysis will be performed.
   - Dropdown menu labeled "Choose sets to do enrichment for."
   - Choices include various gene set collections like KEGG, GO, Hallmarks, etc.
   - See the help icon next to the dropdown menu for more details on the sets.
   - Multiple selections allowed.

**3. Provide Geneset for Over-Representation Analysis:**
 - Provide a custom gene set for over-representation analysis.
 - Dropdown menu labeled "Choose a gene set to hand over to enrich."
 - Options
   - ProvidedGeneSet: Upload a custom gene set file for over-representation analysis 
     in the file upload below.
   - HeatmapGenes: Use the genes from the heatmap for over-representation analysis.

**4. Upload Gene Set for Over-Representation Analysis:**
   - Upload a custom gene set file for over-representation analysis.
   - File upload input labeled "Select a file (.csv, 1 column, ENSEMBL, e.g., ENSMUSG....)"
   - Visible when "ProvidedGeneSet" is selected.

**5. Select Universe for Over-Representation Analysis:**
   - Choose the universe for over-representation analysis.
   - Dropdown menu labeled "Select a Universe for enrichment."
   - Choices include "default," "allPresentGenes_after_pre_process," or "allPresentGenes_before_pre_process."
   - Single selection only.

**6. Test Correction Method:**
   - Choose the test correction method for the enrichment analysis. For more details 
     on the correction methods, click on the help icon at `Significance Analysis`.
   - Dropdown menu labeled "Test correction."
   - Choices include _None_, _Bonferroni_, _Benjamini-Hochberg_, _Benjamini Yekutieli_,
     _Holm_, _Hommel_, _Hochberg_ and _FDR_.
   - Single selection only.
   
</details>


