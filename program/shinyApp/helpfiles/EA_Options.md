## Enrichment Analysis Options

---
💡 **Tip**: For more detailed information, please visit <a href="https://icb-dcm.github.io/cOmicsArt/interface-details/enrichment-analysis.html" target="_blank">here</a>.

In cOmicsArt you can do either a gene set enrichment analysis or an over-representation analysis. The options for both analyses are described below.
For more details read here on
[Gene Set Enrichment Analysis](https://www.pnas.org/doi/abs/10.1073/pnas.0506580102) 
and/or [Over-Representation Analysis](https://doi.org/10.1093/bioinformatics/bth456).

**Choose an organism**
- You need to select the correct organism from which your data is derived. This is used to translate and harmonize the entities labels to entrez gene ids.
- Choices include Human and mouse, in the brackets you see the exact version used. 

Now, you can choose between the two analyses by selecting either of the two below `Choose 
type of Analysis.` Depending on your choice different options need to be set.


<h3>Gene Set Enrichment Options</h3> 
<details>
<summary><span style="color:blue">Click here</span></summary>
<br>

**1. Choose Metric for Gene Sorting:**
   - The GSEA takes a ranked list as input. The ranking can be done on different metrics. Which metric to use varies on the context
   - The possible options are "Log Fold change", "absolute LogFold change" and "t-statistic value"
    - Log Fold change: The log2 fold change of the gene expression between the two groups specified further below. Note, taht no arbitrary cutoff  based on significance level is applied.
    - Absolute LogFold change: The absolute value of the log2 fold change of the gene expression between the two groups specified further below. Note, that no arbitrary cutoff  based on significance level is applied.
    - t-statistic value: The t-statistic value of the gene expression between the two groups specified further below. Note, that here no effect size is taken into account. Still, a positive t-statistic value indicates that the gene is upregulated in the treatment group, while a negative value indicates that the gene is downregulated in the treatment group.

**2. Choose type for LFC_based ordering:**
   - Here, you select the annotation type (options are the names of the columns within your supplied sample annotation). 
   - This selection will be used to determine the options for **treatment group** and the **reference group** for the log2 fold change calculation.
   - Note, you need at least 2 samples per group to calculate the log2 fold change.

**3. Choose Gene Sets for Enrichment:**
   - Select the gene sets for which enrichment analysis will be performed.
   - Choices include various gene set collections like KEGG, GO, Hallmarks, etc.
   - See the help icon next to the dropdown menu for more details on the sets.
   - Multiple selections allowed.

**4. Test Correction Method:**
   - Choose the test correction method for the enrichment analysis. 
   - Choices include "None," "Bonferroni," "Benjamini-Hochberg," "Benjamini Yekutieli," "Holm," "Hommel," "Hochberg," and "FDR."

</details>

<h3>Over-Representation Analysis Options</h3>
<details>
<summary><span style="color:blue">Click here</span></summary>
<br>

**1. Choose Gene Sets for Enrichment:**
   - Select the gene sets for which enrichment analysis will be performed.
   - Choices include various gene set collections like KEGG, GO, Hallmarks, etc.
   - See the help icon next to the dropdown menu for more details on the sets.
   - Multiple selections allowed.

**2. Choose a gene set to hand over to enrich:**
 - ORA takes a gene set of interest into account, which needs to be specified here.
 - Options:
   - ProvidedGeneSet: Upload a custom gene set file for over-representation analysis 
     in the file upload below. This should be a .csv file with a single column of gene ids.
   - HeatmapGenes: Use the genes from the heatmap for over-representation analysis. Note, that
   for this option it is necassary to press respective button with the Heatmap tab!

**3. Upload Gene Set for Over-Representation Analysis:**
   - Upload a custom gene set file for over-representation analysis.
   - File upload input labeled "Select a file (.csv, 1 column, ENSEMBL, e.g., ENSMUSG....)"
   - Visible when "ProvidedGeneSet" is selected.

**4. Select an Universe for Enrichment:**
   - Choose the universe for over-representation analysis. As ORA check if you set of interest is enriched vs what you expect you need to specify the 'normal' situation, called the universe.
   - Options:
     - default: The default universe is all genes in the organism selected (clusterProfiler's default) - Not recommended if you performed e.g. an omic experiment hence actually profiling the universe of genes.
     - after_pre_process: The universe is all genes that are present in the data after pre-processing.
     - before_pre_process: The universe is all genes that are present in the data before pre-processing.
   - Single selection only.

**5. Test Correction Method:**
   - Choose the test correction method for the enrichment analysis. 
   - Choices include "None," "Bonferroni," "Benjamini-Hochberg," "Benjamini Yekutieli," "Holm," "Hommel," "Hochberg," and "FDR."

   
</details>


