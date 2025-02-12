## Data Preprocessing
***  
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit 
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/03-pre-processing.html" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

### Key Steps:

The Data Pre-processing is a crucial step in data analyis as all the following analysis are based on the pre-processed data.
We provide several Processing types which categorize different pre-processing options. Some of those options have additional arguments you must provide:

1. First choose your ** Processing Type **
  - No - pre-processing
  - Filtering
  - Omic- specific
  - log-based
  - miscellenous

2. Then choose the precise option (dependent on your type)
  - No - pre-processing
    - No pre-processing 
    Nothing is done to the data. This is useful if you have already pre-processed your data.
  - Filtering:
    - Global Filtering:
    Useful to remove low abundant entities.
    Define the minimal amount summed over all samples to keep the respective entitie. For Transcriptomics a common default is 10. 
    - Sample-wise Filtering:
    Useful to remove low abundant entities which are low abundant in a number of samples.
    Define the minimial amount per sample, and the number of samples that have to have that amount. A common default is 10 and smallest group size (e.g.  you have 3 replicates of control and 4 of diseased the smallest group size is 3.)
  - Omic-Specific:
    - DESeq2:
    Apply DESeq2's variance stabilizing transformation. This is useful only for transcriptomics data. You need to choose the factor of interest. You can choose multiple. These will intepreted as additive factors.
    - limma voom:
    Apply limma voom transformation. You need to choose the factor of interest. You can choose multiple. These will intepreted as additive factors. You can also specifc whether you want to include an intercept or not. Most often you want to keep an      intercept (default)


                           "No pre-processing" = c("No pre-processing" = "none"),
                           "Filtering" = c("global Filtering" = "filterOnly",
                                           "sample-wise Filtering" = "filterPerSample"),
                           "Omic-Specific" = c("DESeq2" = "vst_DESeq",
                                               "limma voom" = "limma_voom"),
                           "Log-Based" = c("log10" = "log10", "log2" = "log2", "Natural logarithm" = "ln"),
                           "Miscellaneous" = c("Pareto scaling" = "pareto_scaling",
                                               "Centering & Scaling" = "simpleCenterScaling", 
                                               "Scaling 0-1" = "Scaling_0_1")

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