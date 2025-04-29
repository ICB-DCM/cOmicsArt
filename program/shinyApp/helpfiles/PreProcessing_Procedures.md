## **Data Preprocessing**  
***  
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more details, visit  
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/03-pre-processing.html" target="_blank" style="font-weight: bold;">this page</a>.
</div>  
<br>

### **Key Steps**  

**Data Preprocessing** is a crucial step, as all downstream analyses rely on pre-processed data. We provide different **Processing Types**, each with specific options. Constant entities across all samples are always removed. Some options require additional parameters.  

### **1. Choose Your Processing Type**  
- **No Pre-processing**  
- **Filtering**  
- **Omic-Specific**  
- **Log-Based**  
- **Miscellaneous**  

All options (except "No Pre-processing") apply minimal **omics-specific filtering** to remove low-abundance entities:  
- **Transcriptomics:** Remove rows where total counts across samples are **≤10**.  
- **Metabolomics/Lipidomics:** Remove rows where the **median value across samples is 0**.  

### **2. Select a Specific Option (Based on Type)**  

#### **No Pre-processing**  
- No changes are applied. Useful if data is already pre-processed.  

#### **Filtering**  
- **Global Filtering:** Removes low-abundance entities by setting a **minimum total count** across samples (e.g., 10 for transcriptomics).  
- **Sample-wise Filtering:** Removes entities that are low in a **minimum number of samples**. Requires setting:  
  - **Minimum count per sample**  
  - **Number of samples meeting this threshold** (e.g., smallest group size in replicates).  

#### **Omic-Specific Processing**  
- **DESeq2:** Applies **variance stabilizing transformation** (VST) for transcriptomics. Requires selecting one or more **factors of interest** (interpreted additively).  
- **limma voom:** Transforms data for linear modeling. Choose **factors of interest** and whether to **include an intercept** (default: yes).  
- **TMM Normalization:** Adjusts for sequencing depth and composition biases in RNA-Seq by **trimming extreme log-fold changes**. Returns **TMM-normalized counts per million (CPM)**.  

#### **Log-Based Scaling**  
- Applies **log transformation** based on the selected logarithm base. If the data contains zeros, **log(x+1)** is used to avoid infinite values.  

#### **Miscellaneous Scaling Methods**  
Useful for already **normalized or pre-processed data**, these methods adjust for **distribution, variability, or scale differences**:  
- **Pareto Scaling:** Divides values by the **square root of the standard deviation**.  
- **Centering & Scaling:** **Subtracts the mean** and **divides by the standard deviation**.  
- **Min-Max Scaling (0-1):** **Rescales values** between **0 and 1** by subtracting the minimum and dividing by the range.  
