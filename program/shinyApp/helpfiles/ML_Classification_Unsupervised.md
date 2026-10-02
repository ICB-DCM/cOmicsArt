## Unsupervised Learning (k-means Clustering)

***

k-means clustering discovers natural groupings in your data without using condition labels. It groups samples with similar expression patterns together.

**Number of clusters (k):**

- **Description:** How many groups to divide samples into
- **Range:** 2-10 clusters
- **Tips:**
  - Start with 2-3 clusters for simple exploratory analysis
  - Try different k values to see which gives most meaningful groupings
  - Check if clusters correspond to known biological conditions or reveal batch effects

**Gene Filtering:**

- **Filter to most variable genes:**
  - Reduces noise by focusing on genes that vary most across samples
  - Speeds up computation for large datasets
  - Recommended: Use 500-1000 genes for typical RNA-seq datasets

**Interpretation:**

- Examine the PCA plot colored by cluster assignments
- Download cluster assignments table to see which samples group together
- Compare clusters to known conditions to validate or discover patterns
- Large, well-separated clusters suggest strong biological signal

---
