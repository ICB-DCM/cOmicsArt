## Supervised Learning (SVM Classification)

***

Support Vector Machine (SVM) classification tests whether samples can be accurately classified based on known condition labels.

**Condition Column:**

- **Description:** Select which experimental condition to use for classification
- **Requirements:** Must be categorical (not numeric)
- **Tips:**
  - Choose the main experimental variable (e.g., "treatment", "genotype")
  - Works best with 2-3 distinct conditions
  - Avoid columns with too many unique values or unbalanced groups

**Gene Filtering:**

- **Filter to most variable genes:**
  - Focuses classification on genes most likely to differ between conditions
  - Reduces overfitting and speeds computation
  - Recommended: Use 500-1000 genes for typical datasets

**Interpretation:**

- **Accuracy:** Percentage of correctly classified samples
  - >90%: Strong separation, conditions have very distinct signatures
  - 70-90%: Moderate separation, some overlap between conditions
  - <70%: Weak separation, conditions may not be molecularly distinct

- **Confusion Matrix:** Shows which conditions are confused with each other

- **Decision Boundary Plot:** Visualizes separation in PCA space
  - Well-separated regions = clear molecular differences
  - Overlapping regions = similar expression patterns

**Important Note:**

This implementation trains and tests on the same data (no train/test split). Accuracy may be optimistic. For rigorous classification, use proper cross-validation in downstream R analysis.

---
