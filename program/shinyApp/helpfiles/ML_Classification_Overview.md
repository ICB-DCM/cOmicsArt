## ML Classification Options

***
<div style="border: 2px solid #ffcf30; background-color: #fff0bf; padding: 10px; border-radius: 8px; font-size: 15px;">
<span style="font-size: 20px;">💡</span>  <strong>Tip:</strong> For more detailed information, please visit
<a href="https://icb-dcm.github.io/cOmicsArt/interface-details/10-ml-classification.html" target="_blank" style="font-weight: bold;">this page</a>.
</div>
<br>

Machine learning classification helps you discover patterns in your data and test hypotheses about sample groupings.

**Analysis Type:**

- **Unsupervised Learning:** Discover natural groupings in your data without using condition labels. Use this when you want to explore if samples naturally cluster together based on their expression patterns.

- **Supervised Learning:** Test whether samples can be accurately classified based on known condition labels. Use this to evaluate if expression patterns are strong enough to distinguish between experimental groups.

**When to use each approach:**

- **Unsupervised (k-means):**
  - Exploratory analysis to discover unexpected sample groupings
  - Check if samples cluster by batch effects or biological conditions
  - Identify outlier samples
  - Useful when you don't have strong prior hypotheses

- **Supervised (SVM):**
  - Validate that conditions have distinct molecular signatures
  - Test classification accuracy for known experimental groups
  - Assess if differences are large enough to reliably separate samples
  - Useful for confirming expected biological differences

---
