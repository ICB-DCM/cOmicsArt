## Correlation method

***

Choose **one** of the following correlation methods. These correlation methods provide 
insights into different aspects of relationships between variables. Understanding 
their characteristics helps in choosing the most appropriate method for a given 
analysis. For a brief overview on the methods, see [this article on correlation 
methods](https://en.wikipedia.org/wiki/Correlation_coefficient).

**Pearson Correlation**

**Definition:**
- Pearson correlation measures the linear relationship between two continuous variables.
- It quantifies the degree to which a change in one variable corresponds to a change in another.
  
**Properties:**
- Ranges from -1 to 1.
- 1 indicates a perfect positive linear relationship.
- -1 indicates a perfect negative linear relationship.
- 0 indicates no linear relationship.

**Assumptions:**
- Assumes a linear relationship.
- Sensitive to outliers.

**Spearman Correlation**

**Definition:**
- Spearman correlation assesses the monotonic relationship between two variables.
- It measures the strength and direction of the monotonic relationship.

**Properties:**
- Ranges from -1 to 1.
- Similar to Pearson, but not restricted to linear relationships.
- 1 indicates a perfect positive monotonic relationship.
- -1 indicates a perfect negative monotonic relationship.
- 0 indicates no monotonic relationship.

**Assumptions:**
- Does not assume a linear relationship.
- Robust to outliers.

**Kendall Correlation**

**Definition:**
- Kendall correlation (tau) measures the strength and direction of the ordinal association between two variables.
- It assesses the similarity of the orderings of data points.

**Properties:**
- Ranges from -1 to 1.
- 1 indicates a perfect positive agreement in rankings.
- -1 indicates a perfect negative agreement in rankings.
- 0 indicates no agreement in rankings.

**Assumptions:**
- Non-parametric measure suitable for ordinal data.
- Robust to outliers.

**Choosing the Right Method**

**Considerations:**
- Choose Pearson for linear relationships.
- Choose Spearman for monotonic relationships, especially in the presence of outliers.
- Choose Kendall for ordinal data or when assumptions of linearity or monotonicity are violated.

**Application:**
- Select the correlation method based on the nature of the data and the relationships you want to capture.

**Note:**
- These correlation methods provide insights into different aspects of relationships between variables. Understanding their characteristics helps in choosing the most appropriate method for a given analysis.
