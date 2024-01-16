## Principal Component Analysis (PCA) Options

***
**1. Coloring Options:**

- **Description:**
  - Choose a variable to color the samples in the PCA plot.

- **Options:**
  - Select from available variables in the dataset's sample annotation.

**2. Select Principal Components for Axes:**

- **Description:**
  - Choose the principal components for the x-axis and y-axis in the PCA plot.

- **Options:**
  - "PC1," "PC2," "PC3," "PC4": Select from the available principal components.

**3. Show Loadings on Top:**

- **Description:**
  - Decide whether to plot loadings on top of the PCA plot.

- **Options:**
  - "Yes": Display loadings on top (currently top 5).
  - "No": Do not display loadings on top.

**Data Selection UI:**

Here you can select specific samples to use in the construction of the principal 
components. You can select samples based on the values of the sample annotation. 
**Disclaimer**: If you select your data, the resulting PCA will try to explain 
differences between **only** the selected samples. These results may be governed by 
overall differences and not only specific differences between the selected samples. 
Additionally these PCAs are not comparable to other PCAs comparing different and/or 
all samples and should be **interpreted with caution**. They are however usefull to 
see differences in subgroups that may be dominated by larger differences in the data.

**5. Sample Annotation Types for Data Selection:**

- **Description:**
  - Choose the annotation type for selecting samples in the PCA plot.

- **Options:**
  - Select from available annotation types in the dataset.

**6. Sample Selection:**

- **Description:**
  - Choose specific entities or include all for the selected annotation type.

- **Options:**
  - "all": Include all entities.
  - Select specific entities from the chosen annotation type.

---

