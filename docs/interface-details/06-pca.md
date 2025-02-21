---
title: "PCA"
layout: default
parent: "Interface Details"
nav_order: 6
---

# PCA

The PCA tab is divided into two main sections: the side panel and the main panel.

## Side Panel 📚

In the side panel, you have the following options:

- **Get PCA**: Clicking this button will generate the PCA plot in the main panel based on the selected annotation type and entities.

- **Toggle "Select Data"**: You can toggle on and off "Select Data". This allows you to quickly select data and perform PCA on the determined subset. Note, that if the selection leads to constant entities within the subset those entities will be automatically removed as they do not contain any information.
  
  - **Choose the annotation type to select on**: You can choose an annotation type for selection, such as `cell`. The options are populated based on the sample annotation provided initially. Precisely it is the column names of the sample annotation.

  - **Choose the entities to use**: Select the entities to include in the PCA. The default option is `all`. You can select all present unique within the selected sample annotation category. Note that you can choose multiple items.

- **Choose the variable to color the samples after**: Below the horizontal line, you can choose a variable to color the samples. These options come from the sample annotations provided initially.

- **PC for x-Axis**: Select the principal component for the x-axis. Options include PC1, PC2, PC3, and PC4.

- **PC for y-Axis**: Select the principal component for the y-axis. Options include PC1, PC2, PC3, and PC4.

- **Plot ellipses**: You can choose to plot ellipses around the groups. These ellipses indicate the 95% confidence interval of the group. Hence, if the are overlapping the groups are not significantly different. Note, to draw such ellipses there need to be enough samples within a single group - if that is not the case the ellipses will not be drawn.

- **Plot Loadings on top**: You can choose to plot the loadings on top of the PCA plot (the top 5 are used). You can hover over them to identify the respective entities. For more help on how to interpret loadings within a PCA, check out [this resource on PCA loadings interpretation](https://towardsdatascience.com/what-are-pca-loadings-and-biplots-9a7897f2e559).

## Main Panel 💡

The main panel displays the PCA results. Here are some key points:

- **Tabs in the Main Panel**: The main panel has several tabs:
  - **PCA_plot**: Displays the main PCA plot. The PCA plot is interactive. You can hover over the points to see additional information. The "Select the anno to be shown at tooltip" option allows you to customize the annotation shown in the tooltip. This is populated by the information provided within the sample annotation.
  - **PCA_Loadings**: Shows the loadings plot. Here the highest loadings in absoulte terms are shown. With the displayed slide bars you can adjust the number of positive/negative loadings to be shown. The loadings are specific for each principal component. To change the principal component, you will need to change in the sidebar the PC for x-Axis. You can also adjust what is shown on the y-axis by selecting the wanted annotation usind the drop down menu. The availbale options are taken from the provided row/entite annotation. 
  - **PCA_Loadings_matrix**: Provides a matrix of the loadings. The matrix is an extension of the Loadings plot. Here, you can select the number of Principle components to include. Additionally, you can select the minim value of loadings an entities as to have for at least one of the principle components. Note, the chosen defaults are often faulty and need to be adjusted if you get no meaningful result at first.
  - **Scree_Plot**: Shows the scree plot. The plot is interactive. You can hover over the points to see the precise variance explained by each principal component.

- **Information Display**: At the top of the main panel, some information is displayed regarding the computation of the PCA.

- **Download Options**: The visualization can be downloaded directly in common formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R code and data. For more information, check out [Interface Details](../interface-details.md).

### Other Notes 📌

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
- **PCA Interpretation**: Observing the PCA plot can give insights into the relationships between different samples based on the selected annotation and the principal components chosen for the axes. For more information on PCA interpretation, check out [this video on PCA interpretation](https://www.youtube.com/watch?v=FgakZw6K1QQ). A common analysis workflow would be to pre-process the entire dataset and visualize it within the PCA plot. Coloring after your supplied metadata may help you to identify if you have any batch effects - large variations within your data that are not due to the biological question you are asking. You may want to save this plot e.g. within the Report for future comparison or simply reference. If you see such effects and want to remove them you might want to go back to pre-processing and select batch correction for the variable introducing the unwanted variation. If you assume it is a combination of variables that might not be within your initially loaded sample table you will need to adjust the sample table and re-upload to cOmicsArt (ensure to click Upload new data) before you can select the new variable as batch. Note that we use ComBat in the background which can deal with complex batches meaning they might not be necessarily linear. You can go back to the PCA, ensure it is updated by clicking 'Get PCA', and see the differences.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Understand how to select and filter your data? → Go to [Data selection](02-selection.md)
- Discover the pre-processing options available? → Go to [Pre-processing](03-pre-processing.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform differential analysis on your data? → Go to [Differential Analysis](05-significance-analysis.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---

