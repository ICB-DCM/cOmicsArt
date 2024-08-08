---
title: "Sample Correlation"
layout: default
parent: Interface Details
nav_order: 3
---

# Sample Correlation

The Sample Correlation tab is divided into two main sections: the side panel and the main panel.

## Side Panel

In the side panel, you have the following options:

- **Choose the correlation method**: You can select from Pearson, Spearman, or Kendall correlation methods.
  - [Understanding Correlation Methods](https://ademos.people.uic.edu/Chapter22.html) provides a detailed explanation of each method.
  - **When to use:**
    - **Pearson**: Use this when you assume a linear relationship between your variables and both variables are normally distributed.
    - **Spearman**: Use this when your data is not normally distributed or you have ordinal variables.
    - **Kendall**: Use this when you have small sample sizes or need to handle ties in your data.

- **Get Sample Correlation**: Clicking this button will generate the heatmap in the main panel based on the selected correlation method.

- **Choose the color annotation for the samples**: Below the horizontal line, you can choose different options to color the rows. These options come from the sample annotations provided initially. This can help to potentially explain clustering behavior observed in the heatmap.

  More details on these options can be found under [Required Data Input](required-data-input.md) or [Showcase A](../showcases/showcase-a.md).

## Main Panel

The main panel displays the correlation heatmap. Here are some key points:

- **Information Display**: At the top of the main panel, some information is displayed regarding the computation of the correlation matrix.
- **Heatmap Visualization**: The heatmap provides a visual representation of the sample correlation based on the selected method and coloring option.
- **Download Options**: The visualization can be downloaded directly in common formats (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R code and data. For more information, check out [Interface Details](interface-details.md).
- **Non-Interactive Plot**: Note that this plot is not interactive.

### Other Notes

- **Question Marks**: The displayed question marks provide quick and immediate help. However, since you are reading this documentation, you found the extensive version. Hope it helped!
- **Clustering Behavior**: Observing the clustering in the heatmap can give insights into the relationships between different samples based on the selected annotation.

