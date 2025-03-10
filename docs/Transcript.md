---
title: "Transcript of Tutorial"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---
Welcome to cOmicsArt - Customizable Omic Analysis and Reporting Tool. 

This tutorial is going to show you how to use this application to aid in your omic analysis. This tutorial is self-contained but can also be used to follow along with the YouTube tutorial.

**Disclaimer** : This tutorial is based on cOmicsArt v.1.0.0. Some parts of the interface for example, where to find some of the options have changed and additional options have been added. 
However the majority of this tutorial can be readily still used.

cOmicsArt is designed to be modular. When you start the application, you will only be able to see the data selection section, but if you upload your data and go through the steps progressively, more tabs will become visible.
Let us start by uploading our data.

## Uploading Data ##

To upload your data, you'll need three CSV files:  

- **The data matrix**, where rows represent entities (like genes) and columns represent samples. The values contain e.g. count data.  
- **The sample table**, which includes metadata about the samples.  
- **The entity table**, which contains additional information about the entities.  

When uploading these files, it's important to ensure that the rows in your data matrix align with those in the entity table and that the columns in the data matrix match the rows in the sample table.  

Note: If you need to access the documentation, you can click on "Go to Documentation" from any page which will bring you to our website with an extensive documentation on what to do.
Another option is to click on the helper buttons wherever they are. This would display a brief documentation for the relevant section.

We now move on to uploading the datasets via the file input. For this tutorial, we will choose the example data set which is also available for download:  

- The **data matrix** file: `airway-read-counts-LS.csv`  
- The **sample table** file: `airway-sample-sheet-LS.csv`  
- The **entity table** file: `airway-entity_description-LS.csv`  

When we've uploaded all the files, the first thing we might want to check how is our data interpreted by cOmicsArt. For this we click on "Inspect Data".

Some pitfalls to beware of here are for example if we choose the wrong separator. If that happens, your data might be misread, showing up as a single column instead of a properly formatted table.

At the bottom of the screen there are a multitude of checks where you can check whether your data looks as it should be. The section describes what each color indicates:

- **Green** - Signifies that the dataset is fine
- **Orange** - Signifies that the dataset has some issues but the program can handle it. For example, this dataset contains some entries which are missing and the application can handle it by removing them the table.
- **Red** - Signifies that some changes are required before the application can correctly handle the dataset

Once the inspection of the data is completed, we click on "Upload new data". If the data was successfully uploaded, a message stating this will be visible on the right hand side.
If some of the required files are missing, a warning will be visible on the right hand side of the window.

Additionally if annotations have not already been added to the data, you can perform this step by clicking on "Add Gene Annotation" which is visible on clicking "Further Entitie annotation options". 
Since this has already performed for the test data we are using for the tutorial, we will skip this step. 

Next, you have the option of selecting specific rows or samples:

- Row Selection - This allows you to select specific rows to work with, depending on the column you initially select. First you select a column under "Which annotation type do you want to select on?", followed by selecting the specific rows to include through "Which entities to use?"
In transcriptomics you sometimes only want to use the protein coding genes. To do this, you can choose gene_biotype as the annotation, followed by protien_coding as the entitie. The program would then only use all of the protein coding genes that exists within the data matrix.
Similarly you can do select processed_pseudogenes and the program will use all the processed pseudogenes.

- Sample selection - This allows you to choose among the columns present in the sample table. For example, you can select cells, condition, experiment or the sample column. Depending on the column selected, additionally you can select only specific samples which you want to include the data analysis. 
One may want to do this if there are strong reasons to exclude some samples. By clicking on the specific sample, the program only uses the selection of samples you make.
Suppose you want to include only treated or only untreated samples. You could do this by selecting condition as annotation, followed by the either selecting "trt" to include treated samples or "untrt" to only keep untreated samples.

In our case we want to use all our data. So we can start our journey by clicking on "Start the Journey". 
This will directly bring us to the pre-processing window.

## Pre-Processing ##

**A note on the layout**
The layout of the analysis tabs is as follows:
The left side consists of the sidebar with all the available for plotting and modifying the plot. Within these, the options which need re-computation on being changed are separated from those which do not such re-computation by a separating line.
Once a plot is computed it will be visible on the right side of the screen.  

There are a multitude of pre-processing options available. For more information on each of these options, you can click on the helper button to get a detailed description.
It is also possible to choose batch effects from the columns already present in your data, but keep in mind that batch effects are usually done later when one realizes that there are batch effects.

Example:
For the tutoral, we want to do a log 10 scaling. To do this, we select the log 10 option followed by the "Get Pre-Processing" button.
In this case a distribution matrix and some statistics show us the changes which took place when we transition from the raw data to pre-processed data.

To illustrate how the options below the bar work, we will choose to color the violin plot by the condition variable. 
Since this does not require any additional computation, simply changing the option will update the plot instantaneously.

**A note on further options for plots**
At the bottom of the screen for each of the statistical analysis tabs, each plot will have three options:

- **Save plot** - This option is fairly straightforward. Here you need to choose the file type you want the plot to be saved as, followed by selecting the "Save plot" button. This will open up a window where you can choose a custom file name, and them directly save the plot to your local machine in any directory of your choice.
- **Get the underlying R code and data** - This option gives you the exact R code you need to reproduce the the plot or even further customize it to your liking.
- **Send only to Report** - This option will send the plot along with the options you chose on the left hand panel during analysis, along with any notes that you can take manually towards an HTML report which you can save to your local machine later. This enables you to reproduce any kind of findings during your statistical analysis within the application. 

Once you have completed the initial pre-processing step, all the other statistical analysis tabs will now be visible. You can switch between any of them at any time to do your analysis.
We will now proceed with Sample Correlation.

## Sample Correlation ##

There are three different types of correlation methods to choose from. To get more information on the different types correlation available, you can click on the helper button.
Once the correlation method has been selected, clicking the ""Get Sample Correlation" button will display the heatmap on right hand side of the window.

Example:
In this tutorial we choose the pearson method as the correlation method and additionally choose to color the samples by the cell names.
It is also possible to select multiple options for coloring of the rows which might help in finding clusters and patterns.

Next up, we look at Principal Component Analysis.

## Principal Component Analysis ##

Here you have the choice of selecting a specific subset to perform analysis on. To create the subset, you would first need to select the annotation type, followed by selecting the entities on which you want to perform the principal component analysis.
**Caution**: Selecting data is only advised for looking at variance explained within specific data points. Otherwise it is recommended to use all data get the principal components. 

Example:
For our example, we want to use the entire dataset. Clicking on "Get PCA" will display the principal component plot on the right hand section of the window.

There are four different kinds of plots which are available: 

- **The PCA Plot** : This is the main Principal Component Analysis plot.
- **The Loadings Plot** : This is the loadings plot which shows the highest loadings in absolute terms.
- **The Loadings Matrix** : This plot provides a matrix of the loadings, which is an extension of the Loadings plot. 
- **The Scree Plot** : This is the scree plot.

We focus on the options available for customizing PCA the plot.
You can select a variable to color the samples after as well as select which principal component we want to see on the X and Y axes. 
It is also possible to select an annotation and by hovering over the plot, observe the value of this annotation at each point [Is it point or entitie?] on the plot.

In our example, we want to color the plot by the variable "condition", since one might want to do this to check if clusters exist based on the conditions.
We can do this by clicking on the box below "Choose the variable to color the samples after" and selecting "condition" from the resulting drop-down menu.

Now, we switch to the Loadings plot. 
The loadings plot shows us the loadings for the principal component that we currently have selected on the X-axis. On changing the principal component displayed on the X-axis, the loadings plot changes as well. 
It is also possible to change the Y-axis label of the loadings plot by changing the annotation. This is done by clicking on the box under "Name loadings after" and selecting the desired annotation from the drop-down menu.

We now proceed to Differential Analysis.

## Differential Analysis ##

As a first step, you need to choose the groups for which you want to perform differential analysis. Then you need to select the specific group pairings you want to compare. 
For example, choosing condition as the group allows comparison across treated and untreated observations. You can also choose multiple group pairings and cOmicsArt will directly calculate all the comparisons. 
You would also need to choose the test method, the significance level as well as the test correction method.

Once you have set-up our parameters, selecting the "Get Significance Analysis" starts the computation for all the comparisons that have been selected.

For each selected comparison, a separate tab is created with a table and a volcano plot:

- The table displays raw p-values for each entitie (although we have to treat this with caution). You can also sort the table by any column, for example adjusted p-values (padj) or p-values (pvalues). 
- The volcano tab displays the volcano plots with corrected p-values and uncorrected p-values. You can adjust the plot by choosing different threshold levels.

In addition to the usual options available for the volcano plot, the tables can be also downloaded as CSV file, as Excel files or they can be copied to the clipboard.

Additionally, if more than one comparisons are selected, an additional Result Visualization tab will be displayed. This tab will show similarities and differences between the groups you compared. 
However, if there are no significant results or only one compression has significant results, the Result Visualization tab will be empty.

Example:
For our tutorial, the cell group is selected and the following group pairings are selected for comparison: "N052611:N61311", "N080611:N61311", "N061011:N61311". 
In this comparison, when we look at adjusted p-values, the result visualization is empty because we have no significant similarities or differences between the group pairings we compare.
However, if we choose to look at raw p-values by changing the type of significance to "significant unadjusted" instead of "significant", a plot is now generated which shows the genes which are significant in each of the comparisons.
Across all three comparisons 62 genes are significant, and across the first and second comparison we have 105 genes which are significant. 

By using the option "Intersections to highlight", we can also highlight the plot bars.

We move on to Single Gene Visualisation.

## Single Entitie Visualisation ##

You have the option to select between raw and pre-processed data. Once this is selected, you need to choose the annotation to select the entitie from, followed by the gene to visualise from the list of available genes under "Select the Gene from the list".
Once all the parameters are set, selecting the "Get Single Gene Visualisation"  will generate the visualization on the right hand pane based on the selected options.
Note: Box plots will only be visible if you have more than 3 samples per group. If there are fewer than 4 samples, only dots will be displayed. 

You can also group the samples based on a specific annotation category. This can be done by choosing the annotation from the list of available annotation categories under "Choose the groups to show the data for".

Next part of our tutorial covers Exploratory Analysis.

## Enrichment Analysis ##

There are two types of analysis possible - Gene Set Enrichment (GSEA) and Overrepresentation-Analysis (ORA):

- For Gene Set Enrichment Analysis you will be asked to select which metric to sort your genes after. Here you can choose between logFoldChange or absolute logFoldChange or t-statistic value. 
Following this you would need to specify which two groups to compare against each other using the options under "Choose type for LFC-based ordering", "Choose reference of log2 FoldChange", and "Choose treatment group of log2 FoldChange".

- For "Overrepresentation Analysis" you will need to supply a list of genes, which then will be compared to the gene-set of interest, based on a set of all “possible” gene names.

Irrespective of the type of analysis selected, you will have to select the gene sets to do the analysis for. There are a wide variety of gene sets to choose from. The most standard ones known are KEGG, Hallmarks and Reactome. 
Clicking on the help button next to "Choose sets to do enrichment for" provides a list of all the available genes along with their description.

Once the parameters are selected, clicking "Get Enrichment Analysis" will generate the results. For each gene set selected a tab will be visible.
If any genes were found significant, a standard plot of an enrichment analysis will be visible in the tab for this gene set.

Lastly we proceed to the Heatmap tab. 

## Heatmap ## [The last analysis?]

The Heatmap displays the count matrix in a clustered fashion and you can now select what entities you want to plot. 

Example:
In our example, we only want to plot the first eight entities. To achieve this, we select "Select based on Annotation" under "Select Entities to show", followed by selecting "geneName" as the variable to select the rows after. 
Now we select the the first eight entities, followed by selecting "Get Heatmap" which displays the heatmap plot on the right.

If no differences are visible in the plot, it is possible to perform a row-wise scaling operation to make the differences clearer. Additionally, you can select what to annotate the samples after or the rows after.

## Wrapping Up ##

When you have completed your analysis, you can download the report as an HTML file. This HTML file will contain a report on everything you did and all the choices you made, enabling you to recreate all results displayed in the application during your session, making every result reproducible.

[With this I want to thank you for your attention and I hope you enjoyed the application][Keep?]
