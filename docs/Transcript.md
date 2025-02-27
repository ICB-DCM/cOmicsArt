---
title: "Transcript of Tutorial"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---
Welcome to cOmicsArt - Customizable Omic Analysis and Reporting Tool. This tutorial is going to show you how to use this application to aid in your omic analysis.
This tutorial is self-contained but can also be used to follow along with the YouTube tutorial.

cOmicsArt is designed to be modular. In the beginning you will only be able to see the data selection section, but if you upload your data and go through the steps progressively more tabs will be shown. 
Let us start by uploading our data.

## Uploading Data ##

To upload your data, you'll need three CSV files:  

- **The data matrix**, where rows represent entities (like genes) and columns represent samples. The values contain count data.  
- **The sample table**, which includes metadata about the samples.  
- **The entity table**, which contains additional information about the entities.  

When uploading these files, it's important to ensure that the rows in your data matrix align with those in the entity table and that the columns in the data matrix match the rows in the sample table.  

Note: If you need to access the documentation, you can click on "Go to Documentation" from any page which will bring you to our website with an extensive documentation on what to do.
Another option is to click on the helper buttons wherever they are. They'll show you some kind of help for whatever you're doing.

[Caption of screenshot] This documentation shows you how to upload your data via the file input. 

We choose an example data set which is also available for download:  

- The **data matrix** file: `airway-read-counts-LS.csv`  
- The **sample table** file: `airway-sample-sheet-LS.csv`  
- The **entity table** file: `airway-entity_description-LS.csv`  

When we've uploaded all the files, the first thing we might want to check how is our data interpreted by cOmicsArt. For this we click on "Inspect Data".

Some pitfalls to beware of here are for example if we choose the wrong separator. If that happens, your data might be misread, showing up as a single column instead of a properly formatted table.
At the bottom of the screen there are a multitude of checks where you can check whether your data looks as it should be.
Green signifies that the dataset is fine, whereas red means you need to go into your data and change things and orange means the dataset has some issues but the program can handle it - for example this dataset contains some entries which are missing and the application will just remove those entities from the table.

After the inspection of the data is complete, click on "Upload new data" and now you should see your result in the right hand section of the panel.
An additional thing we can do is adding annotation to the data if we didn't do that already, but since we did this already for the test data we don't need to do it again. 

Next, we can perform row or samples selection:

For the sample selection we first choose among the columns present in the sample table. For example we can choose the cells, condition, experiment or the sample. 
If we select sample, additionally we can select only specific samples we want to include the data analysis. Perhaps there are strong reasons to exclude some samples. By clicking on the specific sample, it removes all other samples and only uses the selection of samples we made.
If we select condition, we can do a similar selection. Here it's treated or untreated but you have multiple conditions you might want to only check one of them. 

For the row selection, the same can be done with genes and you can choose either the gene names here in Ensemble genes, or you might not know those gene names all by heart so you can select the symbols. Again, we can also select which ones we want to use.
You can also use the gene biotype for selection. In transcriptomics you sometimes only want to use the protein coding genes so one can select protein coding and the program would then only use all of the protein coding genes that exists within the data matrix. Similarly you can do the same for processed pseudogenes and it will select all the processed pseudogenes.

In our case we want to use all our data so we just start our journey by clicking on "Start the Journey". 

## Pre-Processing ##

Once we have uploaded the data, clicking on "Start the Journey" will directly bring us to the pre-processing window.

Here you can see the design choice for the layout, which remains consistent for the other the data analysis windows we will cover: 
The left side consists of the sidebar with all the available options. Additionally, the options which need re-computation on being changed are separated from those which do not such re-computation by a separating line.

For example we can choose the vst_DESeq, and then choose the genes. [?]
You can also choose batch effects from the columns already present in your data, but keep in mind that batch effects are usually done later once we realize that there are batch effects.

We want to do a log 10 scaling. To do this, we select the log 10 option followed by the "Get Pre-Processing" button.
This will take a bit of time but once the computation is complete, the results will be shown in the right hand pane. 

In this case a distribution matrix and some statistics show us what changed from the raw data to pre-processed data. 

Once we have pre-processed the data, all the different tabs are now visible.

To illustrate how the options below the bar work, we select condition and the plot updates instantaneously, showing you potential similarities based on the sample condition we selected.

Once you have completed the initial pre-processing step, all the other statistical analysis tabs will now be visible to you. You can switch between any of them at any time to do the analysis.

We will start with Sample Correlation.
[If you want more information on what kind of pre-processing we can do you can click on the helper button or go to the documentation].

## Sample Correlation ##

[Here again on the left side is the sidebar with the available options for the plot]

in this case up above the button things that need recomputing you can always click 
on the help files if you are not quite sure which kind of correlation method for example in this case you want to choose and down below are things that can directly change the plot 

By selecting "Get Sample Correlation" the sample correlation will be plotted and we can see it is already plotting the samples by the cell names. We can also add the condition to check if it plots
 by anything else and see the clusters and patterns.

At the bottom of the screen, each plot we'll have: Save plot, Send Only to Report or Get Underlying R code and data. 

1. Save plot is fairly straightforward - you can choose the file type and by selecting the "Save plot" button, which opens up a window where you can choose a custom file name which then directly 
saves the plot. 
2. Get the underlying R code and data - gives you the exact R code you need to reproduce the the plot or customize it to your liking
3. Send only to Report - this sends the plot along with the options you chose - the correlation method and the color annotation along with notes that you can take manually towards an HTML report to save 
it for later. This enables you to reproduce any kind of findings during your statistical analysis within the application. 

Let's continue with Principal Component Analysis 

## Principal Component Analysis ##

Here you have the choice of selecting specific samples based on the values of the sample annotation to use in the construction of the principal components. 
Selecting data is only advised for looking at variance explained within specific data points. Otherwise it is recommended to use all data get the principal components. 

For now we use all data to get the principal components by clicking on "Get PCA". This displays the principal component plot on the right hand section of the window.

On the left, we have various options to customize the plot. Since these are below the separator line, changing these options does not require any re-computation.
We can select a variable to color the samples in the plot after as well as which principal components we want to see on the X and Y axes. 
If we hover over the plot, at the end of the tool tip we can perhaps see what are the outliers.[?]

Example: Coloring by condition
In this example, we want to color the plot by the variable "condition". One might want to do this to check if clusters exist, based on the conditions.
We can do this by clicking on the box below "Choose the variable to color the samples after" and choosing "condition" from the resulting drop-down menu.
-- [??] --
Here we can see more clustering but it's not the first principle component but the the second one. So we can re-select the principal components that we want to show as well as which axis we want to see it on. 
-- == --

We can switch between the different kinds of plots which are available for the Principal Component Analysis: 

- **The PCA Plot** : This is the main Principal Component Analysis plot.
- **The Loadings Plot** : This is the loadings plot
- **The Loadings Matrix** : This plot provides a matrix of the loadings, which is an extension of the Loadings plot. 
- **he Scree Plot** : This is the scree plot

Now, we switch to the Loading plot. 
This plot shows us the loadings for the principal component that we currently have selected on the X-axis. On changing the principal component displayed on the X-axis, the loadings plot changes as well. 
It is also possible to change the Y-axis label by choosing the sample annotation. We can do this by clicking on the box under "Name loadings after" and selecting the desired annotation from the drop-down menu.

[In addition to the usual Save plot, Send Only to Report or Get Underlying R code and data options below the plot, there are options we can choose to change the plot.
As, before on the left side we have the options to modify the plot and these are separated by the line depending on whether we need to recompute the plot or not and on the right side is the resulting plot]

We now proceed to Differential Analysis.

## Differential Analysis ##

As a first step, we choose the groups, followed by the specific group pairings we want to compare. For example, choosing condition allows comparison across treated and untreated observations. 
We can also choose more than one comparison and the cOmicsArt will directly calculate all the comparisons. We also choose the test method, the significance level as well as the test correction method.

Once we have set-up our parameters, selecting the "Get Significance Analysis" starts the computation for all the comparisons we have selected. 
In case the program has no significant results, the Result Visualisation tab is empty.[?]

Each group paring we compare has its own tab with a table and a volcano plot:

- In the table, we can see raw P values for each entitie (although we have to treat this with caution). You can also sort the table by any column, for example adjusted p values (padj) or p values (pvalues). 
- The Volcano tab shows us the volcano plots with corrected P values and uncorrected P values. We can interact with the plot by choosing different thresholds.

The tables can be downloaded as CSV file, as Excel files or they can be copied to the clipboard, and as in every plot we can send it to the report, get the underlying R code or save the plot.

Example:
We select the following three comparisons: "N052611:N61311", "N080611:N61311", "N061011:N61311". 
For this comparison, when we look at adjusted p-values, the result visualization is currently empty because we have nothing significant. 

But if we choose to look at raw P values by changing the type of significance to "significant unadusted", there is now a plot which shows the genes which are significant in each of the comparisons.
Here, we can see, across all three comparisons 62 genes are significant, and across the first and second comparison we have 105 genes which are significant. 
Additionally, using the option "Intersections to highlight", we can highlight the plot bars.

We move on to Single Gene Visualisation.

## Single Entitie Visualisation ##

We can use Single Entitie Visualisation to look at a specific gene.

First, we select between raw or pre-processed data and then select the gene to visualise from the list of available genes under "Select the Gene from the list".
On selecting the "Get Single Gene Visualisation", we get box plots or dot plots. Box plots will only be visible if you have more than 3 samples per group. If there are fewer than 4 samples, only dots will be displayed. 

You can also group the samples based on the annotation categories by choosing the annotation category from the list of available categories under "Choose the groups to show the data for"

We now move onto the next part of our exploratory analysis.

## Enrichment Analysis ##

There are two types of analysis possible - A gene set enrichment analysis or an over-representation analysis.

For "GeneSetEnrichment Analysis" you will be asked to select which metric to sort your genes after. Here you can choose between logFoldChange or absolute logFoldChange or t-statistic value. 
Following this you would need to specify which two groups to compare against each other.
For "Overrepresentation Analysis" you will have to supply a list of genes, which then will be compared to the gene-set of interest, based on a set of all “possible” gene names.

Irrespective of the type of analysis selected, you will have to select the gene sets to do the analysis for. There are a wide variety of gene sets to choose from. 
The most standard ones known are KEGG, Hallmarks and Reactome. Clicking on the help button next to "Choose sets to do enrichment for" provides a list of all the available genes along with their description.

For each gene set selected a tab will be visible within which you can see the enrichment. 

[Again for the tables you can download the tables and for the plots you have sent only through report get the underlying R code to reproduce this in R directly or save the plot as it is the]

## Heatmap ## [The last analysis?]

We now come to the Heatmap tab. 

The Heatmap displays the count matrix in a clustered fashion and We now can select what entities we want to plot. 

In our example, we only want to plot a the first entities and for this, we select "Select based on Annotation", and "geneName" as the variable to select the rows after and then select the first eight entities.
Selecting "Get Heatmap" displays the heatmap plot on the right.

If no differences are visible in the plot, we can perform row-wise scaling to make the differences more clear. 
Additionally, we can select what do we annotate the samples or the rows after.


[Conclusion?]

The design across all the tabs are the same - Options that are above the separating line on the left hand sidebar will need recomputation things and the options which are below don't and update 
the plot directly. The results always appear on the right hand which can be downloaded as well as sent to the report. 

When we are done with our analysis we can download the report as an HTML file. This HTML file will contain a report on everything you did and all the choices you made, enabling you to recreate 
everything you did in the application making every result reproducible.

[With this I want to thank you for your attention and I hope you enjoyed the application]
