---
title: "Transcript of Tutorial"
output: html_notebook
---
Welcome to cOmicsArt - Customizable Omic Analysis and Reporting Tool. This tutorial is going to show you how to use this application to aid in your statistical analysis.
This tutorial is self-contained but can also be used to follow along with the YouTube tutorial.

Comics art is designed is in a tabular format. In the beginning you only see the data selection but if you upload your data and go through the steps progressively more 
tabs will be shown. Let us start by uploading our data.

## Uploading Data ##

The data will be uploaded with three CSV files. 
1. The data matrix - shows the counts on the rows there are the entities - for example genes and on the columns they are the samples. 
2. The sample table - shows meta information on the samples
3. The entity table - shows any information on the entities

Note : If you need to access the documentation, you can click on "Go to Documentation" from any page which will bring you to our website with an extensive documentation on what to do.
Another option is to click on the helper buttons wherever they are. They'll show you some kind of help for whatever you're doing.

[Caption of screenshot] This documentation shows you how to upload your data via the file input. 

It is important that the rows of the data matrix correspond to the rows of the row annotation while the columns of the data matrix correspond to the rows of the sample table.

We choose an example data set which is also available for download. [For the data matrix we choose the "airway-read-counts-LS.csv" file, for sample annotation we upload the 
"airway-sample-sheet-LS.csv" file, and for entities annotation we upload the "airway-entitie_description-LS.csv" file.]

When we've uploaded all the files, the first thing we might want to check how is our data interpreted in this program. For this we click on "Inspect Data" followed by an additional 
click on "Upload data for visual inspection".

Some pitfalls to beware of here are for example if we choose the wrong separator then it will only have one column and you see that this will be interpreted as wrong.
At the bottom of the screen there are a multitude of checks where you can check whether your data looks as it should be.
Green signifies that the dataset is fine, whereas red means you need to go into your data and change things and orange means the dataset has some issues but the program can handle it - for example 
this dataset contains some entries which are missing and the application will just remove those entities from the table.

After the inspection of the data is complete, click on "Upload new data" and now you should see your result in the right hand section of the pane.
The first thing we can do is we can add annotation by choosing the organism to add the annotation. to the data if we didn't do that already, but since we did this already so we don't need to do it again. 

Next we choose row or samples:

For the sample selection we first choose among the columns present in the sample table. For example we can choose the cells, condition, experiment or the sample. 
If we select sample, additonally we might want to select only specific samples we want to include the data. Perhaps there are strong reasons to exclude some and by clicking on the specific 
sample, it removes all other samples and will only uses these data sets.
If we select condition, we can do a similar selection. Here it's treated or untreated but you have multiple conditions you might want to only check one of them. 

For the row selection, the same can be done with genes and you can choose either the gene names here in Ensemble genes, or you might not know those gene names all by heart so you can select the symbols 
and we can also select which ones we want to use.
You can also use the gene biotype and which shows you functionals. For example in transcriptomics you sometimes only want to use the protein coding genes so we can select protein coding and the program 
would thenonly use all of the protein coding genes that exists have within the data matrix. Similarly you can do this same for processed pseudo genes and it will select all the processed pseudo genes.

In our case we want to use all our data so we just start our journey by clicking on "Start the Journey". 

## Pre-Processing ##

Clicking on this button will directly bring us to the pre-processing window.

Here you can see the design choice - on the left side we have the sidebar with all the available options. Options which need re-computation are separated from those which do not by a black line.

For example we can choose the vst_DESeq, and then choose the genes. [?]
You can also choose batch effects from the columns already present in your data, but keep in mind that batch effects are ususally done later once we realize that there are batch effects.

In our case we want to do a log 10 scaling so we select the log 10 option followed by the "Get Pre-Processing" button.
This will take a bit of time but once the computation is complete, the results will be shown in the right hand pane. 

In this case a distribution matrix and some statistics show us what changed from the raw data to pre-processed data. 

Once we have pre-processed the data, all the different tabs are now visible.

To illustrate how the options below the bar work, we select condition and the plot updates instantaneously, showing you potential similarities based on the sample condition we selected.

Note that now since all the tabs are visible we can always switch between any of them to do the analysis and switch to any other.

We we'll start with sample correlation.

[If you want more information on what kind of pre-processing we can do you can click on the helper button or go to the documentation].

## Sample Correlation ##

Here again on the left side is the sidebar with the available options for the plot. 

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

Let's continue with Principal component analysis 

## Principal Component Analysis ##

We have the choice of selecting data, but for now we just get the principal components. This opens the principal component plot on the right hand pane.

Selecting data which is only advised for looking at variance explained within specific data points. Otherwise it is recommended to just get all the data get the principal components as you know it. 
Below the seperator line, we can select what do we want to color them after, what kind of principal components do we want to see. We can also select what we want to show at the end of the tool tip when 
we hover over the plot see pheraps what are the outliers.

Coloring by condition:
If we want to color it by the condition because if we had treated and untreated we want to see if we see a clustering. Here we can see more clustering but it's not the first principle component but the 
the second one. So we can re-select the principal components that we want to show as well as which axis we want to see it on. 
When we switch to the Loading plot, which shows us the loadings[?] for each principal component that we currently have selected on the x-axis, we can choose between the sample annotation, that is, we can 
choose the gene biotype we can use the gene symbols and everything else.

As, before on the left side we have the options to modify the plot and these are separated by the line depending on wether we need to recompute the plot or not and on the right side is the resulting plot.
We can switch between the different kinds of plots which are available for the Principal Component Analysis, like the PCA plot, the Loadings Plot, the Loadings Matrix and the Scree Plot.
In additon to the ususal Save plot, Send Only to Report or Get Underlying R code and data options below the plot, there are options we can choose to change the plot.

## Significance Analysis ##

We start by choosing the groups we want to compare. For example, choosing condition allows comparsion across treated and untreaded observations. 
We can also choose more than just one comparison and the program will directly calculate all the comparisons. We need also choose the test method, the significance level as well as the test correction method.
Selecting the "Get Significance Analysis" starts the computation for all the comparisions we have selected. In case the program has no significant results, the Result Visualisation tab[?] is empty. 

Each individual comparision tab has a table and a volcano plot:

In the Table tab, we can have a look at the raw P values for each comparision (athough we have to treat this with caution). 
In the table we can sort by any column for example adjusted p values (padj) or p values (pvalues). 
These tables can be downloaded as CSV file, as Excel files or we can copy them to the clipboard.

The Volcano tab shows us the volcano plots with corrected P values and uncorrected P values. We can interact with the plot by choosing different thresholds to show what are we considering as noteworthy 
and as in every plot we can send it to the  report get the underlying R code or save the plot.

As mentioned before, the result visualization is currently empty because we have nothing significant. But if we choose to look at raw P values by changing the type of significance to "significant unadusted", 
for the comparisions we did, we can see the intersections of the genes (for this example) and then we can compare which of the genes (for example we have 62 genes) are significant in all the comparison or which
of the genes are that are significant across the first and second comoparision (for example here we have 105 genes) as well as exactly which genes these are. Additionally we can highlight the plot bars before we
decide to download the plot or send it to the report or get the underlying R code.

## Single Gene Visualization ##

If we have want to have a look at a specific gene. [for example we didn't find it] 

We can select between raw or preprocessed data and on selecting the "Get Single Gene Visualisation", we get box plots or comparisons because if we only have two data points it doesn't draw box plots.
We can choose what do we group these [things?] after and if we select conditon instead of cells, we can see the comparion between treated and untreated along with the statistical test.
By changing the data selection from preprocessed to raw we can see if and how pre-processing changes our data.

[Report?]

## Enrichment Analysis ##

Another integral part of exploratory analysis is the Enrichment Analysis. In our case we have the option between GeneSetEnrichment or OverRepresentation analysis. 

GeneSetEnrichment: In the case of the GeneSetEnrichment analysis you'll be asked what to sort your genes after. Here you can choose between log Fold change or absolute log Fold change. We select the absolute 
log Fold change. Following this you would need to choose by what do you want to compute the log Fold changes, and then you can choose your gene sets that you want to compare to.
There are a wide variety of gene sets to choose from. The most standard ones known are KEGG HallMarks Go and Reactome [?]. [You can also choose the test correction.]

For each gene set you do the enrichment for, a tab will be visible in which you can see the enrichment or you can select the table. 

[Again for the tables you can download the tables and for the plots you have sent only through report get the underlying arod to reproduce this in R directly or save the plot as it is the]

[By clicking on the question mark you can get any kind of information - what the gene sets, are what do and what do they contain]

## Heatmap ##

The last analysis [we cover?] in cOmicsArt will be the Heatmap, which displays the count matrix in a clustered fashion.
We now can select what kind of rows we want to plot. For example if we only want to plot a few entities we can select the row annotation and then select the entities we want to plot.
Here we select the first eight and then click "Get Heatmap".

If no differeces are visible, we can also do a rowwise scaling to make the differences more clear. Additionally, we can select what do we annotate after - the samples or the rows.

[and again can send everything to the report or save it as a plot]

[Conclusion?]

The design across all the tabs are the same - Options that are above the seperating line on the left hand sidebar will need recomputation things and the options which are below don't and update 
the plot directly. The results always appear on the right hand which can be downloaded as well as sent to the report. 

When we are done with our analysis we can download the report as an HTML file. This HTML file will contain a report on everything you did and all the choices you made, enabling you to recreate 
everything you did in the application making every result reproducible.

[With this I want to thank you for your attention and I hope you enjoyed the application]