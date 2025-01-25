---
title: "Transcript of Tutorial"
output: html_notebook
---
Welcome to cOmicsArt - Customizable Omic Analysis and Reporting Tool. This tutorial is going to show you how to use this application to aid in your statistical analysis.
This tutorial is self-contained but can also be used to follow along with the YouTube tutorial.

Comics art is designed is in a tabular format. In the beginning you only see the data selection but if you upload your data and go through the steps progressively more 
tabs will be shown. Let us start by uploading our data.

[Picture of the welcome page]

## Uploading Data ##

The data will be uploaded with three CSV files. The first one is the data matrix showing the counts on the rows there are the entities for example genes on the columns 
they are the samples. The sample table shows you meta information on the samples while the entity table shows you any information on the entities you have. 

[Pictures highlighting browse for the three CSV and upload data tab]

If you need to access the documentation, you can click on "Go to Documentation" from any page which will bring you to our website with an extensive documentation on what to do.

Another option is to click on the helper buttons these small question marks wherever they are. They'll show you some kind of help for whatever you're doing.

[Highlighted Go-To Documentation] [Website Interface Documentation picture] [Highlight question mark picture]

Right now in this example it shows you how to upload your data via the file input. It shows you again what it should look like what it contains. [Bold?] 
And what is important is that the rows of the data Matrix correspond to the rows of the row annotation while the Columns of the data Matrix correspond to the rows of the sample 
table okay so let's just upload our data.

We choose an example data set you can also download this. We just choose the read counts the sample table it's sample sheet and the entity annotation. 
When we've uploaded this the first thing we want to do is we might want to check how is our data interpreted in this program for this we click on "inspect data". 

[Highlight inspect data]

We need an additional click on "upload data for visual inspection" and then it will show us how does our program interpret our data. 

[Highlight upload data]

Pitfalls are here for example if we choose the wrong separator then it will only have ever one column and you see that this will be interpreted as wrong. 

Does it show all the other information and then down here there are a multitude of checks where you can check whether your data looks as it should be.

Green means fine red means you need to go into your data and change things while Orange means yeah it's not good but we can handle it for example in this case it's an 
a values or missing values in the data the the application will just remove those entities from the table.

And then we just click on "upload new data".

[Highlight upload new data]

And as you can see we now see our result part of the design which in this case is the data selection.

[Highlight changes to interface ?]

The first thing we can do is we can add annotation to the data if we didn't do that. We did this already so we don't need to do it and you can choose the organism to 
add the annotation. 

[Add Gene Annotation highlight]

And then you choose row or samples. 
For the sample selection you first choose among the columns that you have in your sample table. For example you can choose the cells, you can also choose the condition or 
you choose the experiment or the sample and you might want to select only specific samples you want to include the data. 

[Highlight specific sample selection]

Perhaps there are strong reasons to exclude some and you just click on them you remove them all and will only
use these data sets you can do the same for the condition. Here it's treated or untreated if you have multiple
conditions you might want to only check one of them. We want to use all data the same can be done with Gene with the
row selection and you can choose either the gene names here in Ensemble genes you might not know those Gene names all by hard so select the symbols and you can select which 
ones you want to use.

You can also use the Gene B biotype and which shows you functionals. So for example in transcriptomics you sometimes only want to use the protein coding genes so you can select 
protein coding and we'll only use all of the protein coding genes that you have within your data matrix you can do the same for prois pseudo genes and it will select all the 
process pseudo gen.

In our case we want to use all our data so we just start our journey by clicking on this button. 

[Highlight button]

## Pre-Processing ##

Clicking on this button will directly bring you to the pre-processing. 

Here you can see again the design choice on the left side we have the Side Bar with all the options that you can choose separated between things that you need to choose and then click on the button again (needs recomputing) or things down below this black line which you can just change (does not need recomputing). 

For example if you choose the DC sequencing you can choose the genes if you want more information on what kind of pre-processing we can do you can click on the helper button or go to the documentation you can also choose batch effects from The Columns you already have in your data said though keep in mind usually batch effects are only done later when realizing that there are actually batch effects so in our case we want to do a log 10 scaling so we need to click on this before clicking this button 

[Higjlight get preprocessing button] 

and then get the pre-processing this will take a bit of time but then when it is done the results will be shown in this case a distribution Matrix some kind of Statistics show you what changed from raw data to pre-processed data and as you can see above all the different tabs are now shown as well from sample correlation PCA significance analysis to Heatmaps.

[Highlight tabs which are now visible]

To show you an example how the options below the bar work we just click on condition for example and without having to reclick the get pre-processing button in time directly the plot updates showing you potential similarities based on the sample condition you you selected to note that now that all the tops are shown we can always switch between any of them do the analysis and go to any other we we'll start with the sample correlation.

## Sample Correlation ##

Here Again by Design on the left side is the sidebar which lets you choose all the options for the plot in this case up above the button things that need recomputing you can always click on the help files if you are not quite sure which kind of correlation method for example in this case you want to choose and down below are things that can directly change the plot if we click on get sample correlation the sample correlation will be plotted and you can see it is already plotting the cells the samples by the cell names we can add the condition to see does a plot by anything else and see kind of clusters and patterns down below where we'll go next you can see that for each plot we'll have save plot sent only to report or get underlying AR save plot is fairly straightforward you choose the file type we click on Save the blot and will open up to just directly save it what you can also do is you can get the underlying R code and data to directly it gives you the exact R code you need to reproduce the the plot and then you can adjust it to your lighting or you can send the plot towards a report which will send the plot along with any kind of options you chose like the Person correlation that you color the samples by cell and condition along with notes that you can take manually towards an HTML report to save it for later this en enables you when you download the report as an HTML at the
end of your statistical analysis to reproduce any kind of findings you had within the application so it shows you what kind of options did you choose what was the pre-processing how did you produce it and this send only to report get
underlying R code and safe plot will always show up below any plot let's Principal Component Analysis (PCA) continue with the principal component analysis here again we can select data or we just get the principal components for now which will open the principal component plot now if we want to select data this is only advised for really looking at variance explained within specific data points but otherwise just get all the data get the principal components as you know it and then down below we can select what do we want to color them after what kind of principal components do we want to see what is the what do we want to show at the end tool to tip if we go over it to hover and see what are preps outliers again we have to send only to report get underlying our code and data and safe plot as everywhere we can take notes and then send it to the report if we want to or we can say well let's directly color it by the condition because if we had treated and untreated do we see a clustering here we can see more for clustering but it's not the first principle component the second one so we can just re select the principal components that we want to show on which
axis we can also switch to the Loading plot which shows us for each principal component that we currently have selected on the x-axis the loadings again we can choose between the sample annotation meaning we can choose the Gene B tag we can use the gene symbols and everything else in General on the left side again for the design options to choose separated by the button with above the button these recp Computing below the button directly updates and on the right side there are the results for example in this case different kind of PCA plots like the PCA plot the loadings the loading Matrix or the street plot and below the plot sometimes are op additional options we can choose to change the blot and then lastly there's always the send only to report get underlying R code and data and save the plot in the significance Significance Analysis analysis we again start by choosing our \options in the sidebar choosing the level or of groups we want to compare like the condition treated or untreated or we choose the cells we can choose more than just one comparison and by clicking on the get significance analysis later it will directly do all the comparisons we need we can choose the test method the significance level and the test correction method and then just click on it and it directly runs all free comparisons we have selected up above where the analysis is running when it's done we can see okay it shows us we didn't find anything that was significant but in case we're in an exploratory case we can also have a look at the raw P values though we have to treat this with caution we can see here for each comparison we have a table or the volcano blood in the table we can sort by whatever means we want to for example the P adjusted values we see all of them are one but some P values are significant ific we can download those tables as CSV files as Excel files or we can copy them to the clipboard or we can have a look at the volcano plots in the volcano plots it shows us the volcano plots with corrected P values and uncorrected P values again we can interact with the plot we can choose different thresholds to show what are we considering as something noteworthy and as in every plot we can send it to the rep report get the underlying R code or save the plot in the result visualization which is currently empty it is empty because we have nothing significant but if we only look at Raw P values we can get a comparison and what it does is it shows us for all the free comparisons or any others we did shows us intersections of those significant genes in our case and then we can compare which of the genes for example we have 62 genes that are significant in all three comparisons does that tell us anything what are those 63 genes or what are the 105 genes in the second first and second comparison we can highlight this we can download the report and again for the complete plot send it to the report get underlying AR code and data we Single Gene Visualization continue if we have want to have a look at a specific  Gene for example we didn't find it we can select between Rob or preprocess dat and the single gene visualization and it draws us box plots or comparisons because if we only have two data points it doesn't draw box plots we can choose what do we group these things after and so if we go to treated and untreated now we see box plot and we see the comparison directly does the statistical test on this and we can also see how did pre-processing change our data or change anything again we can send the plot to the report another integral part of a Enrichment Analysis exploratory analysis is the enrichment analysis in our case we have the option between Gene set enrichment or over representation analysis in the case of the gene set enrichment analysis you'll be asked what to sort your genes after you can choose between Lo fold change or absolute Lo fold changes then again you have to choose by what do you want to compute the your Lo fold changes and then you can choose your Gene sets that you want to compare we have a wide variety of Gene sets the most standard ones known are kek Hall marks go and reactome and by clicking on the question mark again you can get any kind of information what the gene sets are what do they contain you also choose the test correction then for each gene set you you did the enrichment for you'll be shown a tab in which you can see the enrichment or you can select the table again for the tables you can download the tables and for the blots you have sent only through report get the underlying arod to reproduce this in R directly or save the blot as it is the Heatmap last analysis in comics art will be the heat map displaying the count Matrix in a clustered fashion where we now can select what kind of rows do we for example want to plot we only want to plot a few entities so we select the row annotation select the entities we want to plot well the first eight let's say and then just click on the button get heat map again get heat map just as it was get Gene set nutriment analysis get PCA get significance analysis if we don't see any differences we can also do a rowwise scaling to make differences more clear we can select what do we annotate the samples or the rows after and again can send everything to the report or save it as a plot as you can see all the design choices are the same f things that are above the button on the sidebar will need a recomputation things that are below don't need one but update the blood directly and the results we always see the plot or the table or multiple plots we can download them and send them to the report when we are done with our Report analysis we can download the report as an HTML which will create an HTML file which you can directly download and then open it when you open it you will see a report on everything you did and every choices you made enabling you to rec create everything you did in the application again making every result you did in comics art reproducible with this I want to thank you for your attention and I hope you enjoyed the application