---
title: "Showcase A"
layout: default
parent: Showcases
nav_order: 1
---

# Showcase A

The following document is a showcase of the cOmicsART web application using a specific dataset. This showcase demonstrates how one can work with cOmicsART to analyze and visualize data. The showcase is divided into sections. Each section provides key takeaway messages and insights from the analysis at the top before diving into details. 

As a little background information, the dataset used in this showcase is from a study investigating the effects of a high-salt diet on immune defense against kidney infections. The study uses transcriptomics data to investigate how a high-salt diet (HSD) affects immune defense against kidney infections. It finds that an HSD worsens pyelonephritis in mice by impairing neutrophil antibacterial function due to urea buildup and disrupted glucocorticoid rhythms, which also hampers neutrophil development. If you interested in the whole story and the data, please visit the original publication: [A high-salt diet compromises antibacterial neutrophil responses through hormonal perturbation](https://pubmed.ncbi.nlm.nih.gov/32213629/). 

## The Data - Retrieval & Preparation, Upload & Pre-processing
*SHORT SUMMARY*
### Retrieval
The raw data (fastq-files) can be retrieved from the European Nucleotide Archive (ENA) under the project number [PRJEB28204](https://www.ebi.ac.uk/ena/browser/view/PRJEB28204). The data was aligned to the mm10 reference genome with the STAR Aligner. We recieved the resulting count matrix for the purpose of this show-case.

Having the data in a count matrix format, we created the corresponding sample and row annotation table. The sample annotation table contains information about the samples, such as the treatment group, the cell type, and the individual sample ID. The row annotation table contains information about the genes, such as the gene name and the gene type. Here, we pretend that we only had the gene ensembl Id for illustrative purposes. See the (documentation)[../interface-details/required-data-input-md] for more explanation regarding the input.

### Preparation
We created the sample matrix for the project in Excel and saved it as .csv file. Note check out [this site](https://www.ablebits.com/office-addins-blog/convert-excel-csv/) to see how to save a file as csv in Excel:
![Sample Annotation](/OmicShiny/assets/images/sampleTable_showA.png)
Next, we created the row annotation table which consists out of one column only to full-fill the requirement of having a matrix. As for now we had no additional information for the genes.
![Row Annotation](/OmicShiny/assets/images/rowTable_showA.png)

Within this showcase we will actually come back to this step, as we will gain information throughout which we can add to our annotation table. All information that is provided in this step can be utilized during the analysis to filter and group the data or the perform statistical tests.

### Upload

Upon created the sample and row annotation table, we can upload the data to the cOmicsART web application. We select on top omic type transcriptomics and provide then the data matrix, the sammple annotation and finally the entity matrix. A quick insepection of the data upon button click on 'inspect data' shows us that the data was prepped correctly and we can proceed with the analysis. To do so we click on 'Upload new data' and the data is loaded into the app.

As a first step we add some more information to our entities, precisely we request cOmicsART to add gene annotation which are the gene names as well as the biotype of the genes. As we have named our initial column of the row annotation by the type of ID, cOmicsART can automatically detect the correct column within your provided table to use as basis to add the addtional info. If we would just have called it 'ID' it would have still found the correct column but only as we have a single column anyway. Note, you need to have either ensembl IDs or entrez IDs or gene name for the row annotation to work. Upon button click on 'Add Gene Annotation' the data is added, which can take a bit, before returinng to the intital tab to then proceed with the analysis if no selection is done.
We for now will consider at first everything we have and 'Start the Journey' by clicking on the respective button.

### Pre-processing
For the pre-processing we select the DESeq option. Upon selection we specify 'Treatment' as main factor for the model. For more details about the DESeq2 pipleine used in the background, checkout the [DESeq2 vignette](https://bioconductor.org/packages/release/bioc/vignettes/DESeq2/inst/doc/DESeq2.html). The pre-processing involves filtering of lowly expressed genes before applying the DESeq2 pipeline.
After clicking ' Get Pre-Processing' the data is processed and we can assess the diagnostic plots of the sample distribution. We color this after the treatment (sidepanel below horizontal line, hence only perform re-coloring but no re-pre-processing). We can see that the distribution across samples is quite similar and does not depend on the underlying treatment, which was not the case for the Raw Data's distribution. We can conclude that the chosen pre-processing was successful in generating homogeneous distributions across samples.


## Investigating global patterns - Sample Correlation and PCA
*SHORT SUMMARY*

## Sample Correlation
We first investigate whether global patterns within the data itself fit to our expectations, we are at first that samples of similar treatment are more similar to each other. We therefore assess their correlation pattern using the Pearson correlation coefficient.
We can observe that the correlation is across all our samples very high, with rather minor differences between the treatment groups. This could indicate that the treatment does not have a strong effect on the global gene expression patterns. Potential reasons could be that the treatment has a rather weak effect or that the effect is only visible in a subset of the samples. Also, sample correlation might not be the best way to assess the effect of the treatment on such high-dimensional data. Hence we further went to assess the PCA, to identify the linear combinations of features (here genes) that explain the most variance in within the data.

## PCA
Switching to the PCA-panel and performing a PCA colored after treatment, we can observe that samples treated by HSD are less spread in the PC1 vs PC2 plot than the NSD samples. Additionally the samples are separating, although no clear separation is visible. Notable are the rather low numbers of observed variance (indicated on the respective axis). A quick look at the scree plot tab confirms that the first three PCs explain roughly 18%, 15% and 12% of the variance, respectively. The following PCs all explain roughly the same amount of variance. This indicates that the data is rather complex and cannot be easily explained by a few linear combinations (PCA). 
Going back to visually assess the PCA plot, we can also see that along PC1 (hence 'from left to right') two samples stand out on the left. Leveraging the tooltip (and changing Select the anno to be shown at the tooltop to 'global_ID') we can identify the sample NSD_1 and NSD_2 as samples to be differentiated apart from the other samples along PC1. Switching to the tab 'PCA_Loadings' allows us to gain insights into the features with highest loadings for the principal component displayed on the x-Axis. Here we can choose underneath the plot the annotype shown at the y-axis. This information is taken from our supplied row annotation which was extendend by our added gene annotation. 
When choosing gene_type we can see that among the highest loadings are genes of the type 'protein_coding'. Two non-protein coding genes are also among the highest loadings (TEC = to be experimentally confirmed and lncRNA , long-non-coding RNA. For more information regardinf the gene types, please visit [Ensembl Annotation](https://www.ensembl.org/info/genome/genebuild/biotypes.html).
Switching annotation to gene.name we can see that Dusp1 and Ppbp are genes with the highest absolut loadings. A quick literature search reveals that Dusp1 is a gene that has been identified to play a role in renal fibrosis and maintaining mitochondrial function (Tan, Ying, et al. 2022)[https://www.sciencedirect.com/science/article/pii/S2212877822001363] and Ppbp is a gene that has been identified to be a chemotractant for neutrophils (Piccardoni, Paola, et al. 1996)[https://www.thieme-connect.com/products/ejournals/abstract/10.1055/s-0038-1650660].
However, we can observer no separation of the groups along PC1 but rather by a PC1 and PC2 (separation by a diagonal from middle top to bottom right) - hence high Loadings on both principla components need to be assessed. For this we switcht to the PCA_Loadings_Matrix panel. To see somehting we need to adjust the set default parameters at the bottom. From previous Loadings plot, we can deduct that the genereal Loadings levels are rather small - therefore we adjust the 'abslute Loading threshodl to filter entities with low impact' to be 0.05. As we are intererested in PC1 and PC2 we adjust the Number of PC's to include as well. We can observe a number of NA's when stating at gene.name as annotype as not all ensembl IDs have an associated gene name. Switching to the orig IDs cleans the picture. We can confirm by visual inspected that ID ENSMUSG00000024190 (Dusp1) has the combined highes impact, followed by ENSMUSG00000021250 (fos). 

## Following single interestig genes 
To statistically test those two genes identified by their high loadings on PC1 and their difference in expression between the treatments, we can go to the Single Gene Visualisations tab. Here, single test are performed and no multiple testing correction is done - allowing for a quick and precise look up of genes of interest.
Checking Dusp1, Fos and Ppbp we optain signficiant (p<0.05) results for all with Dusp1 amd Fos being upregulated in HSD compared to NSD, while Ppbp is downregulated. If we change the 'groups to show the data for' to Simulation.Treatment we can observe for Pbpb that NSD_1 and NSD_2 behave again different to the other members of the group. This is in line with the PCA results. Note that we cannot 'test' for a difference as we have a single datapoint for each.

 







