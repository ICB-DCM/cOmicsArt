---
title: "Enrichment Analysis"
layout: default
parent: Interface Details
nav_order: 8
---

# Enrichment Analysis

The Enrichment Analysis tab allows you to perform enrichment analysis specifically for transcriptomics data. This section is divided into two areas: the side panel and the main panel.

## Side Panel

In the side panel, you have the following options:

- **Choose an organism**: If you haven't added annotation in the [initial data 
  selection](selection.md), you may choose the organism your data stems from. This is 
  necessary for correct data translation.
  
- **Choose type of Analysis**: You can choose between Gene Set Enrichment (GSEA) and 
  Overrepresentation-Analysis (ORA). For an overview on their functionality we refer 
  to [this review](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3285573/)

- in case of a gene set enrichment analysis, you will have the following options

  - **Select the metric to sort the genes after**: Gene set enrichment analysis sort 
    **all** your genes by some measure and then supplies this ranked list of genes to 
    an algorithm, that goes through this list and subsequently checks whether the gene 
    in question belong to a specified geneset. From this a score is calculated. The 
    choice here is between logFoldChanges, absolute logFoldChanges or t-statistic value.
    Note, that for the latter no effect size is taken into account. Still, a positive t-statistic value indicates that the gene is upregulated in the treatment group, while a negative value indicates that the gene is downregulated in the treatment group. More information can be found [here](https://www.gsea-msigdb.org/gsea/doc/GSEAUserGuideFrame.html?Run_GSEA_Page)


  - With **Choose type for LFC-based ordering**, **Choose reference of log2 
    FoldChange**, and **Choose treatment group of log2 FoldChange** you can specify 
    which two groups to compare against each other.
- in case of an overrepresentation analysis you hand over a list of genes, which then 
  will be compared to the geneset of interest, based on a supplied `universe of genes`,
  i.e. a set of all "possible" gene names. Thus your choice are

  - **Select a Universe for enrichment (default is [clusterProfilers](https://bioconductor.org/packages/release/bioc/html/clusterProfiler.html)
    default)**: Aside from the default options you can also choose the list of **your 
    own** genes from data, before or after preprocessing.
  - **Choose a gene set to hand over to enrich**: either you provide your own set, in 
    which case you will be asked to **select a file** or you can use genes that you 
    have previously used in the [heatmap tab](heatmap.md). Keep in mind however, that 
    you need to have sent it over with the `Save genes shown in Heatmap as list`-button.

In both cases GSEA and ORA, you will have the following additional options:

- **Choose sets to do enrichment for**: different databases (e.g. Kegg, GO) have 
  different genesets. Here you can select multiple databases, whose genesets you want 
  to check out. The genesets are all from the [MSigDB collection](https://www.gsea-msigdb.
  org/gsea/msigdb/collections.jsp), where you also have a brief description on their 
  content.
- **Get Enrichment Analysis**: Clicking this button will generate the results in the 
  main panel

## Main Panel

The main panel displays the results of the enrichment analysis. Here are some key points:

- **Results Tabs**: There are different tabs for viewing the results, specifically, 
  you have on tab for each of the selected sets you chose in the sidebar. Each result 
  tab itself will consist of two more sub-tabs
  - **GenesetX Enrichment**: If any genes were found significant, you will see here a 
    standard plot of an enrichment analysis, showing you specific pathways with their 
    corresponding p-value.
  - **GenesetX Enrichment Table**: This will have a list of all pathways tested for.

  
- **Download Options**: The visualisation can be downloaded directly in common formats 
  (e.g., PNG, TIFF, PDF) or sent to the report. You can also download the underlying R 
  code and data. For more information, check out [Interface Details](interface-details.md). The tables can be downloaded in common formats as well (.csv,
  .xlsx).

### Other Notes

- **Question Marks**: The displayed question marks provide quick and immediate help. They offer guidance and additional information about the options available.

- **Structure Notes**: For structural reasons, it is recommended to start with Heading Level 4 (e.g., #### My personal Title).

