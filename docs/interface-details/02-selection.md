---
title: "Data Selection"
layout: default
parent: Interface Details
nav_order: 2
---

# Data Selection

The Data Selection tab is the initial step to upload and manage your data in cOmicsArt. This section is divided into two main areas: the side panel and the main panel.

## Side Panel 📚

The side panel offers several tabs for different data input methods. The data Upload 
and possible structure are detailed in the [Data Input](01-required-data-input.md). 
Nonetheless, here is a short overview:

### File Input 🛠️

- **Omic Type that is uploaded**: Select the type of omics data you are uploading (Transcriptomics, Lipidomics, Metabolomics).

- **Upload data matrix**: Upload your data matrix file. Rows should be entities (e.g., genes) and columns should be samples.

- **Upload sample annotation**: Upload the sample annotation file. Rows should be samples.

- **Upload entities annotation matrix**: Upload the entities annotation matrix file.

- **Inspect data**: Click this button to inspect the uploaded data. This will open a 
  Modal, in which you can see how the application interprets your data, whether it is 
  able to differentiate between the different columns and - through additional tests - 
  whether the data as you uploaded it is deemed good to go. To get to know more about the Visual Inspection, see at towards the bottom of this page.

- **Upload new data**: Click this button to upload new data files.

### Precompiled 🔧

- **Omic Type that is uploaded**: Select the type of omics data you are uploading (Transcriptomics, Lipidomics, Metabolomics).

- **Load precompiled data**: Upload a precompiled data file (e.g., RDS file). It has 
  the benefit, that - once saved - you only need to upload a single file now instead 
  of all three.

- **Upload new data**: Click this button to upload new precompiled data files.

### Metadata

Very similar to the `File Input`, but replacing the sample annotation with the 
[Metadatasheet](https://www.nature.com/articles/s41597-024-03349-2).

- **Omic Type that is uploaded**: Select the type of omics data you are uploading (Transcriptomics, Lipidomics, Metabolomics).

- **Upload data matrix**: Upload your data matrix file. Rows should be entities (e.g., genes) and columns should be samples.

- **Upload your Meta Data Sheet**: Upload your metadata sheet. This currently replaces the sample annotation.

- **Upload entities annotation matrix**: Upload the entities annotation matrix file. Rows should be entities.

- **Upload new data**: Click this button to upload new metadata files.

### Testdata 🧪

- **Omic Type that is uploaded**: Select the type of omics data you are uploading (Transcriptomics, Lipidomics, Metabolomics).

- **Upload data**: Click this button to start with a test dataset.

## Main Panel 💡

The main panel provides options for detailed data selection:

- **(Optional) Adding Gene Annotation**: In case of transcriptomics data, you can add 
  gene annotation to your sample. For this you only need to choose **Which Organism?
  ** and click the **Add Gene Annotation** button. This will add the entrez gene ids, 
  ensemble gene ids, gene symbols and gene biotype (e.g. protein coding, etc.) to your 
  data.

### Row Selection - Biochemical Entities

- **Which annotation type do you want to select on?**: Here you select a column of 
  your entitie annotation. Based on this column you can then select specific rows to 
  include through **Which entities to use?**. This might be either entitie names or 
  depending on the column you select also functional groups like e.g. "All Protein 
  Coding Genes". The more information you have on your entities, the better you can 
  select here. 

### Sample Selection

- **Which annotation type do you want to select on?**: Similar to the row selection you select a column of 
  your sample annotation. Based on this column you can then select specific samples to 
  include through **Which entities to use?**. You can e.g. select specific 
  samples or all samples from a specific experiment. Again, the more information you 
  have on the samples, the better you can choose here. 

### Save Input As RDS 💾

- **Save file input to upload later**: Click this button to save the file input as an RDS file for later use.

### Visual Inspection
The visual Inspection is designed to help you to identify potential issues of your uploaded data. Upon clicking the "Inspect Data" button, a modal will open. Here you can safely upload your data upon click.
Upon click, cOmicsArt is trying to interpret your data. It can actually detect common formatting issues (such as spaces in your sample names, or only numeric entitie names) and will ask whether you want cOmicsArt to correct them or not. If you choose to try the automatic correction the corrected data files will be prompted to you for download - such that you can upload those files directly next time.
Then the visual inspection uploads the three provided matrices and displays to you all three of them. 
- Carefully look at each matrix - do they look like you expected them? 
- Are there your proper column names and rownames? 
- Is it an acutal matrix or is everything in a single column? 
- Is the first Matrix really the matrix with measurements, the second matrix the sample table (carrying inforamtion about your samples) and the thrid table the entitie table (carrying information about your entities e.g. genes, metabolites, lipids)? 

To provide further help the visual inspection will also perform some automatic checks which status (Yes/No) you can see at the bottom of the modal. Here are required and optional tests. The required test must be all 'Yes' - if this it not the case you will need to correct your data outside of cOmicsArt and reupload once updated. If there are some 'Nos' (in orange color) within the optional tests you might want to consider to correct them. They are handled automatically within cOmicsArt, whereby this often means simply ignored or removed prior to the analysis. If you want to perform your own analysis or further refined the downloaded scripts form e.g. cOmicsArt it is recommended to correct them.
If you are happy with the status you can click on 'Use Data' to proceed with the analysis. If you are not happy you can click on 'Close' and correct your data outside of cOmicsArt.

### Start the Journey 🚀

- **Start the Journey**: Click this button to proceed with the analysis. This will 
  automatically bring you to the [Pre-processing tab](03-pre-processing.md).

### Other Notes 📌

- **Question Marks**: The displayed question marks provide quick and immediate help. They offer guidance and additional information about the options available.

---

## Further Navigation

Do you want to...

- Learn how to upload your data? → Go to [Data Input](01-required-data-input.md)
- Discover the pre-processing options available? → Go to [Pre-processing](03-pre-processing.md)
- Explore how to correlate your samples? → Go to [Sample Correlation](04-sample-correlation.md)
- Perform differential analysis on your data? → Go to [Differential Analysis](05-significance-analysis.md)
- Conduct Principal Component Analysis? → Go to [PCA](06-pca.md)
- Visualize your data with heatmaps? → Go to [Heatmap](07-heatmap.md)
- Visualize individual genes? → Go to [Single Gene Visualisations](08-single-gene-visualisations.md)
- Perform enrichment analysis on your data? → Go to [Enrichment Analysis](09-enrichment-analysis.md)

---
