---
title: "Data Input"
layout: default
parent: Interface Details
nav_order: 1
---


# The Very Start 🚀
To start your own analysis, you need to pass the first hurdle - Uploading your data. (Note: If you want to check out what the app is capable of, you might want to use the Test data. For detailed instructions, check out [Interface Details](interface-details/selection.md)).

Within cOmicsART, you have several options to do so:

- **You have at least a data matrix of your measurements.** Then start [here](#starting-with-a-single-table).
  - A data matrix can be, e.g., a count-table or measured concentrations for multiple entities and samples. "At least" means you might also have a sample table and/or a measurement table.
- **You have a data matrix and a [Metadatasheet](https://www.nature.com/articles/s41597-024-03349-2).**
  - A Metadatasheet is a handy way to organize not only your data but also your group's, developed within the [Hasenauer Lab](https://www.mathematics-and-life-sciences.uni-bonn.de/en/research/hasenauer-group). Currently, the Metadatasheet can be supplied instead of a sample table. We are planning to expand the connection between the Metadatasheet and cOmicsART - stay tuned.
- **You have a cOmicsART created RDS object.**
  - Or you may know what RDS objects are and want to create one for your collaboration partners. Check out its [required structure](#advanced-rds-structure).

# General Remarks on Data Input 🏷️
cOmicsART expects three data tables, which can be supplied in different ways, holding information about the measurement itself, information regarding the measured samples, and information regarding the measured entities, such as genes, metabolites, or lipids. We'll provide here the description of the minimal requirements, but please keep in mind that you are not limited to these. Adjusting the data input, especially annotation, is often part of the explorative phase. Check out [Showcase A](showcases/showcase-a.md) for examples.

<img src="DataInputShiny.png" width="600">

## Starting with a Single Table - Data Matrix 📝
When you have only your omics data table (referred to as the Data Matrix), ensure that entities are organized in rows and samples in columns. There are no limitations on the number of rows and columns (e.g., genes and samples), but you should have more than two entities and samples for meaningful analysis. While cOmicsART is designed for omics analysis, you can upload any type of measurements arranged in a data matrix format. However, please note that some analyses, such as enrichment analysis, might not be meaningful with non-omics data. Also, some provided pre-processing options then might not be relevant - you have the option to select 'None' as pre-processing, giving you the option to perform pre-processing beforehand and upload and analyze the uploaded data.

### Creating a Sample Table 🔧
Having the data matrix, you need to create a fitting Sample table.🖇️ A fitting sample table has the column names of the data matrix in its row names. Each additional column in the sample table carries additional information about the samples (referred to as sample annotation). Examples are condition, height, or phenotype. You are not limited to the number of columns, hence to the number of information you can supply to your samples. 

Within cOmicsART, this information is used, for example, to perform batch correction, data selection, or to visualize respective groups of samples. If this sounds confusing, don't worry - it becomes clearer throughout usage and this documentation. Make sure to check out the [data selection](selection.md) to see how your sample data is utilized in the data selection process.

### Creating an Annotation Table 🔧
Having the data matrix, you need to create a fitting annotation table.🖇️ A fitting annotation table has the row names of the data matrix in its row names. Each additional column in the annotation table carries additional information about the entities (referred to as row annotation). Examples are gene type, lipid class, or other IDs. You are not limited to the number of columns, hence to the number of information you can supply to your entities.

Within cOmicsART, this information can be used within the data selection or to be visualized instead of the row index. For some examples of the data selection, go to [data selection](selection.md).

### All in Short 🎁
The files must be in the following format:<br>
1. **Data Matrix (.csv)** <br>
  - Contains the actual data, e.g., counts<br>
  - Must have the samples (e.g., mouse 1) in the columns and a **unique** naming as column name<br>
  - Must have the entities (e.g., genes) in the rows and a **unique** row name (e.g., ENSEMBL ID)<br>
2. **Sample Table (.csv)** <br>
  - Contains metadata about each sample (e.g., disease status) (To come: combination with the Metadata sheet)<br>
  - Must have the **same** naming as in data matrix (1. colnames) in the row names<br>
  - Must have at least one column named `global_ID`, this will be used to refer to the sample (e.g., in a Volcano plot) (in future not necessary anymore)<br>
3. **Row Annotation (.csv)** <br>
  - Contains metadata about the biological entities (e.g., if a gene is protein-coding)<br>
  - Must have the **same** naming as in data matrix (1. row names) in the row names<br>
  - If you do not have any information, just add a column with the gene names<br>

### Some Pitfalls You May Fall Into
- ⚠️ The files must be uploaded at their respective positions (**data matrix, sample annotation, entities annotation**).
- ⚠️ The files need to be **.csv** files, which also means that they need to be comma-separated, not semicolon-separated. When **exporting from Excel**, make sure to check that the separator is a comma. If your Excel is in German, your default export uses semicolons which will lead to errors!
- ⚠️ As stated above, the **column names in the data matrix** must be the same as the **row names in the sample annotation**. And the **row names in the data matrix** must be the same as the **row names in the entities annotation**. Upon upload, **before** pressing 'Do', you can switch the tab to go to 'Upload for visual inspection'. Here, you can inspect the uploaded files and check if they correspond to your expectations. Additionally, at the bottom, several outputs from tests are given - they should all be 'Yes'.

## Starting with a Single Table and a Metadatasheet 📝 📝

### Starting with a Single Table - Data Matrix 📝
When you have only your omics data table (referred to as the Data Matrix), ensure that entities are organized in rows and samples in columns. There are no limitations on the number of rows and columns (e.g., genes and samples), but you should have more than two entities and samples for meaningful analysis. While cOmicsART is designed for omics analysis, you can upload any type of measurements arranged in a data matrix format. However, please note that some analyses, such as enrichment analysis, might not be meaningful with non-omics data. Also, some provided pre-processing options then might not be relevant - you have the option to select 'None' as pre-processing, giving you the option to perform pre-processing beforehand and upload and analyze the uploaded data.

### The Metadatasheet 📝
The Metadatasheet enables biomedical researchers to organize their data and metadata along the data lifecycle and is suitable for more than just omics data. For more information, see [From Planning Stage Towards FAIR Data: A Practical Metadatasheet For Biomedical Scientists](https://www.nature.com/articles/s41597-024-03349-2). If you have such a metadatasheet, the sample table can be replaced by uploading the Metadatasheet. We aim to majorly enhance the incorporation of the Metadatasheet with cOmicsART.

### Creating an Annotation Table 🔧
Having the data matrix, you need to create a fitting annotation table. A fitting annotation table has the row names of the data matrix in its row names. Each additional column in the annotation table carries additional information about the entities (referred to as row annotation). Examples are gene type, lipid class, or other IDs. You are not limited to the number of columns, hence to the number of information you can supply to your entities.

Within cOmicsART, this information can be used within the data selection or to be visualized instead of the row index. For some examples of the data selection, go to [data selection](selection.md).

## Starting with an RDS Object 📝
Once you have uploaded your three data tables to the app, you can click on 'save as RDS Object'. This is an R-specific object that saves all three files in one object. Hence, for any new analysis, you can just upload this single file and not multiple to save yourself some time. Note that the RDS object is in general not a standardardized format, which means that you cannot upload any RDS object. If you want to create a RDS-object outside cOmicsART see [RDS-Structure](### [Advanced] RDS-Structure)

### 🏆 [Advanced] RDS-Structure
*** to come ***

