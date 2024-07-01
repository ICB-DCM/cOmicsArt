---
title: "Data Input"
layout: default
parent: Interface Details
nav_order: 1
---

# The very start
To start your own analysis, you need to pass the first hurdle - Uploading your data. (Note: If you want to check out what the app is capable of you might want to use the Test data, how to exactly do it check out [Interface Details](interface-details/selection.md)
Within cOmicsART you have several options to do so:

- You have at least a data matrix of your measurements. Then start [here](## Starting with a single table)
  - a data matrix can be, e.g., a count-table, or measured concentrations for multiple entities and samples. At least means, that you might already have also a sample table and /or a measurement table
- You have a data matrix and a [Metadatasheet](https://www.nature.com/articles/s41597-024-03349-2)
  - a Metadasheet is a handy way to organise not only your data, but also your group's and beyond developed within the [Hasenauer Lab](https://www.mathematics-and-life-sciences.uni-bonn.de/en/research/hasenauer-group). At the moment the Metadatasheet can be supplied instead of a sample table. We are planning to expand on the connection of the Metadatasheet and cOmicsART - stay tuned.
- You have a cOmicsART created rds object
  - or you may know what rds objects are and want to create one for your collaboration partners. Check out its [required structure](### [advanced] RDS-Structure)

# General remarks on data input
cOmicsART expects overall three data tables, which can be supplied in different ways, holding information about the measurement itself as well as information regarding the measured samples and information regarding the measured entities, such as genes, metabolites or lipids. We provide here the description of the minimal requirements, however note that you are not limited to theese. Actually adjusting the data input, esspecially annotation is often part of the explorative phase. Checkout [Showcase A](showcases/showcase-a.md) for an examples.

<img src="DataInputShiny.png" width="600">

## Starting with a single table

### The measurement table
### Creating a Sample table
### Creating a annotation table

### in short
The files must be in the following format:<br>
1. data-matrix (.csv) <br>
  - contains the actual data, e.g. counts<br>
  - must have the samples (e.g. mouse 1) in the columns and a **unique** naming as 
    column name<br>
  - must have the entities (e.g. genes) in the rows and a **unique** rowname (e.g. 
    ENSEMBL ID)<br>
2. sample-table (.csv) <br>
  - contains metadata about each sample (e.g. disease status) (To come: combination with the Metadata sheet)<br>
  - must have the **same** naming as in data-matrix (1.colnames) in the rownames<br>
  - must have at least one column name `global_ID`, this will be used to refer to the sample (in e.g. Volcano plot) (in future not necassary anymore)<br>
3. row-annotation (.csv) <br>
  - contains metadata about the biological entities (e.g. if gene is protein coding)<br>
  - must have the **same** naming as in data-matrix (1.rownames) in the rownames<br>
  - if you do not have any information just add a column with the gene-names<br>

### Some pitfalls you may fall into:
* The files must be uploaded at their respective position (**data matrix, 
  sample annotation, entities annotation**).
* The files need to be **.csv** files, which also means that they need to be comma 
  separated not semicolon separated. When **exporting from excel** make sure to check 
  that the separator is a comma. If your Excel is in german, your default export uses semicolons which will lead to errors!
* As stated above, the **column names in the data matrix** must be the same as the **row 
  names in the sample annotation**. And the **row names in the data matrix** must be the 
  same as the **row names in the entities annotation**. Upon Upload **before** pressing 'Do' you can switch the tab to go to 'Upload for visual inspection‘. Here, you can inspect the uploaded files and check fi that corresponds to your expectations. Additionally, at the bottom several outputs from test are given - there should be all 'Yes'.
  
## Starting with a single table and a Metadatasheet

## Starting with a rds-object
### [advanced] RDS-Structure

