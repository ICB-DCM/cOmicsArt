### ShinyApp/helpfiles/DataSelection_DataUploadFileInput.md - Under Development

***

With `Data upload` you can upload your data to the server via explicit csv files.<br>
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
  - must have at least one column names `global_ID`, this will be used to refer to the sample (in e.g. Volcano plot) (in future not necassary anymore)<br>
3. row-annotation (.csv) <br>
  - contains metadata about the biological entities (e.g. if gene is protein coding)<br>
  - must have the **same** naming as in data-matrix (1.rownames) in the rownames<br>
  - if you do not have any information just add a column with the gene-names<br>

Some pitfalls you may fall into:
* The files must be uploaded at their respective position (**data matrix, 
  sample annotation, entities annotation**).
* The files need to be **.csv** files, which also means that they need to be comma 
  separated not semicolon separated. When **exporting from excel** make sure to check 
  that the separator is a comma.
* As stated above, the **column names in the data matrix** must be the same as the **row 
  names in the sample annotation**. And the **row names in the data matrix** must be the 
  same as the **row names in the entities annotation**.