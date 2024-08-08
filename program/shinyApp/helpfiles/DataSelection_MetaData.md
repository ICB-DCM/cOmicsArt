### Data Upload via File Input + Meta Data Sheet

***

With `Data upload` you can upload your data to the server via explicit csv files.<br>
The files must be in the following format:<br>
1. data-matrix (.csv) <br>
  - contains the actual data, e.g. counts<br>
  - must have the samples (e.g. mouse 1) in the columns and a **unique** naming as 
    column name<br>
  - must have the entities (e.g. genes) in the rows and a **unique** rowname (e.g. 
    ENSEMBL ID)<br>
2. Meta Data sheet (.csv) <br><br>
Here you can upload you `Meta Data Sheet` to easily upload all data at once in 
addition to a very detailed metadata information on the samples. This not only eases 
the upload but also allows you to investigate the data in much more detail. <br><br>
For a guide on the metadata sheet, please refer to the paper by [Seep et al.](TBA). 
<br><br>
3. row-annotation (.csv) <br>
  - contains metadata about the biological entities (e.g. if gene is protein coding)<br>
  - must have the **same** naming as in data-matrix (1.rownames) in the rownames<br>
  - if you do not have any information just add a column with the gene-names<br>

Some pitfalls you may fall into:
* The files must be uploaded at their respective position (**data matrix, 
  sample annotation, entities annotation**).
* The files need to be **.csv** files, which also means that they need to be comma 
  separated not semicolon separated. When **exporting from excel** make sure to check 
  that the separator is a comma. If your Excel is in german, your default export uses semicolons which will lead to errors!
* As stated above, the **column names in the data matrix** must be the same as the **row 
  names in the sample annotation**. And the **row names in the data matrix** must be the 
  same as the **row names in the entities annotation**. Upon Upload **before** pressing 'Do' you can switch the tab to go to 'Upload for visual inspection‘. Here, you can inspect the uploaded files and check fi that corresponds to your expectations. Additionally, at the bottom several outputs from test are given - there should be all 'Yes'.
