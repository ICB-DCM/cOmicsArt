## Working with ShinyOmics and Metabolon Data

This should be a little guide to showcase ways you can use ShinyOmics to analyse the Metabolon data!

### Preliminary:

1.  Upload the provided data-object at `Load precompiled data`

2.  select `Metabolomics`as `Omic Type that is uploaded` (all unsuitable options as `Which Organsim`should dissapear)

3.  Click `Do`

To analyse all data at hand:

4.  Select something else than `COMP_ID` for the `Row selection` e.g. `SUPER_PATHWAY`

5.  Click `Start the Journey`

### Preprocessing:

You are provided the normalised data, which is not log-transformed (also provided by Metabolon). To get to this log-transformed data choose `ln` as preprocessing. Be aware that now, you cannot compute LFC - values for e.g. Volcano plots! To have those you can leave the data unprocessed, ergo selecting `none` To actually do the pre-processing click `Pre-Preocess`. You should see in the main panel some text, including the dimensions of your dataset.

For this little tutorial we will do the PCA-plots for the log-transformed data and the remainder for the none-preprocessed data.

### PCA

Input: ln-processed data Click `Go to PCA` or click on the tab at the top named `PCA`
The options for `Choose the variable to color the samples after` are directly taken from the Metabolon Sample Metadata Sheet.
You are most likely interested in `GROUP_NAME`, `OFFSPRING_DIET` and `MOTHER_DIET`.
Note:
You can add another layer of customised annotation to the tooltip shown when hovering over points in the PCA-plot itself (e.g. `CLIENT_IDENTIFIER` to retrieve the information which specific sample is at hand/hover).
If you want to save the plot at hand you might want to download the plot to location specified by yourself (also here `pdf` as output is possible). For this specify the `File Type` and click `Save plot`. This plot will then be also automatically send to the overall report!
If you just want to save it in the overall Report click `Send only to Report`. The report also includes, which steps happened before, to ensure the reproducibilty of the plot at hand. You might also want to save the same plot but with different coloring to compare them side by side.

To consult the Loadings plot, select the corresponding panel. You can change the annotation of the y-axis by selecting a different option at `Select the annotype shown at y-axis`. You probably want to choose `PLOT_NAME` here. To see the Loadings of the second Principle Component, select `PC2` for `PC for x-Axis` in the purple sidebar.


### Volcano
To be able to calculate Log-Fold-Changes go back to the Pre-Processing Tab and select `none` for the following steps. To lock your selection click `Pre-Process`again. Now click `Go to Volcano`.

You see an already rotating circle on the rigth, however it will not produce any results as the default setting of the left are not meaningful to get the LFC for (as there is only one sample per group `CLIENT_IDENTIFIER` selected per default).
Select for example `OFFSPRING_DIET`, and choose the desired reference and treatment group in the two selection fields below. Click `Do Volcano Plot`. After a short time (seconds) the plot should appear on the right.
It is again an interactive plot, where you can hover over the points.
To adjust the thresholds of LFC and adj. pvalue you can change the values in the yellow sidebar. The changes are adapted immediately. There is no need to press the button again.
You can simply download all entities that are intresting in correspondence to the your threshold by clicking `Save intresting entities (all red points)`
If you would like to save the calculated LFCs and corresponding pValues you can click on the tab `Volcano_table`. You can download the table by copying it (same as Ctrl+C) or as csv or excel file (see options at top of the table).
If you like to see the raw data as well you can toggle in the yellow sidebar `show next to LFCs also the raw data that was used to calculate`.
Note:
`Send DE Genes to enrichment analysis ` is not applicable for Metabolomics data at the moment.

### Heatmap
The Heatmap is really flexible and can show all sorts of stuff (depending on your selection). You can choose from different `Row selections`, different underyling data (pre-processed data or specified Log Fold Changes), different annotations to color columns and rows after, if you would like to apply clustering to the data shown, whether you want row-wise scaling.
Per default, all rownames are hidden if there would be more than 25 selected entities to plot (how many you have with your specified options can be seen at the text box below the heatmap). You can also adjust this threshold as well as the labels to show at the rows.
Note:
- `Send genes shown to enrichment analysis` is currently not meaningful for metabolomics data
- `Save genes shown in Heatmap as list` currently does not change corresponding to `Choose the lable of rows` (to be changed soon)

#### Two possible examples to use the Heatmap panel
##### Select the top 20 most significant metabolites 
- Select under `Row selection`: `significant_LFC`and `TopK` (make sure delete `all`)
- Toggle the `show options (LFC-related)` (do so by clicking on it)
- Choose for `Choose type for LFC-based ordering` : `GROUP_NAME`
- choose for the reference `CD CD CD` and for the treatment `HFD HFD HFD` (all other combinations are possible)
- Press `Do Heatmap to display`
The heatmap is generated. Now you can adjust the metadata shown for the rows and columns. You could select for example:
- `Choose the variable to color the samples after (Multiples are possible)`: `GROUP_NAME` & `OFFSPRING_DIET`
- `Choose the variable to color the rows after (Multiples are possible)`: `SUPER_PATHWAY`
- `Choose the label of rows`: `PLOT_NAME`

##### Select all metabolites with significant LFCs expressed between the Offspring diets and show the top 20 with highest LFC
- Select under `Row selection`: `LFC_onlySig` and `TopK` (make sure delete `all`)
- Toggle the `show options (LFC-related)` (do so by clicking on it)
- Choose for `Choose type for LFC-based ordering` : `OFFSPRING_DIET`
- choose for the reference `CD` and for the treatment `HFD`
- Press `Do Heatmap to display`
The heatmap is generated and you can adjust the aesthetics, as before


### Single Gene Visualisations
Here you can dig into the actual raw data or preprocessed data, for single entities or for groups and peform statistical testing (t.test) for any combinations you would like.
Two examples should give you an idea how you can utilze this panel:

#### Compare for all entities belonging to Lipids their overall changes between the 6 different groups:
- Choose `preprocessed` for `Choose data to use`
- Choose `boxplots_withTesting`for `Choose the style of visualisation`
- Choose `SUPER_PATHWAY` for `Select Annotation you want to select an entitie from`
- Choose `Lipid` for `Select the Gene from the list` (I apologize for the bad labelling here, will be updated in the future)
- Choose `GROUP_NAME` for `Choose the groups the show the data for` 
Now a another selection option as appear in the main panel, where you specifiy the comparisons which should be tested
- Select all options for `Select your desired comparisons`
- Click `Get single gene visulaisations`




