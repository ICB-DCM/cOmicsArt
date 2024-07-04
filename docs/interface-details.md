---
title: "Interface Details"
layout: default
has_children: true
nav_order: 2
---

# Interface Details
![A comic about a cat finding cOmicsART](/OmicShiny/assets/images/cOmicsOctopus.png)
*Image generated using DALL-E by OpenAI. Adjusted by Lea Seep*

If you need tab-specific information, navigate to the respective documentation using the left sidebar.

## General Design Characteristics

### User Interface and Analysis Features

The cOmicsART user interface evolves gradually to guide users through the necessary steps:

- 📥 **Data Input Tab**: The initial tab where users upload their data. A successful data upload is necessary before accessing the next tab.
- ⚙️ **Pre-Processing Tab**: This tab becomes accessible after a successful data upload. It offers multiple options to prepare the data for analysis, including diagnostic plots to check for normal distribution.
- 📈 **Analysis Tabs**: Six analysis options are available, each in its own tab. These tabs provide interactive result visualizations and various analytical functions.

To get more information on the tabs, navigate to the respective documentation using the left sidebar.

### Navigation Principles

- 🖱️ **Button Clicks**: Used to evoke actions.
- ⏳ **Loading Bars**: Display waiting times during processing.
- ❓ **Question Marks**: Provide quick help and guide to respective detailed documentation found on this very site.
- 🔽 **Dropdown Menus**: Offer options for selection. Sometimes multiple items can be selected. If clicking on a selected item it is removed from the selection.
- ☑️ **Checkboxes**: If a checkbox is clicked, the respective feature is activated. If clicked again, the feature is deactivated.
- 📏 **Sliding Bars**: Used to adjust numeric parameters or ranges. The range of the sliding bar is displayed above the bar. Simply slide the bar to the desired value.
- 📝 **Text Fields**: Used to enter text. Within cOmicsART these fields are mostly used to hold your Notes, which are saved in the HTML report at the appropriate position. You can use [markdown syntax](https://www.markdownguide.org/cheat-sheet/) here.

### Features Across Tabs

<div style="overflow: hidden;">
  <div style="float: left; width: 70%;">
    <ol>
      <li><strong>Side Panel Structure:</strong> Each tab, except the input tab, has a side panel divided by a horizontal line:
        <ul>
          <li><strong>Upper Section:</strong> Contains options affecting the analysis, requiring recomputation when changed. The analysis is triggered by a "Do/Get analysis" button above the division line.</li>
          <li><strong>Lower Section:</strong> Displays secondary parameters with changes reflecting immediately in the results (automatic update).</li>
        </ul>
      </li>
      <li><strong>Main Panel Structure:</strong> Each main panel contains the visualization of the analysis results. Some panels are further subdivided to show multiple results, for example, the Significance Analysis tab.</li>
      <li><strong>Picture Download Options:</strong> Users can download visualizations and results in common formats (e.g., PNG, TIFF, PDF). There are respective buttons to select the file format. Upon 'Save plot' the file is downloaded to the local machine.</li>
      <li><strong>Sending Visualizations to Report:</strong> Upon button click on 'Send only to report' the current shown visualization and associated parameters are saved to the Report. Hence, you can play around with parameters and can save the one of interest to you without cluttering the report with all tried options.</li>
      <li><strong>Get Underlying R Code and Data:</strong> Upon button click, the R script and respective data to generate shown plot will be presented for download. The script includes the Data selection, pre-processing as well as the respective analysis. For more details make sure to check out [Code and Data](code-and-data.md).</li>
      <li><strong>Notes:</strong> At the bottom of each tab you can find the Notes field - here you can enter text which will be saved within the report. You can use [markdown syntax](https://www.markdownguide.org/cheat-sheet/) here.</li>
    </ol>
  </div>
  <div style="float: right; width: 30%;">
    <img src="{{ site.baseurl }}/assets/images/design_principleSidePanel.png" alt="The design of the side panel" style="width: 100%;">
  </div>
</div>
