# Guided Tour around the Shiny
library(cicerone)
guide <- Cicerone$
  new( id = "guide")$
  step(
    el="TitleID_normal",
    title="Welcome",
    description = "We are happy to guide you through. If you still have open questions please feel free to drop us a message (cOmicsArtist@outlook.de)!"
  )$step(
    el = "Quit_App", # element to attach the information to
    title = "Let us start easy",
    description = "Here you can end the App right away, if it is already annoying you. It will give you another chance to download the report which is generated automatically during your use of the app"
  )$
  step(
    el="Data_selection",
    title="First Step: Data Upload & Selection",
    description = "This needs to be filled"
  )$
  step(
    el="mainPanel_DataSelection",
    title="Data Upload",
    description = "This needs to be filled"
  )$
  step(
    el="sidebar1",
    title="Sidebar",
    description = "This is the sidebar. In general you can choose options or procedures which results will be displayed in the main Panel."
  )$
  step(
    el=".omicType",
    title=" Choose your type of Omic",
    description="After you have selected your input data specify here what kind of data you uploaded. This is crucial as some pre-processing steps are omic-type specific. If you upload a pre-compiled set and select the wrong omic-type you will get an error (can be seen at the bottom of the Main Panel)",
    is_id=F
  )$
  step(
    el=".AddGeneSymbols_ui",
    title="Adding Gene Annotation",
    description="For transcriptomics data, you have the option to add some gene-annotation to your supplied entitie data. Gene_type (such as protein-coding or miRNA) will be added as well as gene Symbols",
    is_id=F
  )$
  step(
    el="refresh1",
    title="Click it",
    description="With this 'Do' you will really upload the data to cOmicsArt. Make sure you have selected the correct Omic Type and have decided whether you want additional annotation or not. If you change anything above this button make sure to click it again, so that you enforce an update!"
  )$
  step(
    el=".DataSelection",
    title="Click it",
    description="...",
    is_id=F
  )$
  step(
    el=".SampleSelection",
    title="Click it",
    description="...",
    is_id=F
  )
