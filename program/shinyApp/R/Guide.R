# Guided Tour around the Shiny
library(cicerone)
guide <- Cicerone$
  new()$
  # step(
  #   el="TitleID_normal",
  #   title="Welcome",
  #   description = "I'm happy to guide you through. If you still have open question please feel free to drop me a message (lea.seep@uni-bonn.de)!"
  #   )$
  # step(
  #   el = "Quit_App", # element to attach the information to
  #   title = "Let us start easy",
  #   description = "Here you can end the App right away, if it is already annoying you. It will give you another chance to download the report which is generated automatically during your use of the app"
  # )$
  step(
    el="sidebar1",
    title="Omic Type Test",
    description = "This is a test"
  )$
  step(
    el="[data-value='Data selection']",
    title=" Test",
    description="Test",
    is_id=F
  )$
  step(
    el=".omicType",
    title=" Test",
    description="Test",
    is_id=F
  )$
  step(
    el=".AddGeneSymbols_ui",
    title="Test2",
    description="...",
    is_id=F
  )
