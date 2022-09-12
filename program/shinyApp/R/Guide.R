# Guided Tour around the Shiny
library(cicerone)
guide <- Cicerone$
  new()$
  step(
    el="TitleID_normal",
    title="Welcome",
    description = "I'm happy to guide you through. If you still have open question please feel free to drop me a message (lea.seep@uni-bonn.de)!"
    )$
  # step(
  #   el="TitleID_pride",
  #   title="Welcome",
  #   description = "I'm happy to guide you through. If you still have open question please feel free to drop me a message (lea.seep@uni-bonn.de)!"
  # )$
  step(
    el = "Quit_App", # element to atttach the information to
    title = "Let us start easy",
    description = "Here you can end the App right away, if it is already annoying you. It will give you another chance to download the report which is generated automatically during your use of the app"
  )
