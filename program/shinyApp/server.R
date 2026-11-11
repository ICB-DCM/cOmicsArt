server <- function(input,output,session){

  # fill session_if textOutput with current session$token
  output$session_id <- renderText({
      paste0("Current session: ", session$token)
  })

  # getCurrentVersion(updateDESCRIPTION=T) # Where to Place this ? So it does not always get 'updated'?
  # Can we add this somehow as necassary to every new release?

# Security section ----
  options(shiny.maxRequestSize=70*(1024^2)) # request 70MB

  #### Session folder
  # www/<token>/ holds this session's report and files; it is deleted when
  # the session ends (onSessionEnded below), never at the start of another
  create_session_dir(session)
  observe_helpers()
  
  
# Download Report pdf ----
  DownloadReport_server("DownloadTestModule")
  
# Layout upon Start ----
  hideTab(inputId = "tabsetPanel1", target = "Pre-processing")
  hideTab(inputId = "tabsetPanel1", target = "Sample Correlation")
  hideTab(inputId = "tabsetPanel1", target = "Differential Analysis")
  hideTab(inputId = "tabsetPanel1", target = "PCA")
  hideTab(inputId = "tabsetPanel1", target = "Heatmap")
  hideTab(inputId = "tabsetPanel1", target = "Single Gene Visualisations")
  hideTab(inputId = "tabsetPanel1", target = "Enrichment Analysis")
  hideTab(inputId = "tabsetPanel1", target = "ML Classification")
  shinyjs::hideElement(id = "selection-mainPanel_other")

# Set Up ad Show user Landing page ----
  # Define the guide
  # Note do this in here to avoid setting a global upon close
  guide_welcome <- Cicerone$
    new(id = "guide",
        opacity = 0.9,
        padding = 10,
        keyboard_control = TRUE)$
    step(
      el = "start_tour",
      title = "Welcome to cOmicsArt!",
      position = "right-center",
      description = HTML("
      <div style='min-width: 300px; min-height: 150px; padding: 10px;'>
        <img src='Logo_cOmicsArt_clear.png' alt='cOmicsArt Logo' style='max-width:90%;'>
        <div style='font-size: 18px; margin-top: 10px;'>
          <p><i class='fas fa-question-circle'></i> Need help? Press the blue button</p>
          <p><i class='fas fa-rocket'></i> Want to start directly? Click 'Next'.</p>
            <div style='font-size: 14px; color: #777; margin-top: 15px;'>
              <p><input type='checkbox' id='set_cookie_checkbox'> Do not show next time (sets a cookie)</p>
            </div>
        </div>
      </div>
    ")
    )$
    step(
      el = "tabsetPanel1",
      title = "Welcome to cOmicsArt!",
      description = "You pressed next - redirection triggered",
    )
  
  # Check if the cookie is present and update the output
  # On page load, check if the cookie exists
  observe({
    shinyjs::runjs("
      if (!checkHasBeenBeforeCookie()) {
        Shiny.setInputValue('first_visit', true);
      } else {
        Shiny.setInputValue('first_visit', false);
      }
    ")
  })

  # If it is the user's first visit, start the guide
  observeEvent(input$first_visit, {
    # shinytest2 runs the app in test mode: skip the welcome tour there
    if (input$first_visit && !isTRUE(getShinyOption("testmode"))) {
      guide_welcome$init()$start()
    } else {
      showTab(inputId = "tabsetPanel1", target = "Data selection", select = TRUE)
    }
  })
  
  # Delete the cookie if the toggle input is checked
  observeEvent(input$set_cookie, {
      shinyjs::runjs("deleteCookie('hasBeenBefore')")
      showNotification("Cookie 'hasBeenBefore' deleted. Please refresh the page.")
  })

  # Start the tour when the "Start Tour" button is clicked
  observeEvent(input$start_tour, {
    print("Star Tour")
    shinyjs::runjs("document.querySelector('.driver-close-btn').click();")
    guide$init()$start()
  })

  observeEvent(input$guide_cicerone_next, {
    print("Next")
    shinyjs::runjs("document.querySelector('.driver-close-btn').click();")
    showTab(inputId = "tabsetPanel1",target = "Data selection",select = T)
  })
  
  output$help_tab_info <- renderText({
    HTML(paste0(
    "Please select an image to display within the sidebar(left)<br>",
    "To confirm your selection click the button labelled 'GO show me help!' within the sidebar."
    ))
  })

  # Help tab: WelcomePage image rendering (reactive to width/height sliders)
  # Note: Show/hide logic now handled client-side by conditionalPanel in ui.R
  output$WelcomePage <- renderImage({
    req(input$ImageSelect == "WelcomePage")  # Only render when WelcomePage is selected
    list(
      src = "www/WelcomPage.png",
      contentType = "image/png",
      width = paste0(input$ImageWidth,"%"),
      height = input$ImageHeight
    )
  }, deleteFile = FALSE)

  # Help tab: Info text that updates based on selection
  output$help_tab_info <- renderText({
    if(input$ImageSelect == "WelcomePage") {
      HTML(paste0(
        "As you selected the WelcomePage on the <b>left</b>, you can see a screenshot of the WelcomePage below.<br>",
        "If you want to see the full documentation, click ",
        "<a href='https://icb-dcm.github.io/cOmicsArt/' target='_blank'>here</a>",
        ".<br>or click on the link on the top left of the screen 'Go To Documentation'.<br><br>",
        "Within cOmicsArt this box will display information depending on the current tab you are in.<br> A tab represents an analysis."
      ))
    } else if(input$ImageSelect == "nothing selected") {
      HTML(paste0(
        "You have selected 'nothing selected' - hence there is nothing to show<br>.",
        "Do you want to see something else? Try to select a different Image (Select Image) on the left!<br>",
        "Did you maybe just press the button?"
      ))
    } else {
      ""  # Empty for YouTube Tutorial
    }
  })

  observeEvent(input$NextPanel_tutorial,{
    showTab(inputId = "tabsetPanel1",target = "Data selection",select = T)
  })



# Session folder ----
  # www/<token>/ is removed when the session ends
  session$onSessionEnded(function() {
    # Delete the folder with the session token
    if(dir.exists(paste0("www/",session$token))){
      # Remove the directory and its contents
      unlink(paste0("www/",session$token), recursive = TRUE)
      if (!dir.exists(paste0("www/",session$token))) {
        cat("Session cleanup: Directory successfully removed.\n")
      } else {
        cat("Session cleanup: Directory could not be removed.\n")
      }
    }
  })
## Quit App Button ----
  observeEvent(input$Quit_App,{
    showModal(
      modalDialog(
        tags$h4('You can download the complete report by clicking on the link'),
        footer=tagList(
          a(
            href="Report.html", 
            "Download report", 
            download=NA, 
            target="_blank"
          ),
          actionButton(
            inputId = "Done",
            label = "Done"
          ),
          modalButton('Cancel')
        )
      )
    )
  })
  
  observeEvent(input$Done,{
    removeModal()
    show_toast(
      title = "Good Bye!",
      type = "success",
      position = "top",
      timerProgressBar = FALSE,
      width = "100%"
      )
    session$close()
  })
  
# Upload ----
  upload <- upload_server("upload")

  # Show the Selection controls and the Pre-processing tab
  observeEvent(upload$done(), {
    shinyjs::showElement("selection-mainPanel_other")
    if(upload$source() != "testdata"){
      showTab(inputId = "tabsetPanel1", target = "Pre-processing")
    }
  })
  observeEvent(upload$cleared(), {
    # as before the first Upload: no Selection controls, no Pre-processing tab
    shinyjs::hideElement("selection-mainPanel_other")
    hideTab(inputId = "tabsetPanel1", target = "Pre-processing")
  })

# Selection ----
  selection <- selection_server(
    "selection",
    raw = upload$raw,
    upload_done = upload$done
  )

  # "Go to Preprocessing" / "Start the Journey": report lines, then Pre-processing
  observeEvent(selection$confirmed(),{
    chosen <- selection$inputs()
    # add row and col selection options
    fun_LogIt(session, message = "**DataSelection** - The following selection was conducted:")
    fun_LogIt(session, message = paste0(
      "**DataSelection** - Samples:\n\t DataSelection - based on: ",
      chosen$sample_type,": ",
      paste(chosen$sample_selection,collapse = ", ")
    ))
    fun_LogIt(session, message = paste0(
      "**DataSelection** - Entities:\n\t DataSelection - based on: ",
      chosen$row_type,
      ": ",paste(chosen$row_selection,collapse = ", ")
    ))
    
    fun_LogIt(session, message = "### Publication Snippet")
    fun_LogIt(session, message = snippet_dataInput(
      raw = upload$raw(),
      selection = selection$selected()$params,
      omic_type = upload$omic_type(),
      organism = upload$organism()
    ))
    fun_LogIt(session, message = "<br>")
    showTab(inputId = "tabsetPanel1",target = "Pre-processing",select = T)
  })
  
# Pre-processing ----
  preprocessing <- preprocessing_server(
    "preprocessing",
    upload = upload$raw,
    omic_type = upload$omic_type,
    selected = selection$selected,
    selection_confirmed = selection$confirmed,
    upload_done = upload$done,
    organism = upload$organism
  )

  observeEvent(preprocessing$start_discovering(), {
    guide_startDiscover$init()$start()
  })

# Dataset ----
  # the module id starts with a letter so its IDs are valid CSS selectors
  datasets <- list("1" = dataset_server("dataset_1", upload, selection, preprocessing))

  # the analysis tabs show while Dataset 1 is pre-processed; its version
  # changes once per new Dataset, run or clear, not on every change of the
  # controls
  observeEvent(datasets[["1"]]$version(), {
    dataset <- datasets[["1"]]
    if (is.null(dataset$processed())) hide_tabs() else show_tabs(dataset$omic_type())
  })

}
