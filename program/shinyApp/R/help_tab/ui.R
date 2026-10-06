help_tab_sidebar_panel <- sidebarPanel(
  id = "sidebar_help_tab",
  h4("Sidebar"),
  div(id="firstQ",p("Here you can select parameters which are important for your analysis.") %>% helper(type = "markdown", content = "helpTab_question")),
  br(),
  div(
    id = "ImageSelectArea",
    shinyWidgets::virtualSelectInput(
  inputId = "ImageSelect",
  label = "Select Image",
  choices = c("nothing selected", "WelcomePage", "YouTube Tutorial"),
  selected = "nothing selected"
  )
  ),
  # Horizontal line
  actionButton(
     inputId = "get_help",
     label = "GO!",
     icon = icon('paper-plane'),
     style = "color: black; background-color: white; border-color: black;"
   ),
  div(
    id = "horizontalLine",
    hr(style = "border-top: 1px solid #858585;")
    ),
  div(
    id = "options",
    sliderInput("ImageWidth",
                label="image width",
                min = 50, 
                max = 100, 
                post  = " %", 
                value = 100),
    radioButtons(
      inputId = "ImageHeight",
      label = "image height",
      choices = c("300px","400px", "500px"),
      selected = "400px",
    )
  )
  # Add some option that renders the 
  )



help_tab_main_panel <- mainPanel(
  id = "mainPanel_help_tab",
  h4("Main Panel",id = "Test"),
  actionButton(
    "start_tour", 
    span(icon("hand-pointer"),"Tour around cOmicsArt"),
    style = "
    background-color: #00c6ff; 
    color: white; 
    padding: 10px 20px; 
    border-radius: 10px; 
    font-size: 15px;
    font-weight: bold;
    transition: transform 0.2s;"),
  div(
    id = "help_tab_info",
    htmlOutput(outputId = "help_tab_info", container = pre),
  ),
  # Conditional content based on ImageSelect (client-side rendering for performance)
  conditionalPanel(
    condition = "input.ImageSelect == 'WelcomePage'",
    imageOutput("WelcomePage")
  ),
  conditionalPanel(
    condition = "input.ImageSelect == 'YouTube Tutorial'",
    tags$div(
      id = "youtube_container",
      tags$iframe(
        id = "youtube_iframe",
        src = "https://www.youtube.com/embed/pTGjtIYQOak",
        frameborder = "0",
        allow = "accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",
        allowfullscreen = TRUE,
        style = "width: 100%; height: 600px;" # Will be updated by Shiny inputs
      )
    )
  ),
  # JavaScript to update YouTube iframe dimensions based on inputs
  tags$script(HTML("
    $(document).on('shiny:inputchanged', function(event) {
      if (event.name === 'ImageWidth' || event.name === 'ImageHeight') {
        var iframe = document.getElementById('youtube_iframe');
        if (iframe) {
          if (event.name === 'ImageWidth') {
            iframe.style.width = event.value + '%';
          }
          if (event.name === 'ImageHeight') {
            iframe.style.height = event.value;
          }
        }
      }
    });
  ")),
  # Line break for additional spacing
  br(),
  div(
    # Action button
    actionButton(
      inputId = "NextPanel_tutorial",
      label = "Take me to the Analysis Start",
      width = "100%",
      icon = icon('rocket'),
      style = "color: #050505; background-color: #70BF4F47; border-color: #000000"
    )
  )

)


help_tab_panel <- tabPanel(
  title = tagList(icon("home"), tags$span("Welcome to cOmicsArt")), 
  value = "Welcome to comicsArt",
  id = "help_tab",
  fluid = T,
  h4("User Help"),
  ################################################################################
  # Data Selection
  ################################################################################
  help_tab_sidebar_panel,
  help_tab_main_panel
)
