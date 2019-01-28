# Example of dropdownButton shinyThings Shiny Module
library(shiny)

button_options <- c(
  "Eleven" = "eleven",
  "Will Byers" = "will",
  "Mike Wheeler" = "mike",
  "Dustin Henderson" = "dustin",
  "Lucas Sinclair" = "lucas"
)

ui <- fluidPage(
  titlePanel("shinyThings Dropdown Button"),
  sidebarLayout(
    sidebarPanel(
      shinyThings::dropdownButtonUI(
        id = "dropdown",
        options = button_options,
        label = "Characters"
      )
    ),
    mainPanel(
      tags$p("The last button pressed was ..."),
      verbatimTextOutput("chosen")
    )
  )
)

server <- function(input, output) {
  last_clicked <- shinyThings::dropdownButton("dropdown", button_options)
  output$chosen <- renderPrint({ last_clicked() })
}

shinyApp(ui = ui, server = server)
