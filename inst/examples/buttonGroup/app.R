# Example of buttonGroup shinyThings Shiny Input
library(shiny)

button_options <- c(
  "Eleven" = "eleven",
  "Will Byers" = "will",
  "Mike Wheeler" = "mike",
  "Dustin Henderson" = "dustin",
  "Lucas Sinclair" = "lucas"
)

ui <- fluidPage(
  titlePanel("shinyThings Button Group"),
  tags$h4("Radio Buttons"),
  shinyThings::buttonGroup(
    inputId = "button_radio",
    options = button_options
  ),
  tags$p(),
  verbatimTextOutput("chosen_radio"),
  tags$h4("Checkbox Buttons"),
  shinyThings::buttonGroup(
    inputId = "button_checkbox",
    options = button_options,
    multiple = TRUE
  ),
  tags$p(),
  verbatimTextOutput("chosen_checkbox")
)

server <- function(input, output) {
  output$chosen_radio <- renderPrint({ input$button_radio })
  output$chosen_checkbox <- renderPrint({ input$button_checkbox })
}

shinyApp(ui = ui, server = server)
