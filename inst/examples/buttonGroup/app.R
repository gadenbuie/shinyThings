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
  verbatimTextOutput("chosen_checkbox"),
  tags$h4("Buttons with Style"),
  shinyThings::buttonGroup(
    inputId = "button_style",
    options = button_options,
    btn_class = paste0("btn-", c("primary", "success", "info", "warning", "danger")),
    multiple = TRUE
  ),
  tags$p(),
  verbatimTextOutput("chosen_style"),
  tags$h4("Buttons with Initial Settings"),
  shinyThings::buttonGroup(
    inputId = "button_init",
    options = button_options,
    selected = c("eleven", "mike", "lucas"),
    btn_class = "btn-default btn-xs",
    multiple = TRUE
  ),
  tags$p(),
  verbatimTextOutput("chosen_init")
)

server <- function(input, output) {
  output$chosen_radio <- renderPrint({ input$button_radio })
  output$chosen_checkbox <- renderPrint({ input$button_checkbox })
  output$chosen_style <- renderPrint({ input$button_style })
  output$chosen_init <- renderPrint({ input$button_init })
}

shinyApp(ui = ui, server = server)
