library(shiny)
library(shinyThings)
radioSwitchButtons_default_style(
  selected_background = "#faa555"
)

ui <- fluidPage(
  inputPanel(

    radioButtons("std", "Standard Radios", c("Yes" = "yes", "No" = "no"), inline = TRUE),

    radioSwitchButtons(
      inputId = "simple",
      label = "Yes or No?",
      choices = c("Yes" = "yes", "No" = "no"),
      selected = "no"
    )
  ),
  actionButton("make_yes", "Say Yes!"),

  inputPanel(
    radioSwitchButtons(
      inputId = "other",
      label = "Yes or No?",
      choices = c("Yes" = "yes", "No" = "no", "Maybe?" = "maybe"),
      selected_background = "#eb1455"
    ),

    radioSwitchButtons(
      inputId = "small",
      label = "Style",
      choices = c("plain", "bold", "italic"),
      choice_labels = list(
        tags$span(style = "font-weight: normal", "P"),
        tags$strong("B"),
        tags$em("I")
      )
    )
  ),

  verbatimTextOutput("values")
)

server <- function(input, output, session) {
  output$values <- renderPrint({
    str(list(
      radioButtons       = input$std,
      radioSwitchButtons = input$simple,
      moreThanTwo        = input$other,
      style              = input$small
    ))
  })

  observeEvent(input$make_yes, {
    updateRadioSwitchButtons("simple", "yes")
  })
}

shinyApp(ui, server)
