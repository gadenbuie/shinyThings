# Example of buttonGroup shinyThings Shiny Input
library(shiny)

button_choices <- c(
  "Eleven" = "eleven",
  "Will Byers" = "will",
  "Mike Wheeler" = "mike",
  "Dustin Henderson" = "dustin",
  "Lucas Sinclair" = "lucas"
)

ui <- function(request) {
  fluidPage(
    titlePanel("shinyThings Button Group"),
    id = "button_group_page",
    tags$h4("Radio Buttons"),
    shinyThings::buttonGroup(
      inputId = "button_radio",
      choices = button_choices
    ),
    tags$p(),
    verbatimTextOutput("chosen_radio"),

    tags$h4("Checkbox Buttons"),
    shinyThings::buttonGroup(
      inputId = "button_checkbox",
      choices = button_choices,
      multiple = TRUE
    ),
    tags$p(),
    verbatimTextOutput("chosen_checkbox"),

    tags$h4("Buttons with Style"),
    shinyThings::buttonGroup(
      inputId = "button_style",
      choices = button_choices,
      btn_class = paste0("btn-", c("primary", "success", "info", "warning", "danger")),
      multiple = TRUE
    ),
    tags$p(),
    verbatimTextOutput("chosen_style"),

    tags$h4("Buttons with Initial Settings"),
    shinyThings::buttonGroup(
      inputId = "button_init",
      choices = button_choices,
      selected = c("eleven", "mike", "lucas"),
      btn_class = "btn-default btn-sm",
      multiple = TRUE
    ),
    tags$p(),
    verbatimTextOutput("chosen_init"),

    tags$h4("Easy Buttons (Removes Spaces)"),
    shinyThings::buttonGroup(
      inputId = "button_easy",
      choices = c("Eleven", "Will Byers", "Mike Wheeler")
    ),
    tags$p(),
    verbatimTextOutput("chosen_easy"),

    bookmarkButton()
  )
}

server <- function(input, output) {
  output$chosen_radio    <- renderPrint(input$button_radio)
  output$chosen_checkbox <- renderPrint(input$button_checkbox)
  output$chosen_style    <- renderPrint(input$button_style)
  output$chosen_init     <- renderPrint(input$button_init)
  output$chosen_easy     <- renderPrint(input$button_easy)
}

shinyApp(ui = ui, server = server, enableBookmarking = "url")
