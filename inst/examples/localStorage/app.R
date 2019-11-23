library(shiny)
library(shinyThings)

ui <- fluidPage(
  div(
    class = "row",
    div(
      class = "col-xs-6",
      selectInput("key", "Key", choices = c(
        "Name" = "name",
        "Email" = "email",
        "Favorite Color" = "color"
      ))
    ),
    div(
      class = "col-xs-6",
      uiOutput ("ui_value"),
    )
  ),
  actionButton('clear_item', "Clear Item"),
  actionButton('clear_all', "Clear All"),
  verbatimTextOutput("info"),
  use_localStorage()
)

server <- function(input, output, session) {
  output$ui_value <- renderUI({
    value <- input[["_localStorage"]][[input$key]]
    if (is.null(value)) value <- ""
    textInput("value", "Value", value = value)
  })

  debounced_value <- debounce(reactive(input$value), 2000)

  observeEvent(debounced_value(), {
    req(debounced_value())
    store <- list()
    store[[input$key]] <- debounced_value()
    localStorage_store(.list = store, overwrite = TRUE)
  })

  output$info <- renderPrint({
    str(list(
      session_token = session$token,
      localstorage = input[["_localStorage"]]
    ))
  })

  observeEvent(input$clear_item, localStorage_remove(input$key))
  observeEvent(input$clear_all, localStorage_clear())
}

shinyApp(ui, server)
