library(shiny)
library(shinyThings)

ui <- fluidPage(
  # Add the Undo/Redo buttons to the UI
  undoHistoryUI("hist", back_text = "Step Backward", fwd_text = "Step Forward"),

  # A simple text input element whose history we'll track
  textInput("text", "Enter your text here"),

  # Debugging elements for the demo
  verbatimTextOutput("v"),
  tags$h4("debug"),
  undoHistoryUI_debug("hist")
)

server <- function(input, output, session) {
  # Use undoHistory() to keep track of the value of input$text
  undo_app_state <- undoHistory(
    id = "hist",
    value = reactive({
      # Value must be a reactive, but can be any structure you want
      req(!is.null(input$text))
      input$text
    })
  )

  # Use an observer to receive updates from undoHistory() and update the app.
  observe({
    req(!is.null(undo_app_state())) #<< Need to update app whenever not NULL

    # Manually update app UI and reactive values
    updateTextInput(session, "text", value = undo_app_state())
  })

  # Just for debugging
  output$v <- renderPrint(input$text)
}

shinyApp(ui, server)
