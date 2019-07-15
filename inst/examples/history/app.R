library(shiny)
library(shinyThings)

ui <- fluidPage(
  historyUI("hist", back_text = "Step Backward", fwd_text = "Step Forward"),
  textInput("text", "Enter your text here"),
  verbatimTextOutput("v"),
  tags$h4("debug"),
  historyUI_debug("hist")
)

server <- function(input, output, session) {
  update_app_state <- history(
    id = "hist",
    value = reactive({
      req(input$text)
      input$text
    })
  )

  observe({
    req(update_app_state())
    cat("\nupdate_app_state(): ", update_app_state())
    updateTextInput(session, "text", value = update_app_state())
  })

  output$v <- renderPrint(input$text)
}

shinyApp(ui, server)
