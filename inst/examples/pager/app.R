# Example of pager shinyThings Shiny Module
library(shiny)

ui <- fluidPage(
  titlePanel("shinyThings Pagination"),
  sidebarLayout(
    sidebarPanel(
      width = 6,
      tags$h4("paginationUI()"),
      shinyThings::paginationUI("pager", width = 12, offset = 0, class = "text-center"),
      tags$hr(),
      sliderInput("page_break", "Page Size", min = 1, max = 6, step = 1, value = 3),
      helpText(tags$code("page_break")),
      tags$hr(),
      tags$h4("pagerUI()"),
      shinyThings::pagerUI("pager", centered = FALSE)
    ),
    mainPanel(
      width = 6,
      tags$p("Page indices:"),
      verbatimTextOutput("page_indices"),
      tags$p(HTML("Paged output (<code>letters</code>):")),
      uiOutput("paged_output")
    )
  )
)

server <- function(input, output) {
  ## page_break and n_items can be reactive or fixed values
  # page_break <- 4
  # n_items <- length(letters)
  n_items <- reactiveVal(length(letters))
  page_break <- reactive({input$page_break})

  page_indices <- shinyThings::pager("pager", n_items, page_break)

  output$page_indices <- renderPrint({
    page_indices()
  })

  output$paged_output <- renderUI({
    tags$ul(
      lapply(letters[page_indices()], tags$li)
    )
  })
}

shinyApp(ui = ui, server = server)
