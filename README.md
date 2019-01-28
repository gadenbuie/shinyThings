
<!-- README.md is generated from README.Rmd. Please edit that file -->

# shinyThings

## Installation

You can install shinyThings from Github via

``` r
# install.packages("remotes")
remotes::install_github("gadenbuie/shinyThings")
```

## Components

## Components

### dropdownButton

![](man/figures/README-dropdownButton-example.png)

``` r
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
```
