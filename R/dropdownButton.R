#' @describeIn dropdownButton Creates the dropdown button UI.
#'
#' @param id The shared `id` of the `dropdownButtonUI()` and the
#'   `dropdownButton()` module
#' @param options A named vector of options and labels. The name is the label
#'   that will appear on the button and the value is the id of the input that is
#'   returned from the Shiny modules.
#' @param label The button text of the "master" button containing the dropdown
#'   buttons.
#' @param type Type of dropdown: one of `"dropdown"` or `"dropup"`
#' @param buttonId The HTML id of the dropdown button.
#' @param class CSS classes to be added to the dropdown button.
#' @family dropdownButton
#' @export
dropdownButtonUI <- function(
  id,
  options,
  label = "Options",
  type = "dropdown",
  buttonId = paste0(id, "-dropdown"),
  class = "btn-default"
) {
  ns <- NS(id)
  if (!grepl("btn-", class)) class <- paste("btn-default", class)
  if (is.null(class) || length(class) < 1) {
    class <- "btn-default"
  } else if (length(class) > 1) {
    class <- paste(class, collapse = " ")
  }
  tags$div(
    class = paste("btn-group", type),
    tags$button(
      class = paste("btn", class, "dropdown-toggle"),
      role = "button",
      id = buttonId,
      "data-toggle" = "dropdown",
      "aria-haspopup" = "true",
      "aria-expanded" = "false",
      label , tags$span(class = "caret")
    ),
    tags$ul(
      class = "dropdown-menu box-shadow",
      "aria-labelledby" = buttonId,
      purrr::imap(options, dropdown_buttons, ns = ns)
    )
  )
}

dropdown_buttons <- function(input_id, text, ns) {
  tags$li(actionLink(ns(input_id), text), class = "dropdown-item")
}

#' Bootstrap Dropdown Button Module
#'
#' Creates a bootstrap dropdown button UI, with a server module that returns
#' the id of the most recently clicked button.
#'
#' @section Usage:
#'
#' - ui: [dropdownButtonUI()]
#' - server: [dropdownButton()]
#'
#' @examples
#' \dontrun{
#' library(shiny)
#'
#' button_options <- c(
#'   "Option A" = "opt_a",
#'   "Option B" = "opt_b"
#' )
#'
#' ui <- fluidPage(
#'   dropdownButtonUI(
#'     id = "dropdown",
#'     options = button_options,
#'     label = "Options"
#'   ),
#'   verbatimTextOutput("chosen")
#' )
#'
#' server <- function(input, output) {
#'   last_clicked <- dropdownButton("dropdown", button_options)
#'   output$chosen <- renderPrint({ last_clicked() })
#' }
#'
#' shinyApp(ui = ui, server = server)
#' }
#' @inheritParams dropdownButtonUI
#' @family dropdownButton
#' @export
dropdownButton <- function(id, options) {
  shiny::callModule(dropdownButtonModule, id = id, options = options)
}

dropdownButtonModule <- function(input, output, session, options) {
  ns <- session$ns

  if (!inherits(options, "reactive")) options <- reactiveVal(options)
  prev_state <- NULL
  observe({
    prev_state <<- stats::setNames(rep(0L, length(options())), names(options()))
  })

  this_state <- reactive({
    purrr::map_dbl(options(), ~ input[[.]] %||% 0L)
  })

  clicked_button <- reactive({
    this_state <- this_state()
    updated_state <- which(prev_state != this_state)
    if (length(updated_state) > 1) {
      warning("More than one button state was updated!")
    }
    prev_state <<- this_state
    options()[updated_state]
  })

  return(reactive(clicked_button()))
}

#' @describeIn dropdownButton Example app demonstrating usage of the
#'   dropdownButton module.
#' @inheritParams shiny::runApp
#' @export
dropdownButtonDemo <- function(display.mode = c("showcase", "normal", "auto")) {
  shiny::runApp(system.file("examples", "dropdownButton", package = "shinyThings"),
                display.mode = match.arg(display.mode))
}
