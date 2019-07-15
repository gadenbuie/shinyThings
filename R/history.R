#' Undo/Redo History Buttons
#'
#' This is a simple Shiny module for undo/redo history. The Shiny module accepts
#' an arbitrary reactive data value. Changes in the state of this reactive value
#' are tracked and added to the user's history. The user can then repeatedly
#' undo and redo to walk through this stack. The module returns the currrent
#' selected value of the reactive from this historical stack, or `NULL` when
#' the app state was changed by the user. Because this reactive can hold
#' arbitrary data about the state of the Shiny app, it is up to the app
#' developer to use the returned current value to update the Shiny apps' inputs
#' and UI elements.
#'
#' @examples
#' \dontrun{
#' library(shiny)
#' library(shinyThings)
#'
#' ui <- fluidPage(
#'   historyUI("hist", back_text = "Step Backward", fwd_text = "Step Forward"),
#'   textInput("text", "Enter your text here"),
#'   verbatimTextOutput("v"),
#'   tags$h4("debug"),
#'   historyUI_debug("hist")
#' )
#'
#' server <- function(input, output, session) {
#'   update_app_state <- history(
#'     id = "hist",
#'     value = reactive({
#'       req(input$text)
#'       input$text
#'     })
#'   )
#'
#'   observe({
#'     req(update_app_state())
#'     cat("\nupdate_app_state(): ", update_app_state())
#'     updateTextInput(session, "text", value = update_app_state())
#'   })
#'
#'   output$v <- renderPrint(input$text)
#' }
#'
#' shinyApp(ui, server)
#'
#' }
#'
#' @param id The module id
#' @param class The class applied to the parent button group container that
#'   holds the undo/redo buttons.
#' @param btn_class The classes applied to the buttons. Use a single character
#'   vector to apply the same class to both buttons, or a character vector of
#'   length 2 to apply individual classes to each button, (undo/redo
#'   respectively).
#' @param back_text,fwd_text The button text
#' @param back_title,fwd_title The button title (shown on hover)
#' @param back_icon,fwd_icon The icons used for the buttons
#' @param value The reactive expression with the values should be saved for the
#'   user's history. This expression can contain arbitrary data and be of any
#'   structure as long as it returns a single value (or list). Each change in
#'   this value is stored, so the module may not work well for storing large
#'   data sets.
#' @param value_debounce Debounce rate in milliseconds for the `value` reactive
#'   expression. To avoid saving spurious changes in `value`, the expression is
#'   debounced. See [shiny::debounce()] for more information.
#' @name history
#' @export
history <- function(id, value, value_debounce = 500) {
  shiny::callModule(
    historyModule,
    id = id,
    value = value,
    value_debounce = value_debounce
  )
}

#' @describeIn history Create the UI elements for the undo/redo buttons
#' @export
historyUI <- function(
  id,
  class = NULL,
  btn_class = "btn btn-default",
  back_text = NULL,
  back_title = "Undo",
  back_icon = shiny::icon("undo"),
  fwd_text = NULL,
  fwd_title = "Redo",
  fwd_icon = shiny::icon("redo")
) {
  ns <- shiny::NS(id)
  stopifnot(is.character(btn_class))
  if (length(btn_class) == 1) {
    btn_class <- rep(btn_class, 2)
  } else if (length(btn_class) != 2) {
    stop(paste(
      "`btn_class` must be length 1 (applied to both buttons) or 2 (applied to",
      "the undo/redo buttons respectively)."
    ))
  }
  htmltools::tagList(
    htmltools::htmlDependency(
      name    = "shinythings",
      version = utils::packageVersion("shinyThings"),
      package = "shinyThings",
      src     = "js",
      script  = "history.js"
    ),
    tags$div(
      class = spaces("btn-group", class),
      role = "group",
      `aria-label` = "Undo/Redo History",
      tags$button(
        id = ns("history_back"),
        class = spaces(btn_class[1], "action-button disabled"),
        disabled = TRUE,
        title = back_title,
        if (!is.null(back_icon)) back_icon,
        back_text
      ),
      tags$button(
        id = ns("history_forward"),
        type = "button",
        class = spaces(btn_class[2], "action-button disabled"),
        disabled = TRUE,
        title = fwd_title,
        if (!is.null(fwd_icon)) fwd_icon,
        fwd_text
      )
    )
  )
}

#' @describeIn history Debug the saved state. This adds a
#'   [shiny::verbatimTextOutput()] UI element that reports the current history
#'   stacks. `.$history` contains the historical states that are accessed when
#'   undoing or walking backward and `.$future` contains the (psuedo-)future
#'   states to for redo (or walking forward). `.$current` contains the current
#'   value that is reported by the `history()` module. Note that this value will
#'   be `NULL` when the user is driving the apps state updating.
#' @export
historyUI_debug <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    verbatimTextOutput(ns("v_stack"))
  )
}

#' @describeIn history Example app demonstrating usage of the history module.
#' @inheritParams shiny::runApp
#' @export
historyDemo <- function(display.mode = c("showcase", "normal", "auto")) {
  shiny::runApp(
    pkg_file("examples", "history"),
    display.mode = match.arg(display.mode)
  )
}

historyModule <- function(
  input,
  output,
  session,
  value = reactive(NULL),
  value_debounce = 500
) {
  ns <- session$ns

  # changes in record get pushed to top of `stack$history`
  # if the user backs into historical values,
  # then they are moved to top of stack_future
  stack <- reactiveValues(history = list(), future = list(), current = NULL)

  output$v_stack <- renderPrint({
    str(reactiveValuesToList(stack))
  })

  # The module will induce a change in value() when moving through history
  # this flag will track whether the module triggered the last change
  SELF_UPDATE <- 0L

  value_debounced <- debounce(value, value_debounce)

  # Add updates to value_debounced() into the stack$history
  observe({
    req(!is.null(value_debounced()))
    if (SELF_UPDATE > 0L) {
      # Don't store latest change in history because it came from the module
      SELF_UPDATE <<- SELF_UPDATE -1L
      return()
    }
    dbg(id = ns(""), "Adding new record() to stack$history")
    now <- paste(as.numeric(Sys.time()))
    this <- list()
    this[[now]] <- value_debounced()
    stack$history <- c(this, isolate(stack$history))
    stack$future <- list()
    stack$current <- NULL
  })

  # Enable forward/backward buttons if there are values in stack
  has_history <- reactive({
    !(is.null(stack$history) || length(stack$history) <= 1)
  })

  has_future <- reactive({
    !(is.null(stack$future) || length(stack$future) == 0)
  })

  btn_state_lag <- c(FALSE, FALSE)

  observe({
    btn_state <- c(has_history(), has_future())
    if (identical(btn_state, btn_state_lag)) return()

    btn_ids <- ns(c("history_back", "history_forward"))

    dbg(id = ns(""), "Updating button state: ")
    btn_state_send <- list()
    if (any(btn_state))  btn_state_send$enable  <- as.list(btn_ids[btn_state])
    if (any(!btn_state)) btn_state_send$disable <- as.list(btn_ids[!btn_state])
    btn_state_lag <<- btn_state
    session$sendCustomMessage("historyButtons", btn_state_send)
  })

  restore_stack_item <- function(item) {
    timestamp <- names(item)[1] %>% as.numeric() %>% as.POSIXct(origin = "1970-01-01")
    dbg(id = ns(""), "Restoring previous state from ", timestamp)

    stack$current <- item[[1]]
  }

  # Move back in time
  observeEvent(input$history_back, {
    req(length(stack$history > 1))
    # copy stack to save all changes at once at the end
    .stack <- stack
    .stack$current <- NULL

    # current value goes to the future stack
    .stack$future <- c(.stack$history[1], .stack$future)

    # pop the current value off of the history stack
    .stack$history <- .stack$history[-1]

    # restore the previous value
    SELF_UPDATE <<- SELF_UPDATE + 1L
    stack <- .stack
    restore_stack_item(.stack$history[1])
  })

  # Move forward in time
  observeEvent(input$history_forward, {
    req(length(stack$history > 1), length(stack$future) > 0)

    .stack <- stack
    .stack$current <- NULL

    # top of future stack goes to top of history stack
    .stack$history <- c(.stack$future[1], .stack$history)

    # pop the top of future stack
    .stack$future <- .stack$future[-1]

    # restore the (pseudo-)future value
    SELF_UPDATE <<- SELF_UPDATE + 1L
    stack <- .stack
    restore_stack_item(.stack$history[1])
  })

  return(reactive(stack$current))
}
