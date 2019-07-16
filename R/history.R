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
#'   # Add the Undo/Redo buttons to the UI
#'   undoHistoryUI("hist", back_text = "Step Backward", fwd_text = "Step Forward"),
#'
#'   # A simple text input element whose history we'll track
#'   textInput("text", "Enter your text here"),
#'
#'   # Debugging elements for the demo
#'   verbatimTextOutput("v"),
#'   tags$h4("debug"),
#'   undoHistoryUI_debug("hist")
#' )
#'
#' server <- function(input, output, session) {
#'   # Use undoHistory() to keep track of the value of input$text
#'   undo_app_state <- undoHistory(
#'     id = "hist",
#'     value = reactive({
#'       # Value must be a reactive, but can be any structure you want
#'       req(!is.null(input$text))
#'       input$text
#'     })
#'   )
#'
#'   # Use an observer to receive updates from undoHistory() and update the app.
#'   observe({
#'     req(!is.null(undo_app_state())) #<< Need to update app whenever not NULL
#'
#'     # Manually update app UI and reactive values
#'     updateTextInput(session, "text", value = undo_app_state())
#'   })
#'
#'   # Just for debugging
#'   output$v <- renderPrint(input$text)
#' }
#'
#' shinyApp(ui, server)
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
#' @param back_icon,fwd_icon The icons used for the buttons, passed to
#'   [shiny::icon()]. Set to `NULL` for no icon. You can also add arbitrary HTML
#'   to `back_text` and `fwd_text` as the inner HTML of the `<button>` element.
#' @param value The reactive expression with the values should be saved for the
#'   user's history. This expression can contain arbitrary data and be of any
#'   structure as long as it returns a single value (or list). Each change in
#'   this value is stored, so the module may not work well for storing large
#'   data sets.
#' @param value_debounce_rate Debounce rate in milliseconds for the `value`
#'   reactive expression. To avoid saving spurious changes in `value`, the
#'   expression is debounced. See [shiny::debounce()] for more information.
#' @export
undoHistory <- function(id, value, value_debounce_rate = 500) {
  shiny::callModule(
    undoHistoryModule,
    id = id,
    value = value,
    value_debounce_rate = value_debounce_rate
  )
}

#' @describeIn undoHistory Create the UI elements for the undo/redo buttons
#' @export
undoHistoryUI <- function(
  id,
  class = NULL,
  btn_class = "btn btn-default",
  back_text = NULL,
  back_title = "Undo",
  back_icon = "undo",
  fwd_text = NULL,
  fwd_title = "Redo",
  fwd_icon = "redo"
) {
  ns <- shiny::NS(id)
  stopifnot(is.null(class) || is.character(class))
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
      script  = "undoHistory.js"
    ),
    tags$div(
      class = spaces("btn-group", class),
      role = "group",
      `aria-label` = "Undo/Redo History",
      tags$button(
        id = ns("history_back"),
        class = spaces(btn_class[1], "action-button disabled"),
        `data-val` = 0L,
        disabled = TRUE,
        title = back_title,
        if (!is.null(back_icon)) shiny::icon(back_icon),
        back_text
      ),
      tags$button(
        id = ns("history_forward"),
        type = "button",
        class = spaces(btn_class[2], "action-button disabled"),
        `data-val` = 0L,
        disabled = TRUE,
        title = fwd_title,
        if (!is.null(fwd_icon)) shiny::icon(fwd_icon),
        fwd_text
      )
    )
  )
}

#' @describeIn undoHistory Debug the saved state. This adds a
#'   [shiny::verbatimTextOutput()] UI element that reports the current history
#'   stacks. `.$history` contains the historical states that are accessed when
#'   undoing or walking backward and `.$future` contains the (psuedo-)future
#'   states to for redo (or walking forward). `.$current` contains the current
#'   value that is reported by the `undoHistory()` module. Note that this value
#'   will be `NULL` when the user is driving the apps state updating.
#' @export
undoHistoryUI_debug <- function(id) {
  ns <- shiny::NS(id)
  tagList(
    verbatimTextOutput(ns("v_stack"))
  )
}

#' @describeIn undoHistory Example app demonstrating usage of the history module.
#' @inheritParams shiny::runApp
#' @export
undoHistoryDemo <- function(display.mode = c("showcase", "normal", "auto")) {
  shiny::runApp(
    pkg_file("examples", "history"),
    display.mode = match.arg(display.mode)
  )
}

undoHistoryModule <- function(
  input,
  output,
  session,
  value = reactive(NULL),
  value_debounce_rate = 500
) {
  ns <- session$ns

  ref_time <- as.integer(Sys.time())

  abs_time <- function(rel_time) {
    abs_time <- as.integer(rel_time) + ref_time
    as.POSIXct(abs_time, origin = "1970-01-01")
  }

  # changes in record get pushed to top of `stack$history`
  # if the user backs into historical values,
  # then they are moved to top of stack_future
  stack <- reactiveValues(history = list(), future = list(), current = NULL)

  output$v_stack <- renderPrint({
    str(reactiveValuesToList(stack))
  })

  value_debounced <- debounce(value, value_debounce_rate)

  # Add updates to value_debounced() into the stack$history
  observe({
    req(!is.null(value_debounced()))
    current_value <- isolate(stack$current)
    if (!is.null(current_value) && identical(current_value, value_debounced())) {
      # Don't store latest change in history because it came from the module
      # or is the same as the most recent state
      return()
    }
    dbg(id = ns(""), "Adding new value from app to stack$history")
    now <- paste(as.integer(Sys.time()) - ref_time)
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

    dbg(
      id = ns(""), "Updating button state:",
      if (any(btn_state)) paste0(" +", paste(btn_ids[btn_state], collapse = ",")),
      if (any(!btn_state)) paste0(" -", paste(btn_ids[!btn_state], collapse = ","))
    )
    btn_state_send <- list()
    if (any(btn_state)) btn_state_send$enable  <- as.list(btn_ids[btn_state])
    if (any(!btn_state)) btn_state_send$disable <- as.list(btn_ids[!btn_state])
    btn_state_lag <<- btn_state
    session$sendCustomMessage("undoHistoryButtons", btn_state_send)
  })

  restore_stack_item <- function(item) {
    timestamp <- names(item)[1] %>% abs_time()
    dbg(id = ns(""), "Restoring previous state from ", timestamp)

    stack$current <- item[[1]]
  }

  # Move back in time
  observeEvent(input$history_back, {
    req(length(stack$history) > 1)

    # copy stack to save all changes at once at the end
    .stack <- reactiveValuesToList(stack)
    .stack$current <- NULL

    # current value goes to the future stack
    .stack$future <- c(.stack$history[1], .stack$future)

    # pop the current value off of the history stack
    .stack$history <- .stack$history[-1]

    # restore the previous value
    stack$future <- .stack$future
    stack$history <- .stack$history
    restore_stack_item(.stack$history[1])
  }, priority = 1000)

  # Move forward in time
  observeEvent(input$history_forward, {
    req(length(stack$history) > 0, length(stack$future) > 0)

    .stack <- reactiveValuesToList(stack)
    .stack$current <- NULL

    # top of future stack goes to top of history stack
    .stack$history <- c(.stack$future[1], .stack$history)

    # pop the top of future stack
    .stack$future <- .stack$future[-1]

    # restore the (pseudo-)future value
    stack$future <- .stack$future
    stack$history <- .stack$history
    restore_stack_item(.stack$history[1])
  }, priority = 1000)

  return(reactive(stack$current))
}
