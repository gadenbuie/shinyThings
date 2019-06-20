#' A Bootstrap Button Group Input
#'
#' This input operates like a [shiny::radioButtons()] or
#' [shiny::checkboxGroupInput()] input.
#'
#' @param inputId The input id
#' @inheritParams dropdownButton
#' @param btn_class A single class applied to each individual button, or a
#'   vector of button classes for each button (must be same length as `options`)
#' @param selected The buttons, by button value, that should be activated.
#' @param multiple By default, only a single button may be toggled at a time.
#'   If `multiple` is `TRUE`, then `buttonGroup()` returns a character vector
#'   of the selected button values.
#' @param ... Passed to [htmltools::div()]
#' @export
buttonGroup <- function(
  inputId,
  options,
  btn_class = "btn-default",
  selected = NULL,
  multiple = FALSE,
  aria_label = NULL,
  ...
) {
  if (!is.null(selected) && !is.na(selected)) {
    selected_lgl <- options %in% selected
  } else {
    selected_lgl <- rep(FALSE, length(options))
    selected <- NULL
  }

  btn_class <- btn_class %||% "btn-default"

  if (length(btn_class) > 1 && length(btn_class) != length(options)) {
    stop("`btn_class` must be length one or the same length as `options`")
  }
  if (length(btn_class) == 1) btn_class <- rep(btn_class, length(options))


  button_options <- list(
    input_id = unname(options),
    text = names(options) %||% options,
    class = btn_class,
    selected = selected_lgl
  )

  button_list <- purrr::pmap(button_options, button)

  tagList(
    htmltools::htmlDependency(
      name = "shinythings-assets", version = packageVersion("shinyThings"),
      package = "shinyThings",
      src = "resources",
      script = "shinythingsButtonGroup.js"
    ),
    tags$div(
      class = "shinythings-btn-group btn-group",
      id = inputId,
      `data-input-id` = inputId,
      `data-active` = paste0('["', selected, '"]', collapse = '", "'),
      `data-multiple` = as.integer(multiple),
      role = "group",
      ...,
      button_list
    )
  )
}

button <- function(input_id, text, class = "btn btn-default", selected = FALSE) {
  class <- paste(class, collapse = " ")
  if (selected) class <- paste(class, "active")
  class <- paste("btn", class)
  tags$button(id = input_id, class = class, text)
}
