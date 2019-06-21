#' A Bootstrap Button Group Input
#'
#' This input operates like a [shiny::radioButtons()] or
#' [shiny::checkboxGroupInput()] input.
#'
#' @param inputId The input id
#' @param choices A vector of choices for the button group. The names will be
#'   used for button labels and the value are returned by the input. If an
#'   unnamed vector is provided, the button labels and values returned will be
#'   the same with the exception that spaces are replaced with `"_"` in the
#'   value returned by the input.
#' @param btn_class A single class applied to each individual button, or a
#'   vector of button classes for each button (must be same length as
#'   `choices`). For more information see
#'   <https://getbootstrap.com/docs/3.3/css/#buttons>. The default button class
#'   is, appropriately, `"btn-default"`. Be sure to incldue this or a similar
#'   button style class if you modify `btn_class`.
#' @param selected The buttons, by button value, that should be activated.
#' @param multiple By default, only a single button may be toggled at a time.
#'   If `multiple` is `TRUE`, then `buttonGroup()` returns a character vector
#'   of the selected button values.
#' @param ... Passed to [htmltools::div()]
#' @export
buttonGroup <- function(
  inputId,
  choices,
  btn_class = "btn-default",
  selected = NULL,
  multiple = FALSE,
  aria_label = NULL,
  ...
) {
  if (!is.null(selected)) {
    stopifnot(!any(is.na(selected)))
    selected_lgl <- choices %in% selected
  } else {
    selected_lgl <- rep(FALSE, length(choices))
    selected <- NULL
  }

  btn_class <- btn_class %||% "btn-default"

  if (length(btn_class) > 1 && length(btn_class) != length(choices)) {
    stop("`btn_class` must be length one or the same length as `options`")
  }
  if (length(btn_class) == 1) btn_class <- rep(btn_class, length(choices))

  if (is.null(names(choices))) {
    names(choices) <- choices
  }
  if (any(grepl(" ", choices))) {
    warning("Replaced spaces with `_` in buttonGroup() options")
  }
  choices <- gsub(" ", "_", choices)

  button_options <- list(
    input_id = paste0(inputId, "__", unname(choices)),
    text = names(choices),
    class = btn_class,
    selected = selected_lgl
  )

  button_list <- purrr::pmap(button_options, button)

  tagList(
    htmltools::htmlDependency(
      name = "shinythings",
      version = packageVersion("shinyThings"),
      package = "shinyThings",
      src = "resources",
      script = "shinythingsButtonGroup.js"
    ),
    tags$div(
      class = "shinythings-btn-group btn-group",
      id = inputId,
      `data-input-id` = inputId,
      `data-active` = if (!is.null(selected)) jsonlite::toJSON(selected) else "",
      `data-multiple` = as.integer(multiple),
      role = "group",
      ...,
      button_list
    )
  )
}

#' @describeIn buttonGroup Example app demonstrating usage of the buttonGroup
#'   input.
#' @inheritParams shiny::runApp
#' @export
buttonGroupDemo <- function(display.mode = c("showcase", "normal", "auto")) {
  shiny::runApp(
    pkg_file("examples", "buttonGroup"),
    display.mode = match.arg(display.mode)
  )
}

button <- function(input_id, text, class = "btn btn-default", selected = FALSE) {
  class <- paste(class, collapse = " ")
  if (selected) class <- paste(class, "active")
  class <- paste("btn", class)
  tags$button(id = input_id, class = class, text)
}
