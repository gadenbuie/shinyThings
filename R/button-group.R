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
#' @param btn_icon An single icon name or a vector of icon names (must be the
#'   same length as `choices`) to be applied to the buttons. See [shiny::icon()]
#'   for more information.
#' @param choice_labels A list of labels for the choices that can be arbitrary
#'   HTML if wrapped in `HTML()`.
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
  btn_icon = NULL,
  choice_labels = names(choices),
  selected = NULL,
  multiple = FALSE,
  aria_label = NULL,
  ...
) {

  if (!is.null(choice_labels) && length(choice_labels) != length(choices)) {
    stop("`choice_labels` must be the same length as `choices`")
  }

  if (any(grepl(" ", choices))) {
    warning("Replaced spaces with `_` in buttonGroup() options")
  }
  choices <- gsub(" ", "_", choices)

  selected <- restoreInput(inputId, selected)
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

  btn_icon <- prep_button_icon(btn_icon, choices)

  button_options <- list(
    input_id = paste0(inputId, "__", unname(choices)),
    text = choice_labels,
    class = btn_class,
    icon = btn_icon,
    selected = selected_lgl
  )

  button_list <- button_options %>%
    purrr::discard(is.null) %>%
    purrr::pmap(make_button)

  tagList(
    htmltools::htmlDependency(
      name    = "shinythings",
      version = packageVersion("shinyThings"),
      package = "shinyThings",
      src     = "resources",
      script  = "shinythingsButtonGroup.js"
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

make_button <- function(input_id, text = NULL, class = "btn btn-default", icon = "", selected = FALSE) {
  class <- paste(class, collapse = " ")
  if (selected) class <- paste(class, "active")
  class <- paste("btn", class)
  tags$button(id = input_id, class = class, if (icon != "") shiny::icon(icon), text)
}

prep_button_icon <- function(btn_icon, choices) {
  if (is.null(btn_icon)) {
    return(rep("", length(choices)))
  }

  # btn icons must be length 1 (all buttons), length of choices, or named
  if (length(btn_icon) == 1) {
    btn_icon <- rep(btn_icon, length(choices))
  } else {
    if (is.null(names(btn_icon))) {
      if (length(btn_icon) != length(choices)) {
        stop("`btn_icon` must be length one or the same length as `options`")
      }
    } else {
      btn_icons <- rep("", length(choices))
      names(btn_icons) <- unname(choices)
      for (choice in intersect(choices, names(btn_icon))) {
        btn_icons[choice] <- btn_icon[choice]
      }
      btn_icon <- btn_icons
    }
  }
  return(btn_icon)
}
