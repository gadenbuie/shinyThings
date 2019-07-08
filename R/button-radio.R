#' Radio Switch Buttons
#'
#' This input creates a radio switch that works like [shiny::radioButtons()]
#' with the appearance of a button.
#'
#' @param inputId The input id
#' @inheritParams buttonGroup
#' @param selected The value that should be active initially.
#' @param selected_background Background color of the label when selected. Can
#'   be set globally via [radioSwitchButtons_default_style()]. Default value
#'   is `"#007BFF"`.
#' @param selected_color Text color of the label text when selected. Can be set
#'   globally via [radioSwitchButtons_default_style()]. Default value is
#'   `"#FFFFFF"`.
#' @param not_selected_background Background color of the label when not
#'   selected. Can be set globally via [radioSwitchButtons_default_style()].
#'   Default value is `"#FFFFFF"`.
#' @param not_selected_color Text color of the label text when not selected. Can
#'   be set globally via [radioSwitchButtons_default_style()]. Default value is
#'   `"#AAAAAA"`.
#' @export
radioSwitchButtons <- function(
  inputId,
  label = NULL,
  choices,
  choice_labels = names(choices),
  selected = choices[1],
  selected_background = NULL,
  selected_color = NULL,
  not_selected_background = NULL,
  not_selected_color = NULL
) {
  if (!is.null(choice_labels) && length(choice_labels) != length(choices)) {
    stop("`choice_labels` must be the same length as `choices`")
  }

  if (grepl("\\s", inputId)) {
    stop("`inputId` must not contain spaces")
  }

  selected <- restoreInput(inputId, selected)
  if (!is.null(selected)) {
    stopifnot(!any(is.na(selected)))
    if (length(selected) > 1) {
      warning("Only one choice may be selected, using first")
      selected <- selected[1]
    }
    selected_lgl <- choices %in% selected
  } else {
    selected_lgl <- rep(FALSE, length(choices))
    selected <- NULL
  }

  input_args <- list(
    name = rep(inputId, length(choices)),
    id = sprintf("%s%02d", inputId, seq_along(choices)),
    value = choices,
    label = choice_labels,
    is_selected = selected_lgl
  )

  radio_inputs <- input_args %>% purrr::pmap(make_radio_button)

  # Get default colors if not specified
  selected_background <- selected_background %||%
    getOption("shinythings.radioSwitch.selected_background", "#007BFF")
  selected_color <- selected_color %||%
    getOption("shinythings.radioSwitch.selected_color", "#FFFFFF")
  not_selected_background <- not_selected_background %||%
    getOption("shinythings.radioSwitch.not_selected_background", "#FFFFFF")
  not_selected_color <- not_selected_color %||%
    getOption("shinythings.radioSwitch.not_selected_color", "#AAAAAA")

  radio_css_template <- paste(
    readLines(pkg_file("css", "radio-switch-button.css")), collapse = "\n"
  )
  radio_css <- whisker::whisker.render(radio_css_template)

  htmltools::tagList(
    htmltools::htmlDependency(
      name = "shinyThings",
      version = packageVersion("shinyThings"),
      package = "shinyThings",
      src = "js",
      script = "input-binding-radio-switch-button.js"
    ),
    htmltools::singleton(
      tags$head(tags$style(radio_css))
    ),
    tags$div(
      class = "shinythings-radio-buttons",
      if (!is.null(label)) tags$label(`for` = inputId, label),
      tags$div(
        class = "shinythings-radio-inputs",
        id = inputId,
        radio_inputs
      )
    )
  )
}

#' @describeIn radioSwitchButtons Update the selected value of the radio switch
#'   button input associated with `inputId`.
#' @export
updateRadioSwitchButtons <- function(
  inputId,
  selected = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  stopifnot(is.character(selected) || is.null(selected))

  if (length(selected) > 1) {
    warning("Only one choice may be selected, using first")
    selected <- selected[1]
  }

  if (is.null(selected)) selected <- list(NULL)

  session$sendInputMessage(inputId, list(value = selected))
}

#' @describeIn radioSwitchButtons Set default values for the radio switch
#'   buttons.
#' @export
radioSwitchButtons_default_style <- function(
  selected_background = NULL,
  selected_color = NULL,
  not_selected_background = NULL,
  not_selected_color = NULL
) {
  old_opts <- options()
  if (!is.null(selected_background)) {
    options("shinythings.radioSwitch.selected_background" = selected_background)
  }
  if (!is.null(selected_color)) {
    options("shinythings.radioSwitch.selected_color" = selected_color)
  }
  if (!is.null(not_selected_background)) {
    options("shinythings.radioSwitch.not_selected_background" = not_selected_background)
  }
  if (!is.null(not_selected_color)) {
    options("shinythings.radioSwitch.not_selected_color" = not_selected_color)
  }
  invisible(old_opts)
}

#' @describeIn radioSwitchButtons Example app demonstrating the usage of the
#'   radioSwitchButtons input.
#' @inheritParams shiny::runApp
#' @export
radioSwitchButtonsDemo <- function(
  display.mode = c("showcase", "normal", "auto")
) {
  shiny::runApp(
    pkg_file("examples", "radioSwitchButtons"),
    display.mode = match.arg(display.mode)
  )
}


make_radio_button <- function(name, id, value, label = value, is_selected = FALSE) {
  radio_input <- tags$input(
    type = "radio",
    name = name,
    id = id,
    value = value
  )
  if (is_selected) {
    radio_input$attribs$checked <- NA
  }

  htmltools::tagList(
    radio_input,
    tags$label(`for` = id, label)
  )
}
