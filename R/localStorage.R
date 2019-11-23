#' Access the Browser's Local Storage
#'
#' Store and retrieve items from the browser's local storage.
#'
#' @name localStorage
NULL

#' @describeIn localStorage Store key-value pairs in the browser's local storage.
#' @export
localStorage_store <- function(
  ...,
  overwrite = FALSE,
  .list = NULL,
  session = shiny::getDefaultReactiveDomain()
) {
  values <- c(.list, list(...))
  if (!length(values)) return()
  for (key in names(values)) {
    session$sendCustomMessage('setLocalStorage', list(
      key = key,
      value = values[[key]],
      overwrite = overwrite
    ))
  }
}

#' @describeIn localStorage Remove a key-value pair from the browser's local storage.
#' @export
localStorage_remove <- function(key, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage('removeLocalStorage', key)
}

#' @describeIn localStorage Clear the browser's local storage.
#' @export
localStorage_clear <- function(clear = TRUE, session = shiny::getDefaultReactiveDomain()) {
  session$sendCustomMessage('clearLocalStorage', clear)
}

#' @describeIn localStorage Use the browser's local storage in your Shiny app.
#'   Include this function in your UI to expose the input `"_localStorage"` in
#'   your app and to enable the `localStorage_*()` functions.
#' @export
use_localStorage <- function() {
  htmltools::tagList(
    htmltools::htmlDependency(
      name = "shinyThings.localStorage",
      version = utils::packageVersion("shinyThings"),
      package = "shinyThings",
      src = "js",
      script = "localStorage.js",
      all_files = FALSE
    )
  )
}

#' @describeIn localstorage Example app demonstrating usage of `localStorage`..
#' @inheritParams shiny::runApp
#' @export
localStorage_demo <- function(display.mode = c("showcase", "normal", "auto")) {
  shiny::runApp(system.file("examples", "localStorage", package = "shinyThings"),
                display.mode = match.arg(display.mode))
}
