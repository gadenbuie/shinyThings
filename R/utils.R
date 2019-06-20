`%||%` <- function(x, y) if (is.null(x)) y else x

pkg_file <- function(...) {
  system.file(..., package = "shinyThings", mustWork = TRUE)
}
