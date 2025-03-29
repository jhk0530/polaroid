#' @title polaroid
#'
#' @description run polaroid shiny application in appDir
#' @seealso https://github.com/jhk0530/polaroid
#'
#' @examples if (interactive()) {
#'   polaroid()
#' }
#'
#' @import shiny
#' @return None
#'
#' @export
#'

polaroid <- function() {
  required_pkgs <- c("argonDash", "argonR", "colourpicker", "hexSticker",
                     "htmltools", "magick", "shinyjs", "showtext",
                     "stringr", "sysfonts")

  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      paste("Please install missing packages:", paste(missing_pkgs, collapse = ", ")),
      call. = FALSE
    )
  }

  appDir <- system.file("polaroid", package = "polaroid")
  if (appDir == "") {
    stop("Could not find Directory, Try re-install", call. = FALSE)
  }

  shiny::runApp(
    appDir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}
