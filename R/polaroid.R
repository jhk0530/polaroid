#' @title polaroid
#'
#' @description run polaroid shiny application in appDir
#' @seealso https://github.com/jhk0530/polaroid
#'
#' @examples if(interactive()){ polaroid() }
#'
#' @import shiny
#' @return None
#'
#' @export
#'


polaroid <- function() {

  appDir <- system.file("polaroid", package = "polaroid")
  if (appDir == "") {
    stop(
      "Could not find Directory, Try re-install",
      call. = FALSE
    )
  }

  runApp(
    appDir,
    launch.browser = TRUE,
    display.mode = "normal"
  )
}
