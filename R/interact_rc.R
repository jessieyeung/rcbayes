#' Run Interactive Rogers-Castro App
#' @description Run an interactive Rogers-Castro app. Use interactive sliders to see how parameters affect the Rogers-Castro age schedules.
#' @import shiny
#' @return No return value, called for interactive widget
#' @export
#'
#' @examples
#' \dontrun{
#' interact_rc()
#' }
interact_rc <- function() {
  appDir <- system.file("shiny-app", "RC_app", package = "rcbayes")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `rcbayes`.", call. = FALSE)
  }

  shiny::runApp(appDir, display.mode = "normal")
}
