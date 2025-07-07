#' Launch Interactive Extreme Value Explorer
#'
#' Launches a Shiny application for interactive exploration of extreme value
#' analysis in chaotic dynamical systems.
#'
#' @param ... Additional arguments passed to \code{shiny::runApp()}
#'
#' @return Launches the Shiny application
#' @export
#'
#' @examples
#' \dontrun{
#' launch_explorer()
#' }
launch_explorer <- function(...) {
  app_dir <- system.file("shiny", package = "chaoticds")
  
  if (app_dir == "") {
    stop("Shiny app not found. Please check package installation.")
  }
  
  # Check for required packages
  required_packages <- c("shiny", "shinydashboard", "ggplot2", "plotly")
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    message("Installing required packages: ", paste(missing_packages, collapse = ", "))
    install.packages(missing_packages)
  }
  
  message("Launching Interactive Extreme Value Explorer...")
  shiny::runApp(app_dir, launch.browser = TRUE, ...)
}