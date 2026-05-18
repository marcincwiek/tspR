# utils.R
# Package-level utilities and the main entry point for end users.
# After installing tspR, run_app() is the only function most users need.

#' Launch the TSP Shiny dashboard
#'
#' Finds the bundled Shiny app inside the installed package and runs it.
#' This is the main entry point for end users — no need to know anything
#' about the package internals.
#'
#' @param launch.browser Logical. Open the app in your default browser?
#'   Default TRUE. Set to FALSE to open in the RStudio Viewer pane.
#' @return Nothing. Runs the Shiny app (blocking call).
#' @export
#' @examples
#' \dontrun{
#'   run_app()
#' }
run_app <- function(launch.browser = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    stop(
      "Package 'shiny' is required to run the dashboard.\n",
      "Install it with: install.packages('shiny')",
      call. = FALSE
    )
  }

  app_dir <- system.file("shiny", package = "tspR")

  if (nchar(app_dir) == 0) {
    stop(
      "Shiny app directory not found inside the package.\n",
      "Try reinstalling tspR with devtools::load_all().",
      call. = FALSE
    )
  }

  shiny::runApp(app_dir, launch.browser = launch.browser)
}
