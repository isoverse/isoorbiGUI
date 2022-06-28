#' Run the report generator user interface
#'
#' Starts a graphical user interface to generate standard reports for Orbitrap data. Note that while the GUI is running, your R session will be blocked. To stop the GUI and return to your R session, either click the \code{Close} button inside the GUI navigation bar or press \code{Esc} in your R session.
#'
#' The reports app needs to be installed prior to use as it is an expansion of the core functionality of the \code{isoorbi} package. Run \code{\link{iso_install_reports_app}} to install the app.
#'
#' @inheritParams iso_start_isoxl_app
#' @export
#' @family reports functions
iso_start_reports_app <- function(log = FALSE, port = 5000) {
  # launch viewer
  start_orbitrap_reports_gui(
    local = TRUE,
    launch = TRUE,
    log = log,
    port = port
  )
}

# get additional dependencies
get_reports_app_dependencies <- function() {
  return(c("shinyjs", "shinyFiles", "fs"))
}

#' Install the orbitrap reports GUI
#'
#' @inheritParams iso_install_isoxl_app
#' @family reports functions
#' @export
iso_install_reports_app <- function(create_desktop_shortcut = TRUE) {
  install_app(
    "reports",
    suppressWarnings(check_dependencies(get_reports_app_dependencies())),
    create_desktop_shortcut = create_desktop_shortcut
  )
}

# orbitrap report generator gui start function
# @param ... parameters passed on to runApp
start_orbitrap_reports_gui <- function(local, launch, log, ...) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting. If you would like to keep a call to iso_start_orbitrap_reports_gui in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  stopifnot(!missing(local))
  stopifnot(!missing(launch))
  stopifnot(!missing(log))

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # check dependencies
  missing_deps <- check_dependencies(get_reports_app_dependencies())
  if (length(missing_deps) > 0) {
    # missing dependencies, generate placeholder app
    app <- create_missing_dependencies_info_gui("reports", missing_deps)
  } else {
    # generate app
    app <- shinyApp(
      ui = orbitrap_reports_ui(),
      server = orbitrap_reports_server(local = local)
    )
  }

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}

