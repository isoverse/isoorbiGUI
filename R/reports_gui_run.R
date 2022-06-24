#' Run the report generator user interface
#'
#' Provides a local graphical user interface to generate standard reports for Orbitrap data. Note that while the GUI is running, your R session will be blocked. To stop the GUI and return to your R session, either click the \code{Close} button inside the GUI navigation bar or press \code{Esc} in your R session.
#'
#' @details \code{iso_start_orbitrap_reports_gui} starts a personal GUI that is only accessible on the computer it is running on.
#'
#' @param log whether to show log info messages or not.
#' @export
iso_start_orbitrap_reports_gui <- function(log = FALSE) {
  # launch viewer
  start_orbitrap_reports_gui(
    local = TRUE,
    launch = TRUE,
    log = log,
    port = 3838
  )
}

# get additional dependencies
get_orbitrap_reports_gui_dependencies <- function() {
  return(c("shinyjs", "shinyFiles", "fs"))
}

# check additional dependencies for reports gui
check_orbitrap_reports_gui_dependencies <- function() {
  additional_dependencies <- get_orbitrap_reports_gui_dependencies()
  missing_packages <- additional_dependencies[!(additional_dependencies %in% installed.packages()[,"Package"])]
  if (length(missing_packages) > 0)
    warning("there are missing dependencies for the Orbitrap Reports GUI: ",
            paste(missing_packages, collapse = ", "),
            immediate. = TRUE, call. = FALSE)
  return(invisible(missing_packages))
}

#' Install dependencies for reports GUI
#'
#' @param
#' @export
iso_install_orbitrap_reports_gui <- function() {
  missing_deps <- suppressWarnings(check_orbitrap_reports_gui_dependencies())
  if (length(missing_deps) > 0) {
    message("Installing the following missing dependencies: ", paste(missing_deps, collapse = ", "))
    install.packages(missing_deps)
  } else {
    message("All dependencies are installed. Run iso_start_orbitrap_reports_gui() to start the user interface.")
  }
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
  missing_deps <- check_orbitrap_reports_gui_dependencies()
  if (length(missing_deps) > 0) {
    # missing dependencies, generate placeholder app
    app <-
      shinyApp(
        ui = fluidPage(
          h1("Missing dependencies", style = "color: red;"),
          p("The following dependencies for this GUI are not yet installed: ", strong(paste(missing_deps, collapse = ", "))),
          p("Please close the app and run ", code("iso_install_orbitrap_reports_gui()"), " from an R console before restarting the GUI."),
          actionButton("close", "Close"),
        ),
      server = function(input, output) {
        observeEvent(input$close, {
          # SK note: could also close tab window here but that requires some javascript that would require shinyjs
          stopApp()
        })
      }
    )
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

