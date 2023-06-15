#' Run the IsoXL user interface
#'
#' Starts a graphical user interface to explore Orbitrap data. Note that while the GUI is running, your R session will be blocked. To stop the GUI and return to your R session, either click the \code{Close} button inside the GUI navigation bar or press \code{Esc} in your R session.
#'
#' @param log whether to show log info messages or not. By default, shows logs when running in server mode (\code{iso_start_viewer_server}) and does not show them otherwise.
#' @param reset whether to reset GUI settings for the data folder to their defaults. This option is only available when running a personal GUI as servers do not save any settings. Resetting settings is usually only necessary if your GUI crashed unexpectedly and gets stuck during restart. Please consider reporting the details of the crash at https://github.com/isoverse/isoorbi/issues, especially if you can easily reproduce it.
#' @param launch whether to launch the app right away
#' @param port the localhost port where the app will be accessible (if launch = TRUE), e.g. https://127.0.0.1:5000 (note that if it is a port that is open in your firewall such as 3838, the GUI will be accessible on your local network at your IP address https://xxx.xxx.xxx.xxx:3838)
#' @export
#' @family isoxl functions
iso_start_isoxl_app <- function(log = TRUE, reset = FALSE, launch = TRUE, port = 5000) {
  # launch viewer
  start_orbitrap_gui(
    local = TRUE,
    launch = launch,
    log = log,
    reset = reset,
    port = port
  )
}

# get additional dependencies
get_isoxl_app_dependencies <- function() {
  # FIXME: needs more info
  return(c("shinyjs", "shinyWidgets")) #envipat?
}

#' Install the orbitrap IsoXL GUI
#'
#' @param create_desktop_shortcut whether to create a desktop shortcut for the app
#' @family isoxl functions
#' @export
iso_install_isoxl_app <- function(create_desktop_shortcut = TRUE) {
  install_app(
    "isoxl",
    suppressWarnings(check_dependencies(get_isoxl_app_dependencies())),
    create_desktop_shortcut = create_desktop_shortcut
  )
}

#' Run the IsoXL user interface on a server
#'
#' @details \code{iso_start_isoxl_app_server} starts a server for the Orbitrap data GUI (used in deployment on e.g. shinyapps.io). Typically this function is not called directly. Use \code{iso_setup_isoxl_app} instead to set up the file structure for server deployment.
#'
#' @family isoxl functions
#' @export
iso_start_isoxl_app_server <- function() {
  start_orbitrap_gui(
    local = FALSE,
    launch = FALSE,
    log = TRUE,
    reset = TRUE,
    port = 3838
  )
}

#' @rdname iso_start_isoxl_app_server
#'
#' @details \code{iso_setup_isoxl_app_server} sets up the IsoXL user interface app in the target folder for deployment on a server (e.g. shinyapps.io).
#'
#' @param dir the directory where to create the launch scripts (absolute path or relative to your current working directory), default is 'app'
#' @param overwrite whether to overwrite existing launch files (default is FALSE)
#' @export
iso_setup_isoxl_app_server <- function(dir = "app", overwrite = FALSE) {

  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  paste0("Info: setting up orbitrap GUI launch files in folder '", dir, "'...") %>% message()

  # copy app.R template file
  message(" - creating 'app.R' launch script... ", appendLF = FALSE)
  file_exists <- file.exists(file.path(dir, "app.R"))
  if (!file_exists || overwrite) {
    cat(
      c(
        "# launch orbitrap GUI",
        "isoorbi::iso_start_isoxl_app_server()"
      ),
      sep = "\n",
      file = file.path(dir, "app.R"),
      append = FALSE
    )
  }
  if (file_exists && overwrite) message("existing file overwritten.")
  else if (file_exists) message("failed (file already exists, use overwrite = TRUE)")
  else message("complete")

  # omit from github
  message(" - adding .gitignore... ", appendLF = FALSE)
  if (!file.exists(file.path(dir, ".gitignore")) || overwrite)
    cat("# exclude apps from github (by default)\n*\n", file = file.path(dir, ".gitignore"), append = FALSE)
  message("complete.")

  # infos
  message(" -> deploy with iso_deploy_isoxl_app_server(\"", dir, "\")")
}

#' @rdname iso_start_isoxl_app_server
#'
#' @details \code{iso_deploy_isoxl_app_server} deploys the isoxl app to shinyapps.
#' @export
iso_deploy_isoxl_app_server <- function(dir = "app") {
  stop("not yet implemented", call. = FALSE)
}


# Orbitrap GUI start function
# @param ... parameters passed on to runApp
start_orbitrap_gui <- function(local, launch, log, reset, ...) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting. If you would like to keep a call to iso_start_orbitrap_gui in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  stopifnot(!missing(local))
  stopifnot(!missing(launch))
  stopifnot(!missing(log))
  stopifnot(!missing(reset))

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # check dependencies
  missing_deps <- check_dependencies(get_isoxl_app_dependencies())
  if (length(missing_deps) > 0) {
    # missing dependencies, generate placeholder app
    app <- create_missing_dependencies_info_gui("isoxl", missing_deps)
  } else {
    # generate app
    app <- shinyApp(
      ui = orbitrap_ui(),
      server = orbitrap_server(
        local = local,
        reset = reset
      )
    )
  }

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}

#FIXME: work towards a standalone version
#test electricShine
# #https://github.com/chasemc/electricShine
# run_app_electricShine <- function(options = list()) {
#   shiny::shinyApp(ui = orbitrap_ui(),
#                   server = orbitrap_server(local = local, reset = reset),
#                   options = options)
# }

