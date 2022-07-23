# gui management =====

# check dependencies for gui
check_dependencies <- function(dependencies) {
  missing_packages <- dependencies[!(dependencies %in% installed.packages()[,"Package"])]
  if (length(missing_packages) > 0)
    warning("there are missing dependencies for the GUI: ",
            paste(missing_packages, collapse = ", "),
            immediate. = TRUE, call. = FALSE)
  return(invisible(missing_packages))
}

# create shortcut for gui launch
# @param name of the shortcut file
# @param func the isoorbi function to call
# @param the port to launch
# @param operating_system "windows" or "unix", should be detected automatically
# @param icon optional icon file name (has to be available in inst/icons/)
# @param overwrite whether to overwrite the file if it already exists
create_gui_desktop_shortcut <- function(shortcut, func, port = 5000, operating_system = .Platform$OS.type, icon = NULL, overwrite = FALSE) {

  # both windows and mac os
  desktop_path <- fs::path_expand("~/Desktop")

  # OS specific paths and file names
  if (operating_system == "windows") {
    # windows bat file and command
    desktop_file <- file.path(desktop_path, paste0(shortcut, ".bat"))
    R_executable <- file.path(R.home(), "bin", "Rscript.exe")
    exec_cmd <- sprintf("\"%s\" -e \"%s\"", R_executable, cmd)
  } else if (operating_system == "unix") {
    # mac os .command filed and command
    desktop_file <- file.path(desktop_path, paste0(shortcut, ".command"))
    R_executable <- file.path(R.home(), "bin", "Rscript")
    exec_cmd <- sprintf("\"%s\" -e \"isoorbi::%s(port = %s)\" & open http://localhost:%s/;",
                        R_executable, func, port, port)
  } else {
    stop("unsupported operating system: ", operating_system, call. = FALSE)
  }

  # check if new shortcut
  new <- FALSE
  if (file.exists(desktop_file) && !overwrite) {
    message("INFO: desktop shortcut ", basename(desktop_file),
            " already exists. Set overwrite = TRUE to overwrite it.")
    return(invisible(desktop_file))
  } else if (file.exists(desktop_file) && overwrite) {
    message("INFO: desktop shortcut ", basename(desktop_file),
            " already exists but will be updated...")
  } else {
    message("INFO: creating desktop shortcut ", basename(desktop_file), "...")
    new <- TRUE
  }

  # create shortcut and set permissions
  write(exec_cmd, desktop_file)
  if (operating_system == "windows") {
    # anything to do?
  } else if (operating_system == "unix") {
    # os specfic messages
    message("INFO: setting permissions for desktop shortcut...")
    Sys.chmod(desktop_file, mode = "0777")
  }

  # create icon
  if (!is.null(icon) && new) {
    if (operating_system == "windows") {
      icon_info <- paste0(
        "INFO: copying icon file '", icon, "' to the desktop... ",
        "please do xyz (@TODO: provide instructions for windows)."
      )
    } else if (operating_system == "unix") {
      icon_info <- paste0(
        "INFO: copying icon file '", icon, "' to the desktop... ",
        "please manually right click '", basename(desktop_file),
        "' on the desktop -> more info; ",
        "then drag and drop icon file onto icon in the left top corner ",
        "and delete icon file from desktop."
      )
    }

    icons <- list.files(system.file(package = "isoorbi", "icons"))
    if (icon %in% icons) {
      icon_source_path <- system.file(package = "isoorbi", "icons", icon)
      icon_desktop_path <- file.path(desktop_path, icon)
      message(icon_info)
      file.copy(from = icon_source_path, to = icon_desktop_path)
    } else {
      warning("icon '", icon, "' is not part of the package", immediate. = TRUE, call. = FALSE)
    }
  }

  # return file path
  return(invisible(desktop_file))
}

# missing dependencies info gui
create_missing_dependencies_info_gui <- function(name, missing_deps) {
  shinyApp(
    ui = fluidPage(
      h1("Missing dependencies", style = "color: red;"),
      p("The following dependencies for the ", name, " GUI are not yet installed: ", strong(paste(missing_deps, collapse = ", "))),
      p("Please close the app and run ", code(paste0("isoorbi::iso_install_", stringr::str_to_lower(name), "_app()")), " from an R console before restarting the GUI."),
      actionButton("close", "Close"),
    ),
    server = function(input, output) {
      observeEvent(input$close, {
        # SK note: could also close tab window here but that requires some javascript that would require shinyjs
        stopApp()
      })
    })
}

# general utilities for modules ====

`%then%` <- function(a, b) {
  if (is.null(a)) b else a
}

# display gui message
gui_message <- function(type = c("info", "debug"), ...) {
  module_message(ns = NULL, type = type, ...)
}

# display module message
# @param type if this is an info meessage or debug (debug only shows if in debug mode)
# @param type if this is an info message or debug (debug only shows if in debug mode)
module_message <- function(ns, type = c("info", "debug"), ...) {
  if (!setting("log") || (type == "debug" && !setting("debug"))) return()
  prefix <- if(type == "info") "INFO" else if (type == "debug") "DEBUG" else stop("don't know message type", type)
  ns <- if(!is.null(ns)) paste0(" (NS '", ns(NULL), "')") else ""
  cat(file=stderr(), prefix, ns, ": ", ..., "\n", sep = "")
}

# convenience function for adding spaces (not the most elegant way but works)
spaces <- function(n) {
  HTML(rep("&nbsp;", n))
}

# convenience function for adding input with tooltip with default parameters
tooltipInput <- function(input, inputId, ..., tooltip = NULL) {
  tagList(
    do.call(input, args = c(list(inputId = inputId), list(...))),
    if (!is.null(tooltip)) shinyBS::bsTooltip(inputId, tooltip)
  )
}

# convenience function for adding output with tooltip with default parameters
tooltipOutput <- function(input, outputId, ..., tooltip = NULL) {
  tagList(
    do.call(input, args = c(list(outputId = outputId), list(...))),
    if (!is.null(tooltip)) shinyBS::bsTooltip(outputId, tooltip)
  )
}


# convenience function for inline inputs
inlineInput <- function(input, inputId, label, ..., tooltip = NULL) {
  tagList(
    inline(tags$label(label, `for` = inputId)),
    inline(tooltipInput(input, inputId, label = NULL, ..., tooltip = tooltip))
  )
}

# adding an inline UI item
inline <- function(...) {
  div(style = "display: inline-block;", ...)
}

# inline pulled to the right (this should come BEFORE the item it is inline but to the right with)
rightInline <- function(...) {
  div(class = "pull-right", ...)
}

# default box
default_box <- function(..., status = "info", solidHeader = TRUE, collapsible = TRUE) {
  box(..., status = status, solidHeader = solidHeader, collapsible = collapsible)
}

# custom download
customDownloadButton <- function(outputId, label = "Download", icon = icon("download"), class = NULL, ...) {
  tags$a(id = outputId, class = paste("btn btn-default shiny-download-link", class),
         href = "", target = "_blank", download = NA,
         icon, label, ...)
}
