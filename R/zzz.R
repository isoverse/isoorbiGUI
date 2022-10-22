.onLoad <- function(libname, pkgname) {
  # set default package options
  default_options <- list(
    isoorbi.log = TRUE,
    isoorbi.debug = FALSE
  )
  options(default_options)
  invisible()
}

.onAttach <- function(libname, pkgname) {
  shiny::addResourcePath('www',
                         system.file('www',
                                     package = 'isoorbiGUI'))
}
