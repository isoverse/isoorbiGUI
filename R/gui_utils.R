# general utilites for modules ====

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
  prefix <- if(type == "info") "INFO: " else if (type == "debug") "DEBUG: " else stop("don't know message type", type)
  ns <- if(!is.null(ns)) paste0(" (NS: ", ns(NULL), ")") else ""
  cat(file=stderr(), prefix, ..., ns, "\n", sep = "")
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
