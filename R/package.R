#' @keywords internal
"_PACKAGE"

# import functions/packages used so frequently we don't want to always refer to the namespace explicitly
#' @importFrom magrittr %>%
#' @import shiny
#' @import shinydashboard
#' @import ggplot2
#' @import shinycssloaders
#' @import plotly
#' @import tippy
#' @import dplyr
#' @import scales
#' @import ggrepel
#' @import tidyverse
#' @import openxlsx
#' @import DT
#' @import RColorBrewer
#' @import mailtoR
#' @import enviPat
NULL

#' @export
magrittr::`%>%`

# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c("."))

# release questions (elaborate once selenium testing for the GUI is implemented)
# https://cran.r-project.org/web/packages/RSelenium/vignettes/shinytesting.html
release_questions <- function() {
  c(
    "Is it passing win-builder?"
  )
}
