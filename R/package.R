#' @keywords internal
"_PACKAGE"

# import functions/packages used so frequently we don't want to always refer to the namespace explicitly
#' @import dplyr
#' @import scales
#' @import ggrepel
#' @import shinydashboard
#' @import shiny
#' @import shinyWidgets
#' @import tidyr
#' @importFrom magrittr %>%
#' @import openxlsx
#' @import DT
#' @import plotly
#' @import shinycssloaders
#' @import ggplot2
#' @import RColorBrewer
#' @import tippy
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
