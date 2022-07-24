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
#' @rawNamespace import(DT, except = c(dataTableOutput, renderDataTable))
#' @import plotly
#' @import shinycssloaders
#' @rawNamespace import(ggplot2, except = last_plot)
#' @import RColorBrewer
#' @import tippy
#' @import mailtoR
#' @import enviPat
#' @import shinyBS
#' @import shinyvalidate
NULL

#' @export
magrittr::`%>%`

# quiets concerns of R CMD check about . that appears in pipelines
utils::globalVariables(c(".",
                         "filename",
                         "Injection",
                         "compound",
                         "scan.no",
                         "isotopolog",
                         "Isotopolog",
                         "i.scans",
                         "n.scans",
                         "ions.incremental",
                         "Basepeak",
                         "Basepeak.Ions",
                         "na.omit",
                         "Sample.Name",
                         "Ratio",
                         "time.min",
                         "Ratio.SEM",
                         "Shot.Noise.permil",
                         "relSE.permil",
                         "Mins.to.1mio",
                         "No.of.scans",
                         "Shot.Noise.permil",
                         "isotopes",
                         "mass",
                         "z",
                         "tic",
                         "sd",
                         "it.ms",
                         "TICxIT",
                         "median",
                         "selected_",
                         "neff",
                         "value", # names may still change in code
                         "key",
                         "head",
                         "install.packages",
                         "installed.packages",
                         "data",
                         "write.table",
                         "lm",
                         "quantile")) #FIXME: add others, okay like this?

# release questions (elaborate once selenium testing for the GUI is implemented)
# https://cran.r-project.org/web/packages/RSelenium/vignettes/shinytesting.html
release_questions <- function() {
  c(
    "Is it passing win-builder?"
  )
}
