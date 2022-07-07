#' Variable selection for plot user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
varselect_mod_ui <- function(id) {
  ns <- NS(id)

  # define choices for X and Y variable selection
  var_choices <- list(  "scan.no", "time.min",
                         "ions.incremental", "tic", "it.ms", "intensity", "resolution",
                         "peakResolution", "peakNoise", "mzMeasured", "basePeakIntensity",
                         "rawOvFtT", "intensCompFactor", "agc", "agcTarget", "microscans",
                         "numberLockmassesFound", "analyzerTemperature"
  )

  # assemble UI elements
  tagList(
    selectInput(
      ns("xvar"),
      "Select X variable",
      choices = var_choices,
      selected = var_choices[[2]]
    ),
    selectInput(
      ns("yvar"),
      "Select Y variable",
      choices = var_choices,
      selected = var_choices[[3]]
    ),
    selectInput(
      ns("groupvar"),
      "Select facet variable",
      choices = list(
        `None` = "",
        `isotopolog` = "isotopolog",
        `compound` = "compound",
        `filename` = "filename"
      )
    )
  )
}


varselect_mod_density_ui <- function(id) {
  ns <- NS(id)

  # define choices for X and Y variable selection
  var_choices <- list(  "scan.no", "time.min",
                        "ions.incremental", "tic", "it.ms", "intensity", "resolution",
                        "peakResolution", "peakNoise", "mzMeasured", "basePeakIntensity",
                        "rawOvFtT", "intensCompFactor", "agc", "agcTarget", "microscans",
                        "numberLockmassesFound", "analyzerTemperature"
  )

  # assemble UI elements
  tagList(
    selectInput(
      ns("xvar"),
      "Select X variable",
      choices = var_choices,
      selected = var_choices[[2]]
    ),
    selectInput(
      ns("groupvar"),
      "Select facet variable",
      choices = list(
        `None` = "",
        `isotopolog` = "isotopolog",
        `compound` = "compound",
        `filename` = "filename"
      )
    )
  )
}

#' Variable selection module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#'
#' @return list with following components
#' \describe{
#'   \item{xvar}{reactive character indicating x variable selection}
#'   \item{yvar}{reactive character indicating y variable selection}
#'   \item{facetvar}{reactive character indicating variable used for defining plot facets}
#' }
varselect_mod_server <- function(input, output, session) {


  return(
    list(
      xvar = reactive({ input$xvar }),
      yvar = reactive({ input$yvar }),
      facetvar = reactive({
        if (input$groupvar == "") {
          return(NULL)
        } else {
          return(input$groupvar)
        }
      })
    )
  )
}


#' Data viewer user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
dataviewer_mod_ui <- function(id) {
  ns <- NS(id)
  tagList(
    DT::DTOutput(ns("table"))
  )
}

#' Data viewer module server-side processing
#'
#' @param input,output,session standard \code{shiny} boilerplate
#' @param dataset data frame (reactive) used in scatterplots as produced by
#'   the \code{brushedPoints} function in the scatterplot module
#'
#' @return reactive vector of row IDs corresponding to the current view in the
#'   datatable widget.
dataviewer_mod_server <- function(input, output, session, dataset) {

   cols_select <- c("filename", "scan.no", "time.min", "compound", "isotopolog",
                    "ions.incremental", "tic", "it.ms", "intensity", "resolution",
                    "peakResolution", "peakNoise", "mzMeasured", "basePeakIntensity",
                    "rawOvFtT", "intensCompFactor", "agc", "agcTarget", "microscans",
                    "numberLockmassesFound", "analyzerTemperature"
                    )

   #FIXME: get column names as variables for plotting

  output$table <- DT::renderDataTable({
    filter(dataset(), selected_) %>%
      select(one_of(cols_select))
  },  options = list(scrollX = TRUE, autoWidth = FALSE )) #filter = 'top', selection = 'none',
}



