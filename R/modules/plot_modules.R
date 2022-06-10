#' Scatterplot module user interface
#'
#' @param id, character used to specify namespace, see \code{shiny::\link[shiny]{NS}}
#'
#' @return a \code{shiny::\link[shiny]{tagList}} containing UI elements
#' @export
#'
#' @examples
scatterplot_mod_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        h4("Plot 1", align = "center"),
        width = 6,
        withSpinner(
          plotOutput(
          ns("plot1"),
          brush = brushOpts(
            id = ns("brush"),
            resetOnNew = FALSE
          )
        )
        )
      ),
      column(
        h4("Plot 2", align = "center"),
        width = 6,
        withSpinner(
          plotOutput(
          ns("plot2"),
          brush = brushOpts(
            id = ns("brush"),
            resetOnNew = FALSE
          )
        )
        )
      )
    )
  )
}


densityplot_mod_ui <- function(id) {
  
  
  ns <- NS(id)
  
  tagList(
    fluidRow(
      column(
        h4("Plot 3", align = "center"),
        width = 6,
        withSpinner(
          plotlyOutput(
            ns("plot3"),
            # brush = brushOpts(
            #   id = ns("brush"),
            #   resetOnNew = FALSE
            # )
          )
        )
      ),
      column(
        h4("Plot 4", align = "center"),
        width = 6,
        withSpinner(
          plotlyOutput(
            ns("plot4"),
            # brush = brushOpts(
            #   id = ns("brush"),
            #   resetOnNew = FALSE
            # )
          )
        )
      )
    )
  )
}


#' Scatterplot module server-side processing
#'
#' This module produces a scatterplot with the sales price against a variable selected by the user.
#'
#' @param input,output,session standard \code{shiny} boilerplate.
#' @param dataset data frame (non-reactive) with variables necessary for scatterplot.
#' @param plot1_vars reactive list of plot 1 x variable, y variable, and grouping variable.
#' @param plot2_vars reactive list of plot 2 x variable, y variable, and grouping variable.
#' @param highlight_ind boolean indicating whether to perform annotation of data points
#'   on the plot. Default is \code{FALSE}.
#'
#' @return list with following components:
#' \describe{
#'   \item{plot_data}{reactive data frame containing only the \code{xvar} and \code{yvar} variables}.
#'   \item{plot_object}{reactive \code{ggplot2} object for the scatterplot}.
#' }
#' @export
#'
#' @examples
scatterplot_mod_server <- function(input,
                                   output,
                                   session,
                                   dataset,
                                   plot1vars,
                                   plot2vars,
                                   label = reactive(FALSE)) {
  
  
  dat <- reactive({
    
    
    brushedPoints(dataset, input$brush, allRows = TRUE)
  })
  
  output$plot1 <- renderPlot({
    
    validate(
    need(plot1vars$xvar() %in% colnames(dat()), "Variable x not in data set. Was file processed as simple output in IsoX?"), #add more validation later
    need(plot1vars$yvar() %in% colnames(dat()), "Variable y not in data set. Was file processed as simple output in IsoX?"),
    )
    
         
    scatter_isox(
      dat(),
      xvar = plot1vars$xvar(),
      yvar = plot1vars$yvar(),
      facetvar = plot1vars$facetvar(),
      label = label()
    
    )
  })
  
  output$plot2 <- renderPlot({
    

    
    validate(
      need(plot2vars$xvar() %in% colnames(dat()), "Variable x not in data set. Was file processed as simple output in IsoX?"), #add more validation later
      need(plot2vars$yvar() %in% colnames(dat()), "Variable y not in data set. Was file processed as simple output in IsoX?"),
    )
    
    scatter_isox(
      dat(),
      xvar = plot2vars$xvar(),
      yvar = plot2vars$yvar(),
      facetvar = plot2vars$facetvar(),
      label = label()
    
    )
  })
  
 
  
  dat
}

densityplot_mod_server <- function(input,
                                   output,
                                   session,
                                   dataset,
                                   plot3vars,
                                   plot4vars,
                                   label = reactive(FALSE)) {
  
  
dat2 <- reactive({
  
  
    brushedPoints(dataset, input$brush, allRows = TRUE)
  })
  
 
  output$plot3 <- renderPlotly({
    
    validate(
      need(plot3vars$xvar() %in% colnames(dat2()), "Variable x not in data set. Was file processed as simple output in IsoX?") #add more validation later
    )
    
    ggplotly(
    
    
    density_isox(
      dat2(),
      xvar = plot3vars$xvar(),
      facetvar = plot3vars$facetvar(),
      label = label()
      
    )
    )
  })
  
  output$plot4 <- renderPlotly({
    
    validate(
      need(plot4vars$xvar() %in% colnames(dat2()), "Variable x not in data set. Was file processed as simple output in IsoX?") #add more validation later
    )
    
    ggplotly(
    density_isox(
      dat2(),
      xvar = plot4vars$xvar(),
      facetvar = plot4vars$facetvar(),
      label = label()
      
    )
    )
  })
  
  dat2
}
