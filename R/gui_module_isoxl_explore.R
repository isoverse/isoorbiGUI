# test ----
# test the dual inlet module as a standalone app
#' @title test_module_isoxl_explore
#' @description ...
#' @param port ...
#' @return ...
#' @export
test_module_isoxl_explore <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Exlore Module Test"),
        module_isoxl_explore_ui(id = "explore")
      ),
      server = function(input, output) {
        dual_inlet <-
          callModule(module_isoxl_explore_server, id = "explore")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}


# server ----

#' @title module_isoxl_explore_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
module_isoxl_explore_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # Download test data: .isox file ----------------------------------

  output$download_test_data <- downloadHandler(
    filename = function() {
      "testfile_DualInlet_small.isox" #FIXME: provide name as a variable
    },
    content = function(file) {
      write.table(values$data, file, sep="\t", row.names = F)
    }
  )


  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot = NULL #NEED THIS? note: all data will be plotted
  )

  # use test data ----
  observeEvent(input$test_data, {

    if (input$test_data == TRUE) {

      shinyjs::hide("main-plot")
      shinyjs::hide("main-rest")

      values$data <- iso_read_isox_file(system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi"))



      {shinyjs::hide("upload_panel")}

    }

    if (input$test_data == FALSE) {


      shinyjs::hide("main-plot")
      shinyjs::hide("main-rest")
      {shinyjs::show("upload_panel")}

      # reset the reactive values that hold the file and data
      values$upload_file <- NULL
      values$upload_error <- NULL
      values$data <- NULL

    }


    {shinyjs::hide("explore-input-panel")}


  })

  # upload file ----
  observeEvent(input$upload, {
    module_message(
      ns,
      "info",
      "new file uploaded file '",
      input$upload$name,
      "' into temporary path ",
      input$upload$datapath
    )

    # reset the reactive values that hold the file and data
    values$upload_file <- input$upload$name
    values$upload_error <- NULL
    values$data <- NULL

    if (input$test_data == FALSE) {
      # try to read the uploaded file
      tryCatch(


        values$data <- iso_read_isox_file(input$upload$datapath),
        warning = function(w)
          values$upload_error <- w$message,
        error = function(e)
          values$upload_error <- e$message

      )
    } else {
      # try to read the test file
      tryCatch(

        values$data <- iso_read_isox_file(system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi"))

        # ,
        # warning = function(w)
        #   values$upload_error <- w$message,
        # error = function(e)
        #   values$upload_error <- e$message
      )

    }

  })

  # provide feedback on file upload success
  output$upload_info <- renderText({
    req(values$upload_file)
    validate(need(is.null(values$upload_error), message = values$upload_error))
    paste0(values$upload_file, " uploaded")
  })

  # update plot options ----
  observeEvent(values$data, {

    shinyjs::hide("explore-main-panel")
    shinyjs::hide("explore-input-panel") #FIXME: optimized placing the show and hide commands

    # module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorrect input file --> hide)

      shinyjs::hide("explore-main-panel")
      shinyjs::hide("explore-input-panel")

      # shinyjs::hide("parameters-panel")
      # shinyjs::hide("main-plot")
      # updateSelectInput(inputId = "isotopolog", choices = c())
      # updateSelectInput(inputId = "base_peak", choices = c())
      # updateSelectInput(inputId = "filename", choices = c())
      # updateSelectInput(inputId = "compound", choices = c())
      # shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())
      # updateNumericInput(inputId = "rare", value = c())
      # updateNumericInput(inputId = "extreme", value = c())



    } else {
      # yes data --> update parameters and show

      # updateSelectInput(
      #   inputId = "isotopolog",
      #   choices = levels(values$data$isotopolog),
      #   selected = values$data$isotopolog[2]
      # )
      # updateSelectInput(
      #   inputId = "base_peak",
      #   choices = levels(values$data$isotopolog),
      #   selected = values$data$isotopolog[1]
      # )
      # updateSelectInput(
      #   inputId = "filename",
      #   choices = c("all", levels(values$data$filename)),
      #   selected = "all"
      # )
      # updateSelectInput(
      #   inputId = "compound",
      #   choices = c("all", levels(values$data$compound)),
      #   selected = "all"
      # )
      # shinyWidgets::updateNumericRangeInput(session,
      #                                       inputId = "timerange",
      #                                       value = 0:ceiling(max(values$data$time.min)))
      #
      # updateNumericInput(
      #   inputId = "rare",
      #   min = 0, #how to avoid negative input by keyboard?
      #   max = 100,
      #   value = 30
      # )
      #
      # updateNumericInput(
      #   inputId = "extreme",
      #   min = 0, #how to avoid negative input by keyboard?
      #   max = 10,
      #   value = 1,
      #   step = 0.1
      # )

      shinyjs::show("explore-input-panel")
    }
  }, ignoreNULL = FALSE)

  # Data: Exploration -------------------------------------------------------



  # data.explore <- reactive({
  #
  #   req(input$upload_explore)
  #
  #
  #   ext <- tools::file_ext(input$upload_explore$name)
  #   switch(ext,
  #          isox = readr::read_tsv(input$upload_explore$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
  #                                 col_types = list(
  #                                   filename = readr::col_factor(),
  #                                   compound = readr::col_factor(),
  #                                   isotopolog = readr::col_factor()
  #                                 ) ),
  #
  #
  #          validate("Invalid file; Please upload a .isox file")
  #   )
  #
  # })

  # Plot: Exploration -------------------------------------------------------

  observeEvent(input$action_explore, {

    shinyjs::show("explore-main-panel")

    module_message(
      ns, "info",
      "generating plot with selected parameters:" #,
      # "\n - isotopolog: ", input$isotopolog %then% "",
      # "\n - basepeak: ", input$base_peak %then% "",
      # "\n - files: ", paste(input$filename %then% "", collapse = ", "),
      # "\n - compounds: ", paste(input$compound %then% "", collapse = ", "),
      # "\n - timerange: ", paste(input$timerange %then% "", collapse = " to "),
      # "\n - rare: ", input$rare %then% "",
      # "\n - extreme: ", input$extreme %then% ""#)#edit
    )


    values$plot <- values$data
  })


observeEvent(input$action_explore, {




    plot1vars <- callModule(varselect_mod_server, "plot1_vars")
    plot2vars <- callModule(varselect_mod_server, "plot2_vars")

    dat <- callModule(



      scatterplot_mod_server,
      "plots",
      # dataset = data.explore(), #somehow this gets not updated when new isox file is loaded!!??

      dataset = values$data,
      plot1vars = plot1vars,
      plot2vars = plot2vars,
      label = reactive({ input$label })
    )

    callModule(dataviewer_mod_server, "dataviewer", dat)


    plot3vars <- callModule(varselect_mod_server, "plot3_vars")
    plot4vars <- callModule(varselect_mod_server, "plot4_vars")

    dat2 <- callModule(
      densityplot_mod_server,
      "plots",
      #dataset = data.explore(),
      dataset = values$data,
      plot3vars = plot3vars,
      plot4vars = plot4vars,
      label = reactive({ input$label })
    )

    callModule(dataviewer_mod_server, "dataviewer", dat2)
  })

}

# user interface ----

#' @title module_isoxl_explore_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_explore_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      # Use test data instead of uploading data (or any data that has been already uploaded)

      tippy(
        checkboxInput(ns("test_data"), "Use test data", value = FALSE),

        "For details on test data see Contact tab",
        animation="scale",
        #theme="light",
        placement ="top-start"

      ),

      hr(),

      div(
        id = ns("upload_panel"), #can be

      # upload ----
      fileInput(
        ns("upload"),
        label = "Upload .isox file",
        accept = c(".isox"),
        placeholder = "No file loaded..."
      ),

      tags$style(paste0("#",
                        ns("upload_info"),
                        "{
               font-size:12px;
               color:navy;
         }")),

      textOutput(ns("upload_info")),

      div(style="margin-bottom:10px")

      ),


      div( id = ns("explore-input-panel"), #used to hide certain ui elements initially


      actionButton(ns("action_explore"), "Explore data"),



      tags$hr(style = "border-color: grey;"),

      h4("Plot 1"),

      wellPanel(varselect_mod_ui(ns("plot1_vars"))),


      h4("Plot 2"),

      wellPanel(varselect_mod_ui(ns("plot2_vars"))),

      h4("Plot 3"),

      wellPanel(varselect_mod_density_ui(ns("plot3_vars"))),



      h4("Plot 4"),
      wellPanel(varselect_mod_density_ui(ns("plot4_vars")))

    )
    ),


    # UI: Explore Data Main Panel ---------------------------------------------

    # parameters ----
    div(
      id = ns("explore-main-panel"),

    mainPanel(
      width = 9,


      downloadButton(ns("download_test_data"), "Download Test Data (.isox)", color="red"),

      div(style="margin-bottom:10px"),

      #titlePanel("Explore IsoX File"),

      #verbatimTextOutput(outputId="test", placeholder = TRUE), ##temp


      box(
        title = "Interactive plots of MS data (select points and update display to show in table)",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        scatterplot_mod_ui(ns("plots")),
        checkboxInput(ns("label"), "Label selected points?", value = FALSE)
      ),


      hr(),



      box(
        title = "Table of selected data points",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        dataviewer_mod_ui(ns("dataviewer"))
      ),


      box(
        title = "Density plots",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        densityplot_mod_ui(ns("plots"))
      )


    )
  )
  ))
}

