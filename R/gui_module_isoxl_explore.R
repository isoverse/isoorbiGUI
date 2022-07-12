# server ----

module_isoxl_explore_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # shinyjs::hide("explore-main-panel")
  # shinyjs::hide("explore-input-panel")

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot = NULL
  )

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
    #values$upload_file <- input$upload$name
    values$upload_file <- input$upload$name
    values$upload_error <- NULL
    values$data <- NULL

    # try to read the uploaded file
    tryCatch(
      #values$data <- iso_read_isox_file(input$upload$datapath),
      values$data <- iso_read_isox_file(input$upload$datapath),
      warning = function(w)
        values$upload_error <- w$message,
      error = function(e)
        values$upload_error <- e$message
    )
  })

  # provide feedback on file upload success
  output$upload_info <- renderText({
    req(values$upload_file)
    validate(need(is.null(values$upload_error), message = values$upload_error))
    paste0(values$upload_file, " uploaded")
  })



  # update plot options ----
  observeEvent(values$data, {

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


module_isoxl_explore_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      fileInput(
        ns("upload"),
        label = "Upload .isox file",
        accept = c(".isox"),
        placeholder = "No file loaded..."
      ),


      div(
        id = ns("explore-input-panel"),

      #submitButton("Update output", icon("sync")),


      hr(),

      actionButton(ns("action_explore"), "Explore isox data"),



      tags$hr(style = "border-color: grey;"),

      h4("Plot 1"),

      wellPanel(varselect_mod_ui("plot1_vars")),


      h4("Plot 2"),

      wellPanel(varselect_mod_ui("plot2_vars")),

      h4("Plot 3"),

      wellPanel(varselect_mod_density_ui("plot3_vars")),



      h4("Plot 4"),
      wellPanel(varselect_mod_density_ui("plot4_vars"))

    )
    ),


    # UI: Explore Data Main Panel ---------------------------------------------

    # parameters ----
    div(
      id = ns("explore-main-panel"),

    mainPanel(
      width = 9,

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

#### OLDER VERSION (WAS NOT MAKING PLOTS!) #####

# test ----
# test the data exploration module as a standalone app

# test_module_isoxl_explore <- function(port = 5000) {
#   app <-
#     shinyApp(
#       ui = fluidPage(
#         shinyjs::useShinyjs(),
#         h1("Data Exploration Module Test"),
#         module_isoxl_explore_ui(id = "explore")
#       ),
#       server = function(input, output) {
#         shot_noise <-
#           callModule(module_isoxl_explore_server, id = "explore")
#       }
#     )
#   runApp(app, display.mode = "normal", port = 5000)
# }

# # server ----
#
# module_isoxl_explore_server <- function(input, output, session) {
#
#   # namespace
#   ns <- session$ns
#
#   # increase upload file size
#   options(shiny.maxRequestSize = 100 * 1024 ^ 2)
#
#
#   ## Load data for data exploration module
#
#   # values
#   values <- reactiveValues(upload_file = NULL,
#                            upload_error = NULL,
#                            data = NULL,
#                            plot = NULL,
#                            plot2 = NULL)
#
#   # upload file ----
#   observeEvent(input$upload, {
#     module_message(
#       ns,
#       "info",
#       "new file uploaded file '",
#       input$upload$name,
#       "' into temporary path ",
#       input$upload$datapath
#     )
#
#     # reset the reactive values that hold the file and data
#     values$upload_file <- input$upload$name
#     values$upload_error <- NULL
#     values$data <- NULL
#
#     # try to read the uploaded file
#     tryCatch(
#       values$data <- iso_read_isox_file(input$upload$datapath),
#       warning = function(w)
#         values$upload_error <- w$message,
#       error = function(e)
#         values$upload_error <- e$message
#     )
#   })
#
#   # provide feedback on file upload success
#   output$upload_info <- renderText({
#     req(values$upload_file)
#     validate(need(is.null(values$upload_error), message = values$upload_error))
#     paste0(values$upload_file, " uploaded")
#   })
#
#   # update plot options ----
#   # observeEvent(values$data, {
#   #
#   #   module_message(ns, "info", "updating parameter table")
#   #   if (is.null(values$data) || nrow(values$data) == 0) {
#   #     # no data (maybe incorrect input file --> hide)
#   #     shinyjs::hide("parameters-panel")
#   #     shinyjs::hide("main-plot")
#   #     updateSelectInput(inputId = "isotopolog", choices = c())
#   #     updateSelectInput(inputId = "base_peak", choices = c())
#   #     updateSelectInput(inputId = "filename", choices = c())
#   #     updateSelectInput(inputId = "compound", choices = c())
#   #     shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())
#   #     updateNumericInput(inputId = "rare", value = c())
#   #     updateNumericInput(inputId = "extreme", value = c())
#   #
#   #
#   #
#   #   } else {
#   #     # yes data --> update parameters and show
#   #     updateSelectInput(
#   #       inputId = "isotopolog",
#   #       choices = levels(values$data$isotopolog),
#   #       selected = values$data$isotopolog[2]
#   #     )
#   #     updateSelectInput(
#   #       inputId = "base_peak",
#   #       choices = levels(values$data$isotopolog),
#   #       selected = values$data$isotopolog[1]
#   #     )
#   #     updateSelectInput(
#   #       inputId = "filename",
#   #       choices = c("all", levels(values$data$filename)),
#   #       selected = "all"
#   #     )
#   #     updateSelectInput(
#   #       inputId = "compound",
#   #       choices = c("all", levels(values$data$compound)),
#   #       selected = "all"
#   #     )
#   #     shinyWidgets::updateNumericRangeInput(session,
#   #                                           inputId = "timerange",
#   #                                           value = 0:ceiling(max(values$data$time.min)))
#   #
#   #     updateNumericInput(
#   #       inputId = "rare",
#   #       min = 0, #how to avoid negative input by keyboard?
#   #       max = 100,
#   #       value = 30
#   #     )
#   #
#   #     updateNumericInput(
#   #       inputId = "extreme",
#   #       min = 0, #how to avoid negative input by keyboard?
#   #       max = 10,
#   #       value = 1,
#   #       step = 0.1
#   #     )
#   #
#   #     shinyjs::show("parameters-panel")
#   #   }
#   # }, ignoreNULL = FALSE)
#
#   shinyjs::hide("parameters-panel")
#   shinyjs::hide("main-panel")
#
#   # Data: Exploration -------------------------------------------------------
#   observeEvent(values$data, {
#
#     shinyjs::show("parameters-panel")
#     shinyjs::hide("main-panel")
#     # Plot: Exploration -------------------------------------------------------
#
#     plot1vars <- callModule(varselect_mod_server, ns("plot1_vars"))
#     plot2vars <- callModule(varselect_mod_server, ns("plot2_vars"))
#
#
#     plot3vars <- callModule(varselect_mod_server, ns("plot3_vars"))
#     plot4vars <- callModule(varselect_mod_server, ns("plot4_vars"))
#   }
#   )
#
#   # generate plot ----
#   observeEvent(input$update, {
#
#     shinyjs::show("main-panel")
#     req(values$data)
#
#     values$plot <- callModule(
#       scatterplot_mod_server,
#       ns("plots"), #ns?
#       dataset = values$data,
#       #somehow this gets not updated when new isox file is loaded!!??
#       plot1vars = plot1vars,
#       plot2vars = plot2vars,
#       label = reactive({
#         input$label
#       })
#     )
#
#     callModule(dataviewer_mod_server, ns("dataviewer"), values$plot) #FIXME: ns() appropriate?
#
#
#     values$plot2 <- callModule(
#       densityplot_mod_server,
#       ns("plots"),
#       dataset = values$data,
#       plot3vars = plot3vars, #ns?
#       plot4vars = plot4vars,
#       label = reactive({
#         input$label
#       })
#     )
#
#     callModule(dataviewer_mod_server, id = "dataviewer", dataset =values$plot2)
#
#   })
#
#
# }
#
# # user interface ----
#
# module_isoxl_explore_ui <- function(id) {
#   # namespace
#   ns <- NS(id)
#
#
#   fluidPage(sidebarLayout(
#     sidebarPanel(
#       width = 3,
#
#       # upload ----
#       fileInput(
#         ns("upload"),
#         label = "Upload .isox file",
#         accept = c(".isox"),
#         placeholder = "No file loaded..."
#       ),
#       textOutput(ns("upload_info")),
#
#
#
#       # parameters ----
#       div(
#         id = ns("parameters-panel"),
#
#         hr(),
#         #submitButton("Update output", icon("sync")),
#         actionButton(ns("update"), "Plot data", icon("sync")),
#
#         tags$hr(style = "border-color: grey;"),
#
#
#
#       h4("Plot 1"),
#
#       wellPanel(varselect_mod_ui(ns("plot1_vars"))),
#
#
#       h4("Plot 2"),
#
#       wellPanel(varselect_mod_ui(ns("plot2_vars"))),
#
#       h4("Plot 3"),
#
#       wellPanel(varselect_mod_density_ui(ns("plot3_vars"))),
#
#       h4("Plot 4"),
#       wellPanel(varselect_mod_density_ui(ns("plot4_vars")))
#
#     )
#     ),
#
#
#     # UI: Explore Data Main Panel ---------------------------------------------
#
#
#     mainPanel(
#
#       width = 9,
#
#
#
#       div(
#         id = ns("main-panel"),
#
#         # #temp for debugging
#         #titlePanel(...),
#
#       h5(
#         "Please note: When you upload a second .isox file, the plots currently are not loading the new data. Reload the website to explore a different .isox file."
#       ),
#
#       hr(),
#       #titlePanel("Explore IsoX File"),
#
#       #verbatimTextOutput(outputId="test", placeholder = TRUE), ##temp
#
#
#       box(
#         title = "Interactive plots of MS data (select points and update display to show in table)",
#         width = "auto" ,
#         height = "auto",
#         status = "primary",
#         solidHeader = T,
#         scatterplot_mod_ui(ns("plots")),
#         checkboxInput("label", "Label selected points?", value = FALSE)
#       ),
#
#       hr(),
#
#       box(
#         title = "Table of selected data points",
#         width = "auto" ,
#         height = "auto",
#         status = "primary",
#         solidHeader = T,
#         dataviewer_mod_ui(ns("dataviewer"))
#       ),
#
#
#       box(
#         title = "Density plots",
#         width = "auto" ,
#         height = "auto",
#         status = "primary",
#         solidHeader = T,
#         densityplot_mod_ui(ns("plots"))
#       )
#       )
#
#     )
#   ))
# }
