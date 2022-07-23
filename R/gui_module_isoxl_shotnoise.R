# test ----
# test the shot noise module as a standalone app

test_module_isoxl_shot_noise <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Shot Noise Module Test"),
        module_isoxl_shotnoise_ui(id = "shot_noise")
      ),
      server = function(input, output) {
        shot_noise <-
          callModule(module_isoxl_shot_noise_server, id = "shot_noise")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_shot_noise_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

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
    values$upload_file <- input$upload$name
    values$upload_error <- NULL
    values$data <- NULL

    # try to read the uploaded file
    tryCatch(
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



    module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorrect input file --> hide)
      shinyjs::hide("parameters-panel")
      shinyjs::hide("main-plot")
      updateSelectInput(inputId = "isotopolog", choices = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      updateSelectInput(inputId = "filename", choices = c())
      updateSelectInput(inputId = "compound", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())
      updateNumericInput(inputId = "rare", value = c())
      updateNumericInput(inputId = "extreme", value = c())



    } else {
      # yes data --> update parameters and show
      updateSelectInput(
        inputId = "isotopolog",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[2]
      )
      updateSelectInput(
        inputId = "base_peak",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[1]
      )
      updateSelectInput(
        inputId = "filename",
        choices = c(levels(values$data$filename)),
        selected = levels(values$data$filename)[1]

      )
      updateSelectInput(
        inputId = "compound",
        choices = c(levels(values$data$compound)),
        selected = levels(values$data$compound)[1]

      )
      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange",
                                            value = 0:ceiling(max(values$data$time.min)))

      updateNumericInput(
        inputId = "rare",
        min = 0, #how to avoid negative input by keyboard?
        max = 100,
        value = 30
      )

      updateNumericInput(
        inputId = "extreme",
        min = 0, #how to avoid negative input by keyboard?
        max = 10,
        value = 1,
        step = 0.1
      )

      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)

  # generate plot ----
  observeEvent(input$render_plot, {
    req(values$data)

    shinyjs::show("main-plot")


    module_message(
      ns, "info",
      "generating plot with selected parameters:",
      "\n - isotopolog: ", input$isotopolog %then% "",
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - files: ", paste(input$filename %then% "", collapse = ", "),
      "\n - compound: ", paste(input$compound %then% "", collapse = ", "),
      "\n - timerange: ", paste(input$timerange %then% "", collapse = " to "),
      "\n - rare: ", input$rare %then% "",
      "\n - extreme: ", input$extreme %then% ""#)#edit
    )


    #FIME: learn how to correctly implement shinyvalidate
    # Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$add_rule("timerange", shinyvalidate::sv_required())

    iv$enable()

    if (iv$is_valid()) {
      # use inputs...
    } else {
      showNotification(
        "Time range is a required input",
        type = "error",
        duration = 10,
      )
    }

    req(iv$is_valid())

    values$plot <- values$data  %>% remove.satellitePeaks() %>% remove.ticxit(input=input) %>% iso_simplify_isox_file() %>% remove.rare(input=input)  %>% iso_filter_isox_file(input=input) %>% assign_basepeak_shotnoise(input=input)  %>% calc_shotnoise() %>% plot_shot_noise(input=input)
    #definitely need to seriously check again the sequence of how to chain filters most appropriately, eg. remove.rare???
  })

  # render plot ----
  output$plot <- renderPlotly({


    req(values$plot)
    # isolate to avoid triggers on other input parameter selections
    isolate({
      plotly::ggplotly(values$plot) %>%
        plotly::config(
          displaylogo = FALSE,
          modeBarButtonsToRemove = c(
            "zoomIn2d",
            "zoomOut2d",
            "lasso2d",
            "select2d",
            "hoverClosestCartesian",
            "hoverCompareCartesian"
          ),
          #options on how to save an image
          toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 600,
            scale = 4,
            filename = paste("shotnoise",
                             input$isotopolog,
                             input$base_peak,
                             sep = "_")
          )
        )
    })
  })

}


# user interface ----

module_isoxl_shotnoise_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      # upload ----
      fileInput(
        ns("upload"),
        label = "Upload .isox file",
        accept = c(".isox"),
        placeholder = "No file loaded..."
      ),
      textOutput(ns("upload_info")),



      # parameters ----
      div(
        id = ns("parameters-panel"),

        hr(),

        actionButton(ns("render_plot"), label = "Plot", icon = icon("rotate")),

        tags$hr(style = "border-color: grey;"),
        selectInput(
          inputId = ns("filename"),
          label = "File",
          c(),
          multiple = FALSE
        ),

        # tippy(
        #   "test text",
        #   placement = "right"),


        selectInput(
          inputId = ns("compound"),
          label = "Compound",
          c(),
          multiple = FALSE),

        tags$hr(style = "border-color: grey;"),

        selectInput(inputId = ns("isotopolog"), label = "Isotopolog", c()),
        selectInput(inputId = ns("base_peak"), label = "Base peak", c()),


        #FIXME: negative values can still be typed into the input
        #FIXME: what if there is an empty input for timerange

        shinyWidgets::numericRangeInput(ns("timerange"), "Time [min]", value = c(), min = 0), #FIXME: validate that timerange[2] > timerange[1]
        tags$hr(style = "border-color: grey;"),

        #currently not used in data processing!


        #Use shinyBS::bsTooltip instead of tippy?
        tippy(
          numericInput(ns("rare"), "Remove weak signals", value = c(), width=NULL),
          "Processing option: Isotopocule must have been detected > x % of scans", placement = "right"),
        tippy(
          numericInput(ns("extreme"), "Remove extreme scans", value = c(), width=NULL),
          "Processing option: Truncate data based on TICxIT", placement = "right")

      ) %>% shinyjs::hidden()

    ),


    # plot -----
    mainPanel(
      # parameters ----
      div(
        id = ns("main-plot"),

        box(
          title = "Shot Noise Plot",
          width = "auto" ,
          height = "auto",
          status = "primary",
          solidHeader = T,
          withSpinner(plotlyOutput(ns("plot")))
        )
        #FIXME: how to save png at higher resolution?
      ),


    )
  ))
}
