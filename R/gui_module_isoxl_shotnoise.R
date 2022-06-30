# test
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
        shot_noise <- callModule(module_isoxl_shot_noise_server, id = "shot_noise")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server
module_isoxl_shot_noise_server <- function(input, output, session) {

  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize=100*1024^2)

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot = NULL
  )

  # upload file ----
  observeEvent(input$upload, {
    module_message(ns, "info", "new file uploaded file '", input$upload$name,
                   "' into temporary path ", input$upload$datapath)

    # reset the reactive values that hold the file and data
    values$upload_file <- input$upload$name
    values$upload_error <- NULL
    values$data <- NULL

    # try to read the uploaded file
    tryCatch(
      values$data <- iso_read_isox_file(input$upload$datapath),
      warning = function(w) values$upload_error <- w$message,
      error = function(e) values$upload_error <- e$message
    )
  })

  # provide feedback on file upload success
  output$upload_info <- renderText({
    req(values$upload_file)
    validate(
      need(is.null(values$upload_error), message = values$upload_error)
    )
    paste0(values$upload_file, " uploaded")
  })

  # update plot options ----
  observeEvent(values$data, {
    module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorret input file --> hide)
      shinyjs::hide("parameters-panel")
      updateSelectInput(inputId = "isotopolog", choices = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      updateSelectInput(inputId = "filename", choices = c())
      updateSelectInput(inputId = "compound", choices = c())
      shinyWidgets::updateNumericRangeInput(
        session, inputId = "timerange", value = c())
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
        choices = c("all", levels(values$data$filename)),
        selected = "all"
      )
      updateSelectInput(
        inputId = "compound",
        choices = c("all", levels(values$data$compound)),
        selected = "all"
      )
      shinyWidgets::updateNumericRangeInput(
        session, inputId = "timerange",
        value = 0:ceiling(max(values$data$time.min))
      )
      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)

  # generate plot ----
  observeEvent(input$render_plot, {
    req(values$data)
    module_message(
      ns, "info",
      "generating plot with selected parameters:",
      "\n - isotopolog: ", input$isotopolog %then% "",
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - files: ", paste(input$filename %then% "", collapse = ", "),
      "\n - compounds: ", paste(input$compound %then% "", collapse = ", "),
      "\n - timerange: ", paste(input$timerange %then% "", collapse = " to ")
    )

    values$plot <- values$data %>% iso_filter_isox_file(input=input) %>% assign_basepeak_shotnoise(input=input)  %>% calc_shotnoise() %>% plot_shot_noise(input=input)

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
            filename = paste(
              "shotnoise",
              input$isotopolog,
              input$basepeak,
              sep = "_"
            )
          )
        )
    })
  })

}



# user interface
module_isoxl_shotnoise_ui <- function(id) {
  # namespace
  ns <- NS(id)

  fluidPage(


    sidebarLayout(


      sidebarPanel(
        width = 3,

        # upload ----
        fileInput(ns("upload"), label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),
        textOutput(ns("upload_info")),

        # parameters ----
        div(
          id = ns("parameters-panel"),
          tags$hr(style="border-color: grey;"),
          selectInput(inputId = ns("isotopolog"), label="Isotopolog", c()),
          selectInput(inputId = ns("base_peak"), label="Base peak", c()),
          selectInput(inputId = ns("filename"), label="Files", c(), multiple = TRUE),
          selectInput(inputId = ns("compound"), label="Compounds", c(), multiple = TRUE),
          shinyWidgets::numericRangeInput(ns("timerange"), "Time [min]", value = c()),
          tags$hr(style="border-color: grey;"),
          actionButton(ns("render_plot"), label = "Plot", icon = icon("sync"))
        ) %>% shinyjs::hidden()


#                    with_tippy(numericInput(
#                      inputId = "INPUT_SN_Rare",
#                      label = "Remove weak isotopologs",
#                      30,
#                      min = 1,
#                      max = 50,
#                      step = 1,
#                      width = NULL
#                    ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_SN_TICxIT",
#                      label = "Remove outlier scans",
#                      1,
#                      min = 0.1,
#                      max = 10,
#                      step = 0.1,
#                      width = NULL
#                    ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right")
#
#

      ),


      # plot -----
      mainPanel(

        box(title = "Shot Noise Plot", width = "auto" , height="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput(ns("plot")))), #how to save png at higher resolution?


      )
    )
  )

}
