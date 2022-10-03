# test ----
# test the shot noise module as a standalone app
#' @title test_module_isoxl_shot_noise
#' @description test the shot noise module as a standalone app
#' @param port ...
#' @return ...
#' @export
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
#' @title module_isoxl_shot_noise_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
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

  # use test data ----
  observeEvent(input$test_data, {

    # values
    values$plot = NULL

    shinyjs::hide("main-plot")



    if (input$test_data == TRUE) {


      values$data <- iso_read_isox_file(system.file("extdata", "testfile_FlowInjection_DataExploration_small.isox", package="isoorbiGUI")) # FIXME: use files from isoorbi?

      # Download test data: .isox file ----------------------------------

      output$download_test_data <- downloadHandler(
        filename = function() {
          "testfile_FlowInjection_DataExploration_small.isox" #FIXME: provide name as a variable
        },
        content = function(file) {
          write.table(values$data, file, quote=FALSE, sep="\t", row.names = F)
        }
      )

      shinyjs::show("test_download_button")


      {shinyjs::hide("upload_panel")}

    }

    if (input$test_data == FALSE) {

      shinyjs::hide("test_download_button")
      {shinyjs::show("upload_panel")}

      # reset the reactive values that hold the file and data
      values$upload_file <- NULL
      values$upload_error <- NULL
      values$data <- NULL

    }


    {shinyjs::hide("parameters-panel")}


  })

  # upload file ----
  observeEvent(input$upload, {

    shinyjs::hide("main-plot")

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
    values$plot <- NULL

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

        values$data <- iso_read_isox_file(system.file("extdata", "testfile_FlowInjection_DataExploration_small.isox", package="isoorbiGUI")) # FIXME: use files from isoorbi?

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
        selected = levels(values$data$isotopolog)[2]
      )
      updateSelectInput(
        inputId = "base_peak",
        choices = levels(values$data$isotopolog),
        selected = levels(values$data$isotopolog)[1]
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

    # values
    values$plot = NULL

    shinyjs::hide("main-plot")


    req(values$data)

    # special safety checks for shinywidget that does not work with shinyvalidate
    if (input$timerange[2] <= input$timerange[1]) showNotification("Input Time [min]: First value has to be smaller than second value",
                                                                   type="error",
                                                                   duration = 20)
    if (input$timerange[1] < 0) showNotification("Input Time [min]: Check first input value",
                                                 type="error",
                                                 duration = 20)
    if (input$timerange[2] <= 0) showNotification("Input Time [min]: Check second input value",
                                                  type="error",
                                                  duration = 20)

    if (input$timerange[1] > max(values$data$time.min) +0.01) showNotification("Input Time [min]: First value exceeds data in uploaded isox files.",
                                                                               type="error",
                                                                               duration = 20) #give a bit leeway
    if (input$timerange[2] > max(values$data$time.min) +0.01) showNotification("Input Time [min]: Second value exceeds data in uploaded isox files.",
                                                                               type="error",
                                                                               duration = 20) #give a bit leeway

    if ((input$timerange[2] - input$timerange[1]) < 0.3) showNotification("Input Time [min]: Time range should be > 0.3 minutes",
                                                                          type="error",
                                                                          duration = 20)


    req(input$timerange[2] > input$timerange[1])
    req(input$timerange[1] >= 0)
    req(input$timerange[2] > 0)
    req(input$timerange[2] <= max(values$data$time.min)+0.01) #give a bit leeway
    req(input$timerange[1] <= max(values$data$time.min)+0.01) #give a bit leeway; ~ redundant
    req((input$timerange[2] - input$timerange[1]) > 0.3)








    #FIME: learn how to correctly implement shinyvalidate
    #Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$enable()


    #Required inputs
    iv$add_rule("filename", shinyvalidate::sv_required())
    iv$add_rule("compound", shinyvalidate::sv_required())
    iv$add_rule("isotopolog", shinyvalidate::sv_required())
    iv$add_rule("base_peak", shinyvalidate::sv_required())
    iv$add_rule("timerange", shinyvalidate::sv_required())
    # #timerange input requires special attention
    iv$add_rule("rare", shinyvalidate::sv_required())
    iv$add_rule("extreme", shinyvalidate::sv_required())

    #Input values
    iv$add_rule("rare", shinyvalidate::sv_between(0, 90))
    iv$add_rule("extreme", shinyvalidate::sv_between(0, 10))
    iv$add_rule("isotopolog", shinyvalidate::sv_not_equal(input$base_peak, message_fmt = "Equal to base peak")) #basepeak and isotopolog must not be identical

    # FIXME: Bug if I delete rare or extreme input after clicking plot button (does not update)

    if (iv$is_valid()) {

      showNotification(
        "Inputs look ok",
        type = "warning",
        duration = 5
      )
      # use inputs...
    } else {
      showNotification(
        "Check for invalid input",
        type = "error",
        duration = 20
      )
    }

    #req(iv$is_valid())

    req(iv$is_valid()) #DON'T PROCEED IF INPUTS ARE NOT VALID


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

    shinyjs::show("main-plot")

    #ACTUAL DATA PROCESSING STEP

    values$plot <- values$data  %>%
                        iso_simplify_isox_file() %>%
                        iso_filter_isox_file(input=input) %>%
                        remove.satellitePeaks() %>%
                        remove.rare(input=input)  %>%
                        remove.ticxit(input=input) %>%
                        assign_basepeak_shotnoise(input=input)  %>%
                        calc_shotnoise() %>%
                        plot_shot_noise(input=input)

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
#' @title module_isoxl_shotnoise_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_shotnoise_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      # Use test data instead of uploading data (or any data that has been already uploaded)

      tippy(
        checkboxInput(ns("test_data"), "Use test data", value = FALSE),

        tooltip = "<span style='font-size:15px;'>See Info for details<span>", allowHTML = TRUE,

        animation="scale",
        #theme="light",
        placement ="top-start"

      ),

      div(
        id = ns("test_download_button"),
        downloadButton(ns("download_test_data"), "Download test data"),
        div(style="margin-bottom:10px")
      ) %>% shinyjs::hidden(),

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



      # parameters ----
      div(
        id = ns("parameters-panel"),

        actionButton(ns("render_plot"), label = "Start", icon = icon("rotate")),

        tags$hr(style = "border-color: grey;"),
        selectInput(
          inputId = ns("filename"),
          label = "File",
          c(),
          multiple = FALSE
        ),

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
          numericInput(ns("rare"), "Remove weak signals (in %)", value = c(), width=NULL),
          tooltip = "<span style='font-size:15px;'>Isotopocule must have been detected <br> in more than x % of scans<span>", allowHTML = TRUE,
          # "Processing option: Isotopocule must have been detected > x % of scans",
          placement = "right"),
        tippy(
          numericInput(ns("extreme"), "Remove extreme scans (in %)", value = c(), width=NULL),
          tooltip = "<span style='font-size:15px;'>Truncate outlying data based on <br> ions in Orbitrap (ITxTIC)<span>", allowHTML = TRUE,
          # "Processing option: Truncate data based on TICxIT",
          placement = "right")

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
