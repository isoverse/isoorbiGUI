# test ----
# test the dual inlet module as a standalone app
#' @title test_module_isoxl_dual_inlet
#' @description ...
#' @param port ...
#' @return ...
#' @export
test_module_isoxl_dual_inlet <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Dual Inlet Module Test"),
        module_isoxl_dual_inlet_ui(id = "dual_inlet")
      ),
      server = function(input, output) {
        dual_inlet <-
          callModule(module_isoxl_dual_inlet_server, id = "dual_inlet")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----
#' @title module_isoxl_dual_inlet_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
module_isoxl_dual_inlet_server <- function(input, output, session) {


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

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")

    # reset the reactive values that hold the file and data
    values$upload_file <- NULL
    values$upload_error <- NULL
    values$data <- NULL

    #reset the outputs
    output$mytable <- NULL
    output$summary <- NULL
    output$plot <- NULL


    if (input$test_data == TRUE) {



    values$data <- iso_read_isox_file(system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbi"))

    # Download test data: .isox file ----------------------------------

    output$download_test_data <- downloadHandler(
      filename = function() {
        "testfile_DualInlet_small.isox" #FIXME: provide name as a variable
      },
      content = function(file) {
        write.table(values$data, file, sep="\t", row.names = F)
      }
    )

    shinyjs::show("test_download_button")

    {shinyjs::hide("upload_panel")}

    }

    if (input$test_data == FALSE) {

      shinyjs::hide("test_download_button")
      {shinyjs::show("upload_panel")}

    }


    {shinyjs::hide("parameters-panel")}


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
    paste0("uploaded ", values$upload_file)
  })


  # update plot options ----
  observeEvent(values$data, {
    module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorrect input file --> hide)
      shinyjs::hide("parameters-panel")
      updateTextInput(inputId = "sample", value = c())
      updateTextInput(inputId = "reference", value = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      updateNumericInput(inputId = "segments", value = c())
      updateNumericInput(inputId = "blocks", value = c()) #FIX: is this what we want? Eg. update when loading a new file?
      updateNumericInput(inputId = "block.time", value = c())

      updateNumericInput(inputId = "startup.time", value = c())
      updateNumericInput(inputId = "idle.time", value = c())
      updateNumericInput(inputId = "rare", value = c())
      updateNumericInput(inputId = "extreme", value = c())
      updateRadioButtons(inputId = "ratio.method", choices = c())



    } else {
      # yes data --> update parameters and show

      updateTextInput(
        inputId = "sample",
        value = paste("sample",  values$data$compound[1], sep = "_")
      )

      updateTextInput(
        inputId = "reference",
        value = paste("reference", values$data$compound[1], sep = "_")
      )

      updateSelectInput(
        inputId = "base_peak",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[1]
      )

      # FIXME: add segments, blocks, block.time, startup.time etc?

      updateNumericInput(
        inputId = "rare",
        min = 0,
        #how to avoid negative input by keyboard?
        max = 90,
        value = 30
      )

      updateNumericInput(
        inputId = "extreme",
        min = 0,
        #how to avoid negative input by keyboard?
        max = 10,
        value = 1,
        step = 0.1
      )

      updateRadioButtons(
        inputId = "ratio.method",
        choiceNames = c("sum",  "mean", "median", "geometric"),
        choiceValues = c("sum",  "mean", "median", "geometric.mean"),
        selected = "sum"
      )


      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)


  isoXL.version <- as.character(utils::packageVersion("isoorbi")) #FIXME: get version information directly from main UI?



  # Table of processing parameters

  observeEvent(input$start, {

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")

    #basic checks
    if (input$idle.time >= input$block.time) showNotification("Error: Input Switch time [min] exceeds Block time [min]",type="error")


    #FIXME: add more and better validations
    #basic checks that input to define dual inlet matches the data

    if ( max(values$data$time.min) < ((input$blocks * input$block.time) + input$startup.time) ) showNotification("Validate inputs: Dual inlet specified exceeds time range in data", type="error")


    #FIME: learn how to correctly implement shinyvalidate
    #Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$enable()

    #Required inputs
    iv$add_rule("sample", shinyvalidate::sv_required())
    iv$add_rule("reference", shinyvalidate::sv_required())
    iv$add_rule("blocks", shinyvalidate::sv_required())
    iv$add_rule("block.time", shinyvalidate::sv_required())
    iv$add_rule("idle.time", shinyvalidate::sv_required())
    iv$add_rule("startup.time", shinyvalidate::sv_required())
    iv$add_rule("segments", shinyvalidate::sv_required())
    iv$add_rule("rare", shinyvalidate::sv_required())
    iv$add_rule("extreme", shinyvalidate::sv_required())
    iv$add_rule("ratio.method", shinyvalidate::sv_required())

    #Input values
    iv$add_rule("blocks", shinyvalidate::sv_between(1, 20))
    iv$add_rule("block.time", shinyvalidate::sv_between(1, 30)) #FIXME: max depends on .isox file
    iv$add_rule("idle.time", shinyvalidate::sv_between(0, 5)) #FIXME: max depends on .isox file
    iv$add_rule("startup.time", shinyvalidate::sv_between(0, 10)) #FIXME: max depends on .isox file
    iv$add_rule("segments", shinyvalidate::sv_between(1,10))
    iv$add_rule("rare", shinyvalidate::sv_between(0, 90))
    iv$add_rule("extreme", shinyvalidate::sv_between(0, 10))



    if (iv$is_valid()) {

      showNotification(
        "Inputs look ok",
        type = "warning",
        duration = 1,
      )
      # use inputs...
    } else {
      showNotification(
        "Check for invalid input",
        type = "error",
        duration = 10,
      )
    }

    #req(iv$is_valid())

    req(iv$is_valid()) #DON'T PROCEED IF INPUTS ARE NOT VALID

    req( max(values$data$time.min) >= ((input$blocks * input$block.time) + input$startup.time) )
    req(input$idle.time < input$block.time)


    #req(values$upload_file)

    shinyjs::show("main-plot")


    # Message with processing parameters

    module_message(
      ns,
      "info",
      "Processing data with selected parameters:",
      "\n - sample: ",
      paste(input$sample %then% ""),
      "\n - reference: ",
      paste(input$reference %then% ""),
      "\n - basepeak: ",
      input$base_peak %then% "",
      "\n - segments: ",
      input$segments %then% "",
      "\n - blocks: ",
      input$blocks %then% "",
      "\n - block.time: ",
      input$block.time %then% "",
      "\n - startup.time: ",
      input$startup.time %then% "",
      "\n - idle.time: ",
      input$idle.time %then% "",
      "\n - rare: ",
      input$rare %then% "",
      "\n - extreme: ",
      input$extreme %then% "",
      #)#edit
      "\n - ratio.method: ",
      input$ratio.method %then% "" #)#edit
    )


    # Data: Dual Inlet Analysis ---------------------------------------------------

    inputs.dual <- data.frame(
      parameter = c(
        "IsoXL Version",
        "Reference",
        "Sample",
        "Blocks",
        "Basepeak",

        "Idle time",
        "Startup time [min]",
        "Block time [min]",
        "Segments",
        "Rarely detected [%]",

        "Extreme TICxIT [%]",
        "Ratio Method"
      ),


      value = c(
        isoXL.version,
        input$reference,
        input$sample,
        input$blocks,
        input$base_peak,


        input$idle.time,
        input$startup.time,
        input$block.time,
        input$segments,
        input$rare,

        input$extreme,
        input$ratio.method
      )

    )





    #req(input$INPUT_Rare)
    data.dual_inlet_filtered <- values$data %>%
      iso_simplify_isox_file() %>%
      na.omit() %>%
      remove.ticxit(input = input) %>% #temp: validate correct functioning
      remove.satellitePeaks() %>%
      remove.rare(input = input)


    #  Dual Inlet annotation table --------------------------------------------

    # req(input$reference)
    # req(input$sample)
    # req(input$blocks)
    # req(input$segments)
    # req(input$idle.time)
    # req(input$startup.time)
    # req(input$block.time)

    dual.inlet.annotation     <- define.dual.inlet(input)



    # Dual Inlet: Merge data with annotation ----------------------------------

    data.annotated <-
      merge.di.annotations(data.dual_inlet_filtered, dual.inlet.annotation)





    # Dual Inlet: Assign base peak --------------------------------------------


    data.basepeak <-
      assign.basepeak(data.annotated, input$base_peak)  #needs validation; e.g. is basepeak present in data?


    # Dual Inlet: Calculate output table for results file ---------------

    data.output <-
      calc.output(df = data.basepeak, INPUT_RatioMethod = input$ratio.method)

    # Dual Inlet: Make Excel file ---------------------------------------------

    data.xls <- make.xls(data.output, params = inputs.dual)


    # Table output: Show rows of .isox file------------------------------------------------


    output$mytable = DT::renderDataTable({
      datatable(
        data.dual_inlet_filtered %>% ungroup() %>% mutate_if(is.double, round, digits = 3),
        rownames = FALSE ,
        extensions = c('Buttons', 'ColReorder'),
        options = list(
          orientation ='landscape',
          scrollX = TRUE,
          dom = 'Bfrtip',
         # buttons = c('colvis', 'copy',  'excel', 'pdf'),
          buttons =
            list('colvis', 'copy',   list(
              extend = 'excel',
              filename = 'IsoXL_DualInlet_isox'
            ), list(
              extend = 'pdf',
              pageSize = 'A4',
              orientation = 'landscape',
              filename = 'IsoXL_DualInlet_isox'
            )),

          colReorder = TRUE#, #columns can be reordered
        )
      )

    })

    # Table output: Summary statistics ----------------------------


    output$summary <- DT::renderDataTable({
      datatable(
        data.output,
        rownames = FALSE ,
        extensions = c('Buttons', 'ColReorder'),
        options = list(
          scrollX = TRUE,
          dom = 'Bfrtip',
         # buttons = c('colvis', 'copy',  'excel', 'pdf'),
         buttons =
           list('colvis', 'copy',  list(
             extend = 'excel',
             filename = 'IsoXL_DualInlet_results'
           ), list(
             extend = 'pdf',
             pageSize = 'A4',
             orientation = 'landscape',
             filename = 'IsoXL_DualInlet_results'
           )),
          colReorder = TRUE #columns can be reordered
        )
      )

    })



    # Plot output: Single Analysis--------------------------------------------------------

    output$plot <- renderPlotly({
      # isolate to avoid triggers on other input parameter selections


      isolate({
        plotly::ggplotly(
          ggplot(data = data.output, aes(
            x = Injection, y = Ratio, color = isotopolog
          )) + scale_color_brewer(palette = "Dark2") +

            geom_errorbar(
              aes(
                ymin = (Ratio - Ratio.SEM),
                ymax = (Ratio + Ratio.SEM)
              ),
              width = 0.7,
              size = 0.3,
              color = "black",
              position = position_dodge(.9)
            )       +        geom_point(size = 3, shape = 10) + facet_wrap(~ filename, ncol = 1),
          # +
          #   scale_x_continuous(breaks=1:100), #temp
          # temp: could be interesting option: + facet_grid(isotopolog~.),

          dynamicTicks = TRUE #change ticks on zoom
        ) %>%

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
              filename = paste("dual_inlet",tools::file_path_sans_ext(input$upload$name), sep = "_") #FIX
            )
          )


      })
    })


    # Make Excel file ---------------------------------------------

    data_dual.xls <-
      make.xls(df.stat = data.output, params = inputs.dual)


# Download output: Excel file ----------------------------------


    output$download_xlsx <- downloadHandler(
      filename = function() {

        if (input$test_data == TRUE) {paste("testfile_DualInlet_small", "_IsoXL.xlsx", sep="")} #FIXME: use a variable for testfile name!
        else{
        paste(tools::file_path_sans_ext(input$upload$name),
              "_IsoXL.xlsx",
              sep = "")}
      },
      content = function(file) {
        saveWorkbook(data_dual.xls, file)
      }
    )


    shinyjs::show("main-rest")

  })
}

# user interface ----
#' @title module_isoxl_dual_inlet_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_dual_inlet_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,


      # Use test data instead of uploading data (or any data that has been already uploaded)

      tippy(
        text = checkboxInput(ns("test_data"), "Use test data", value = FALSE),

        #"For details on test data see Contact tab",
        animation="scale",

        tooltip = "<span style='font-size:15px;'>For details on test data see Contact tab<span>", allowHTML = TRUE,

       # theme="light",
        placement ="top-start"

      ),

      div(
        id = ns("test_download_button"),
        downloadButton(ns("download_test_data"), "Download (.isox)"),
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
        id = ns("parameters-panel"), #can be shown or made hidden


        actionButton(ns("start"), label = "Start", icon = icon("rotate")),


        tags$hr(style = "border-color: grey;"),

        h6("Define Dual Inlet"),

        textInput(
          inputId = ns("sample"),
          label = "Sample",
          value = c()
        ),

        textInput(
          inputId = ns("reference"),
          label = "Reference",
          value = c()
        ),

        selectInput(inputId = ns("base_peak"), label = "Base peak", c()),

        tags$hr(style = "border-color: grey;"),

        h6("Parameters:"),

        numericInput(
          inputId = ns("blocks"),
          label = "Blocks",
          3,
          min = 1,
          max = 100,
          step = 1,
          width = NULL
        ),

        numericInput(
          inputId = ns("block.time"),
          label = "Block time [min]",
          1,
          min = 1,
          max = 30,
          step = 0.5,
          width = NULL
        ),

        numericInput(
          inputId = ns("idle.time"),
          label = "Switch time [min]",
          0.1,
          min = 0,
          max = 5,
          step = 0.1,
          width = NULL
        ),

        tippy(

        numericInput(
          inputId = ns("startup.time"),
          label = "Startup time [min]",
          0,
          min = 0,
          max = 10,
          step = 0.1,
          width = NULL
        ),
        tooltip = "<span style='font-size:15px;'>Initial time to stabilize the spray <span>", allowHTML = TRUE,
        #"Initial time to stabilize the spray ",
        placement = "right"
        ),

        tags$hr(style = "border-color: grey;"),

        h6("Processing options:"),

        tippy(
          numericInput(
            inputId = ns("segments"),
            label = "Segments",
            1,
            min = 1,
            max = 10,
            step = 1,
            width = NULL
          ),
          "Split each block into segments",
          placement = "right"
        ),

        #Use shinyBS::bsTooltip instead of tippy?
        tippy(
          numericInput(
            ns("rare"),
            "Remove weak signals",
            value = c(),
            width = NULL
          ),
          "Isotopocule must have been detected in more than x % of scans",
          placement = "right"
        ),
        tippy(
          numericInput(
            ns("extreme"),
            "Remove extreme scans",
            value = c(),
            width = NULL
          ),
          "Truncate data based on ions in Orbitrap (ITxTIC)",
          placement = "right"
        ),

        radioButtons(
          inputId = ns("ratio.method"),
          label = "Ratio method",
          choiceNames = c("sum",  "mean", "median", "geometric"),
          choiceValues = c("sum",  "mean", "median", "geometric.mean"),
          selected = "sum"
        )

      ) %>% shinyjs::hidden() # so that element does not load during startup and refresh

    ),

    # UI: Dual Inlet Analysis Main Panel ------------------------------------------

    mainPanel(
      width = 9,



      # parameters ----
      div(
        id = ns("main-plot"), #can be shown or hidden

        downloadButton(ns("download_xlsx"), "Download Results (.xlsx)"),

        div(style="margin-bottom:10px"),


       box(
          title = "Overview of Results",
          width = "auto" ,
          height = "auto",
          status = "primary",
          solidHeader = T,
          withSpinner(plotlyOutput(ns("plot")))
        )
      ) %>% shinyjs::hidden(),
      #how to save png at higher resolution?

      hr(),

      div(
        id = ns("main-rest"),



        box(
          title = "Results Table",
          width = "auto" ,
          status = "primary",
          solidHeader = T,
          DT::dataTableOutput(ns("summary"))
        ),

        hr(),


        box(
          title = "Filtered .isox File",
          width = "auto" ,
          status = "primary",
          solidHeader = T,
          DT::dataTableOutput(ns("mytable"))
        )

      ) %>% shinyjs::hidden()

    )

  ))
}
