# test ----
# test the flow injection analysis module as a standalone app

test_module_isoxl_flow <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Flow Injection Analysis Module Test"),
        module_isoxl_flow_ui(id = "flow")
      ),
      server = function(input, output) {
        flow <-
          callModule(module_isoxl_flow_server, id = "flow")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_flow_server <- function(input, output, session) {

# namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot = NULL #revise
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
      shinyjs::hide("main-rest")
      updateSelectInput(inputId = "filename.sel", choices = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())
      updateNumericInput(inputId = "rare", value = c())
      updateNumericInput(inputId = "extreme", value = c())
      updateRadioButtons(inputId = "ratio.method", choices =c())



    } else {
      # yes data --> update parameters and show
      updateSelectInput(
        inputId = "filename.sel",
        choices = levels(values$data$filename),
        selected = values$data$filename
      )

      updateSelectInput(
        inputId = "base_peak",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[1]
      )

      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange",
                                            value = 0:ceiling(max(values$data$time.min))
                                            # Fix: How to avoid negative numbers?
                                            # https://stackoverflow.com/questions/42164171/protect-shiny-numericinput-from-negative-numbers
      )

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

      updateRadioButtons(
        inputId = "ratio.method",
        choiceNames = c("sum",  "mean", "median", "geometric"),
        choiceValues = c("sum",  "mean", "median", "geometric.mean"),
        selected = "sum"
      )

      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)


  isoXL.version <- as.character(utils::packageVersion("isoorbi")) #get version information directly



  # Table of processing parameters

  observeEvent(input$start, {

    # special safety checks for shinywidget that does not work with shinyvalidate
    if (input$timerange[2] <= input$timerange[1]) showNotification("Input Time [min]: First value has to be smaller than second value",type="error")
    if (input$timerange[1] < 0) showNotification("Input Time [min]: Check first input value",type="error")
    if (input$timerange[2] <= 0) showNotification("Input Time [min]: Check second input value",type="error")

    if (input$timerange[1] > max(values$data$time.min) +0.01) showNotification("Input Time [min]: First value exceeds data in uploaded isox files.",type="error") #give a bit leeway
    if (input$timerange[2] > max(values$data$time.min) +0.01) showNotification("Input Time [min]: Second value exceeds data in uploaded isox files.",type="error") #give a bit leeway

    if ((input$timerange[2] - input$timerange[1]) < 0.5) showNotification("Input Time [min]: Time range should be > 0.5 minute",type="error")


    req(input$timerange[2] > input$timerange[1])
    req(input$timerange[1] >= 0)
    req(input$timerange[2] > 0)
    req(input$timerange[2] <= max(values$data$time.min)+0.01) #give a bit leeway
    req(input$timerange[1] <= max(values$data$time.min)+0.01) #give a bit leeway; ~ redundant
    req((input$timerange[2] - input$timerange[1]) > 0.5)



    #Use shinyvalidate
    #Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$enable()


    #Required inputs
    iv$add_rule("filename.sel", shinyvalidate::sv_required())
    iv$add_rule("base_peak", shinyvalidate::sv_required())
    iv$add_rule("timerange", shinyvalidate::sv_required())
    #timerange input requires special attention
    iv$add_rule("rare", shinyvalidate::sv_required())
    iv$add_rule("extreme", shinyvalidate::sv_required())
    iv$add_rule("ratio.method", shinyvalidate::sv_required())

    #Input values
    iv$add_rule("rare", shinyvalidate::sv_between(0, 90))
    iv$add_rule("extreme", shinyvalidate::sv_between(0, 10))

    # FIXME: Bug if I delete rare or extreme input after clicking plot button

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



    shinyjs::show("main-plot")

    # Message with processing parameters

    module_message(
      ns, "info",
      "Processing data with selected parameters:",
      "\n - files selected: ", paste(input$filename.sel %then% "", collapse = ", "),
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - timerange: ", paste(input$timerange %then% "", collapse = " to "),
      "\n - rare: ", input$rare %then% "",
      "\n - extreme: ", input$extreme %then% "", #)#edit
      "\n - ratio method: ", input$ratio.method %then% "" #)#edit
    )

##### Data: Flow Analysis ---------------------------------------------------


    inputs.flow <-


      #req(input$INPUT_Flow_Basepeak) #temp

      data.frame(
        parameter = c(
          "IsoXL Version",
          "INPUT_Flow_Filename",
          "INPUT_Flow_Basepeak",
          "INPUT_Flow_timerange[1]",
          "INPUT_Flow_timerange[2]" ,
          "INPUT_Flow_Rare",
          "INPUT_Flow_TICxIT",
          "INPUT_Flow_RatioMethod"
        ),
        #,"INPUT_Flow_IONS"),


        value = c(
          isoXL.version,
          #paste(input$filename.sel, sep="", collapse=""), #FIX
          input$filename.sel[1],
          input$base_peak,
          input$timerange[1],
          input$timerange[2],
          input$rare,
          input$extreme,
          input$ratio.method
        )
        #   ,input$INPUT_Flow_IONS)

      )


    #     validate(
    #
    #
    #       ####  need(length(levels(data.flow()$filename)) <2, paste("Warning: There is more than one 'filename' in data. May want to use Flow Injection tab? These filenames have been observed",levels(data.flow()$filename ))),
    #       need(input$INPUT_Flow_timerange[2] <= max(data.flow()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.flow()$time.min) , " min.") ),
    #       need(input$INPUT_Flow_timerange[2] > input$INPUT_Flow_timerange[1], paste("Warning: Time selection minimun is greater than maximum") )
    #
    #     )


    data.flow_filtered <-   values$data %>%
      filter(filename %in% input$filename.sel) %>% #include only selected filenames (new UI feature 05-July-2022)
      na.omit() %>%

      mutate(Sample.Name = input$sample) %>% #temp: implemented as factor?

      remove.ticxit(input = input) %>%
      remove.satellitePeaks() %>%
      remove.rare(input = input) %>%
      #
      filter(time.min>  input$timerange[1], time.min <  input$timerange[2])

      # before IsoXL I had a more sophisticated procedure to find the sample vs solvent. Implement this going forward?




    # Table output: Flow Analysis show rows of isox file------------------------------------------------


    output$mytable.flow = DT::renderDataTable({

      datatable(
        data.flow_filtered %>% ungroup() %>% mutate_if(is.double, round, digits = 2),
        rownames = FALSE ,
        extensions = c('Buttons', 'ColReorder'),
        options = list(
          scrollX = TRUE,
          #scroll bar to see entire table
          dom = 'Bfrtip',
          buttons = c('colvis', 'copy',  'excel', 'pdf'),
          colReorder = TRUE #columns can be reordered
        )
      )

    })

    # Flow Analysis: Assign base peak --------------------------------------------


    data.flow_basepeak <- assign.basepeak.single(df = data.flow_filtered, INPUT_Basepeak = input$base_peak) #confirm that grouping is correct; Move function to common core?

    #needs validation; e.g. is base peak present in data?


    # Flow Analysis: Calculate output table for results file ---------------



    data.flow_output <- calc.output.flow(df = data.flow_basepeak, INPUT_RatioMethod = input$ratio.method)


    # Table output: Flow Analysis, summary statistics ----------------------------


    output$summary.flow <- DT::renderDataTable({

      datatable( data.flow_output, rownames= FALSE ,
                 extensions = c('Buttons','ColReorder'),
                 options = list(
                   # autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scrollbar when table is not visible
                   # columnDefs = list(list(
                   #   targets = c(6L, 10L), visible = FALSE #hide some columns so the table is not as wide, can be activated using column button; remain in output
                   # )),
                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE #columns can be reordered
                 )
      )

    })

# Plot output: Flow Analysis--------------------------------------------------------

      output$plot <- renderPlotly({

        # isolate to avoid triggers on other input parameter selections
        isolate({

        plotly::ggplotly(

          ggplot(data = data.flow_output, aes(x = filename, y = Ratio, color = isotopolog)) + scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

            geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
                              ymax=(Ratio+Ratio.SEM)),
                          width=0.7,
                          size=0.3,
                          color="black",
                          position=position_dodge(.9))       +
            geom_point(size=3, shape=10) ,

          dynamicTicks = TRUE #change ticks on zoom
        ) %>% plotly::config(

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
              filename = paste("flow_injection",
                               tools::file_path_sans_ext(input$upload$name),
                              sep="_")
                              )
          )
        })

      })

# Flow analysis: Make Excel file ---------------------------------------------

data_flow.xls <- make.xls(df.stat = data.flow_output, params = inputs.flow) #FIX


# Download output: Flow Analysis Excel file ----------------------------------


output$download_xlsx <- downloadHandler(
  filename = function(){paste(tools::file_path_sans_ext(input$upload$name), "_IsoXL.xlsx", sep="")},
  content = function(file) {
    saveWorkbook(data_flow.xls, file)
  }
)

    shinyjs::show("main-rest")

  })

}


# user interface ----

module_isoxl_flow_ui <- function(id) {


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

      tags$style(paste0("#",
                        ns("upload_info"),
                        "{
               font-size:12px;
               color:navy;
         }")),

      textOutput(ns("upload_info")),


      # parameters ----
      div(
        id = ns("parameters-panel"),

        hr(),
        actionButton(ns("start"), label = "Start", icon = icon("rotate")),

        tags$hr(style = "border-color: grey;"),

        h6("Define Parameters"),

        selectInput(
          inputId = ns("filename.sel"),
          label = "File",
          c(),
          multiple = TRUE
        ),

        selectInput(inputId = ns("base_peak"), label = "Base peak", c()),

        shinyWidgets::numericRangeInput(ns("timerange"), "Time [min]", value = c(), min=0),
        tags$hr(style = "border-color: grey;"),

        h6("Processing options:"),

        #Use shinyBS::bsTooltip instead of tippy?
        tippy(
          numericInput(
            ns("rare"),
            "Remove weak signals",
            value = c(),
            width = NULL
          ),
          "Processing option: Isotopocule must have been detected > x % of scans",
          placement = "right"
        ),
        tippy(
          numericInput(
            ns("extreme"),
            "Remove extreme scans",
            value = c(),
            width = NULL
          ),
          "Processing option: Truncate data based on TICxIT",
          placement = "right"
        ),

        radioButtons(
          inputId = ns("ratio.method"),
          label = "Ratio method",
          choiceNames = c("sum",  "mean", "median", "geometric"),
          choiceValues = c("sum",  "mean", "median", "geometric.mean"),
          selected = "sum"
        )
      ) %>% shinyjs::hidden()

    ),

    # UI: Flow Analysis Main Panel ------------------------------------------

  mainPanel(


    width = 9,

    # parameters ----
    div(
      id = ns("main-plot"),

      box(
        title = "Overview of Results",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        withSpinner(plotlyOutput(ns("plot")))
      )
    ), #how to save png at higher resolution?

    hr(),
    div(
      id = ns("main-rest"),
      downloadButton(ns("download_xlsx"), "Download Results (.xlsx)"),

      hr(),


      box(
        title = "Results Table",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("summary.flow"))
      ),

      hr(),


      box(
        title = "Filtered .isox File",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("mytable.flow"))
      )
    )
  )

  ))
}
