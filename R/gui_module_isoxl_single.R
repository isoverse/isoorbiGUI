# test ----
# test the single analysis module (direct infusion) as a standalone app

test_module_isoxl_single <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Single Analysis Module Test"),
        module_isoxl_single_ui(id = "single")
      ),
      server = function(input, output) {
        shot_noise <-
          callModule(module_isoxl_single_server, id = "single")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_single_server <- function(input, output, session, isoXL.version) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot.single = NULL
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
      updateSelectInput(inputId = "filename", choices = c())
      updateSelectInput(inputId = "compound", choices = c())
      updateTextInput(inputId = "sample", value = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())
      updateNumericInput(inputId = "rare", value = c())
      updateNumericInput(inputId = "extreme", value = c())
      updateRadioButtons(inputId = "ratio.method", choices =c())



    } else {
      # yes data --> update parameters and show
      updateSelectInput(
        inputId = "filename",
        choices = c(levels(values$data$filename)),
        selected = values$data$filename[1]
      )
      updateSelectInput(
        inputId = "compound",
        choices = c(levels(values$data$compound)),
        selected = values$data$compound[1]
      )

      updateTextInput(
        inputId = "sample",
      #  choices = c(levels(values$data$compound)),
        value = values$data$sample[1]
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


  isoXL.version <- utils::packageVersion("isoorbi") #FIXME: get version information directly?



  # Table of processing parameters

  observeEvent(input$start, {

    shinyjs::show("main-plot")


    # Message with processing parameters

    module_message(
      ns, "info",
      "Processing data with selected parameters:",
      "\n - files: ", paste(input$filename %then% "", collapse = ", "),
      "\n - compounds: ", paste(input$compound %then% "", collapse = ", "),
      "\n - sample: ", paste(input$sample %then% ""),
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - timerange: ", paste(input$timerange %then% "", collapse = " to "),
      "\n - rare: ", input$rare %then% "",
      "\n - extreme: ", input$extreme %then% "", #)#edit
      "\n - ratio.method: ", input$ratio.method %then% "" #)#edit
    )


    # Data: Single Analysis ---------------------------------------------------

    inputs.single <- data.frame( parameter = c(

        "IsoXL Version",
        "INPUT_Single_sample",
        "INPUT_Single_Basepeak",
        "INPUT_Single_timerange[1]",
        "INPUT_Single_timerange[2]" ,
        "INPUT_Single_Rare",
        "INPUT_Single_TICxIT",
        "INPUT_Single_RatioMethod"),


        value = c(isoXL.version,
                  input$sample,
                  input$base_peak,
                  input$timerange[1],
                  input$timerange[2],
                  input$rare,
                  input$extreme,
                  input$ratio.method)

      )

    data.single_filtered <-   values$data %>%
                                          na.omit() %>%

                                          mutate(Sample.Name = input$sample) %>% #temp: implemented as factor?

                                           remove.ticxit(input = input) %>%
                                           remove.satellitePeaks() %>%
                                           remove.rare(input = input) %>%
                                          #
                                           filter(time.min>  input$timerange[1], time.min <  input$timerange[2])



# Table output: Single Analysis show rows of isox file------------------------------------------------


    output$mytable.single = DT::renderDataTable({

      datatable(    data.single_filtered %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder', 'Select'),

                 options = list(
                   #   autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scroll bar to see enture table

                   #   columnDefs = list(list(
                   #
                   #        targets = c(2L), visible = FALSE #hide some columns
                   # )),

                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE#, #columns can be reordered,
                   # selection = list(target = "column")#,
                   # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
                 )
      )


    })

# Single Analysis: Assign base peak --------------------------------------------


      data.single_basepeak <- assign.basepeak.single(df = data.single_filtered, INPUT_Basepeak = input$base_peak)

      #needs validation; e.g. is base peak present in data?


      # Single Analysis: Calculate output table for results file ---------------



      data.single_output <- calc.output.single(df = data.single_basepeak, INPUT_RatioMethod = input$ratio.method)


      # Table output: Single Analysis, summary statistics ----------------------------


      output$summary.single <- DT::renderDataTable({

        datatable( data.single_output, rownames= FALSE ,
                   extensions = c('Buttons','ColReorder'),
                   options = list(
                     # autoWidth = TRUE, #scroll bar to see enture table
                     scrollX = TRUE, #scrollbar when table is not visible
                     columnDefs = list(list(
                       targets = c(6L, 10L), visible = FALSE #hide some columns so the table is not as wide, can be activated using column button; remain in output
                     )),
                     dom = 'Bfrtip',
                     buttons = c('colvis','copy',  'excel', 'pdf' ),
                     colReorder = TRUE #columns can be reordered
                   )
        )

      })

    #   # Plot output: Single Analysis--------------------------------------------------------
    #
    #

    # isolate to avoid triggers on other input parameter selections
    # isolate({
    #   plotly::ggplotly(values$plot) %>%
    #     plotly::config(
    #       displaylogo = FALSE,
    #       modeBarButtonsToRemove = c(
    #         "zoomIn2d",
    #         "zoomOut2d",
    #         "lasso2d",
    #         "select2d",
    #         "hoverClosestCartesian",
    #         "hoverCompareCartesian"
    #       ),
    #       #options on how to save an image
    #       toImageButtonOptions = list(
    #         format = "png",
    #         width = 900,
    #         height = 600,
    #         scale = 4,
    #         filename = paste("shotnoise",
    #                          input$isotopolog,
    #                          input$base_peak,
    #                          sep = "_")
    #       )
    #     )
    # })



      output$plot.single <- renderPlotly({

        # isolate to avoid triggers on other input parameter selections
        isolate({


        plotly::ggplotly(

          ggplot2::ggplot(data = data.single_output, ggplot2::aes(x = Shot.Noise.permil, y = relSE.permil, color=isotopolog)) +

            ggplot2::scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

            # geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
            #                   ymax=(Ratio+Ratio.SEM)),
            #               width=0.7,
            #               size=0.3,
            #               color="black",
            #               position=position_dodge(.9))       +
            ggplot2::geom_point(size=3, shape=10) +
            ggplot2::geom_abline(slope=1) +
            ggplot2::ylab(paste("relative SEM ", "[\u2030]")) + ggplot2::xlab(paste("shot noise limit", "[\u2030]")),

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
                filename = paste("single_analysis",
                                 tools::file_path_sans_ext(ns(input$upload$name)),
                                 sep = "_")
              )
            )

      })
   })


      # Single analysis: Make Excel file ---------------------------------------------

      data_single.xls <- make.xls.onetab(df.stat = data.single_output, params = inputs.single)


    # Download output: Single Analysis Excel file ----------------------------------


    output$download_xlsx <- downloadHandler(
      filename = function(){  paste(tools::file_path_sans_ext(input$upload$name), "_IsoXL.xlsx", sep="")},
      content = function(file) {
        saveWorkbook(data_single.xls, file)
      }
    )


    shinyjs::show("main-rest")

    })



}


# user interface ----

module_isoxl_single_ui <- function(id) {

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

        tags$hr(style = "border-color: grey;"),

        h6("Define Single Analysis"),

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
          label = "Compounds",
          c(),
          multiple = FALSE),


        textInput(
          inputId = ns("sample"),
          label = "Sample",
          value = "sample"
        ),

        selectInput(inputId = ns("base_peak"), label = "Base peak", c()),


        shinyWidgets::numericRangeInput(ns("timerange"), "Time [min]", value = c()),
        tags$hr(style = "border-color: grey;"),


        h6("Processing options:"),

        #currently not used in data processing!


        #Use shinyBS::bsTooltip instead of tippy?
        tippy(
          numericInput(ns("rare"), "Remove weak signals", value = c(), width=NULL),
          "Processing option: Isotopocule must have been detected > x % of scans", placement = "right"),
        tippy(
          numericInput(ns("extreme"), "Remove extreme scans", value = c(), width=NULL),
          "Processing option: Truncate data based on TICxIT", placement = "right"),

        radioButtons(
          inputId = ns("ratio.method"),
          label = "Ratio method",
          choiceNames = c("sum",  "mean", "median", "geometric"),
          choiceValues = c("sum",  "mean", "median", "geometric.mean"),
          selected = "sum"
        ),

        hr(),
        actionButton(ns("start"), label = "Start", icon = icon("sync"))
      ) %>% shinyjs::hidden()

    ),

    # UI: Single Analysis Main Panel ------------------------------------------

    mainPanel(

      width = 9,


      #hr(),
      # parameters ----
      div(
        id = ns("main-plot"),

      box(
        title = "Overview of Results",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        withSpinner(plotlyOutput(ns("plot.single")))
      )
      ),
      #how to save png at higher resolution?

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
        DT::dataTableOutput(ns("summary.single"))
      ),

      hr(),


      box(
        title = "Filtered .isox File",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("mytable.single"))
      )

)

    )

    # # plot -----
    # mainPanel(
    #   box(
    #     title = "Single Analysis Plot",
    #     width = "auto" ,
    #     height = "auto",
    #     status = "primary",
    #     solidHeader = T,
    #     withSpinner(plotlyOutput(ns("plot")))
    #   ),
    #   #how to save png at higher resolution?
    #
    #
    # )
  ))
}
