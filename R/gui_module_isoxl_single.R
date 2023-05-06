# test ----
# test the single analysis module (direct infusion) as a standalone app
#'@title test_module_isoxl_single
#'@description test the single analysis module (direct infusion) as a standalone app
#'@param port Device port being used
#'@return ...
#'@export
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
#' @title module_isoxl_single_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
module_isoxl_single_server <- function(input, output, session) {


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

  # use test data ----
  observeEvent(input$test_data, {

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")

    # reset the reactive values that hold the file and data
    values$upload_file <- NULL
    values$upload_error <- NULL
    values$data <- NULL

    #reset the outputs
    # output$mytable.single <- NULL
    # output$summary.single <- NULL
    # output$plot <- NULL

    if (input$test_data == TRUE) {

      shinyjs::hide("main-plot")
      shinyjs::hide("main-rest")

      values$data <-isoorbi::orbi_read_isox(file = system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbiGUI")) # FIXME: use files from isoorbi?

      # Download test data: .isox file ----------------------------------

      output$download_test_data <- downloadHandler(
        filename = function() {
          "testfile_DualInlet_small.isox" #FIXME: provide name as a variable
        },
        content = function(file) {
          utils::write.table(values$data, file, quote=FALSE, sep="\t", row.names = F)
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


        values$data <-isoorbi::orbi_read_isox(file = input$upload$datapath),
        warning = function(w)
          values$upload_error <- w$message,
        error = function(e)
          values$upload_error <- e$message

      )
    } else {
      # try to read the test file
      tryCatch(

        values$data <-isoorbi::orbi_read_isox(file = system.file("extdata", "testfile_DualInlet_small.isox", package="isoorbiGUI")) # FIXME: use files from isoorbi?

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

      updateSelectInput(inputId = "filename.sel", choices = c())
      updateSelectInput(inputId = "compound", choices = c())
      updateTextInput(inputId = "sample", value = c())
      updateSelectInput(inputId = "base_peak", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange", value = c())

      updateNumericInput(inputId = "segments", value = c())
      updateNumericInput(inputId = "rare", value = c())
      updateNumericInput(inputId = "extreme", value = c())
      updateRadioButtons(inputId = "ratio.method", choices =c())



    } else {
      # yes data --> update parameters and show
      updateSelectInput(
        inputId = "filename.sel",
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
        choices = levels(values$data$isotopocule),
        selected = values$data$isotopocule[1]
      )

      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange",
                                            value = c(min(values$data$time.min), max(values$data$time.min))
                                            # Fix: How to avoid negative numbers?
      # https://stackoverflow.com/questions/42164171/protect-shiny-numericinput-from-negative-numbers
      )

      # FIXME: add segments,

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
        choiceValues = c("sum",  "mean", "median", "geometric_mean"),
        selected = "sum"
      )

      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)


  isoXL.version <- as.character(utils::packageVersion("isoorbiGUI"))



  # Table of processing parameters

  observeEvent(input$start, {

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")


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

    if (input$timerange[1] > max(values$data$time.min) +0.05) showNotification("Input Time [min]: First value exceeds data in uploaded isox files.",
                                                                               type="error",
                                                                               duration = 20) #give a bit leeway
    if (input$timerange[2] > max(values$data$time.min) +0.05) showNotification("Input Time [min]: Second value exceeds data in uploaded isox files.",
                                                                               type="warning",
                                                                               duration = 20) #give a bit leeway

    if ((input$timerange[2] - input$timerange[1]) < 0.5) showNotification("Input Time [min]: Time range should be > 0.5 minute",
                                                                          type="error",
                                                                          duration = 20)


    req(input$timerange[2] > input$timerange[1])
    req(input$timerange[1] >= 0)
    req(input$timerange[2] > 0)
   # req(input$timerange[2] <= max(values$data$time.min)+0.05) #give a bit leeway
    req(input$timerange[1] <= max(values$data$time.min)+0.05) #give a bit leeway; ~ redundant
    req((input$timerange[2] - input$timerange[1]) > 0.5)



    #FIME: learn how to correctly implement shinyvalidate
    #Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$enable()


    #Required inputs
    iv$add_rule("filename.sel", shinyvalidate::sv_required())
    iv$add_rule("compound", shinyvalidate::sv_required())
    iv$add_rule("sample", shinyvalidate::sv_required())
    iv$add_rule("timerange", shinyvalidate::sv_required())
    #timerange input requires special attention
    iv$add_rule("rare", shinyvalidate::sv_required())

    iv$add_rule("segments", shinyvalidate::sv_required())
    iv$add_rule("extreme", shinyvalidate::sv_required())
    iv$add_rule("ratio.method", shinyvalidate::sv_required())
    #
    # #Input values
    iv$add_rule("rare", shinyvalidate::sv_between(0, 90))
    iv$add_rule("extreme", shinyvalidate::sv_between(0, 10))
    iv$add_rule("segments", shinyvalidate::sv_between(1,5))

    # FIXME: Bug if I delete rare or extreme input after clicking plot button (does not update)

    if (iv$is_valid()) {

      showNotification(
        "Inputs look ok",
        type = "warning",
        duration = 5,
      )
      # use inputs...
    } else {
      showNotification(
        "Check for invalid input",
        type = "error",
        duration = 20,
      )
    }

    #req(iv$is_valid())

    req(iv$is_valid()) #DON'T PROCEED IF INPUTS ARE NOT VALID


    #RESET main plot
    output$plot <- renderPlotly({
      # isolate to avoid triggers on other input parameter selections

      isolate({
        plotly::ggplotly(
        )
      })
    })

    shinyjs::show("main-plot")


    # Message with processing parameters

    module_message(
      ns, "info",
      "Processing data with selected parameters:",
      "\n - files: ", paste(input$filename.sel %then% "", collapse = ", "),
      "\n - compounds: ", paste(input$compound %then% "", collapse = ", "),
      "\n - sample: ", paste(input$sample %then% ""),
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - timerange: ", paste(input$timerange %then% "", collapse = " to "),
      "\n - segments: ",
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
        "Segments",
        "INPUT_Single_Rare",
        "INPUT_Single_TICxIT",
        "INPUT_Single_RatioMethod"),


        value = c(isoXL.version,
                  input$sample,
                  input$base_peak,
                  input$timerange[1],
                  input$timerange[2],
                  input$segments,
                  input$rare,
                  input$extreme,
                  input$ratio.method)

      )

    data.single_filtered <-   values$data %>%
                                          dplyr::filter(.data$filename %in% input$filename.sel) %>% #include only selected files
                                          stats::na.omit() %>%
                                          dplyr::mutate(sample_name = input$sample) %>%
                                          dplyr::filter(.data$time.min>  input$timerange[1], .data$time.min <  input$timerange[2]) %>%
                                          isoorbi::orbi_filter_satellite_peaks() %>%
                                          isoorbi::orbi_filter_weak_isotopocules(min_percent = input$rare) %>%
                                          isoorbi::orbi_filter_scan_intensity(outlier_percent = input$extreme)  %>%
                                          group_by(.data$filename) %>% #TEMP: 06-May-2023
                                          mutate(Segments = ggplot2::cut_interval(.data$scan.no,
                                                                                  n = input$segments,
                                                                                  labels = FALSE)) %>%
                                          mutate(filename = paste(.data$filename, .data$Segments))
    # %>%
    #                                       dplyr::mutate(Segment = ceiling(
    #                                           (scan.no -min(scan.no)+1) / ((max(scan.no) - min(scan.no)+1)/input$segments ))
    #                                           ) # FIXME: fix this!

    # FIXME: segments do not work
    #SEGMENTATION
    # if (input$segments > 1) {
    #
    #   data.single_filtered <- data.single_filtered   %>%
    #     dplyr::mutate(filename=as.factor(paste(filename, segment, sep="_")))
    #
    # }

# Table output: Single Analysis show rows of isox file------------------------------------------------


    output$mytable.single = DT::renderDataTable({

      datatable(    data.single_filtered %>% ungroup() %>% mutate_if(is.numeric, round, digits=4) %>%
                      group_by(.data$filename, .data$Segments), rownames= FALSE,
                 extensions = c('Buttons','ColReorder'),

                 options = list(
                   scrollX = TRUE, #scroll bar to see enture table
                   dom = 'Bfrtip',
                   #buttons = c('colvis','copy',  'excel', 'pdf' ),

                   buttons =
                     list('colvis', 'copy',   list(
                       extend = 'excel',
                       filename = 'IsoXL_Single_isox'
                     ), list(
                       extend = 'pdf',
                       pageSize = 'A4',
                       orientation = 'landscape',
                       filename = 'IsoXL_Single_isox'
                     )),

                   colReorder = TRUE #columns can be reordered
                 )
      )

    })

# Single Analysis: Assign base peak --------------------------------------------


      data.single_basepeak <-isoorbi::orbi_define_basepeak(dataset = data.single_filtered, basepeak_def = input$base_peak)

      #needs validation; e.g. is base peak present in data?


      # Single Analysis: Calculate output table for results file ---------------



    data.output <- isoorbi::orbi_summarize_results(dataset = data.single_basepeak, ratio_method = input$ratio.method)

  message(colnames(data.output))

    data.output.renamed <- data.output  %>% dplyr::rename(
       Filename = "filename",
       Sample = "sample_name",
       BasePeak = "basepeak",
       Isotopocule = "isotopocule",
      Compound = "compound",
        SEM = "ratio_sem",
      `SEM (permil)`= "ratio_relative_sem_permil",
       `Shot Noise (permil)` = "shot_noise_permil",
       `Minutes to 1E6 ions` = "minutes_to_1e6_ions",
        Scans  = "number_of_scans",
        Ratio = "ratio"

    ) %>%
      select(
        "Filename",
        "Sample",
        "Compound",
        "Isotopocule",
        "Ratio",
        "SEM",
        "SEM (permil)",
        "Shot Noise (permil)",
        "Minutes to 1E6 ions",
        "Scans"
      )



    #FIXME: with select() column reordering did not work for some reason...


      # Table output: Single Analysis, summary statistics ----------------------------


      output$summary.single <- DT::renderDataTable({

        datatable( data.output.renamed, rownames= FALSE,
                   extensions = c('Buttons','ColReorder'),
                   options = list(
                     # autoWidth = TRUE, #scroll bar to see enture table
                     scrollX = TRUE, #scrollbar when table is not visible
                     columnDefs = list(list(
                       targets = c(6L, 9L), visible = FALSE #hide some columns so the table is not as wide, can be activated using column button; remain in output
                     )),
                     dom = 'Bfrtip',
                     #buttons = c('colvis','copy',  'excel', 'pdf' ),
                     buttons =
                       list('colvis', 'copy',  list(
                         extend = 'excel',
                         filename = 'IsoXL_Single_results'
                       ), list(
                         extend = 'pdf',
                         pageSize = 'A4',
                         orientation = 'landscape',
                         filename = 'IsoXL_Single_results'
                       )),
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
    #                          input$isotopocule,
    #                          input$base_peak,
    #                          sep = "_")
    #       )
    #     )
    # })



      output$plot.single <- renderPlotly({

        # isolate to avoid triggers on other input parameter selections
        isolate({


        plotly::ggplotly(

          ggplot2::ggplot(data = data.output, ggplot2::aes(x = .data$shot_noise_permil,
                                                           y = .data$ratio_relative_sem_permil,
                                                           color = .data$isotopocule)) +

            ggplot2::scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

            # geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
            #                   ymax=(Ratio+Ratio.SEM)),
            #               width=0.7,
            #               size=0.3,
            #               color="black",
            #               position=position_dodge(.9))       +
            ggplot2::geom_point(size=3, shape=10) +
            ggplot2::geom_abline(slope=1) +
            ggplot2::facet_grid(filename~.) + #FIXME: add segments
            ggplot2::theme(legend.position="right") +
            ggplot2::ylab(paste("Relative SEM ", "[\u2030]")) + ggplot2::xlab(paste("Shot Noise Limit", "[\u2030]")),

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
                filename = if (input$test_data == TRUE) {paste("testfile_Single", sep="")} #FIXME: use a variable for testfile name!
                else{
                  paste("single_analysis",
                        tools::file_path_sans_ext(input$upload$name),
                        sep="_")}


              )
            )

      })
   })


      # Single analysis: Make Excel file ---------------------------------------------

      data_single.xls <- make.xls.onetab(df.stat = data.output, params = inputs.single)


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
        saveWorkbook(data_single.xls, file)
      }
    )


    shinyjs::show("main-rest")

    })



}


# user interface ----
#' @title module_isoxl_single_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_single_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      # Use test data instead of uploading data (or any data that has been already uploaded)

      tippy(
        checkboxInput(ns("test_data"), "Use test data", value = FALSE),

        tooltip = "<span style='font-size:15px;'>For details see Info<span>", allowHTML = TRUE,
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

        actionButton(ns("start"), label = "Start", icon = icon("rotate")),

        tags$hr(style = "border-color: grey;"),

        h5("Define Direct Infusion:"),
        selectInput(
          inputId = ns("filename.sel"),
          label = "File",
          c(),
          multiple = TRUE
        ),


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


        shinyWidgets::numericRangeInput(ns("timerange"), "Time [min]", value = c(), min=0, step=0.1),
        tags$hr(style = "border-color: grey;"),


        h5("Processing options:"),

        #currently not used in data processing!
        tippy(
          numericInput(
            inputId = ns("segments"),
            label = "Segments",
            1,
            min = 1,
            max = 5,
            step = 1,
            width = NULL
          ),
          tooltip = "<span style='font-size:15px;'>Split each sample run into segments<span>", allowHTML = TRUE,
          # "Split each block into segments",
          placement = "right"
        ),

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
          placement = "right"),
 # FIXME: add mouseover explanations for different ratio methods
        radioButtons(
          inputId = ns("ratio.method"),
          label = "Ratio method",
          choiceNames = c("sum",  "mean", "median", "geometric"),
          choiceValues = c("sum",  "mean", "median", "geometric_mean"),
          selected = "sum"
        )
      ) %>% shinyjs::hidden()

    ),

    # UI: Single Analysis Main Panel ------------------------------------------

    mainPanel(

      width = 9,


      #hr(),
      # parameters ----
      div(
        id = ns("main-plot"),

        downloadButton(ns("download_xlsx"), "Download Results (.xlsx)"),

        div(style="margin-bottom:10px"),


      box(
        title = "Overview of Results",
        width = "auto" ,
      #  height = "auto",
        status = "primary",
        solidHeader = T,
        withSpinner(plotlyOutput(ns("plot.single")))
      )
      )%>% shinyjs::hidden(),
      #how to save png at higher resolution?

      hr(),

      div(
        id = ns("main-rest"),



      box(
        title = "Summary of Results",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("summary.single"))
      ),

      hr(),


      box(
        title = "Filtered Data",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("mytable.single"))
      )

) %>% shinyjs::hidden()

    )

    # # plot -----
    # mainPanel(
    #   box(
    #     title = "Direct Infusion Plot",
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
