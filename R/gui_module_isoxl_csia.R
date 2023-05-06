# test ----
# test the CSIA injection analysis module as a standalone app
# test the CSIA module as a standalone app
#' @title test_module_isoxl_csia
#' @description ...
#' @param port ...
#' @return ...
#' @export
test_module_isoxl_csia <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("CSIA Module Test"),
        module_isoxl_csia_ui(id = "csia")
      ),
      server = function(input, output) {
        csia <-
          callModule(module_isoxl_csia_server, id = "csia")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----
#' @title module_isoxl_csia_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
module_isoxl_csia_server <- function(input, output, session) {

# namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot.overview = NULL,
    plot = NULL #revise
  )

  observeEvent(req(nrow(values$data) != 0), {

    # Plot output: CSIA Analysis--------------------------------------------------------

    output$plot.overview <- renderPlotly({


      # isolate to avoid triggers on other input parameter selections
      isolate({
        plotly::ggplotly(
          ggplot(
            data = values$data,
            aes(x = .data$time.min, y = .data$ions.incremental, color = .data$filename)
          ) +

            scale_color_brewer(palette = "Dark2") + #temp: could be segments... for x axis

            xlab("Time [min]") +
            ylab("Ions") +

            facet_grid(.data$isotopocule ~ .data$compound, scales = "free_y") +

            #strip.text.x = element_blank(),
            theme(
              axis.text.x = element_text(
                angle = 30,
                vjust = 0.5,
                hjust = 1
              ),
              legend.position = 'bottom'
            ) +
            geom_point(size = 0.1),


          dynamicTicks = TRUE #change ticks on zoom
        )  %>% layout(legend = list(orientation = "h", y = -0.2))  %>% plotly::config(
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

            #FIXME: use a variable for testfile name!
            filename = if (input$test_data == TRUE) {
              paste("testfile_csia", sep = "")
            }

            else{
              paste("csia_injection",
                    tools::file_path_sans_ext(input$upload$name),
                    sep = "_")
            }
          )
        )
      })

    })

    shinyjs::show("overview-plot")

  })


  # use test data ----
  observeEvent(input$test_data, {

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")
    shinyjs::hide("overview-plot")

    # reset the reactive values that hold the file and data
    values$upload_file <- NULL
    values$upload_error <- NULL
    values$data <- NULL

    #reset the outputs
    output$mytable.csia <- NULL
    output$summary.csia <- NULL
    output$plot.overview <- NULL
    output$plot <- NULL

    if (input$test_data == TRUE) {



      values$data <-isoorbi::orbi_read_isox(file = system.file("extdata", "testfile_csia.isox", package="isoorbiGUI"))





      # Download test data: .isox file ----------------------------------

      output$download_test_data <- downloadHandler(
        filename = function() {
          "testfile_csia.isox" #FIXME: provide name as a variable
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

        values$data <-isoorbi::orbi_read_isox(file = system.file("extdata", "testfile_csia.isox", package="isoorbiGUI"))

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
  observeEvent( values$data, {

    module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorrect input file --> hide)
      shinyjs::hide("parameters-panel")
      shinyjs::hide("main-plot")
      shinyjs::hide("main-rest")
      updateSelectInput(inputId = "filename.sel", choices = c())
      updateSelectInput(inputId = "base_peak", choices = c())


      updateSelectInput(inputId = "compound.sel1", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange1", value = c())

      updateSelectInput(inputId = "compound.sel2", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange2", value = c())

      updateSelectInput(inputId = "compound.sel3", choices = c())
      shinyWidgets::updateNumericRangeInput(session, inputId = "timerange3", value = c())


      updateNumericInput(inputId = "segments", value = c())
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
        choices = levels(values$data$isotopocule),
        selected = values$data$isotopocule[1]
      )

      #Compound 1

      updateSelectInput(
        inputId = "compound.sel1",
        choices = levels(values$data$compound),
        selected = levels(values$data$compound)[1]
      )

      #calculate timerange1
      peakTime <- values$data %>% filter(.data$compound == values$data$compound[1]) %>% #FIXME
                                  filter(.data$ions.incremental > 0.1* max(.data$ions.incremental))
      minTime <- min(peakTime$time.min)
      maxTime <- max(peakTime$time.min)

      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange1",
                                            value = c(minTime, maxTime)
      )

      #Compound 2

      comp2 <- ifelse( length(levels(values$data$compound)) > 1, levels(values$data$compound)[2], levels(values$data$compound)[1] )

      updateSelectInput(
        inputId = "compound.sel2",
        choices = levels(values$data$compound), #drop first compound [-1]
        selected = comp2
      )

      #calculate timerange2
      peakTime <- values$data %>% filter(.data$compound == comp2) %>% #FIXME
        filter(.data$ions.incremental > 0.1* max(.data$ions.incremental))
      minTime <- 0
      maxTime <- 0
      minTime <- min(peakTime$time.min)
      maxTime <- max(peakTime$time.min)

      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange2",
                                            value = c(minTime, maxTime)
      )

      #Compound 3

      comp3 <- ifelse( length(levels(values$data$compound)) > 2, levels(values$data$compound)[3], levels(values$data$compound)[1] )

      updateSelectInput(
        inputId = "compound.sel3",
        choices = levels(values$data$compound), #drop first compound [-1] [-2]
        selected = comp3
      )

      #calculate timerange3
      peakTime <- values$data %>% filter(.data$compound == comp3) %>% #FIXME
        filter(.data$ions.incremental > 0.1* max(.data$ions.incremental))
      minTime <- 0
      maxTime <- 0
      minTime <- min(peakTime$time.min)
      maxTime <- max(peakTime$time.min)

      shinyWidgets::updateNumericRangeInput(session,
                                            inputId = "timerange3",
                                            value = c(minTime, maxTime)
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


  isoXL.version <- as.character(utils::packageVersion("isoorbiGUI")) #get version information directly



  # Table of processing parameters

  observeEvent(input$start, {

    #FIXME
    module_message(ns, "info", "start has been pressed")

    shinyjs::hide("main-plot")
    shinyjs::hide("main-rest")

    # special safety checks for shinywidget that does not work with shinyvalidate
    if (input$timerange1[2] <= input$timerange1[1]) showNotification("Input Time [min]: First value has to be smaller than second value",
                                                                   type="error",
                                                                   duration=20)
    if (input$timerange1[1] < 0) showNotification("Input Time [min]: Check first input value",
                                                 type="error",
                                                 duration=20)
    if (input$timerange1[2] <= 0) showNotification("Input Time [min]: Check second input value",
                                                  type="error",
                                                  duration=20)

    if (input$timerange1[1] > max(values$data$time.min) +0.05) showNotification("Input Time [min]: First value exceeds data in uploaded isox files.",
                                                                               type="error",
                                                                               duration=20) #give a bit leeway
    if (input$timerange1[2] > (max(values$data$time.min) +0.05)) showNotification("Input Time [min]: Second value exceeds data in uploaded isox files.",
                                                                               type="warning",
                                                                               duration=20) #give a bit leeway

    if ((input$timerange1[2] - input$timerange1[1]) < 0.1) showNotification("Input Time [min]: Time range should be > 0.1 minute",
                                                                          type="error",
                                                                  duration=20)

    #FIXME: Improve this req()
    module_message(ns, "info", "check for required inputs")

    req(input$timerange1[2] > input$timerange1[1])
    req(input$timerange1[1] >= 0)
    req(input$timerange1[2] > 0)
    # #req(input$timerange1[2] <= max(values$data$time.min)+0.05) #give a bit leeway
     req(input$timerange1[1] <= max(values$data$time.min)+0.05) #give a bit leeway; ~ redundant
     req((input$timerange1[2] - input$timerange1[1]) > 0.1)





    #Use shinyvalidate
    #Create an InputValidator object
    iv <- shinyvalidate::InputValidator$new()

    iv$enable()


    #Required inputs
    iv$add_rule("filename.sel", shinyvalidate::sv_required())
    iv$add_rule("compound.sel1", shinyvalidate::sv_required())
    #iv$add_rule("compound.sel2", shinyvalidate::sv_required())
    #iv$add_rule("compound.sel3", shinyvalidate::sv_required())
    iv$add_rule("base_peak", shinyvalidate::sv_required())
    iv$add_rule("timerange1", shinyvalidate::sv_required())
    #iv$add_rule("timerange2", shinyvalidate::sv_required())
    #iv$add_rule("timerange3", shinyvalidate::sv_required())
    #timerange1 input requires special attention

    iv$add_rule("segments", shinyvalidate::sv_required())

    iv$add_rule("rare", shinyvalidate::sv_required())
    iv$add_rule("extreme", shinyvalidate::sv_required())
    iv$add_rule("ratio.method", shinyvalidate::sv_required())

    #Input values
    iv$add_rule("rare", shinyvalidate::sv_between(0, 90))
    iv$add_rule("extreme", shinyvalidate::sv_between(0, 10))
    iv$add_rule("segments", shinyvalidate::sv_between(1,10))


    # FIXME: Bug if I delete rare or extreme input after clicking plot button

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

    #FIXME
    module_message(ns, "info", "start has been pressed")

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
      "\n - files selected: ", paste(input$filename.sel %then% "", collapse = ", "),
      "\n - basepeak: ", input$base_peak %then% "",
      "\n - compound 1 selected: ", paste(input$compound.sel1 %then% "", collapse = ", "),
      "\n - timerange1: ", paste(input$timerange1 %then% "", collapse = " to "),
      "\n - compound 2 selected: ", paste(input$compound.sel2 %then% "", collapse = ", "),
      "\n - timerange2: ", paste(input$timerange2 %then% "", collapse = " to "),
      "\n - compound 3 selected: ", paste(input$compound.sel3 %then% "", collapse = ", "),
      "\n - timerange3: ", paste(input$timerange3 %then% "", collapse = " to "),
      "\n - segments: ",  input$segments %then% "",
      "\n - rare: ", input$rare %then% "",
      "\n - extreme: ", input$extreme %then% "", #)#edit
      "\n - ratio method: ", input$ratio.method %then% "" #)#edit
    )

##### Data: CSIA ---------------------------------------------------


    inputs.csia <-


      #req(input$INPUT_CSIA_Basepeak) #temp

      data.frame(
        parameter = c(
          "IsoXL Version",
          "INPUT_CSIA_Filename",
          "INPUT_CSIA_Basepeak",
          "INPUT_CSIA_Compound1",
          "INPUT_CSIA_timerange1[1]",
          "INPUT_CSIA_timerange1[2]",
          "INPUT_CSIA_Compound2",
          "INPUT_CSIA_timerange2[1]",
          "INPUT_CSIA_timerange2[2]",
          "INPUT_CSIA_Compound3",
          "INPUT_CSIA_timerange3[1]",
          "INPUT_CSIA_timerange3[2]",
          "Segments",
          "INPUT_CSIA_Rare",
          "INPUT_CSIA_TICxIT",
          "INPUT_CSIA_RatioMethod"
        ),
        #,"INPUT_CSIA_IONS"),


        value = c(
          isoXL.version,
          #paste(input$filename.sel, sep="", collapse=""), #FIX
          input$filename.sel[1],
          input$base_peak,
          input$compound.sel1[1],
          input$timerange1[1],
          input$timerange1[2],
          input$compound.sel2[1],
          input$timerange2[1],
          input$timerange2[2],
          input$compound.sel3[1],
          input$timerange3[1],
          input$timerange3[2],
          input$segments,
          input$rare,
          input$extreme,
          input$ratio.method
        )
        #   ,input$INPUT_CSIA_IONS)

      )




    # FIXME: before IsoXL I had a more sophisticated procedure to find the sample vs solvent. Implement this going forward?

    data.csia_filtered <-   values$data %>%

      dplyr::filter( (.data$compound %in% input$compound.sel1 & #CHECK
                        (.data$time.min>  input$timerange1[1] | .data$time.min <  input$timerange1[2])) |
                     (.data$compound %in% input$compound.sel2 &
                        (.data$time.min>  input$timerange2[1] | .data$time.min <  input$timerange2[2])) |
                     (.data$compound %in% input$compound.sel3 &
                        (.data$time.min>  input$timerange3[1] | .data$time.min <  input$timerange3[2]))
                      ) %>%
      stats::na.omit() %>%
      dplyr::filter(.data$time.min>  input$timerange1[1], .data$time.min <  input$timerange1[2]) %>%
      isoorbi::orbi_filter_satellite_peaks() %>%
      isoorbi::orbi_filter_weak_isotopocules(min_percent = input$rare) %>%
      isoorbi::orbi_filter_scan_intensity(outlier_percent = input$extreme)  %>%
      # remove.satellitePeaks() %>%
      # remove.rare(input = input) %>%
      # remove.ticxit(input = input) %>%
      group_by(.data$filename) %>%
      dplyr::mutate(segment = ceiling( (.data$scan.no -min(.data$scan.no)+1) / ((max(.data$scan.no) - min(.data$scan.no)+1)/input$segments ))) # FIXME: fix this!


    #SEGMENTATION
    if (input$segments > 1) {

      data.csia_filtered <- data.csia_filtered            %>%
        dplyr::mutate(filename=as.factor(paste(.data$filename, .data$segment, sep="_")))

    }




    # Table output: csia Analysis show rows of isox file------------------------------------------------


    output$mytable.csia = DT::renderDataTable({

      datatable(
        data.csia_filtered %>% ungroup() %>% mutate_if(is.double, round, digits = 2),
        rownames = FALSE ,
        extensions = c('Buttons', 'ColReorder'),
        options = list(
          scrollX = TRUE,
          #scroll bar to see entire table
          dom = 'Bfrtip',
          #buttons = c('colvis', 'copy',  'excel', 'pdf'),
          buttons =
            list('colvis', 'copy',   list(
              extend = 'excel',
              filename = 'IsoXL_csiaInjection_isox'
            ), list(
              extend = 'pdf',
              pageSize = 'A4',
              orientation = 'landscape',
              filename = 'IsoXL_CSIA_isox'
            )),
          colReorder = TRUE #columns can be reordered
        )
      )

    })

    # CSIA Analysis: Assign base peak --------------------------------------------


    data.csia_basepeak <-isoorbi::orbi_define_basepeak(dataset = data.csia_filtered, basepeak_def = input$base_peak) #confirm that grouping is correct; Move function to common core?

    #needs validation; e.g. is base peak present in data?


    # CSIA Analysis: Calculate output table for results file ---------------



    data.output <-isoorbi::orbi_summarize_results(dataset = data.csia_basepeak, ratio_method =  input$ratio.method)

    data.output.renamed <- data.output  %>% dplyr::rename(

      Filename = .data$filename,
      # `Sample` = sample_name,
      BasePeak = .data$basepeak,
      Isotopocule = .data$isotopocule,
      Compound = .data$compound,
      SEM = .data$ratio_sem,
      `SEM (permil)` = .data$ratio_relative_sem_permil,
      `Shot Noise (permil)` = .data$shot_noise_permil,
      `Minutes to 1E6 ions` = .data$minutes_to_1e6_ions,
      Scans = .data$number_of_scans,
      Ratio = .data$ratio#,
      #`Block` = block

    ) %>%
      select(

        "Filename",
        # Sample,
        # Block,
        "Compound",
        "Isotopocule",
        "Ratio",
        "SEM",
        "SEM (permil)",
        "Shot Noise (permil)",
        "Minutes to 1E6 ions",
        "Scans"
      )


    # Table output: CSIA Analysis, summary statistics ----------------------------


    output$summary.csia <- DT::renderDataTable({

      datatable( data.output.renamed, rownames= FALSE ,
                 extensions = c('Buttons','ColReorder'),
                 options = list(
                   # autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scrollbar when table is not visible
                   columnDefs = list(list(
                     targets = c(4L, 7L), visible = FALSE #hide some columns; results in an empty table if column number not available!
                   )),
                   dom = 'Bfrtip',
                   #buttons = c('colvis','copy',  'excel', 'pdf' ),
                   buttons =
                     list('colvis', 'copy',  list(
                       extend = 'excel',
                       filename = 'IsoXL_csiaInjection_results'
                     ), list(
                       extend = 'pdf',
                       pageSize = 'A4',
                       orientation = 'landscape',
                       filename = 'IsoXL_csiaInjection_results'
                     )),
                   colReorder = TRUE #columns can be reordered
                 )
      )

    })

# Plot output: csia Analysis--------------------------------------------------------

      output$plot <- renderPlotly({

        # isolate to avoid triggers on other input parameter selections
        isolate({

        plotly::ggplotly(

          ggplot(data = data.output, aes(x = .data$filename, y = .data$ratio, color = .data$isotopocule)) +
            scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis
            xlab("Filename") +
            ylab("Ratio") +

            theme(axis.text.x = element_text(angle = 30, vjust = 0.5, hjust=1)) +


            geom_errorbar(aes(ymin=(.data$ratio- .data$ratio_sem),
                              ymax=(.data$ratio+ .data$ratio_sem)),
                          width=0.4,
                          size=0.3,
                          color="black",
                          position=position_dodge(.9))       +
            geom_point(size=3, shape=10) ,

          dynamicTicks = TRUE #change ticks on zoom
        ) %>% layout(legend=list(title=list(text='Isotopocule'))) %>% plotly::config(

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

              filename = if (input$test_data == TRUE) {paste("testfile_csia", sep="")} #FIXME: use a variable for testfile name!
              else{
                paste("csia_injection",
                      tools::file_path_sans_ext(input$upload$name),
                      sep="_")}

                              )
          )
        })

      })

# csia analysis: Make Excel file ---------------------------------------------

data_csia.xls <- make.xls(df.stat = data.output, params = inputs.csia) #FIX





    # Download output: Excel file ----------------------------------


    output$download_xlsx <- downloadHandler(
      filename = function() {
        if (input$test_data == TRUE) {
          paste("testfile_csia", "_IsoXL.xlsx", sep = "")
        } #FIXME: use a variable for testfile name!
        else{
          paste(tools::file_path_sans_ext(input$upload$name),
                "_IsoXL.xlsx",
                sep = "")
        }
      },
      content = function(file) {
        saveWorkbook(data_csia.xls, file)
      }
    )




    shinyjs::show("main-rest")

  })

}


# user interface ----
#' @title module_isoxl_csia_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_csia_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      # Use test data instead of uploading data (or any data that has been already uploaded)

      tippy(
        checkboxInput(ns("test_data"), "Use test data", value = FALSE),
        tooltip = "<span style='font-size:15px;'>See Info for details<span>", allowHTML = TRUE,

        # "For details on test data see Info",
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

        h5("Define Compound-Specific LC/MS Analysis:"),

        selectInput(
          inputId = ns("filename.sel"),
          label = "File",
          c(),
          multiple = TRUE
        ),

        selectInput(inputId = ns("base_peak"), label = "Base peak", c()),

        tags$hr(style = "border-color: grey;"),


        selectInput(
          inputId = ns("compound.sel1"),
          label = "Compound 1",
          c(),
          multiple = FALSE
        ),
        shinyWidgets::numericRangeInput(ns("timerange1"), "Time [min]", value = c(), min=0, step=0.1),

        hr(),


        selectInput(
          inputId = ns("compound.sel2"),
          label = "Compound 2",
          c(),
          multiple = FALSE
        ),
        shinyWidgets::numericRangeInput(ns("timerange2"), "Time [min]", value = c(), min=0, step=0.1),

        hr(),

        selectInput(
          inputId = ns("compound.sel3"),
          label = "Compound 3",
          c(),
          multiple = FALSE
        ),
        shinyWidgets::numericRangeInput(ns("timerange3"), "Time [min]", value = c(), min=0, step=0.1),


        tags$hr(style = "border-color: grey;"),

        h5("Processing options:"),

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
          tooltip = "<span style='font-size:15px;'>Split each sample run into segments<span>", allowHTML = TRUE,
          # "Split each block into segments",
          placement = "right"
        ),

        #Use shinyBS::bsTooltip instead of tippy?
        tippy(
          numericInput(
            ns("rare"),
            "Remove weak signals (in %)",
            value = c(),
            width = NULL
          ),
          tooltip = "<span style='font-size:15px;'>Isotopocule must have been detected <br> in more than x % of scans<span>", allowHTML = TRUE,

          # "Processing option: Isotopocule must have been detected > x % of scans",
          placement = "right"
        ),
        tippy(
          numericInput(
            ns("extreme"),
            "Remove extreme scans (in %)",
            value = c(),
            width = NULL
          ),
          tooltip = "<span style='font-size:15px;'>Truncate outlying data based on <br> ions in Orbitrap (ITxTIC)<span>", allowHTML = TRUE,
          # "Processing option: Truncate data based on TICxIT",
          placement = "right"
        ),
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

    # UI: CSIA  Main Panel ------------------------------------------

  mainPanel(

    div(
      id = ns("overview-plot"),

    box(
      title = "Overview of Data",
      width = "auto" ,
      height = "auto",
      status = "primary",
      solidHeader = T,
      withSpinner(plotlyOutput(ns("plot.overview")))
    )    ) %>% shinyjs::hidden(),


    width = 9,

    # parameters ----
    div(
      id = ns("main-plot"),

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
    )%>% shinyjs::hidden(), #how to save png at higher resolution?

    hr(),
    div(
      id = ns("main-rest"),


      box(
        title = "Summary of Results",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("summary.csia"))
      ),

      hr(),


      box(
        title = "Filtered Data",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput(ns("mytable.csia"))
      )
    )
  )

  ))
}
