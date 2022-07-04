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


  ##### Data: Flow Analysis ---------------------------------------------------


  inputs.flow <- reactive({


    req(input$INPUT_Flow_Basepeak) #temp

    data.frame( parameter = c(
      "IsoXL Version",
      "INPUT_Flow_Basepeak",
      "INPUT_Flow_timerange[1]",
      "INPUT_Flow_timerange[2]" ,
      "INPUT_Flow_Rare",
      "INPUT_Flow_TICxIT",
      "INPUT_Flow_RatioMethod",
      "INPUT_Flow_IONS"),


      value = c(isoXL.version,
                input$INPUT_Flow_Basepeak,
                input$INPUT_Flow_timerange[1],
                input$INPUT_Flow_timerange[2],
                input$INPUT_Flow_Rare,
                input$INPUT_Flow_TICxIT,
                input$INPUT_Flow_RatioMethod,
                input$INPUT_Flow_IONS)


    )
  })

  data.flow <- reactive({



    req(input$upload_Flow_Injection)


    ext <- tools::file_ext(input$upload_Flow_Injection$name)
    switch(ext,
           isox = readr::read_tsv(input$upload_Flow_Injection$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                  col_types = list(
                                    filename = readr::col_factor(),
                                    compound = readr::col_factor(),
                                    isotopolog =  readr::col_factor()
                                  ) ),


           validate("Invalid file; Please upload a .isox file")
    )
  })


  data.flow_filtered <- reactive({

    req(input$INPUT_Flow_Rare)
    req(input$INPUT_Flow_timerange)
    req(input$INPUT_Flow_timerange)
    #req(input$INPUT_Flow_sample)

    validate(


      ####  need(length(levels(data.flow()$filename)) <2, paste("Warning: There is more than one 'filename' in data. May want to use Flow Injection tab? These filenames have been observed",levels(data.flow()$filename ))),
      need(input$INPUT_Flow_timerange[2] <= max(data.flow()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.flow()$time.min) , " min.") ),
      need(input$INPUT_Flow_timerange[2] > input$INPUT_Flow_timerange[1], paste("Warning: Time selection minimun is greater than maximum") )

    )



    return(data.flow() %>%
             na.omit() %>%

             #mutate(Sample.Name = input$INPUT_Flow_sample) %>% #temp: implemented as factor? Not needed, I think

             remove.ticxit(input=input$INPUT_Flow_TICxIT) %>%
             remove.satellitePeaks() %>%
             remove.rare(input = input$INPUT_Flow_Rare) %>%

             filter(time.min>  input$INPUT_Flow_timerange[1], time.min <  input$INPUT_Flow_timerange[2])
    )


  })


  # Flow Analysis: Assign base peak --------------------------------------------


  data.flow_basepeak <- reactive({

    req(input$INPUT_Flow_Basepeak)

    assign.basepeak.single(df = data.flow_filtered(), INPUT_Basepeak = input$INPUT_Flow_Basepeak)  #function from single analysis should be of has groub_by(filename)

  }) #needs validation; e.g. is base peak present in data?


  # Flow Analysis: Calculate output table for results file ---------------



  data.flow_output <- reactive({

    req(input$INPUT_Flow_Basepeak)

    calc.output.flow(df = data.flow_basepeak(), INPUT_RatioMethod = input$INPUT_Flow_RatioMethod)

  })


  # Flow analysis: Make Excel file ---------------------------------------------

  data_flow.xls <- reactive({  make.xls(df.stat = data.flow_output(), params=inputs.flow() ) }) ##temp add correct parameter file!!!

  ####



  # Plot output: Flow Injection--------------------------------------------------------


  output$plot.flow <- renderPlotly({


    ggplotly(

      ggplot(data = data.flow_output(), aes(x = filename, y = Ratio, color=isotopolog)) + scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

        geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
                          ymax=(Ratio+Ratio.SEM)),
                      width=0.7,
                      size=0.3,
                      color="black",
                      position=position_dodge(.9))       +         geom_point(size=3, shape=10) ,

      dynamicTicks = TRUE #change ticks on zoom
    )%>%

      config(displaylogo = FALSE,
             modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar

      config( #options on how to save an image
        toImageButtonOptions = list(
          format = "png",
          width = 900,
          height = 600,
          scale = 4,
          filename = paste("flow_injection", input$upload_Flow_Injection$name, sep="_")
        )
      )

  })

  # Table output: Flow Analysis, summary statistics ----------------------------


  output$summary.flow <- DT::renderDataTable({

    datatable( data.flow_output(), rownames= FALSE , #temp, table with stat did not render
               extensions = c('Buttons','ColReorder'),
               options = list(
                 # autoWidth = TRUE, #scroll bar to see enture table
                 scrollX = TRUE, #scrollbar when table is not visible
                 columnDefs = list(list(
                   targets = c(6L, 9L), visible = FALSE #using these rows, the table does not display
                 )),
                 dom = 'Bfrtip',
                 buttons = c('colvis','copy',  'excel', 'pdf' ),
                 colReorder = TRUE #columns can be reordered
               )
    )

  })




  # Table output: Flow Analysis show rows of isox file------------------------------------------------


  output$mytable.flow = DT::renderDataTable({

    datatable( data.flow_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
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
    #How to export only visible columns?
    # buttons = list(
    #   list(extend = "excel", exportOptions = list(columns = ":visible"))
    # )

    # DT::datatable(
    #   mtcars,
    #   options = list(columnDefs = list(list(
    #     targets = 0:7, visible = FALSE
    #   )), buttons = c('colvis'), dom = 'Bfrtip'),
    #   extensions = 'Buttons'
    # )


    #https://datatables.net/reference/option/columns.visible

  })

  # Download output: Flow Analysis Excel file ----------------------------------


  output$download_Flow_xlsx <- downloadHandler(
    filename = function(){  paste(tools::file_path_sans_ext(input$upload_Flow_Injection$name), "_IsoXL.xlsx", sep="")},
    content = function(file) {
      saveWorkbook(data_flow.xls(), file)
    }
  )

}


# user interface ----

module_isoxl_flow_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,



      fileInput(
        "upload_Flow_Injection",
        label = "Upload .isox file",
        accept = c(".isox"),
        placeholder = "No file loaded..."
      ),

      hr(),

      #submitButton("Update output", icon("sync")),
      actionButton("update", "Update", icon("sync")),

      tags$hr(style = "border-color: grey;"),

      h6("Define Parameters"),

      # textInput(inputId = "INPUT_Flow_sample", label="Sample", value = "sample"),

      textInput(
        inputId = "INPUT_Flow_Basepeak",
        label = "Base peak",
        value = "M0"
      ),
      #change back to M0 as default


      shinyWidgets::numericRangeInput(
        "INPUT_Flow_timerange",
        "Time [min]",
        value = c(1, 10),
        min = 0,
        max = 100
      ),


      tags$hr(style = "border-color: grey;"),

      h6("Processing options:"),

      # Hover over: with_tippy(textInput("input", "input with tooltip"), "Input text", placement = "right")

      # with_tippy(numericInput(
      #   inputId = "INPUT_Flow_Segments",
      #   label = "Segments",
      #   1,
      #   min = 1,
      #   max = 10,
      #   step = 1,
      #   width = NULL
      # ), "Processing option: Split blocks into multiple segments", placement = "right"), #Not consistently working...

      with_tippy(
        numericInput(
          inputId = "INPUT_Flow_Rare",
          label = "Remove weak isotopologs",
          30,
          min = 1,
          max = 50,
          step = 1,
          width = NULL
        ),
        "Processing option: Isotopolog must have been observed in at least x % of scans",
        placement = "right"
      ),

      with_tippy(
        numericInput(
          inputId = "INPUT_Flow_TICxIT",
          label = "Remove outlier scans",
          1,
          min = 0.1,
          max = 10,
          step = 0.1,
          width = NULL
        ),
        "Processing option: Truncate data x%/100-x% based on TICxIT",
        placement = "right"
      ),

      radioButtons(
        inputId = "INPUT_Flow_RatioMethod",
        label = "Ratio method",
        choiceNames = c("sum",  "mean", "median", "geometric"),
        choiceValues = c("sum",  "mean", "median", "geometric.mean"),
        selected = "sum"
      ),

      #radioButtons(inputId = "INPUT_Flow_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
      radioButtons(
        inputId = "INPUT_Flow_IONS",
        label = "Ratio input",
        choiceNames = c("ions"),
        choiceValues = c("ions.incremental"),
        selected =  "ions.incremental"
      ),
      #intensity is not yet implemented


    ),


    # UI: Flow Injection Main Panel -------------------------------------------


    mainPanel(
      width = 9,


      #hr(),

      downloadButton("download_Flow_xlsx", "Download Results (.xlsx)"),

      hr(),


      box(
        title = "Overview of Results",
        width = "auto" ,
        height = "auto",
        status = "primary",
        solidHeader = T,
        withSpinner(plotlyOutput("plot.flow"))
      ),
      #how to save png at higher resolution?


      hr(),


      box(
        title = "Results Table",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput("summary.flow")
      ),

      hr(),


      box(
        title = "Filtered .isox File",
        width = "auto" ,
        status = "primary",
        solidHeader = T,
        DT::dataTableOutput("mytable.flow")
      ),





    )
  ))
}
