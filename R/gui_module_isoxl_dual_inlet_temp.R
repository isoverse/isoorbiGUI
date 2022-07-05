# # test ----
# # test the dual inlet module as a standalone app
#
# test_module_isoxl_dual_inlet <- function(port = 5000) {
#   app <-
#     shinyApp(
#       ui = fluidPage(
#         shinyjs::useShinyjs(),
#         h1("Dual Inlet Module Test"),
#         module_isoxl_dual_inlet_ui(id = "dual_inlet")
#       ),
#       server = function(input, output) {
#         dual_inlet <-
#           callModule(module_isoxl_dual_inlet_server, id = "dual_inlet")
#       }
#     )
#   runApp(app, display.mode = "normal", port = 5000)
# }
#
# # server
# module_isoxl_dual_inlet_server <- function(input, output, session) {
#
#   # namespace
#   ns <- session$ns
#
#   # increase upload file size
#   options(shiny.maxRequestSize = 100 * 1024 ^ 2)
#
#   # Data: Dual Inlet --------------------------------------------------------------
#
#   inputs.dual_inlet <- reactive({
#
#     req(input$upload_Dual_Inlet) #temp; needs to be coded more elegantly; prone to bugs (e.g. Excel output file cannot be generated)
#     req(input$INPUT_Sample)
#     req(input$INPUT_Ref)
#     req(input$INPUT_Basepeak)
#
#     data.frame( parameter = c(
#       "IsoXL Version",
#       "upload_Dual_Inlet",
#       "INPUT_Sample",
#       "INPUT_Ref",
#       "INPUT_Basepeak",
#       "INPUT_Blocks",
#       "INPUT_Block.time" ,
#       "INPUT_Idle.time",
#       "INPUT_Startup.time",
#       "INPUT_Segments",
#       "INPUT_Rare",
#       "INPUT_TICxIT",
#       "INPUT_RatioMethod",
#       "INPUT_IONS"),
#
#
#       value = c(isoXL.version,
#                 input$upload_Dual_Inlet$name,
#                 input$INPUT_Sample,
#                 input$INPUT_Ref,
#                 input$INPUT_Basepeak,
#                 input$INPUT_Blocks,
#                 input$INPUT_Block.time,
#                 input$INPUT_Idle.time,
#                 input$INPUT_Startup.time,
#                 input$INPUT_Segments,
#                 input$INPUT_Rare,
#                 input$INPUT_TICxIT,
#                 input$INPUT_RatioMethod,
#                 input$INPUT_IONS)
#
#
#     )
#   })
#
#
#
#   data.dual_inlet <- reactive({
#
#
#     req(input$upload_Dual_Inlet)
#
#
#     ext <- tools::file_ext(input$upload_Dual_Inlet$name)
#     switch(ext,
#            isox = readr::read_tsv(input$upload_Dual_Inlet$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
#                                   col_types = list(
#                                     filename = readr::col_factor(), #col_factor() conflicts with scales package
#                                     compound = readr::col_factor(),
#                                     isotopolog = readr::col_factor()
#                                   ) ),
#
#
#            validate("Invalid file; Please upload a .isox file") #check that this line works
#     )
#   })
#
#
#   data.dual_inlet_filtered <- reactive({
#
#     req(input$INPUT_Rare)
#
#     return(data.dual_inlet() %>%
#              na.omit() %>%
#              remove.ticxit( input = input$INPUT_TICxIT ) %>% #temp: validate correct functioning
#              remove.satellitePeaks() %>%
#              remove.rare(input = input$INPUT_Rare) )
#
#
#   })
#
#
#
#
#
#   #  Dual Inlet annotation table --------------------------------------------
#
#   dual.inlet.annotation     <- reactive({
#     req(input$INPUT_Ref)
#     req(input$INPUT_Sample)
#     req(input$INPUT_Blocks)
#     req(input$INPUT_Segments)
#     req(input$INPUT_Idle.time)
#     req(input$INPUT_Startup.time)
#     req(input$INPUT_Block.time)
#
#     define.dual.inlet(INPUT_Ref= input$INPUT_Ref,
#                       INPUT_Sample= input$INPUT_Sample,
#                       INPUT_Blocks= input$INPUT_Blocks,
#                       INPUT_Segments= input$INPUT_Segments,
#                       INPUT_Idle.time = input$INPUT_Idle.time,
#                       INPUT_Startup.time= input$INPUT_Startup.time,
#                       INPUT_Block.time= input$INPUT_Block.time)
#
#   })
#
#
#
#   # Dual Inlet: Merge data with annotation ----------------------------------
#
#
#   data.annotated <- reactive({
#     merge.di.annotations(data.dual_inlet_filtered(), dual.inlet.annotation())
#   })
#
#
#
#   # Dual Inlet: Assign base peak --------------------------------------------
#
#
#   data.basepeak <- reactive({assign.basepeak(data.annotated(), input$INPUT_Basepeak)  }) #needs validation; e.g. is basepeak present in data?
#
#
#   # Dual Inlet: Calculate output table for results file ---------------
#
#
#
#   data.output <- reactive({calc.output(df = data.basepeak(), INPUT_RatioMethod = input$INPUT_RatioMethod) })
#
#
#
#   # Dual Inlet: Make Excel file ---------------------------------------------
#
#   data.xls <- reactive({  make.xls(data.output(), params=inputs.dual_inlet()) })
#
#
#   # Plot output: Dual Inlet --------------------------------------------------------
#
#
#   output$plot <- renderPlotly({
#
#
#     ggplotly(
#
#       ggplot(data = data.output(), aes(x = Injection, y = Ratio, color=isotopolog)) + scale_color_brewer(palette="Dark2") +
#
#         geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
#                           ymax=(Ratio+Ratio.SEM)),
#                       width=0.7,
#                       size=0.3,
#                       color="black",
#                       position=position_dodge(.9))       +        geom_point(size=3, shape=10) + facet_wrap(~ filename, ncol=1), # temp: could be interesting option: + facet_grid(isotopolog~.),
#
#       dynamicTicks = TRUE #change ticks on zoom
#     ) %>%
#
#       config(displaylogo = FALSE,
#              modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar
#
#       config( #options on how to save an image
#         toImageButtonOptions = list(
#           format = "png",
#           width = 900,
#           height = 600,
#           scale = 4,
#           filename = paste("dual_inlet", input$upload_Dual_Inlet$name, sep="_")
#         )
#       )
#
#     #plot_ly(data.output(), x = ~Injection, y = ~Ratio, type = 'scatter', mode = 'markers', color=factor(data.output$isotopolog()))
#   })
#
#
#
#
#
#   # Table output: Dual Inlet, show rows of isox file------------------------------------------------
#
#
#   #test case
#   # output$mytable = DT::renderDataTable({
#   #
#   # DT::datatable(
#   #   mtcars,
#   #   data.dual_inlet_filtered(),
#   #   options = list(columnDefs = list(list(
#   #     targets = 0:7, visible = FALSE
#   #   )), buttons = c('colvis'), dom = 'Bfrtip'),
#   #   extensions = 'Buttons'
#   # )
#   # })
#
#   output$mytable = DT::renderDataTable({
#     DT::datatable( data.dual_inlet_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
#                    extensions = c('Buttons','ColReorder', 'Select'),
#
#                    options = list(
#                      #   autoWidth = TRUE, #scroll bar to see enture table
#                      scrollX = TRUE, #scroll bar to see enture table
#
#                      #   columnDefs = list(list(
#                      #
#                      #        targets = c(2L), visible = FALSE #hide some columns
#                      # )),
#
#                      dom = 'Bfrtip',
#
#                      # buttons = list(
#                      #   list(extend = c("colvis","excel"),
#                      #        exportOptions = list(columns = ":visible"))
#                      # ),
#                      buttons = list(list(extend = 'colvis'),
#                                     list(extend = 'copy', exportOptions = list(columns = ':visible')),
#                                     list(extend = 'excel', exportOptions = list(columns = ':visible')),
#                                     list(extend = 'pdf', exportOptions = list(columns = ':visible'))
#                      ), #temp donwload only visible columns
#
#                      #   buttons = c('colvis','copy',  'excel', 'pdf'),
#                      # exportOptions = list(columns = ":visible"),
#                      colReorder = TRUE#, #columns can be reordered,
#                      # selection = list(target = "column")#,
#                      # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
#                      #
#                    )
#     )
#     #How to export only visible columns?
#     # buttons = list(
#     #   list(extend = "excel", exportOptions = list(columns = ":visible"))
#     # )
#
#
#
#     #https://datatables.net/reference/option/columns.visible
#
#   })
#
#
#
#   # Table output: Dual Inlet, summary statistics ----------------------------
#
#
#   output$summary <- DT::renderDataTable({
#
#     datatable( data.output(), rownames= FALSE ,
#                extensions = c('Buttons','ColReorder'),
#                options = list(
#                  # autoWidth = TRUE, #scroll bar to see enture table
#                  scrollX = TRUE, #scrollbar when table is not visible
#                  columnDefs = list(list(
#                    targets = c(7L, 10L), visible = FALSE
#                  )),
#                  dom = 'Bfrtip',
#                  buttons = list(list(extend = 'colvis'),
#                                 list(extend = 'copy', exportOptions = list(columns = ':visible')),
#                                 list(extend = 'excel', exportOptions = list(columns = ':visible')),
#                                 list(extend = 'pdf', exportOptions = list(columns = ':visible'))
#                  ), #temp donwload only visible columns; validate more thoroughly and implement in all tables; define common sytles in a function?
#                  colReorder = TRUE #columns can be reordered
#                )
#     )
#
#   })
#
#
#
#   # Download output: Dual Inlet Excel file ----------------------------------
#
#
#   output$download_Dual_Inlet_xlsx <- downloadHandler(
#     filename = function(){  paste(tools::file_path_sans_ext(input$upload_Dual_Inlet$name), "_IsoXL.xlsx", sep="")},
#     content = function(file) {
#       saveWorkbook(data.xls(), file)
#
#     }
#   )
#
#
#
# }
#
# # user interface
# module_isoxl_dual_inlet_ui <- function(id) {
#   # namespace
#   ns <- NS(id)
#
#   fluidPage(
#
#     sidebarLayout(
#
#       sidebarPanel(width = 3,
#
#                    fileInput("upload_Dual_Inlet", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),
#
#                    hr(),
#
#                    #submitButton("Update output", icon("sync")),
#                    actionButton("update", "Update", icon("sync")),
#
#                    #shinyWidgets::numericRangeInput("n", "Rows", value = c(1,5), min=1, max=100000),
#
#                    tags$hr(style="border-color: grey;"),
#
#                    h6("Define Dual Inlet"),
#
#                    textInput(inputId = "INPUT_Sample", label="Sample", value = "sample"),
#                    textInput(inputId = "INPUT_Ref", label="Reference", value = "refmat"),
#                    textInput(inputId = "INPUT_Basepeak", label="Base peak", value = "15N"), #change back to M0 as default
#
#                    tags$hr(style="border-color: grey;"),
#                    h6("Parameters"),
#
#                    numericInput(
#                      inputId = 'INPUT_Blocks',
#                      label = "Blocks",
#                      7,
#                      min = 1,
#                      max = 100,
#                      step = 1,
#                      width = NULL
#                    ),
#
#                    numericInput(
#                      inputId = "INPUT_Block.time",
#                      label = "Block time [min]",
#                      10,
#                      min = 1,
#                      max = 20,
#                      step = 1,
#                      width = NULL
#                    ),
#
#                    numericInput(
#                      inputId = "INPUT_Idle.time",
#                      label = "Switch time [min]",
#                      0.5,
#                      min = 0,
#                      max = 2,
#                      step = 0.1,
#                      width = NULL
#                    ),
#
#                    numericInput(
#                      inputId = "INPUT_Startup.time",
#                      label = "Startup time [min]",
#                      0,
#                      min = 0,
#                      max = 10,
#                      step = 0.1,
#                      width = NULL
#                    ),
#
#                    tags$hr(style="border-color: grey;"),
#
#                    h6("Processing options:"),
#
#                    # Hover over: with_tippy(textInput("input", "input with tooltip"), "Input text", placement = "right")
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_Segments",
#                      label = "Segments",
#                      1,
#                      min = 1,
#                      max = 10,
#                      step = 1,
#                      width = NULL
#                    ), "Processing option: Split blocks into multiple segments", placement = "right"), #Not consistently working...
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_Rare",
#                      label = "Remove weak isotopologs",
#                      30,
#                      min = 1,
#                      max = 50,
#                      step = 1,
#                      width = NULL
#                    ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_TICxIT",
#                      label = "Remove outlier scans",
#                      1,
#                      min = 0.1,
#                      max = 10,
#                      step = 0.1,
#                      width = NULL
#                    ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right"),
#
#                    radioButtons(inputId = "INPUT_RatioMethod", label="Ratio method", choiceNames = c( "sum",  "mean", "median", "geometric"), choiceValues = c( "sum",  "mean", "median", "geometric.mean"), selected= "sum"),
#
#                    #radioButtons(inputId = "INPUT_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
#                    radioButtons(inputId = "INPUT_IONS", label="Ratio input", choiceNames = c( "ions"), choiceValues = c( "ions.incremental"), selected=  "ions.incremental"), #temp: intensity is not yet implemented
#       ),
#
#
#       # UI: Dual Inlet Main Panel -----------------------------------------------
#
#
#       mainPanel(width = 9,
#
#
#                 #hr(),
#
#                 downloadButton("download_Dual_Inlet_xlsx", "Download Results (.xlsx)"),
#
#                 hr(),
#
#
#                 box(title = "Overview of Results", width = "auto" ,  height ="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput("plot"))), #height="auto",
#
#
#                 hr(),
#
#
#                 box(title = "Results Table", width = "auto" ,status = "primary", solidHeader = T, DT::dataTableOutput("summary")),
#
#                 hr(),
#
#
#                 box(title = "Filtered .isox File", width = "auto" ,status = "primary", solidHeader = T, DT::dataTableOutput("mytable")),
#
#
#       )
#     )
#   )
#
# }
