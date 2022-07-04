# # test ----
# # test the single analysis module as a standalone app
#
# test_module_isoxl_single <- function(port = 5000) {
#   app <-
#     shinyApp(
#       ui = fluidPage(
#         shinyjs::useShinyjs(),
#         h1("Single Analysis Module Test"),
#         module_isoxl_single_ui(id = "single")
#       ),
#       server = function(input, output) {
#         single <-
#           callModule(module_isoxl_single_server, id = "single")
#       }
#     )
#   runApp(app, display.mode = "normal", port = 5000)
# }
#
# # server ----
#
# module_isoxl_single_server <- function(input, output, session) {
#   # namespace
#   ns <- session$ns
#
#   # increase upload file size
#   options(shiny.maxRequestSize = 100 * 1024 ^ 2)
#
#
#
#   # Data: Single Analysis ---------------------------------------------------
#
#   inputs.single <- reactive({
#
#
#     #req() #temp
#
#     data.frame( parameter = c(
#       "IsoXL Version",
#       "INPUT_Single_sample",
#       "INPUT_Single_Basepeak",
#       "INPUT_Single_timerange[1]",
#       "INPUT_Single_timerange[2]" ,
#       "INPUT_Single_Rare",
#       "INPUT_Single_TICxIT",
#       "INPUT_Single_RatioMethod",
#       "INPUT_Single_IONS"),
#
#
#       value = c(isoXL.version,
#                 input$INPUT_Single_sample,
#                 input$INPUT_Single_Basepeak,
#                 input$INPUT_Single_timerange[1],
#                 input$INPUT_Single_timerange[2],
#                 input$INPUT_Single_Rare,
#                 input$INPUT_Single_TICxIT,
#                 input$INPUT_Single_RatioMethod,
#                 input$INPUT_Single_IONS)
#
#
#     )
#   })
#
#
#   data.single <- reactive({
#
#
#
#     req(input$upload_Single_Analysis)
#
#
#     ext <- tools::file_ext(input$upload_Single_Analysis$name)
#     switch(ext,
#            isox = readr::read_tsv(input$upload_Single_Analysis$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
#                                   col_types = list(
#                                     filename = readr::col_factor(),
#                                     compound = readr::col_factor(),
#                                     isotopolog = readr::col_factor()
#                                   ) ),
#
#
#            validate("Invalid file; Please upload a .isox file")
#     )
#   })
#
#
#   data.single_filtered <- reactive({
#
#     req(input$INPUT_Single_Rare)
#     req(input$INPUT_Single_timerange)
#     req(input$INPUT_Single_timerange)
#     req(input$INPUT_Single_sample)
#
#     validate(
#
#
#       need(length(levels(data.single()$filename)) <2, paste("Warning: There is more than one 'filename' in data. May want to use Flow Injection tab? These filenames have been observed",levels(data.single()$filename ))),
#       need(input$INPUT_Single_timerange[2] <= max(data.single()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.single()$time.min) , " min.") ),
#       need(input$INPUT_Single_timerange[2] > input$INPUT_Single_timerange[1], paste("Warning: Time selection minimun is greater than maximum") )
#
#     )
#
#
#
#     return(data.single() %>%
#              na.omit() %>%
#
#              mutate(Sample.Name = input$INPUT_Single_sample) %>% #temp: implemented as factor?
#
#              remove.ticxit(input = input$INPUT_Single_TICxIT) %>%
#              remove.satellitePeaks() %>%
#              remove.rare(input = input$INPUT_Single_Rare) %>%
#
#              filter(time.min>  input$INPUT_Single_timerange[1], time.min <  input$INPUT_Single_timerange[2])
#     )
#
#
#   })
#
#   # Single Analysis: Assign base peak --------------------------------------------
#
#
#   data.single_basepeak <- reactive({
#
#     req(input$INPUT_Single_Basepeak)
#
#     assign.basepeak.single(df = data.single_filtered(), INPUT_Basepeak = input$INPUT_Single_Basepeak)
#
#   }) #needs validation; e.g. is base peak present in data?
#
#
#   # Single Analysis: Calculate output table for results file ---------------
#
#
#
#   data.single_output <- reactive({
#
#     req(input$INPUT_Single_Basepeak)
#
#     calc.output.single(df = data.single_basepeak(), INPUT_RatioMethod = input$INPUT_Single_RatioMethod)
#
#   })
#
#
#   # Single analysis: Make Excel file ---------------------------------------------
#
#   data_single.xls <- reactive({  make.xls.onetab(df.stat = data.single_output(), params = inputs.single()) })
#
#
#   # Plot output: Single Analysis--------------------------------------------------------
#
#
#   output$plot.single <- renderPlotly({
#
#
#     ggplotly(
#
#       ggplot(data = data.single_output(), aes(x = Shot.Noise.permil, y = relSE.permil, color=isotopolog)) + scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis
#
#         # geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
#         #                   ymax=(Ratio+Ratio.SEM)),
#         #               width=0.7,
#         #               size=0.3,
#         #               color="black",
#         #               position=position_dodge(.9))       +
#         geom_point(size=3, shape=10) +
#         geom_abline(slope=1) +
#         ylab(paste("relative SEM ", "[\u2030]")) + xlab(paste("shot noise limit", "[\u2030]")),
#
#       dynamicTicks = TRUE #change ticks on zoom
#     )%>%
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
#           filename = paste("single_analysis", input$upload_Single_Analysis$name, sep="_")
#         )
#       )
#
#
#   })
#
#
#
#   # Table output: Single Analysis, summary statistics ----------------------------
#
#
#   output$summary.single <- DT::renderDataTable({
#
#     datatable( data.single_output(), rownames= FALSE ,
#                extensions = c('Buttons','ColReorder'),
#                options = list(
#                  # autoWidth = TRUE, #scroll bar to see enture table
#                  scrollX = TRUE, #scrollbar when table is not visible
#                  columnDefs = list(list(
#                    targets = c(6L, 10L), visible = FALSE #hide some columns so the table is not as wide, can be activated using column button; remain in output
#                  )),
#                  dom = 'Bfrtip',
#                  buttons = c('colvis','copy',  'excel', 'pdf' ),
#                  colReorder = TRUE #columns can be reordered
#                )
#     )
#
#   })
#
#
#
#   # Table output: Single Analysis show rows of isox file------------------------------------------------
#
#
#   output$mytable.single = DT::renderDataTable({
#
#     datatable( data.single_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
#                extensions = c('Buttons','ColReorder', 'Select'),
#
#                options = list(
#                  #   autoWidth = TRUE, #scroll bar to see enture table
#                  scrollX = TRUE, #scroll bar to see enture table
#
#                  #   columnDefs = list(list(
#                  #
#                  #        targets = c(2L), visible = FALSE #hide some columns
#                  # )),
#
#                  dom = 'Bfrtip',
#                  buttons = c('colvis','copy',  'excel', 'pdf' ),
#                  colReorder = TRUE#, #columns can be reordered,
#                  # selection = list(target = "column")#,
#                  # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
#                )
#     )
#     #How to export only visible columns?
#     # buttons = list(
#     #   list(extend = "excel", exportOptions = list(columns = ":visible"))
#     # )
#
#     # DT::datatable(
#     #   mtcars,
#     #   options = list(columnDefs = list(list(
#     #     targets = 0:7, visible = FALSE
#     #   )), buttons = c('colvis'), dom = 'Bfrtip'),
#     #   extensions = 'Buttons'
#     # )
#
#
#     #https://datatables.net/reference/option/columns.visible
#
#   })
#
#
#   # Download output: Single Analysis Excel file ----------------------------------
#
#
#   output$download_Single_xlsx <- downloadHandler(
#     filename = function(){  paste(tools::file_path_sans_ext(input$upload_Single_Analysis$name), "_IsoXL.xlsx", sep="")},
#     content = function(file) {
#       saveWorkbook(data_single.xls(), file)
#     }
#   )
#
#
#
# }
#
#
# # user interface ----
#
# module_isoxl_single_ui <- function(id) {
#
#   # namespace
#   ns <- NS(id)
#
#   fluidPage(sidebarLayout(
#     sidebarPanel(
#       width = 3,
#
#
#
#       fileInput(
#         "upload_Single_Analysis",
#         label = "Upload .isox file",
#         accept = c(".isox"),
#         placeholder = "No file loaded..."
#       ),
#
#       hr(),
#
#       submitButton("Update output", icon("sync")),
#
#       tags$hr(style = "border-color: grey;"),
#
#       h6("Define Single Analysis"),
#
#       textInput(
#         inputId = "INPUT_Single_sample",
#         label = "Sample",
#         value = "sample"
#       ),
#
#       textInput(
#         inputId = "INPUT_Single_Basepeak",
#         label = "Base peak",
#         value = "15N"
#       ),
#       #change back to M0 as default
#
#
#       shinyWidgets::numericRangeInput(
#         "INPUT_Single_timerange",
#         "Time [min]",
#         value = c(1, 10),
#         min = 0,
#         max = 100
#       ),
#
#
#       tags$hr(style = "border-color: grey;"),
#
#       h6("Processing options:"),
#
#       # Hover over: with_tippy(textInput("input", "input with tooltip"), "Input text", placement = "right")
#
#       # with_tippy(numericInput(
#       #   inputId = "INPUT_Single_Segments",
#       #   label = "Segments",
#       #   1,
#       #   min = 1,
#       #   max = 10,
#       #   step = 1,
#       #   width = NULL
#       # ), "Processing option: Split blocks into multiple segments", placement = "right"), #Not consistently working...
#
#       with_tippy(
#         numericInput(
#           inputId = "INPUT_Single_Rare",
#           label = "Remove weak isotopologs",
#           30,
#           min = 1,
#           max = 50,
#           step = 1,
#           width = NULL
#         ),
#         "Processing option: Isotopolog must have been observed in at least x % of scans",
#         placement = "right"
#       ),
#
#       with_tippy(
#         numericInput(
#           inputId = "INPUT_Single_TICxIT",
#           label = "Remove outlier scans",
#           1,
#           min = 0.1,
#           max = 10,
#           step = 0.1,
#           width = NULL
#         ),
#         "Processing option: Truncate data x%/100-x% based on TICxIT",
#         placement = "right"
#       ),
#
#       radioButtons(
#         inputId = "INPUT_Single_RatioMethod",
#         label = "Ratio method",
#         choiceNames = c("sum",  "mean", "median", "geometric"),
#         choiceValues = c("sum",  "mean", "median", "geometric.mean"),
#         selected = "sum"
#       ),
#
#       #radioButtons(inputId = "INPUT_Single_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
#       radioButtons(
#         inputId = "INPUT_Single_IONS",
#         label = "Ratio input",
#         choiceNames = c("ions"),
#         choiceValues = c("ions.incremental"),
#         selected =  "ions.incremental"
#       ),
#       #intensity is not yet implemented
#
#
#     ),
#
#
#     # UI: Single Analysis Main Panel ------------------------------------------
#
#     mainPanel(
#       width = 9,
#
#
#       #hr(),
#
#       downloadButton("download_Single_xlsx", "Download Results (.xlsx)"),
#
#       hr(),
#
#
#       box(
#         title = "Overview of Results",
#         width = "auto" ,
#         height = "auto",
#         status = "primary",
#         solidHeader = T,
#         withSpinner(plotlyOutput("plot.single"))
#       ),
#       #how to save png at higher resolution?
#
#
#       hr(),
#
#
#       box(
#         title = "Results Table",
#         width = "auto" ,
#         status = "primary",
#         solidHeader = T,
#         DT::dataTableOutput("summary.single")
#       ),
#
#       hr(),
#
#
#       box(
#         title = "Filtered .isox File",
#         width = "auto" ,
#         status = "primary",
#         solidHeader = T,
#         DT::dataTableOutput("mytable.single")
#       ),
#
#
#     )
#   ))
# }
