# # test ----
# # test the make_inputFile module as a standalone app
#
# test_module_isoxl_make_inputFile <- function(port = 5000) {
#   app <-
#     shinyApp(
#       ui = fluidPage(
#         shinyjs::useShinyjs(),
#         h1("Make IsoX input file Module Test"),
#         module_isoxl_make_inputFile_ui(id = "make_inputFile")
#       ),
#       server = function(input, output) {
#         make_inputFile <-
#           callModule(module_isoxl_make_inputFile_server, id = "make_inputFile")
#       }
#     )
#   runApp(app, display.mode = "normal", port = 5000)
# }
#
# # server ----
#
# module_isoxl_make_inputFile_server <- function(input, output, session) {
#
#   # namespace
#   ns <- session$ns
#
#   # Output table: Make IsoX Input  ------------------------------------------
#
#   #temporary section
#
#   INPUT_formula.check <- reactive({
#     run_check(INPUT_formula = input$INPUT_IsoX_sumformula)
#   })
#
#   #data(isotopes) not found 12-June-2022
#   data(isotopes, package = "enviPat")#neeed for isopat function
#
#   pattern <- reactive({
#
#     validate(
#       need(sum(as.data.frame(check_chemform(isotopes, chemforms= INPUT_formula.check(), get_list=TRUE))) <= 200, 'Sum formula has unusually many atoms (>200)')
#     )
#
#     data.out <- isopattern(
#       isotopes=isotopes,
#       chemforms = INPUT_formula.check(),
#       threshold =  input$INPUT_IsoX_threshold,
#       plotit = FALSE,
#       charge = input$INPUT_IsoX_charge, #eg 1 or -1
#       emass = 0.00054858,
#       algo = 1,
#       rel_to = 4,
#       verbose = TRUE,
#       return_iso_calc_amount = FALSE
#     )
#
#
#     validate(
#       need(data.out[1] != "error", 'ERROR: cannot parse this sum formula with isopattern function') #example: NO00; not caught by check_chemform; still issues with P10; Si10 check validation/ code more
#     ) #temp
#
#     return(data.out)
#   })
#
#
#
#
#   out.isox <- reactive({ make.isox.input(pattern.list = pattern(), #temp!
#                                          INPUT_compound = input$INPUT_IsoX_compound,
#                                          INPUT_charge = input$INPUT_IsoX_charge,
#                                          INPUT_tolerance= input$INPUT_IsoX_tolerance,
#                                          INPUT_massOffset = input$INPUT_IsoX_massoffset/1000) #mDa!
#
#   })
#
#
#
#
#
#   # Preview isotopologs.tsv --------------------------------------------------
#
#
#
#   output$isox = DT::renderDataTable({
#
#     # validate(
#     #   need(1>2, c("","Thanks for testing this feature. Hope to have it implemented soon."))
#     # )
#     #
#     datatable( out.isox(), rownames= FALSE ,
#                extensions = c('Buttons','ColReorder', 'Select'),
#                options = list(
#                  scrollX = TRUE, #scroll bar to see enture table
#                  dom = 'Bfrtip',
#                  buttons = list( #temp: option to show all rows of table (for copy & paste); show more rows in other tables also?
#                    list(
#                      extend = "collection",
#                      text = 'Show All',
#                      action = DT::JS("function ( e, dt, node, config ) {
#                                     dt.page.len(-1);
#                                     dt.ajax.reload();
#                                 }")
#                    ),
#                    list(extend = 'copy')
#                  )
#                  #buttons = c('copy' )#,
#                  #colReorder = TRUE#, #columns can be reordered,
#                )
#     )
#
#
#
#   })
#
#
#   # Download isotopologs.tsv ------------------------------------------------
#
#   output$download_IsoX <- downloadHandler(
#     filename = function(){"isotopologs.tsv"},
#     content = function(fname){
#       write.table(out.isox(), fname, sep="\t", row.names =FALSE, quote = FALSE)
#     }
#   )
#
# }
#
# # user interface ----
#
# module_isoxl_make_inputFile_ui <- function(id) {
#
#
#   # namespace
#   ns <- NS(id)
#
#   fluidPage(sidebarLayout(
#     sidebarPanel(
#       width = 3,
#
#       textInput(
#         inputId = "INPUT_IsoX_sumformula",
#         label = "Sum formula",
#         value = "NO3"
#       ),
#
#       numericInput(
#         inputId = "INPUT_IsoX_charge",
#         label = "Charge",-1,
#         min = -5,
#         max = 5,
#         step = 1,
#         width = NULL
#       ),
#
#       #radioButtons(inputId = "INPUT_IsoX_charge", label=NULL, choiceNames = c( "+1", "−1"), choiceValues = c( "1", "-1"), selected= "-1", inline = TRUE),
#
#       #submitButton("Update", icon("sync")),
#       actionButton("update", "Update", icon("sync")),
#
#
#
#       tags$hr(style = "border-color: grey;"),
#
#
#       #tags$hr(style="border-color: grey;"),
#
#       #h6("Define Signal"),
#
#       textInput(
#         inputId = "INPUT_IsoX_compound",
#         label = "Compound",
#         value = "nitrate"
#       ),
#
#       numericInput(
#         inputId = 'INPUT_IsoX_tolerance',
#         label = "Tolerance [mDa]",
#         0.3,
#         min = 0.1,
#         max = 3,
#         step = 0.1,
#         width = NULL
#       ),
#
#
#
#       with_tippy(
#         numericInput(
#           inputId = "INPUT_IsoX_threshold",
#           label = "Threshold [% of M0]",
#           0.0001,
#           min = 0.00001,
#           max = 10,
#           step = 0.0001,
#           width = NULL
#         ),
#         "Processing option: Threshold to include an isotopolog (in ppm abundance)",
#         placement = "right"
#       ),
#
#
#       #tags$hr(style="border-color: grey;"),
#       #h6("Override algorithm"), #add button/options?
#
#       numericInput(
#         inputId = "INPUT_IsoX_massoffset",
#         label = "Offset m/z [mDa]",
#         0,
#         min = -5,
#         max = 5,
#         step = 0.1,
#         width = NULL
#       ),
#
#       h6("The option below is not implemented yet"),
#       shinyWidgets::sliderTextInput(
#         "INPUT_IsoX_resolution",
#         "Orbitrap Resolution:" ,
#         choices = c("7.5k", "15k", "30k", "60k", "120k", "240k", "480k", "1M"),
#         #add further months
#         selected = c("15k")
#       ),
#     ),
#
#
#
#
#     # UI: Create IsoX Input Main Panel ----------------------------------------
#
#
#     mainPanel(
#       width = 9,
#
#
#       hr(),
#
#       downloadButton("download_IsoX", "Download Input File for IsoX"),
#
#       hr(),
#
#       box(
#         title = "Preview of isotoplogs.tsv",
#         width = "auto" ,
#         status = "primary",
#         solidHeader = T,
#         DT::dataTableOutput("isox")
#       ),
#
#
#     )
#   ))
# }
