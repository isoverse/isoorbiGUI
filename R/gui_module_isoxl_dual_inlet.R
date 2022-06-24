# server
module_isoxl_dual_inlet_server <- function(input, output, session) {

  # namespace
  ns <- session$ns



}

# user interface
module_isoxl_dual_inlet_ui <- function(id) {
  # namespace
  ns <- NS(id)

  fluidPage(

    sidebarLayout(

      sidebarPanel(width = 3,

                   fileInput("upload_Dual_Inlet", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                   hr(),

                   submitButton( "Update output", icon("sync")),

                   #shinyWidgets::numericRangeInput("n", "Rows", value = c(1,5), min=1, max=100000),

                   tags$hr(style="border-color: grey;"),

                   h6("Define Dual Inlet"),

                   textInput(inputId = "INPUT_Sample", label="Sample", value = "sample"),
                   textInput(inputId = "INPUT_Ref", label="Reference", value = "refmat"),
                   textInput(inputId = "INPUT_Basepeak", label="Base peak", value = "15N"), #change back to M0 as default

                   tags$hr(style="border-color: grey;"),
                   h6("Parameters"),

                   numericInput(
                     inputId = 'INPUT_Blocks',
                     label = "Blocks",
                     7,
                     min = 1,
                     max = 100,
                     step = 1,
                     width = NULL
                   ),

                   numericInput(
                     inputId = "INPUT_Block.time",
                     label = "Block time [min]",
                     10,
                     min = 1,
                     max = 20,
                     step = 1,
                     width = NULL
                   ),

                   numericInput(
                     inputId = "INPUT_Idle.time",
                     label = "Switch time [min]",
                     0.5,
                     min = 0,
                     max = 2,
                     step = 0.1,
                     width = NULL
                   ),

                   numericInput(
                     inputId = "INPUT_Startup.time",
                     label = "Startup time [min]",
                     0,
                     min = 0,
                     max = 10,
                     step = 0.1,
                     width = NULL
                   ),

                   tags$hr(style="border-color: grey;"),

                   h6("Processing options:"),

                   # Hover over: with_tippy(textInput("input", "input with tooltip"), "Input text", placement = "right")

                   with_tippy(numericInput(
                     inputId = "INPUT_Segments",
                     label = "Segments",
                     1,
                     min = 1,
                     max = 10,
                     step = 1,
                     width = NULL
                   ), "Processing option: Split blocks into multiple segments", placement = "right"), #Not consistently working...

                   with_tippy(numericInput(
                     inputId = "INPUT_Rare",
                     label = "Remove weak isotopologs",
                     30,
                     min = 1,
                     max = 50,
                     step = 1,
                     width = NULL
                   ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),

                   with_tippy(numericInput(
                     inputId = "INPUT_TICxIT",
                     label = "Remove outlier scans",
                     1,
                     min = 0.1,
                     max = 10,
                     step = 0.1,
                     width = NULL
                   ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right"),

                   radioButtons(inputId = "INPUT_RatioMethod", label="Ratio method", choiceNames = c( "sum",  "mean", "median", "geometric"), choiceValues = c( "sum",  "mean", "median", "geometric.mean"), selected= "sum"),

                   #radioButtons(inputId = "INPUT_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
                   radioButtons(inputId = "INPUT_IONS", label="Ratio input", choiceNames = c( "ions"), choiceValues = c( "ions.incremental"), selected=  "ions.incremental"), #temp: intensity is not yet implemented
      ),


      # UI: Dual Inlet Main Panel -----------------------------------------------


      mainPanel(width = 9,


                #hr(),

                downloadButton("download_Dual_Inlet_xlsx", "Download Results (.xlsx)"),

                hr(),


                box(title = "Overview of Results", width = "auto" ,  height ="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput("plot"))), #height="auto",


                hr(),


                box(title = "Results Table", width = "auto" ,status = "primary", solidHeader = T, DT::dataTableOutput("summary")),

                hr(),


                box(title = "Filtered .isox File", width = "auto" ,status = "primary", solidHeader = T, DT::dataTableOutput("mytable")),


      )
    )
  )

}
