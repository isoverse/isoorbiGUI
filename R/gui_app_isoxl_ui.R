

#' Orbitrap ShinyApp UI
#'
#' Generates the top-level GUI, rest is generated dynamically by \code{orbitrap_server}.
#'
#' @param app_title the title of the application
#' @param app_color the dashboard color, see \link[shinydashboard]{dashboardPage} skin for available options
#' @param sidebar_width the width of the left-side navigation sidebar
orbitrap_ui <- function(app_title = "Orbitrap Data Explorer", app_color = "blue", sidebar_width = NULL) {

  #box_default <- "#222d32" # darker
  box_default <- "#2c3b41" # ligther

  # set spinner color
  options(spinner.color = app_color)

  dashboardPage(
    # SKIN ----
    skin = app_color,

    # HEADER ----
    dashboardHeader(title = app_title, titleWidth = sidebar_width),

    # SIDEBAR ----
    dashboardSidebar(

      width = sidebar_width,

      # menu
      sidebarMenu(
        id = "menu",
        h5(a(
          paste("Version", as.character(utils::packageVersion("isoorbi"))),
          href = "https://github.com/isoverse/isoorbi", target = "_blank"),
          align = "center"),
        menuItem("Dual Inlet", tabName = "dual_in", icon = icon("grip-lines")),
        menuItem("Flow Injection", tabName = "flow_in", icon = icon("syringe")),
        menuItem("Single Analysis", tabName = "single_in", icon = icon("vial")),
        menuItem("Explore Data", tabName = "explore_in", icon = icon("eye")),
        menuItem("Shot Noise", tabName = "shot_in", icon = icon("bullseye")),
        tags$hr(style="border-color: grey;"),
        menuItem("Make IsoX Input File", tabName = "make_in", icon = icon("magic")),
        menuItem("Contact", tabName = "info_in", icon = icon("info-circle")),
        tags$li(a(uiOutput("help", inline = TRUE)))
      ),

      # shinjs and shinydashboard extensions
      # Caj: Do we need to add dependencies here again, in addition to DESCRIPTION?
      shinyjs::useShinyjs(),
      shinyWidgets::useShinydashboard(),
      #shinycssloaders::withSpinner(),
      #tippy::with_tippy(),


      # css headers
      tags$head(
        tags$style(
          type="text/css",
          HTML(paste(
            # error validation output
            #".shiny-output-error-validation { color: red; font-size: 16px; }", # do we want this red?
            ".shiny-output-error-info { color: black; font-size: 20px; padding: 20px; }",
            # adjust sidebar height
            #".sidebar {height: 2000px}", # FIXME: make this dynamically long enough
            # body top padding
            ".box-body {padding-top: 5px; padding-bottom: 0px}",
            # pads on shiny items
            ".form-group, .selectize-control {margin-bottom: 0px;}",
            # custom background box
            sprintf(".box.box-solid.box-info>.box-header{color:#fff; background: %s; background-color: %s;}", box_default, box_default),
            sprintf(".box.box-solid.box-info{border:1px solid %s;}", box_default),
            sep="\n"))
        )
      )

    ),

    # BODY ====
    dashboardBody(

      #div(class = "row",

      tabItems(

        # UI: Dual Inlet ----------------------------------------------------------

        tabItem(
          tabName = "dual_in",
          h4("Process data from a Dual Inlet experiment"),
          module_isoxl_dual_inlet_ui(id = "dual_inlet")
        ),

        tabItem(tabName = "flow_in",

                h4("Process data from flow injection experiments"),

                fluidPage(

                  sidebarLayout(

                    sidebarPanel(width = 3,



                                 fileInput("upload_Flow_Injection", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                                 hr(),

                                 submitButton( "Update output", icon("sync")),

                                 tags$hr(style="border-color: grey;"),

                                 h6("Define Parameters"),

                                 # textInput(inputId = "INPUT_Flow_sample", label="Sample", value = "sample"),

                                 textInput(inputId = "INPUT_Flow_Basepeak", label="Base peak", value = "M0"), #change back to M0 as default


                                 shinyWidgets::numericRangeInput("INPUT_Flow_timerange", "Time [min]", value = c(1,10), min=0, max=100),


                                 tags$hr(style="border-color: grey;"),

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

                                 with_tippy(numericInput(
                                   inputId = "INPUT_Flow_Rare",
                                   label = "Remove weak isotopologs",
                                   30,
                                   min = 1,
                                   max = 50,
                                   step = 1,
                                   width = NULL
                                 ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),

                                 with_tippy(numericInput(
                                   inputId = "INPUT_Flow_TICxIT",
                                   label = "Remove outlier scans",
                                   1,
                                   min = 0.1,
                                   max = 10,
                                   step = 0.1,
                                   width = NULL
                                 ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right"),

                                 radioButtons(inputId = "INPUT_Flow_RatioMethod", label="Ratio method", choiceNames = c( "sum",  "mean", "median", "geometric"), choiceValues = c( "sum",  "mean", "median", "geometric.mean"), selected= "sum"),

                                 #radioButtons(inputId = "INPUT_Flow_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
                                 radioButtons(inputId = "INPUT_Flow_IONS", label="Ratio input", choiceNames = c( "ions"), choiceValues = c( "ions.incremental"), selected=  "ions.incremental"), #intensity is not yet implemented


                    ),


                    # UI: Flow Injection Main Panel -------------------------------------------


                    mainPanel(width = 9,


                              #hr(),

                              downloadButton("download_Flow_xlsx", "Download Results (.xlsx)"),

                              hr(),


                              box(title = "Overview of Results", width = "auto" , height="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput("plot.flow"))), #how to save png at higher resolution?


                              hr(),


                              box(title = "Results Table", width = "auto" ,status = "primary",solidHeader = T, DT::dataTableOutput("summary.flow")),

                              hr(),


                              box(title = "Filtered .isox File", width = "auto" ,status = "primary",solidHeader = T, DT::dataTableOutput("mytable.flow")),





                    )
                  )
                )
        ),

        # UI: Single Analysis -----------------------------------------------------

        tabItem(tabName = "single_in",

                h4("Analyze a single sample analysis"),

                fluidPage(

                  sidebarLayout(

                    sidebarPanel(width = 3,



                                 fileInput("upload_Single_Analysis", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                                 hr(),

                                 submitButton( "Update output", icon("sync")),

                                 tags$hr(style="border-color: grey;"),

                                 h6("Define Single Analysis"),

                                 textInput(inputId = "INPUT_Single_sample", label="Sample", value = "sample"),

                                 textInput(inputId = "INPUT_Single_Basepeak", label="Base peak", value = "15N"), #change back to M0 as default


                                 shinyWidgets::numericRangeInput("INPUT_Single_timerange", "Time [min]", value = c(1,10), min=0, max=100),


                                 tags$hr(style="border-color: grey;"),

                                 h6("Processing options:"),

                                 # Hover over: with_tippy(textInput("input", "input with tooltip"), "Input text", placement = "right")

                                 # with_tippy(numericInput(
                                 #   inputId = "INPUT_Single_Segments",
                                 #   label = "Segments",
                                 #   1,
                                 #   min = 1,
                                 #   max = 10,
                                 #   step = 1,
                                 #   width = NULL
                                 # ), "Processing option: Split blocks into multiple segments", placement = "right"), #Not consistently working...

                                 with_tippy(numericInput(
                                   inputId = "INPUT_Single_Rare",
                                   label = "Remove weak isotopologs",
                                   30,
                                   min = 1,
                                   max = 50,
                                   step = 1,
                                   width = NULL
                                 ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),

                                 with_tippy(numericInput(
                                   inputId = "INPUT_Single_TICxIT",
                                   label = "Remove outlier scans",
                                   1,
                                   min = 0.1,
                                   max = 10,
                                   step = 0.1,
                                   width = NULL
                                 ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right"),

                                 radioButtons(inputId = "INPUT_Single_RatioMethod", label="Ratio method", choiceNames = c( "sum",  "mean", "median", "geometric"), choiceValues = c( "sum",  "mean", "median", "geometric.mean"), selected= "sum"),

                                 #radioButtons(inputId = "INPUT_Single_IONS", label="Ratio input", choiceNames = c( "ions",  "intensity"), choiceValues = c( "ions.incremental",  "intensity"), selected=  "ions.incremental"),
                                 radioButtons(inputId = "INPUT_Single_IONS", label="Ratio input", choiceNames = c( "ions"), choiceValues = c( "ions.incremental"), selected=  "ions.incremental"), #intensity is not yet implemented


                    ),


                    # UI: Single Analysis Main Panel ------------------------------------------

                    mainPanel(width = 9,


                              #hr(),

                              downloadButton("download_Single_xlsx", "Download Results (.xlsx)"),

                              hr(),


                              box(title = "Overview of Results", width = "auto" , height="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput("plot.single"))), #how to save png at higher resolution?


                              hr(),


                              box(title = "Results Table", width = "auto" ,status = "primary",solidHeader = T, DT::dataTableOutput("summary.single")),

                              hr(),


                              box(title = "Filtered .isox File", width = "auto" ,status = "primary",solidHeader = T, DT::dataTableOutput("mytable.single")),


                    )
                  )
                )
        ),

        # UI: Shot Noise Analysis -------------------------------------------------

        tabItem(tabName = "shot_in",

                h4("Analyze the observed precision of a ratio versus shot noise limits"),

                module_isoxl_shotnoise_ui(id = "shot_noise")
        ),

        # UI: Explore Data --------------------------------------------------------


        tabItem(tabName = "explore_in",

                h4("Explore MS data in more detail"),

                fluidPage(

                  sidebarLayout(

                    sidebarPanel(width=3,



                                 fileInput("upload_explore", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                                 hr(),

                                 submitButton("Update output", icon("sync")),

                                 #actionButton("action_explore", "Explore isox data"),

                                 tags$hr(style="border-color: grey;"),



                                 h4("Plot 1"),

                                 wellPanel(varselect_mod_ui("plot1_vars")),


                                 h4("Plot 2"),

                                 wellPanel(varselect_mod_ui("plot2_vars")),

                                 h4("Plot 3"),

                                 wellPanel(varselect_mod_density_ui("plot3_vars")),



                                 h4("Plot 4"),
                                 wellPanel(varselect_mod_density_ui("plot4_vars"))


                    ),


                    # UI: Explore Data Main Panel ---------------------------------------------


                    mainPanel(width=9,

                              h5("Please note: When you upload a second .isox file, the plots currently are not loading the new data. Reload the website to explore a different .isox file."),

                              hr(),
                              #titlePanel("Explore IsoX File"),

                              #verbatimTextOutput(outputId="test", placeholder = TRUE), ##temp


                              box(title = "Interactive plots of MS data (select points and update display to show in table)", width = "auto" , height="auto", status = "primary", solidHeader = T, scatterplot_mod_ui("plots"),
                                  checkboxInput("label", "Label selected points?", value = FALSE)),


                              hr(),



                              box(title = "Table of selected data points", width = "auto" , height="auto", status = "primary", solidHeader = T, dataviewer_mod_ui("dataviewer")),


                              box(title = "Density plots", width = "auto" , height="auto", status = "primary", solidHeader = T, densityplot_mod_ui("plots"))


                    )
                  )
                )

        ),

        # UI: Create IsoX Input ---------------------------------------------------
        tabItem(tabName = "make_in",

                h4("Create an IsoX input file for your project"),

                fluidPage(

                  sidebarLayout(

                    sidebarPanel(width = 3,

                                 textInput(inputId = "INPUT_IsoX_sumformula", label="Sum formula", value = "NO3"),

                                 numericInput(
                                   inputId = "INPUT_IsoX_charge",
                                   label = "Charge",
                                   -1,
                                   min = -5,
                                   max = 5,
                                   step = 1,
                                   width = NULL
                                 ),

                                 #radioButtons(inputId = "INPUT_IsoX_charge", label=NULL, choiceNames = c( "+1", "−1"), choiceValues = c( "1", "-1"), selected= "-1", inline = TRUE),

                                 submitButton( "Update", icon("sync")),

                                 tags$hr(style="border-color: grey;"),


                                 #tags$hr(style="border-color: grey;"),

                                 #h6("Define Signal"),

                                 textInput(inputId = "INPUT_IsoX_compound", label="Compound", value = "nitrate"),

                                 numericInput(
                                   inputId = 'INPUT_IsoX_tolerance',
                                   label = "Tolerance [mDa]",
                                   0.3,
                                   min = 0.1,
                                   max = 3,
                                   step = 0.1,
                                   width = NULL
                                 ),



                                 with_tippy(numericInput(
                                   inputId = "INPUT_IsoX_threshold",
                                   label = "Threshold [% of M0]",
                                   0.0001,
                                   min = 0.00001,
                                   max = 10,
                                   step = 0.0001,
                                   width = NULL),
                                   "Processing option: Threshold to include an isotopolog (in ppm abundance)", placement = "right"),





                                 #tags$hr(style="border-color: grey;"),
                                 #h6("Override algorithm"), #add button/options?

                                 numericInput(
                                   inputId = "INPUT_IsoX_massoffset",
                                   label = "Offset m/z [mDa]",
                                   0,
                                   min = -5,
                                   max = 5,
                                   step = 0.1,
                                   width = NULL
                                 ),

                                 h6("The option below is not implemented yet"),
                                 shinyWidgets::sliderTextInput("INPUT_IsoX_resolution","Orbitrap Resolution:" ,
                                                 choices = c("7.5k", "15k", "30k", "60k", "120k", "240k", "480k","1M"), #add further months
                                                 selected = c("15k")
                                 ),
                    ),




                    # UI: Create IsoX Input Main Panel ----------------------------------------


                    mainPanel(width = 9,


                              hr(),

                              downloadButton("download_IsoX", "Download Input File for IsoX"),

                              hr(),




                              box(title = "Preview of isotoplogs.tsv", width = "auto" ,status = "primary",solidHeader = T, DT::dataTableOutput("isox")),





                    )
                  )
                )
        ),

        # UI: Contact and info -------------------------------------------------

        tabItem(tabName = "info_in",

                h4("Contact and infos"),

                module_isoxl_info_ui(id = "info_in")
        ),

        # UI: Contact and Info ---------------------------------------------------


        # tabItem(tabName = "info_in",
        #
        #
        #         fluidPage(
        #
        #           sidebarLayout(
        #
        #
        #
        #             sidebarPanel(
        #
        #               h4("Contact and infos"),
        #
        #               tags$hr(style="border-color: grey;"),
        #               h4("Caj Neubauer"),
        #               h4("University of Colorado Boulder"),
        #
        #
        #               mailtoR(email = "123caj@gmail.com",
        #                       text = "click here to send me an email",
        #                       subject = "IsoXL",
        #                       body = "Hi Caj, I have a question or bug report for IsoXL."),
        #
        #
        #               use_mailtoR()
        #
        #             ),
        #
        #
        #             # UI: Contact and Infos Main Panel ----------------------------------------
        #
        #             mainPanel(
        #
        #
        #             )
        #           )
        #         )
        # )

      )

          #   tabItem("data", div(id = "data-panel", column(width = 12, uiOutput("data") %>% withSpinner(type = 5, proxy.height = "450px")))),
          #   tabItem("devices", div(id = "devices-panel", column(width = 12, uiOutput("devices")))),
          #   tabItem("experiments", div(id = "experiments-panel", column(width = 12, uiOutput("experiments")))),
          #   tabItem("live", div(id = "live-panel", column(width = 12, uiOutput("live"))))
          # )
      #)
    )
  )

}


