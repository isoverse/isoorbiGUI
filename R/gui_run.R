#' Run the user interface
#'
#' Provides a graphical user interface to explore Orbitrap data. Note that while the GUI is running, your R session will be blocked. To stop the GUI and return to your R session, either click the \code{Close} button inside the GUI navigation bar (if running it in non-server mode) or press \code{Esc} in your R session.
#'
#' @details \code{iso_start_orbitrap_gui} starts a personal GUI that is only accessible on the computer it is running on.
#'
#' @param data_folder path pointing to the folder where the data files are located. If none is provided when running a personal GUI, will ask to pick a folder on startup. Servers always require the data folder to be preset to ensure data security.
#' @param log whether to show log info messages or not. By default, shows logs when running in server mode (\code{iso_start_viewer_server}) and does not show them otherwise.
#' @param reset whether to reset GUI settings for the data folder to their defaults. This option is only available when running a personal GUI as servers do not save any settings. Resetting settings is usually only necessary if your GUI crashed unexpectedly and gets stuck during restart. Please consider reporting the details of the crash at https://github.com/isoverse/isoorbi/issues, especially if you can easily reproduce it.
#' @export
iso_start_orbitrap_gui <- function(data_folder = NULL, log = FALSE, reset = FALSE) {
  # launch viewer
  start_orbitrap_gui(
    data_folder = data_folder,
    local = TRUE,
    launch = TRUE,
    log = log,
    reset = reset
  )
}

#' @rdname iso_start_orbitrap_gui
#' @details \code{iso_start_orbitrap_gui_server} starts a server for the Orbitrap data GUI. If running it from an R session on a personal computer, it will be available in the local network at the computer's IP address, port 3838. For example, if you run it on your computer and your IP address is 192.168.0.42, other computers on your network can launch your GUI by pointing a browser at http://192.168.0.4:3838 (note that if your firewall or local network blocks port 3838, this will not be possible). If running on a shinyapps server such as shinyapps.io instead, you have to set \code{launch = FALSE} because the shinyapps server takes care of launching the app on demand.
#'
#' @param launch whether to launch the shiny server immediately (if starting the server from a local R instance) or only on request (if running on a server such as shinyApps.io).
#' @export
iso_start_orbitrap_gui_server <- function(data_folder, launch = TRUE, log = TRUE) {
  if (missing(data_folder))
    stop("Cannot start an Orbitrap GUI server without preset data folder.", call. = FALSE)
  start_orbitrap_gui(
    data_folder = data_folder,
    local = FALSE,
    launch = launch,
    log = log,
    reset = TRUE,
    launch.browser = FALSE,
    host = "0.0.0.0",
    port = 3838
  )
}

# Orbitrap GUI start function
# @param ... parameters passed on to runApp
start_orbitrap_gui <- function(data_folder, local, launch, log, reset, ...) {

  # check for knitting
  if (isTRUE(getOption('knitr.in.progress'))) {
    warning("cannot launch the GUI during knitting. If you would like to keep a call to iso_start_orbitrap_gui in your RMarkdown file and avoid this warning, make sure to set ```{r, eval = FALSE} in the options for this code chunk.", call. = FALSE, immediate. = TRUE)
    return(invisible(NULL))
  }

  stopifnot(!missing(data_folder))
  stopifnot(!missing(local))
  stopifnot(!missing(launch))
  stopifnot(!missing(log))
  stopifnot(!missing(reset))

  if (is.null(data_folder) && !local) {
    stop("cannot run in server mode without a data folder", call. = FALSE)
  } else if (!is.null(data_folder) && !dir.exists(data_folder)) {
    stop("data folder does not exist: ", data_folder, call. = FALSE)
  }

  # log messages
  if (log) turn_log_on()
  else turn_log_off()

  # make sure shinyBS on attach runs
  shinyBS:::.onAttach()

  # generate app
  app <- shinyApp(
    ui = orbitrap_ui(),
    server = orbitrap_server(
      data_folder = data_folder,
      local = local,
      reset = reset
    )
  )

  # launch if local
  if (launch) {
    runApp(app, display.mode = "normal", ...)
  } else {
    return(app)
  }
}


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

        tabItem(tabName = "dual_in",

                h4("Process data from a Dual Inlet experiment"),

                fluidPage(

                  sidebarLayout(

                    sidebarPanel(width = 3,

                                 fileInput("upload_Dual_Inlet", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                                 hr(),

                                 submitButton( "Update output", icon("sync")),

                                 #numericRangeInput("n", "Rows", value = c(1,5), min=1, max=100000),

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
        )
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


#' Setup the orbitrap user interface app
#'
#' Sets up the necessary files for launching the user interface in the target folder. This allows for easy running of the gui via \code{\link[shiny]{runApp}} or deployment as a server application (e.g. on shinyapps.io).
#' @param dir the directory where to create the launch scripts (absolute path or relative to your current working directory), default is 'app'
#' @param overwrite whether to overwrite existing launch files (default is FALSE)
#' @export
iso_setup_orbitrap_gui <- function(dir = "app", overwrite = FALSE) {

  if(!dir.exists(dir)) dir.create(dir, recursive = TRUE)

  paste0("Info: setting up orbitrap GUI launch files in folder '", dir, "'...") %>% message()

  # copy app.R template file
  message(" - creating 'app.R' launch script... ", appendLF = FALSE)
  file_exists <- file.exists(file.path(dir, "app.R"))
  if (!file_exists || overwrite) {
    cat(
      c(
        "# launch orbitrap GUI",
        "isoorbi::iso_start_orbitrap_gui_server(",
        "    data_folder = \"data\",",
        "    launch = FALSE,",
        "    log = TRUE",
        ")"
      ),
      sep = "\n",
      file = file.path(dir, "app.R"),
      append = FALSE
    )
  }
  if (file_exists && overwrite) message("existing file overwritten.")
  else if (file_exists) message("failed (file already exists, use overwrite = TRUE)")
  else message("complete")

  # data folder
  message(" - adding data folder... ", appendLF = FALSE)
  if(!dir.exists(file.path(dir, "data"))) dir.create(file.path(dir, "data"))
  message("complete.")

  # omit from github
  message(" - adding .gitignore... ", appendLF = FALSE)
  if (!file.exists(file.path(dir, ".gitignore")) || overwrite)
    cat("# exclude apps from github (by default)\n*\n\n# always exclude data\ndata/\n", file = file.path(dir, ".gitignore"), append = FALSE)
  message("complete.")

  # infos
  message(" -> run with shiny::runApp(\"", dir, "\")")
  message(" -> deploy with rsconnect::deployApp(\"", dir, "\")")
}


