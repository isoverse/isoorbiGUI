#' Orbitrap ShinyApp UI
#'
#' Generates the top-level GUI, rest is generated dynamically by \code{orbitrap_server}.
#'
#' @param app_title the title of the application
#' @param app_color the dashboard color, see \link[shinydashboard]{dashboardPage} skin for available options
#' @param sidebar_width the width of the left-side navigation sidebar
orbitrap_ui <- function(app_title = span(tagList(a(align = "left", href='https://www.isoverse.org/', tags$img(src='https://github.com/123caj/123caj.github.io/blob/main/isoorbi-draft.png?raw=true', height=40 ))), "IsoXL"), app_color = "blue", sidebar_width = NULL) {


  box_default <- "#2c3b41" # lighter



  # set spinner color
  options(spinner.color = app_color)

  dashboardPage(title="IsoXL", #Shown as title of browser window/ tab


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
          paste("Version", as.character(utils::packageVersion("isoorbi"))), #get isoorbi package version
          href = "https://github.com/isoverse/isoorbi", target = "_blank"),
          align = "center"),
        menuItem("Dual Inlet", tabName = "dual", icon = icon("grip-lines")),
        menuItem("Flow Injection", tabName = "flow", icon = icon("syringe")),
        menuItem("Single Analysis", tabName = "single", icon = icon("vial")),
        menuItem("Explore Data", tabName = "explore", icon = icon("eye")),
        menuItem("Shot Noise", tabName = "shot", icon = icon("bullseye")),
        tags$hr(style="border-color: grey;"),
        menuItem("Make IsoX Input File", tabName = "make_inputFile", icon = icon("wand-magic-sparkles")),
        menuItem("Contact", tabName = "info", icon = icon("circle-info")),
        tags$li(a(uiOutput("help", inline = TRUE)))
      ),

      # shinjs and shinydashboard extensions
      # Caj: Do we need to add dependencies here again, in addition to DESCRIPTION?
      shinyjs::useShinyjs(),
      shinyWidgets::useShinydashboard(),
      #shinyBS::addTooltip(),
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
          tabName = "dual",
          h4("Dual Inlet experiment"),
          module_isoxl_dual_inlet_ui(id = "dual_inlet")
        ),

        # UI: Flow Injection Analysis ----------------------------------------------

        tabItem(
          tabName = "flow",

          h4("Flow Injection experiments"),

          module_isoxl_flow_ui(id = "flow")

        ),

        # UI: Single Analysis -----------------------------------------------------

        tabItem(
          tabName = "single",

          h4("Single sample experiment"),

          module_isoxl_single_ui(id = "single")
        ),

        # UI: Shot Noise Analysis -------------------------------------------------

        tabItem(
          tabName = "shot",

          h4("Compare data to shot noise limits"),

          module_isoxl_shotnoise_ui(id = "shot_noise")
        ),

        # UI: Explore Data --------------------------------------------------------


        tabItem(tabName = "explore",

                h4("Explore MS data in more detail"),

                module_isoxl_explore_ui(id = "explore")

        ),

        # UI: Create an IsoX Input File ----------------------------------------------
        tabItem(
          tabName = "make_inputFile",

          h4("Create an input file for IsoX"),

          module_isoxl_make_inputFile_ui(id = "make_inputFile")

        ),

        # UI: Contact and info -------------------------------------------------

        tabItem(
          tabName = "info",

          h4("Contact"),

          module_isoxl_info_ui(id = "info")
        )

      )


    )
  )

}


