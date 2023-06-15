# Orbitrap ShinyApp Server
#
# Generates the server part of the shiny app.
#' @title orbitrap_server
#' @description Generates the server part of the shiny app.
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
#' @param reset whether to reset settings if running in local mode (server mode always resets)
#' @return ...
#' @export
orbitrap_server <- function(local = FALSE, reset = FALSE) {

  shinyServer(function(input, output, session) {

    gui_message("info", "starting isoXL GUI")


    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      save_settings = local,
      settings_file = reactive("orbitrap_gui_settings.rds")
    )

    # safely exit
    session$onSessionEnded(function() {
      # important to allow for automatic termination on session end if running locally
      if (local) {
        gui_message("info", "stopping app because session was terminated and we're running in local mode")
        stopApp()
      }
    })

    observeEvent(input$menu, {
      if (input$menu == "close") {
        gui_message("info", "closing window")
        shinyjs::js$closeWindow()
        if (local) {
          gui_message("info", "stopping app because close was pushed and we're running in local mode")
          stopApp()
        }
      }
    })

    # FIXME: create data folder module that also coordinates what happens with the settings
    if (reset) settings$reset()
    else if (local) settings$load()


    # Maximum File Size for Upload --------------------------------------------

    options(shiny.maxRequestSize=100*1024^2) #Otherwise maximum file upload is 5MB!


    # Server: Dual Inlet Analysis -----------------------------------------------------
    dual_inlet <- callModule(module_isoxl_dual_inlet_server, id = "dual_inlet")

    # Server: Single Analysis -----------------------------------------------------
    single <- callModule(module_isoxl_single_server, id = "single")

    # Server: Flow Injection Inlet Analysis -----------------------------------------------------
    flow <- callModule(module_isoxl_flow_server, id = "flow")

    # Server: CSIA LC/MS Analysis -----------------------------------------------------
    #csia <- callModule(module_isoxl_csia_server, id = "csia")

    # Server: Explore Data --------------------------------------------------------
    explore <- callModule(module_isoxl_explore_server, id = "explore")

    # Server: Shot Noise Analysis -------------------------------------------------
    shot_noise <- callModule(module_isoxl_shot_noise_server, id = "shot_noise")

    # Server: Create an IsoX Input File ----------------------------------------------
    make_inputFile <- callModule(module_isoxl_make_inputFile_server, id = "make_inputFile")

    # Server: Info and Contact -------------------------------------------------
    info <- callModule(module_isoxl_info_server, id = "info")



    })

}

