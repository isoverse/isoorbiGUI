#' Orbitrap ShinyApp Server
#'
#' Generates the server part of the shiny app.
#' @param data_folder path to the data folder, if none provided ask for the user to pick one if in local mode
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
#' @param reset wether to reset settings if running in local mode (server mode always resets)
orbitrap_server <- function(data_folder = NULL, local = FALSE, reset = FALSE) {

  shinyServer(function(input, output, session) {

    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      save_settings = local,
      settings_file = reactive("orbitrap_gui_settings.rds")
    )

    # FIXME: create data folder module that also coordinates what happens with the settings
    if (reset) settings$reset()
    else if (local) settings$load()

  })

}
