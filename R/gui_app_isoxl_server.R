#' Orbitrap ShinyApp Server
#'
#' Generates the server part of the shiny app.
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
#' @param reset whether to reset settings if running in local mode (server mode always resets)
orbitrap_server <- function(local = FALSE, reset = FALSE) {

  shinyServer(function(input, output, session) {

    gui_message("info", "starting isoXL GUI")


    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      save_settings = local,
      settings_file = reactive("orbitrap_gui_settings.rds")
    )


    # FIXME: create data folder module that also coordinates what happens with the settings
    if (reset) settings$reset()
    else if (local) settings$load()



    # IsoX Version ------------------------------------------------------------

    isoXL.version <- "Version 0.2.1" #utils::packageVersion("isoorbi") #FIXME: get version information directly?

    # Maximum File Size for Upload --------------------------------------------

    options(shiny.maxRequestSize=100*1024^2) #Otherwise maximum file upload is 5MB!


    # Server: Dual Inlet Analysis -----------------------------------------------------
    dual_inlet <- callModule(module_isoxl_dual_inlet_server, id = "dual_inlet", isoXL.version = isoXL.version)

    # Server: Single Analysis -----------------------------------------------------
    single <- callModule(module_isoxl_single_server, id = "single", isoXL.version = isoXL.version)

    # Server: Flow Injection Inlet Analysis -----------------------------------------------------
    flow <- callModule(module_isoxl_flow_server, id = "flow",  isoXL.version = isoXL.version)

    # Server: Explore Data --------------------------------------------------------
    #explore <- callModule(module_isoxl_explore_server, id = "explore")

    # Data: Exploration -------------------------------------------------------



    data.explore <- reactive({

      req(input$upload_explore)


      ext <- tools::file_ext(input$upload_explore$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_explore$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(),
                                      compound = readr::col_factor(),
                                      isotopolog = readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file")
      )

    })


    # Data: Exploration -------------------------------------------------------



    data.explore <- reactive({

      req(input$upload_explore)


      ext <- tools::file_ext(input$upload_explore$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_explore$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(),
                                      compound = readr::col_factor(),
                                      isotopolog = readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file")
      )

    })

    # Plot: Exploration -------------------------------------------------------



    plot1vars <- callModule(varselect_mod_server, "plot1_vars")
    plot2vars <- callModule(varselect_mod_server, "plot2_vars")

    dat <- callModule(



      scatterplot_mod_server,
      "plots",
      dataset = data.explore(), #somehow this gets not updated when new isox file is loaded!!??
      plot1vars = plot1vars,
      plot2vars = plot2vars,
      label = reactive({ input$label })
    )

    callModule(dataviewer_mod_server, "dataviewer", dat)


    plot3vars <- callModule(varselect_mod_server, "plot3_vars")
    plot4vars <- callModule(varselect_mod_server, "plot4_vars")

    dat2 <- callModule(
      densityplot_mod_server,
      "plots",
      dataset = data.explore(),
      plot3vars = plot3vars,
      plot4vars = plot4vars,
      label = reactive({ input$label })
    )

    callModule(dataviewer_mod_server, "dataviewer", dat2)

    # Server: Shot Noise Analysis -------------------------------------------------
    shot_noise <- callModule(module_isoxl_shot_noise_server, id = "shot_noise")

    # Server: Create an IsoX Input File ----------------------------------------------
    make_inputFile <- callModule(module_isoxl_make_inputFile_server, id = "make_inputFile")

    # Server: Contact and info -------------------------------------------------
    info <- callModule(module_isoxl_info_server, id = "info")


    })

}

