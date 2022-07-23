# orbitrap reports ui
#
# generates the user interface part of the orbitrap reports GUI
module_reports_ui <- function(id) {

  # namespace
  ns <- NS(id)

  # Define UI ----
  ui <- fluidPage(

    # set up shinyjs
    shinyjs::useShinyjs(),

    titlePanel("Orbitrap Reports Generator"),

    fluidRow(

      div(id = ns("loading-panel"), class = "message-panel",
          h2("Loading..."),
          p("Please wait, this may take a few seconds.")) %>% shinyjs::hidden(),
      div(id = ns("saving-panel"), class = "message-panel",
          h2("Saving..."),
          p("Please wait, this may take a few seconds.")) %>% shinyjs::hidden(),

      #  buttons
      column(
        width = 12,
        shinyFiles::shinyDirButton(
          id = ns('folder'), label = 'Select a folder', title = 'Please select your data folder',
          icon = icon("folder-open")
        )
      ),

      # folder info and generate button
      div(id = ns("folder_div"),
          column(12, strong("Folder: "), textOutput(ns("folder_path"))),
          column(12, strong("Info: "), textOutput(ns("folder_info"))),
          column(12, actionButton("generate", "Generate Report", icon = icon("cogs")))
      ) %>% shinyjs::hidden()

    )

  )

}
