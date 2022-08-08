# orbitrap reports ui
#
# generates the user interface part of the orbitrap reports GUI
#' @title orbitrap_reports_ui
#' @description generates the user interface part of the orbitrap reports GUI
#' @return ...
#' @export
orbitrap_reports_ui <- function() {

  # Define UI ----
  ui <- fluidPage(

    # set up shinyjs
    shinyjs::useShinyjs(),

    titlePanel("Orbitrap Reports Generator"),

    fluidRow(

      div(id = "loading-panel", class = "message-panel",
          h2("Loading..."),
          p("Please wait, this may take a few seconds.")) %>% shinyjs::hidden(),
      div(id = "saving-panel", class = "message-panel",
          h2("Saving..."),
          p("Please wait, this may take a few seconds.")) %>% shinyjs::hidden(),

      #  buttons
      column(
        width = 12,
        shinyFiles::shinyDirButton(
          'folder', label = 'Select a folder', title = 'Please select your data folder',
          icon = icon("folder-open")
        ),
        actionButton("close", "Close", icon = icon("window-close"))
      ),

      # folder info and generate button
      div(id = "folder_div",
          column(12, strong("Folder: "), textOutput("folder_path")),
          column(12, strong("Info: "), textOutput("folder_info")),
          column(12, actionButton("generate", "Generate Report", icon = icon("cogs")))
      ) %>% shinyjs::hidden()

    )

  )

}
