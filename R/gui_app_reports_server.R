# orbitrap reports server
#' @title orbitrap_reports_server
#' @description generates the server part of the orbitrap reports GUI
#' @param local whether it's running in local mode or on a server
#' @return ...
#' @export
orbitrap_reports_server <- function(local) {

  if (!local) stop("the orbitrap reports GUI is currently only supported for running on individual computers, not on a server.", call. = FALSE)

  # generate shiny server
  shinyServer(function(input, output, session) {

    # startup
    gui_message("info", "starting Orbitrap reports GUI")

    # fixed parameters
    roots <- c(Home = fs::path_home(), `Working Directory` = ".")

    # reactive values
    values <- reactiveValues(
      folder_path = NULL,
      folder_isox_files = c()
    )


    # close app
    observeEvent(input$close, {
      shinyjs::runjs("window.close();")
      stopApp()
    })

    # folder selection server
    shinyFiles::shinyDirChoose(
      input, 'folder',
      roots = roots,
      defaultRoot = "Home",
      filetypes=c('', 'isox'),
    )

    # folder selection
    observeEvent(input$folder, {
      req(is.list(input$folder))
      values$folder_path <- shinyFiles::parseDirPath(roots, input$folder)
      gui_message("info", "selected folder ", values$folder_path)
      values$folder_isox_files <- list.files(values$folder_path, pattern = "\\.isox")
    })

    # folder info update
    observeEvent(values$folder_path, {
      shinyjs::show("folder_div")
    })
    output$folder_path <- renderText({
      req(values$folder_path)
      values$folder_path
    })
    output$folder_info <- renderText({
      req(values$folder_path)
      sprintf("found %d .isox files", length(values$folder_isox_files))
    })

    # generate report
    observeEvent(input$generate, {
      req(values$folder_path)
      gui_message("info", "generating report for folder ", values$folder_path)
    })

  })

}
