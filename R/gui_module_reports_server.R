# test ----
# test the reports module as a standalone app

test_module_reports <- function(port = getOption("shiny.port")) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Reports Module Test"),
        module_reports_ui(id = "reports")
      ),
      server = function(input, output) {
        reports <-
          callModule(module_reports_server, id = "reports", local = TRUE)
      }
    )
  runApp(app, display.mode = "normal", port = port)
}


# orbitrap reports server
#
# generates the server part of the orbitrap reports GUI
# @param local whether it's running in local mode or on a server
module_reports_server <- function(input, output, session, local) {

  if (!local) stop("the orbitrap reports GUI is currently only supported for running on individual computers, not on a server.", call. = FALSE)

  # namespace
  ns <- session$ns

  # fixed parameters
  roots <- c(Home = fs::path_home(), `Working Directory` = ".")

  # reactive values
  values <- reactiveValues(
    folder_path = NULL,
    folder_isox_files = c()
  )

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
    module_message(ns, "info", "selected folder ", values$folder_path)
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
    module_message(ns, "info", "generating report for folder ", values$folder_path)
  })

}
