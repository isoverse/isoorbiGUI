# test ----
# test the single analysis module as a standalone app

test_module_isoxl_single <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Single Analysis Module Test"),
        module_isoxl_single_ui(id = "single")
      ),
      server = function(input, output) {
        single <-
          callModule(module_isoxl_single_server, id = "single")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_single_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
}


# user interface ----

module_isoxl_single_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout())
}
