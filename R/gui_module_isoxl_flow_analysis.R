# test ----
# test the flow injection analysis module as a standalone app

test_module_isoxl_flow <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Flow Injection Analysis Module Test"),
        module_isoxl_flow_ui(id = "flow")
      ),
      server = function(input, output) {
        flow <-
          callModule(module_isoxl_flow_server, id = "flow")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_flow_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)
}


# user interface ----

module_isoxl_flow_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout())
}
