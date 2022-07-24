# test ----
# test the info module as a standalone app

test_module_isoxl_info <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Info Module Test"),
        module_isoxl_info_ui(id = "info")
      ),
      server = function(input, output) {
        info <-
          callModule(module_isoxl_info_server, id = "info")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_info_server <- function(input, output, session) {
  # namespace
  ns <- session$ns
}

# user interface ----

module_isoxl_info_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(


      #h4("Contact and Infos"),

      h5("Caj Neubauer, Sebastian Kopf & Kristyna Kantnerova"), #FIXME: supply ascii codes for Czech characters
      tags$h1(""),
      h5("University of Colorado Boulder"),

      tags$h1(""),

      #FIX: remove dependency of mailtoR
      #enable custom email alias, e.g. "123caj+IsoXL@gmail.com"

      mailtoR(
        email = "123caj@gmail.com",
        text = "click here to send us an email",
        subject = "IsoXL",
        body = "Hi there, I have a question about IsoXL."
      ),

      use_mailtoR(),

      hr(),

      "File a bug report or feature request ",
      tags$a(href = "https://github.com/isoverse/isoorbi/issues",
             "on our GitHub page")

    ),


    # UI: Empty Main Panel ----------------------------------------
    mainPanel()
  ))
}

