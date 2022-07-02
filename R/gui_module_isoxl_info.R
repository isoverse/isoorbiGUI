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
        info_in <-
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

          fluidPage(

            sidebarLayout(



              sidebarPanel(

                h4("Contact and infos"),

                tags$hr(style="border-color: grey;"),
                h4("Caj Neubauer"),
                h4("University of Colorado Boulder"),


                mailtoR(email = "123caj@gmail.com",
                        text = "click here to send me an email",
                        subject = "IsoXL",
                        body = "Hi Caj, I have a question or bug report for IsoXL."),


                use_mailtoR()

              ),


              # UI: Contact and Infos Main Panel ----------------------------------------

              mainPanel(


              )
            )
          )


}

