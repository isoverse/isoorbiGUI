# test ----
# test the info module as a standalone app
#' @title test_module_isoxl_info
#' @description ...
#' @param port ...
#' @return ...
#' @export
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
#' @title module_isoxl_info_server
#' @description ...
#' @param input ...
#' @param output ...
#' @param session ...
#' @return ...
#' @export
module_isoxl_info_server <- function(input, output, session) {
  # namespace
  ns <- session$ns
}

# user interface ----

#' @title module_isoxl_info_ui
#' @description ...
#' @param id ...
#' @return ...
#' @export
module_isoxl_info_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(


      h4("Contact Information"),

      hr(),

      h5("Caj Neubauer, Sebastian Kopf & Kristyna Kantnerova"), #FIXME: supply ascii codes for Czech characters
      tags$h1(""),
      h5("University of Colorado Boulder"),

      tags$h1(""),

      #FIX: remove dependency of mailtoR
      #enable custom email alias, e.g. "123caj+IsoXL@gmail.com"

      mailtoR::mailtoR(
        email = "123caj@gmail.com",
        text = "click here to send us an email",
        subject = "IsoXL",
        body = "Hi there, I have a question about IsoXL."
      ),

      mailtoR::use_mailtoR(),


    ),


    # UI: Empty Main Panel ----------------------------------------
    sidebarPanel(

      h4("Additional resources"),

      hr(),

      "File a bug report or feature request using the ",
      tags$a(target="_blank", href = "https://github.com/isoverse/isoorbi/issues",
             "GitHub page"),
      div(style="margin-bottom:10px"),
      "Information related to ",
      tags$a(target="_blank", href = " https://github.com/isoverse/isoorbi/blob/main/inst/extdata/description_testfiles.txt",
             "test data"),



      h5("Link to ",
      tags$a(target="_blank", href = "https://cajneubauer.weebly.com/uploads/1/0/3/7/103737032/isox_howto.pdf",
                             "IsoX how-to")),

      h5("Link to other ",
      tags$a(target="_blank", href = "https://isoorbi.shinyapps.io/gmis/",
             "small isotopocule apps")),

    )
  ),
  mainPanel())
}

