library(shiny)

# generate code for navbar runctions
code_navbar_shinyjs_extension <- function() {
  js_code <- c(
    # close window
    closeWindow =
      "shinyjs.closeWindow = function() { window.close(); }"
    )
  tagList(
    shinyjs::extendShinyjs(text = paste(js_code, collapse = "\n"), functions = names(js_code))
  )
}

ui <- fluidPage(
  titlePanel(
    title = tags$head(tags$link(rel = "icon", type = "image/gif",
    href = "icon.png")),
    "Test App"
  ),
  shinyjs::useShinyjs(),
  code_navbar_shinyjs_extension(),
  h1("Standalone shiny app says hello!"),
  actionButton("close", label = "Close App")
)

server <- function(input, output, session) {

  # important to allow for automatic termination on session end
  session$onSessionEnded(function() {
    stopApp()
  })

  observeEvent(input$close, {
    message("closing app")
    shinyjs::js$closeWindow()
    stopApp()
  })

}

shinyApp(ui, server)
