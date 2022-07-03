# test ----
# test the data exploration module as a standalone app

test_module_isoxl_explore <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Data Exploration Module Test"),
        module_isoxl_explore_ui(id = "explore")
      ),
      server = function(input, output) {
        explore <-
          callModule(module_isoxl_explore_server, id = "explore")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}

# server ----

module_isoxl_explore_server <- function(input, output, session) {

  # namespace
  ns <- session$ns

  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)

  # Data: Exploration -------------------------------------------------------



  data.explore <- reactive({

    req(input$upload_explore)


    ext <- tools::file_ext(input$upload_explore$name)
    switch(ext,
           isox = readr::read_tsv(input$upload_explore$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                  col_types = list(
                                    filename = readr::col_factor(),
                                    compound = readr::col_factor(),
                                    isotopolog = readr::col_factor()
                                  ) ),


           validate("Invalid file; Please upload a .isox file")
    )

  })


  # Plot: Exploration -------------------------------------------------------



  plot1vars <- callModule(varselect_mod_server, "plot1_vars")
  plot2vars <- callModule(varselect_mod_server, "plot2_vars")

  dat <- callModule(



    scatterplot_mod_server,
    "plots",
    dataset = data.explore(), #somehow this gets not updated when new isox file is loaded!!??
    plot1vars = plot1vars,
    plot2vars = plot2vars,
    label = reactive({ input$label })
  )

  callModule(dataviewer_mod_server, "dataviewer", dat)


  plot3vars <- callModule(varselect_mod_server, "plot3_vars")
  plot4vars <- callModule(varselect_mod_server, "plot4_vars")

  dat2 <- callModule(
    densityplot_mod_server,
    "plots",
    dataset = data.explore(),
    plot3vars = plot3vars,
    plot4vars = plot4vars,
    label = reactive({ input$label })
  )

  callModule(dataviewer_mod_server, "dataviewer", dat2)









  # Plot output: Data Exploration -------------------------------------------


  # output$plot.explore <- renderPlotly({
  #
  #   ggplotly(
  #
  #     ggplot(data = data.explore(), aes(x = scan.no, y = it.ms)) +
  #       scale_color_brewer(palette="Dark2") +
  #       geom_point(size=3, shape=10)
  #   )
  #
  #
  # })




  #####






  ####



  # Output table: Data Exploration ------------------------------------------

  #temp: is this sectionstill active at all now?

  # output$table.explore = DT::renderDataTable({
  #   datatable( data.explore(), rownames= FALSE ,
  #              extensions = c('Buttons','ColReorder', 'Select'),
  #              options = list(
  #                scrollX = TRUE, #scroll bar to see enture table
  #                dom = 'Bfrtip',
  #                buttons = c('colvis','copy',  'excel', 'pdf' ),
  #                colReorder = TRUE#, #columns can be reordered,
  #              )
  #   )
  #
  # })


}

# user interface ----

module_isoxl_explore_ui <- function(id) {


  # namespace
  ns <- NS(id)

  fluidPage(

    sidebarLayout(

      sidebarPanel(width=3,



                   fileInput("upload_explore", label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),

                   hr(),

                   submitButton("Update output", icon("sync")),

                   #actionButton("action_explore", "Explore isox data"),

                   tags$hr(style="border-color: grey;"),



                   h4("Plot 1"),

                   wellPanel(varselect_mod_ui("plot1_vars")),


                   h4("Plot 2"),

                   wellPanel(varselect_mod_ui("plot2_vars")),

                   h4("Plot 3"),

                   wellPanel(varselect_mod_density_ui("plot3_vars")),



                   h4("Plot 4"),
                   wellPanel(varselect_mod_density_ui("plot4_vars"))


      ),


      # UI: Explore Data Main Panel ---------------------------------------------


      mainPanel(width=9,

                h5("Please note: When you upload a second .isox file, the plots currently are not loading the new data. Reload the website to explore a different .isox file."),

                hr(),
                #titlePanel("Explore IsoX File"),

                #verbatimTextOutput(outputId="test", placeholder = TRUE), ##temp


                box(title = "Interactive plots of MS data (select points and update display to show in table)", width = "auto" , height="auto", status = "primary", solidHeader = T, scatterplot_mod_ui("plots"),
                    checkboxInput("label", "Label selected points?", value = FALSE)),


                hr(),



                box(title = "Table of selected data points", width = "auto" , height="auto", status = "primary", solidHeader = T, dataviewer_mod_ui("dataviewer")),


                box(title = "Density plots", width = "auto" , height="auto", status = "primary", solidHeader = T, densityplot_mod_ui("plots"))


      )
    )
  )
}


