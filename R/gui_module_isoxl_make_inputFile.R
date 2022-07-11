# test ----
# test the make_inputFile module as a standalone app

test_module_isoxl_make_inputFile <- function(port = 5000) {
  app <-
    shinyApp(
      ui = fluidPage(
        shinyjs::useShinyjs(),
        h1("Make IsoX input file Module Test"),
        module_isoxl_make_inputFile_ui(id = "make_inputFile")
      ),
      server = function(input, output) {
        make_inputFile <-
          callModule(module_isoxl_make_inputFile_server, id = "make_inputFile")
      }
    )
  runApp(app, display.mode = "normal", port = 5000)
}


# server ----

module_isoxl_make_inputFile_server <- function(input, output, session) {
  # namespace
  ns <- session$ns

  shinyjs::hide("input-options")
  shinyjs::hide("main-plot")

  #Option to change the compound name (default is sum formula)
  observe({
    x = input$INPUT_IsoX_sumformula

      updateTextInput(session, "INPUT_IsoX_compound",
                      label = "Compound name",
                      value = x
      )

  })


  # increase upload file size
  options(shiny.maxRequestSize = 100 * 1024 ^ 2)



  isoXL.version <- utils::packageVersion("isoorbi") #FIXME: get version information directly?

  data(isotopes, package = "enviPat") #needed for isopat function

  observeEvent(input$start, {

    shinyjs::show("input-options")
    shinyjs::show("main-plot")

    # Output table: Make IsoX Input  ------------------------------------------

    INPUT_formula.check <-
      run_check(INPUT_formula = input$INPUT_IsoX_sumformula)

    pattern <-  isopattern(
        isotopes = isotopes,
        chemforms = INPUT_formula.check,
        threshold =  input$INPUT_IsoX_threshold,
        plotit = FALSE,
        charge = input$INPUT_IsoX_charge, #eg 1 or -1
        emass = 0.00054858,
        algo = 1,
        rel_to = 4,
        verbose = TRUE,
        return_iso_calc_amount = FALSE
      )

    #FIX: add back in some validation for sum formula, error messages

    # validate(
    #   need(sum(as.data.frame(check_chemform(isotopes, chemforms= INPUT_formula.check(), get_list=TRUE))) <= 200, 'Sum formula has unusually many atoms (>200)')
    # )

    #     validate(
    #       need(data.out[1] != "error", 'ERROR: cannot parse this sum formula with isopattern function') #example: NO00; not caught by check_chemform; still issues with P10; Si10 check validation/ code more
    #     ) #temp

    # Adapt for more than one compound?
    #     # validate(
    #     #   need(1>2, c("","Thanks for testing this feature. Hope to have it implemented soon."))
    #     # )
    #     #

    out.isox <- make.isox.input(pattern.list = pattern, #temp!
                                          INPUT_compound = input$INPUT_IsoX_compound,
                                          INPUT_charge = input$INPUT_IsoX_charge,
                                          INPUT_tolerance= input$INPUT_IsoX_tolerance,
                                          INPUT_massOffset = input$INPUT_IsoX_massoffset/1000) #mDa!



# Preview isotopologs.tsv --------------------------------------------------

    output$isox = DT::renderDataTable({

      datatable( out.isox, rownames= FALSE ,
                 extensions = c('Buttons','ColReorder', 'Select'),
                 options = list(
                   scrollX = TRUE, #scroll bar to see enture table
                   dom = 'Bfrtip',
                   buttons = list( #temp: option to show all rows of table (for copy & paste); show more rows in other tables also?
                     list(
                       extend = "collection",
                       text = 'Show All',
                       action = DT::JS("function ( e, dt, node, config ) {
                                      dt.page.len(-1);
                                      dt.ajax.reload();
                                  }")
                     ),
                     list(extend = 'copy')
                   )
                 )
      )

    })


    # Download isotopologs.tsv ------------------------------------------------

    output$download_IsoX <- downloadHandler(
      filename = function(){"isotopologs.tsv"},
      content = function(fname){
        write.table(out.isox, fname, sep="\t", row.names =FALSE, quote = FALSE)
      }
    )

    })

}


# user interface ----

module_isoxl_make_inputFile_ui <- function(id) {

  # namespace
  ns <- NS(id)

  fluidPage(sidebarLayout(
    sidebarPanel(
      width = 3,

      textInput(
        inputId = ns("INPUT_IsoX_sumformula"),
        label = "Sum formula",
        value = "NO3"
      ),

      numericInput(
        inputId = ns("INPUT_IsoX_charge"),
        label = "Charge",-1,
        min = -5,
        max = 5,
        step = 1,
        width = NULL
      ),


      div(
        id = ns("input-options"),

        tags$hr(style = "border-color: grey;"),

        h4("Options for fine tuning:"),

        textInput(
          inputId = ns("INPUT_IsoX_compound"),
          label = "Compound name",
          value = "compound.name"
        ),


        numericInput(
          inputId = ns("INPUT_IsoX_tolerance"),
          label = "Tolerance [mDa]",
          0.3,
          min = 0.1,
          max = 3,
          step = 0.1,
          width = NULL
        ),

        tippy(
          numericInput(
            inputId = ns("INPUT_IsoX_threshold"),
            label = "Threshold [% of M0]",
            0.0001,
            min = 0.00001,
            max = 10,
            step = 0.0001,
            width = NULL
          ),
          "Processing option: Threshold to include an isotopolog (in ppm abundance)",
          placement = "right"
        ),


        numericInput(
          inputId = ns("INPUT_IsoX_massoffset"),
          label = "Offset m/z [mDa]",
          0,
          min = -5,
          max = 5,
          step = 0.1,
          width = NULL
        )
      ),

      hr(),

      actionButton(ns("start"), label = "Make IsoX input file", icon = icon("sync"))

    ),

 # UI: Create IsoX Input Main Panel ----------------------------------------

 mainPanel(width = 9,

           div(
             id = ns("main-plot"),

             downloadButton(ns("download_IsoX"), "Download Input File for IsoX"),

             hr(),

             box(
               title = "Preview of isotoplogs.tsv",
               width = "auto" ,
               status = "primary",
               solidHeader = T,
               DT::dataTableOutput(ns("isox"))
             )
           ))
  ))
}
