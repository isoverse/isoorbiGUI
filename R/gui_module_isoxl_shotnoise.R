# server
module_isoxl_shot_noise_server <- function(input, output, session) {

  # namespace
  ns <- session$ns

  # values
  values <- reactiveValues(
    upload_file = NULL,
    upload_error = NULL,
    data = NULL,
    plot = NULL
  )

  # upload file
  observeEvent(input$upload, {
    module_message(ns, "info", "new file uploaded file ", input$upload$name,
                   " into temporary path ", input$upload$datapath)

    # reset the reactive values that hold the file and data
    values$upload_file <- input$upload$name
    values$upload_error <- NULL
    values$data <- NULL

    # try to read the uploaded file
    tryCatch(
      values$data <- iso_read_isox_file(input$upload$datapath),
      warning = function(w) values$upload_error <- w$message,
      error = function(e) values$upload_error <- e$message
    )
  })

  # provide feedback on file upload success
  output$upload_info <- renderText({
    req(values$upload_file)
    validate(
      need(is.null(values$upload_error), message = values$upload_error)
    )
    paste0(values$upload_file, " uploaded")
  })

  # fill parameters from data
  observeEvent(values$data, {
    module_message(ns, "info", "updating parameter table")
    if (is.null(values$data) || nrow(values$data) == 0) {
      # no data (maybe incorret input file --> hide)
      shinyjs::hide("parameters-panel")
    } else {
      # yes data --> update paraemets and show
      updateSelectInput(
        inputId = "isotopolog",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[2]
      )
      updateSelectInput(
        inputId = "base_peak",
        choices = levels(values$data$isotopolog),
        selected = values$data$isotopolog[1]
      )
      shinyjs::show("parameters-panel")
    }
  }, ignoreNULL = FALSE)


  # plot
  observe({
    # SK: fixme retrigger on button click
    input$update
    module_message(ns, "info", "generating plot")
    isolate(values$plot <- ggplotly(ggplot(head(ggplot2::mpg)) + aes(x = cty, y = hwy) + geom_point()))
  })

  output$plot <- renderPlotly({
    req(values$plot)
    values$plot
  })

  # ### select a specific filename
  #
  # data.SN.filename <- reactive({
  #
  #   req(input$INPUT_SN_filename)
  #
  #
  #   validate(
  #
  #     need( (input$INPUT_SN_filename == "all" | input$INPUT_SN_filename %in% levels(data.SN()$filename ) ), paste("Warning: Filename not in dataset." ,"\n Option: use 'all'.", "\n Individual filenames found are: ", levels(data.SN()$filename ) ) ),
  #     #    need( (input$INPUT_SN_filename != character(0)), paste("Warning: Filename not in dataset." ,"Filenames found are: ", levels(data.SN()$filename ) ) ) #temp how to deal with empty input
  #
  #   )
  #
  #   ifelse(input$INPUT_SN_filename=="all", return(data.SN()), return( data.SN() %>% group_by(filename) %>% filter(filename == input$INPUT_SN_filename) ) )
  #
  #
  #
  # })
  #
  # ### select a specific compound
  #
  # data.SN.filename.compound <- reactive({
  #
  #   req(input$INPUT_SN_compound)
  #
  #   validate(
  #
  #     need( (input$INPUT_SN_compound == "all" | input$INPUT_SN_compound %in% levels(data.SN()$compound ) ), paste("Warning: Compound not in dataset." ,"\n Option: use 'all'.", "\n Individual compounds found are: ", levels(data.SN()$compound ) ) )
  #
  #   )
  #
  #
  #   ifelse(input$INPUT_SN_compound=="all", return(data.SN.filename()), return( data.SN.filename() %>% group_by(compound) %>% filter(compound == input$INPUT_SN_compound) ) )
  #
  #
  # })
  #
  #
  #
  # data.SN.filtered <- reactive({
  #
  #   req(input$INPUT_SN_timerange)
  #   req(input$INPUT_SN_Basepeak)
  #   req(input$INPUT_SN_Isotopolog)
  #
  #   validate(
  #
  #     # need( (input$INPUT_SN_filename=="all" | input$INPUT_SN_filename %in% levels(data.SN()$filename ) ), paste("Warning: Filename not in dataset." ,"Filenamses found are: ", levels(data.SN()$filename ) ) ),
  #
  #     need(input$INPUT_SN_Isotopolog, "Warning: Isotopolog has a number as input."), #temp nor working for number or empty field, whitespace
  #     need(input$INPUT_SN_Basepeak, "Base peak has no input."), #temp nor working
  #     need(input$INPUT_SN_timerange[1], "Input is missing for time range (min)."),
  #     need(input$INPUT_SN_timerange[2], "Input is missing for time range (max)."),
  #
  #     need(input$INPUT_SN_Isotopolog %in% levels(data.SN()$isotopolog), paste("Error: Isotopolog not found in data. Found the following isotopologs: ", levels(data.SN()$isotopolog)) ), #temp; error prints in multiple rows
  #     need(input$INPUT_SN_Basepeak %in% levels(data.SN()$isotopolog), paste("Error: Base peak not found in data. Found the following isotopologs: ", levels(data.SN()$isotopolog))),
  #     need(input$INPUT_SN_timerange[2] <= max(data.SN()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.SN()$time.min) , " min.") )
  #   )
  #
  #
  #
  #
  #   df <- data.SN.filename.compound() %>% group_by(filename) %>% filter(time.min>  input$INPUT_SN_timerange[1], time.min <  input$INPUT_SN_timerange[2]) %>%  filter(isotopolog == input$INPUT_SN_Basepeak | isotopolog == input$INPUT_SN_Isotopolog ) %>%
  #     remove.ticxit(INPUT_TICxIT=input$INPUT_SN_TICxIT) %>%
  #     remove.satellitePeaks() %>%
  #     remove.rare(INPUT_Rare = input$INPUT_SN_Rare) %>%
  #     select(filename, scan.no, time.min, isotopolog, ions.incremental) %>% tidyr::spread(isotopolog, ions.incremental) %>% na.omit()
  #
  #
  #   names(df)[names(df) == as.character(input$INPUT_SN_Basepeak)] <- "Basepeak.Ions"
  #   names(df)[names(df) == as.character(input$INPUT_SN_Isotopolog)] <- "Isotopolog.Ions"
  #
  #   df.out <- df %>% mutate(Ratio= Isotopolog.Ions/ Basepeak.Ions)
  #
  #
  #   return(df.out)
  # })
  #
  # calc.shotnoise <- function(d.neff){
  #
  #   df.out <- data.frame(scans =1:length(d.neff$scan.no),
  #                        neff = 1:length(d.neff$scan.no),
  #                        rse= 1:length(d.neff$scan.no),
  #                        shotnoise = 1:length(d.neff$scan.no),
  #                        shotnoise.neff = 1:length(d.neff$scan.no))
  #
  #   #Geometric mean, geometric SD and SE
  #   gmean <- function(x) exp(mean(log(x))) #define geometric mean
  #   gsd <- function(x) exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))
  #   gse <- function(x) (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) /sqrt(length(x))
  #
  #   for (l in 1:length(d.neff$scan.no)) {
  #     df <- d.neff[1:l,]
  #
  #     neff <- (sum(df$Basepeak.Ions) * sum(df$Isotopolog.Ions))/ (sum(df$Basepeak.Ions) + sum(df$Isotopolog.Ions))
  #     df.out$neff[l] <- neff
  #
  #     shotnoise.neff <- neff^-0.5
  #     df.out$shotnoise.neff[l] <- shotnoise.neff
  #
  #     shotnoise <- ((1/sum(df$Isotopolog.Ions) + (1/sum(df$Basepeak.Ions))))^0.5
  #     df.out$shotnoise[l] <- shotnoise
  #
  #     rse <- gse(d.neff[1:l,]$Ratio) / gmean(d.neff[1:l,]$Ratio)
  #     df.out$rse[l] <- rse
  #
  #   }
  #
  #
  #   df.out <- df.out %>% na.omit() %>% select(-shotnoise, -shotnoise.neff)
  #
  #   df.out <- df.out %>% tidyr::gather(key, value, -scans, -neff)
  #
  #   return(df.out)
  #
  # }
  #
  #
  #
  # data.SN.2plot <- reactive ({ calc.shotnoise(data.SN.filtered())
  # })
  #
  #

  # Plot: Shot noise --------------------------------------------------------
  #
  # output$plot_SN <- renderPlotly({
  #
  #
  #   ggplotly(
  #
  #     ggplot(data = data.SN.2plot(), aes(x= neff, y=  value, color=key)) + #scale_color_brewer(palette="Dark2") +
  #
  #       geom_point(size=0.8, alpha=0.5) +
  #       geom_abline(intercept = 0, slope = -0.5, col="blue") +
  #       labs(y="RSE",
  #            x="effective number of ions")  +
  #       theme(legend.position="none")+
  #
  #       annotate(geom = 'text', label = 'shot noise \nlimit', color="blue", x=0.05*median(data.SN.2plot()$neff),y=median(data.SN.2plot()$value))+ #temp: better/ more elegant way to position label?
  #       scale_x_continuous(trans = log10_trans(), breaks = c(10000, 100000, 1000000,10000000, 100000000), labels=c("1E4","1E5", "1E6", "1E7", "1E8")) +
  #       scale_y_continuous(trans = log10_trans(), breaks = c(0.1, 0.05, 0.01,  0.002, 0.001, 0.0005,0.0002, 0.0001), labels=c("100 permil","50 permil", "10 permil", "2 permil" , "1 permil", "0.5 permil", "0.2 permil", "0.1 permil")),
  #
  #
  #     #dynamicTicks = TRUE #change ticks on zoom
  #   ) %>%
  #
  #     config(displaylogo = FALSE,
  #            modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar
  #
  #     config( #options on how to save an image
  #       toImageButtonOptions = list(
  #         format = "png",
  #         width = 900,
  #         height = 600,
  #         scale = 4,
  #         filename = paste("shotnoise", input$INPUT_SN_Isotopolog, input$INPUT_SN_Basepeak, sep="_")
  #
  #       )
  #     )
  # })
  #
  #
  #
  #
}



# user interface
module_isoxl_shotnoise_ui <- function(id) {
  # namespace
  ns <- NS(id)

  fluidPage(


    sidebarLayout(


      sidebarPanel(
        width = 3,

        # upload
        fileInput(ns("upload"), label="Upload .isox file", accept = c(".isox"), placeholder="No file loaded..."),
        textOutput(ns("upload_info")),

        # parameters
        div(
          id = ns("parameters-panel"),
          tags$hr(style="border-color: grey;"),
          selectInput(inputId = ns("isotopolog"), label="Isotopolog", c()),
          selectInput(inputId = ns("base_peak"), label="Base peak", c()),
          tags$hr(style="border-color: grey;"),
          actionButton(inputId = ns("update"), label = "Plot", icon = icon("sync"))
        ) %>% shinyjs::hidden()

#
#
#
#
#                    textInput(inputId = "INPUT_SN_Isotopolog", label="Isotopolog", value = "18O"), #change back to M0 as default
#                    textInput(inputId = "INPUT_SN_Basepeak", label="Base peak", value = "15N"), #change back to M0 as default
#
#
#                    shinyWidgets::numericRangeInput("INPUT_SN_timerange", "Time [min]", value = c(1,10), min=0, max=100),
#
#                    textInput("INPUT_SN_filename", label = "Filename", value = "all"), #temp add eg NONE
#
#                    textInput("INPUT_SN_compound", label = "Compound", value = "all"), #temp add eg NONE
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_SN_Rare",
#                      label = "Remove weak isotopologs",
#                      30,
#                      min = 1,
#                      max = 50,
#                      step = 1,
#                      width = NULL
#                    ),"Processing option: Isotopolog must have been observed in at least x % of scans", placement = "right"),
#
#                    with_tippy(numericInput(
#                      inputId = "INPUT_SN_TICxIT",
#                      label = "Remove outlier scans",
#                      1,
#                      min = 0.1,
#                      max = 10,
#                      step = 0.1,
#                      width = NULL
#                    ), "Processing option: Truncate data x%/100-x% based on TICxIT",  placement = "right")
#
#



      ),


      # UI: Shot Noise Analysis Main Panel --------------------------------------


      mainPanel(

        box(title = "Shot Noise Plot", width = "auto" , height="auto", status = "primary", solidHeader = T, withSpinner(plotlyOutput(ns("plot")))), #how to save png at higher resolution?


      )
    )
  )

}
