#' Orbitrap ShinyApp Server
#'
#' Generates the server part of the shiny app.
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
#' @param reset whether to reset settings if running in local mode (server mode always resets)
orbitrap_server <- function(local = FALSE, reset = FALSE) {

  shinyServer(function(input, output, session) {

    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      save_settings = local,
      settings_file = reactive("orbitrap_gui_settings.rds")
    )

    # FIXME: create data folder module that also coordinates what happens with the settings
    if (reset) settings$reset()
    else if (local) settings$load()

    #### 11-June-2022: TEST adding original IsoXL code here ###

    options(shiny.maxRequestSize=100*1024^2) #Otherwise maximum file upload is 5MB!




    # IsoX Version ------------------------------------------------------------

    isoXL.version <- "Version 0.1 (Feb-2022)"

    # Data: Dual Inlet --------------------------------------------------------------

    inputs.dual_inlet <- reactive({

      req(input$upload_Dual_Inlet) #temp; needs to be coded more elegantly; prone to bugs (e.g. Excel output file cannot be generated)
      req(input$INPUT_Sample)
      req(input$INPUT_Ref)
      req(input$INPUT_Basepeak)

      data.frame( parameter = c(
        "IsoXL Version",
        "upload_Dual_Inlet",
        "INPUT_Sample",
        "INPUT_Ref",
        "INPUT_Basepeak",
        "INPUT_Blocks",
        "INPUT_Block.time" ,
        "INPUT_Idle.time",
        "INPUT_Startup.time",
        "INPUT_Segments",
        "INPUT_Rare",
        "INPUT_TICxIT",
        "INPUT_RatioMethod",
        "INPUT_IONS"),


        value = c(isoXL.version,
                  input$upload_Dual_Inlet$name,
                  input$INPUT_Sample,
                  input$INPUT_Ref,
                  input$INPUT_Basepeak,
                  input$INPUT_Blocks,
                  input$INPUT_Block.time,
                  input$INPUT_Idle.time,
                  input$INPUT_Startup.time,
                  input$INPUT_Segments,
                  input$INPUT_Rare,
                  input$INPUT_TICxIT,
                  input$INPUT_RatioMethod,
                  input$INPUT_IONS)


      )
    })



    data.dual_inlet <- reactive({


      req(input$upload_Dual_Inlet)


      ext <- tools::file_ext(input$upload_Dual_Inlet$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_Dual_Inlet$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(), #col_factor() conflicts with scales package
                                      compound = readr::col_factor(),
                                      isotopolog = readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file") #check that this line works
      )
    })


    data.dual_inlet_filtered <- reactive({

      req(input$INPUT_Rare)

      return(data.dual_inlet() %>%
               na.omit() %>%
               remove.ticxit( INPUT_TICxIT=input$INPUT_TICxIT ) %>% #temp: validate correct functioning
               remove.satellitePeaks() %>%
               remove.rare(INPUT_Rare = input$INPUT_Rare) )


    })



    # Dual Inlet: define experiment -------------------------------------------

    define.dual.inlet <- function(INPUT_Ref, INPUT_Sample, INPUT_Blocks, INPUT_Segments, INPUT_Idle.time, INPUT_Startup.time, INPUT_Block.time){


      validate(
        need(input$INPUT_Ref, 'Missing input name of reference'),
        need(input$INPUT_Sample, 'Missing input name of sample'),
        need(input$INPUT_Blocks, 'Missing input Blocks'),
        need(input$INPUT_Idle.time, 'Missing input Idle Time'),
        need(input$INPUT_Block.time, 'Missing input Block Time'),
        need(input$INPUT_Startup.time, 'Missing input Startup Time'),

        need(input$INPUT_Idle.time < input$INPUT_Block.time, 'Idle Time is longer than Block Time!'),
        need(input$INPUT_Startup.time < 100, 'Startup time seems too long!'),
        need(input$INPUT_Blocks < 100, 'Looks like too many blocks!'),
        need(input$INPUT_Block.time < 100, 'Block Time appears too long'),
        need(input$INPUT_Segments <10 , 'Looks like too many segments')
      )

      injections <- rep( c(INPUT_Ref, INPUT_Sample), 1E3)[1:INPUT_Blocks] #limited to maximum 1,000 blocks
      injections <- rep(injections, each= INPUT_Segments)
      segment.time <- (INPUT_Block.time - INPUT_Idle.time)/ INPUT_Segments
      start.time <- 0:(INPUT_Blocks-1)*INPUT_Block.time + INPUT_Idle.time #Define start times to integrate (in minutes)
      out.start <- c()

      for (i in 1:(length(start.time))){
        a <- seq(start.time[i],start.time[i]+ INPUT_Block.time - INPUT_Idle.time, segment.time)
        out.start  <- append(out.start, head(a, -1))
      }
      start.time <- out.start + INPUT_Startup.time
      end.time <- start.time + segment.time

      #combine all information
      injection.number <- 1:length(injections)
      dual.inlet.annot <- data.frame(Injection.Number= injection.number,
                                     Injection = injections,
                                     Start.Time = start.time,
                                     End.Time = end.time)
      return(dual.inlet.annot)
    }



    #  Dual Inlet annotation table --------------------------------------------

    dual.inlet.annotation     <- reactive({
      req(input$INPUT_Ref)
      req(input$INPUT_Sample)
      req(input$INPUT_Blocks)
      req(input$INPUT_Segments)
      req(input$INPUT_Idle.time)
      req(input$INPUT_Startup.time)
      req(input$INPUT_Block.time)

      define.dual.inlet(INPUT_Ref= input$INPUT_Ref,
                        INPUT_Sample= input$INPUT_Sample,
                        INPUT_Blocks= input$INPUT_Blocks,
                        INPUT_Segments= input$INPUT_Segments,
                        INPUT_Idle.time = input$INPUT_Idle.time,
                        INPUT_Startup.time= input$INPUT_Startup.time,
                        INPUT_Block.time= input$INPUT_Block.time)

    })



    # Dual Inlet: Merge data with annotation ----------------------------------


    data.annotated <- reactive({
      merge.di.annotations(data.dual_inlet_filtered(), dual.inlet.annotation())
    })



    # Dual Inlet: Assign base peak --------------------------------------------


    data.basepeak <- reactive({assign.basepeak(data.annotated(), input$INPUT_Basepeak)  }) #needs validation; e.g. is basepeak present in data?


    # Dual Inlet: Calculate output table for results file ---------------



    data.output <- reactive({calc.output(df = data.basepeak(), INPUT_RatioMethod = input$INPUT_RatioMethod) })



    # Dual Inlet: Make Excel file ---------------------------------------------

    data.xls <- reactive({  make.xls(data.output(), params=inputs.dual_inlet()) })


    ##### Data: Flow Analysis ---------------------------------------------------


    inputs.flow <- reactive({


      req(input$INPUT_Flow_Basepeak) #temp

      data.frame( parameter = c(
        "IsoXL Version",
        "INPUT_Flow_Basepeak",
        "INPUT_Flow_timerange[1]",
        "INPUT_Flow_timerange[2]" ,
        "INPUT_Flow_Rare",
        "INPUT_Flow_TICxIT",
        "INPUT_Flow_RatioMethod",
        "INPUT_Flow_IONS"),


        value = c(isoXL.version,
                  input$INPUT_Flow_Basepeak,
                  input$INPUT_Flow_timerange[1],
                  input$INPUT_Flow_timerange[2],
                  input$INPUT_Flow_Rare,
                  input$INPUT_Flow_TICxIT,
                  input$INPUT_Flow_RatioMethod,
                  input$INPUT_Flow_IONS)


      )
    })

    data.flow <- reactive({



      req(input$upload_Flow_Injection)


      ext <- tools::file_ext(input$upload_Flow_Injection$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_Flow_Injection$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(),
                                      compound = readr::col_factor(),
                                      isotopolog =  readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file")
      )
    })


    data.flow_filtered <- reactive({

      req(input$INPUT_Flow_Rare)
      req(input$INPUT_Flow_timerange)
      req(input$INPUT_Flow_timerange)
      #req(input$INPUT_Flow_sample)

      validate(


        ####  need(length(levels(data.flow()$filename)) <2, paste("Warning: There is more than one 'filename' in data. May want to use Flow Injection tab? These filenames have been observed",levels(data.flow()$filename ))),
        need(input$INPUT_Flow_timerange[2] <= max(data.flow()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.flow()$time.min) , " min.") ),
        need(input$INPUT_Flow_timerange[2] > input$INPUT_Flow_timerange[1], paste("Warning: Time selection minimun is greater than maximum") )

      )



      return(data.flow() %>%
               na.omit() %>%

               #mutate(Sample.Name = input$INPUT_Flow_sample) %>% #temp: implemented as factor? Not needed, I think

               remove.ticxit(INPUT_TICxIT=input$INPUT_Flow_TICxIT) %>%
               remove.satellitePeaks() %>%
               remove.rare(INPUT_Rare = input$INPUT_Flow_Rare) %>%

               filter(time.min>  input$INPUT_Flow_timerange[1], time.min <  input$INPUT_Flow_timerange[2])
      )


    })

    # Flow Analysis: Assign base peak --------------------------------------------


    data.flow_basepeak <- reactive({

      req(input$INPUT_Flow_Basepeak)

      assign.basepeak.single(df = data.flow_filtered(), INPUT_Basepeak = input$INPUT_Flow_Basepeak)  #function from single analysis should be of has groub_by(filename)

    }) #needs validation; e.g. is base peak present in data?


    # Flow Analysis: Calculate output table for results file ---------------



    data.flow_output <- reactive({

      req(input$INPUT_Flow_Basepeak)

      calc.output.flow(df = data.flow_basepeak(), INPUT_RatioMethod = input$INPUT_Flow_RatioMethod)

    })


    # Flow analysis: Make Excel file ---------------------------------------------

    data_flow.xls <- reactive({  make.xls(df.stat = data.flow_output(), params=inputs.flow() ) }) ##temp add correct parameter file!!!

    ####


    # Data: Single Analysis ---------------------------------------------------

    inputs.single <- reactive({


      #req() #temp

      data.frame( parameter = c(
        "IsoXL Version",
        "INPUT_Single_sample",
        "INPUT_Single_Basepeak",
        "INPUT_Single_timerange[1]",
        "INPUT_Single_timerange[2]" ,
        "INPUT_Single_Rare",
        "INPUT_Single_TICxIT",
        "INPUT_Single_RatioMethod",
        "INPUT_Single_IONS"),


        value = c(isoXL.version,
                  input$INPUT_Single_sample,
                  input$INPUT_Single_Basepeak,
                  input$INPUT_Single_timerange[1],
                  input$INPUT_Single_timerange[2],
                  input$INPUT_Single_Rare,
                  input$INPUT_Single_TICxIT,
                  input$INPUT_Single_RatioMethod,
                  input$INPUT_Single_IONS)


      )
    })


    data.single <- reactive({



      req(input$upload_Single_Analysis)


      ext <- tools::file_ext(input$upload_Single_Analysis$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_Single_Analysis$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(),
                                      compound = readr::col_factor(),
                                      isotopolog = readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file")
      )
    })


    data.single_filtered <- reactive({

      req(input$INPUT_Single_Rare)
      req(input$INPUT_Single_timerange)
      req(input$INPUT_Single_timerange)
      req(input$INPUT_Single_sample)

      validate(


        need(length(levels(data.single()$filename)) <2, paste("Warning: There is more than one 'filename' in data. May want to use Flow Injection tab? These filenames have been observed",levels(data.single()$filename ))),
        need(input$INPUT_Single_timerange[2] <= max(data.single()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.single()$time.min) , " min.") ),
        need(input$INPUT_Single_timerange[2] > input$INPUT_Single_timerange[1], paste("Warning: Time selection minimun is greater than maximum") )

      )



      return(data.single() %>%
               na.omit() %>%

               mutate(Sample.Name = input$INPUT_Single_sample) %>% #temp: implemented as factor?

               remove.ticxit(INPUT_TICxIT=input$INPUT_Single_TICxIT) %>%
               remove.satellitePeaks() %>%
               remove.rare(INPUT_Rare = input$INPUT_Single_Rare) %>%

               filter(time.min>  input$INPUT_Single_timerange[1], time.min <  input$INPUT_Single_timerange[2])
      )


    })

    # Single Analysis: Assign base peak --------------------------------------------


    data.single_basepeak <- reactive({

      req(input$INPUT_Single_Basepeak)

      assign.basepeak.single(df = data.single_filtered(), INPUT_Basepeak = input$INPUT_Single_Basepeak)

    }) #needs validation; e.g. is base peak present in data?


    # Single Analysis: Calculate output table for results file ---------------



    data.single_output <- reactive({

      req(input$INPUT_Single_Basepeak)

      calc.output.single(df = data.single_basepeak(), INPUT_RatioMethod = input$INPUT_Single_RatioMethod)

    })


    # Single analysis: Make Excel file ---------------------------------------------

    data_single.xls <- reactive({  make.xls.onetab(df.stat = data.single_output(), params = inputs.single()) })


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


    # Data: Shot Noise --------------------------------------------------------------


    data.SN <- reactive({



      req(input$upload_SN)


      ext <- tools::file_ext(input$upload_SN$name)
      switch(ext,
             isox = readr::read_tsv(input$upload_SN$datapath, # may no longer need to include: skipNul = TRUE: https://github.com/tidyverse/readr/issues/338
                                    col_types = list(
                                      filename = readr::col_factor(),
                                      compound = readr::col_factor(),
                                      isotopolog = readr::col_factor()
                                    ) ),


             validate("Invalid file; Please upload a .isox file")
      )
    })


    ### select a specific filename

    data.SN.filename <- reactive({

      req(input$INPUT_SN_filename)


      validate(

        need( (input$INPUT_SN_filename == "all" | input$INPUT_SN_filename %in% levels(data.SN()$filename ) ), paste("Warning: Filename not in dataset." ,"\n Option: use 'all'.", "\n Individual filenames found are: ", levels(data.SN()$filename ) ) ),
        #    need( (input$INPUT_SN_filename != character(0)), paste("Warning: Filename not in dataset." ,"Filenames found are: ", levels(data.SN()$filename ) ) ) #temp how to deal with empty input

      )

      ifelse(input$INPUT_SN_filename=="all", return(data.SN()), return( data.SN() %>% group_by(filename) %>% filter(filename == input$INPUT_SN_filename) ) )



    })

    ### select a specific compound

    data.SN.filename.compound <- reactive({

      req(input$INPUT_SN_compound)

      validate(

        need( (input$INPUT_SN_compound == "all" | input$INPUT_SN_compound %in% levels(data.SN()$compound ) ), paste("Warning: Compound not in dataset." ,"\n Option: use 'all'.", "\n Individual compounds found are: ", levels(data.SN()$compound ) ) )

      )


      ifelse(input$INPUT_SN_compound=="all", return(data.SN.filename()), return( data.SN.filename() %>% group_by(compound) %>% filter(compound == input$INPUT_SN_compound) ) )


    })



    data.SN.filtered <- reactive({

      req(input$INPUT_SN_timerange)
      req(input$INPUT_SN_Basepeak)
      req(input$INPUT_SN_Isotopolog)

      validate(

        # need( (input$INPUT_SN_filename=="all" | input$INPUT_SN_filename %in% levels(data.SN()$filename ) ), paste("Warning: Filename not in dataset." ,"Filenamses found are: ", levels(data.SN()$filename ) ) ),

        need(input$INPUT_SN_Isotopolog, "Warning: Isotopolog has a number as input."), #temp nor working for number or empty field, whitespace
        need(input$INPUT_SN_Basepeak, "Base peak has no input."), #temp nor working
        need(input$INPUT_SN_timerange[1], "Input is missing for time range (min)."),
        need(input$INPUT_SN_timerange[2], "Input is missing for time range (max)."),

        need(input$INPUT_SN_Isotopolog %in% levels(data.SN()$isotopolog), paste("Error: Isotopolog not found in data. Found the following isotopologs: ", levels(data.SN()$isotopolog)) ), #temp; error prints in multiple rows
        need(input$INPUT_SN_Basepeak %in% levels(data.SN()$isotopolog), paste("Error: Base peak not found in data. Found the following isotopologs: ", levels(data.SN()$isotopolog))),
        need(input$INPUT_SN_timerange[2] <= max(data.SN()$time.min), paste("Warning: Time selection exceeds time range of data. Maximum time in data is ", max(data.SN()$time.min) , " min.") )
      )




      df <- data.SN.filename.compound() %>% group_by(filename) %>% filter(time.min>  input$INPUT_SN_timerange[1], time.min <  input$INPUT_SN_timerange[2]) %>%  filter(isotopolog == input$INPUT_SN_Basepeak | isotopolog == input$INPUT_SN_Isotopolog ) %>%
        remove.ticxit(INPUT_TICxIT=input$INPUT_SN_TICxIT) %>%
        remove.satellitePeaks() %>%
        remove.rare(INPUT_Rare = input$INPUT_SN_Rare) %>%
        select(filename, scan.no, time.min, isotopolog, ions.incremental) %>% tidyr::spread(isotopolog, ions.incremental) %>% na.omit()


      names(df)[names(df) == as.character(input$INPUT_SN_Basepeak)] <- "Basepeak.Ions"
      names(df)[names(df) == as.character(input$INPUT_SN_Isotopolog)] <- "Isotopolog.Ions"

      df.out <- df %>% mutate(Ratio= Isotopolog.Ions/ Basepeak.Ions)


      return(df.out)
    })

    calc.shotnoise <- function(d.neff){

      df.out <- data.frame(scans =1:length(d.neff$scan.no),
                           neff = 1:length(d.neff$scan.no),
                           rse= 1:length(d.neff$scan.no),
                           shotnoise = 1:length(d.neff$scan.no),
                           shotnoise.neff = 1:length(d.neff$scan.no))

      #Geometric mean, geometric SD and SE
      gmean <- function(x) exp(mean(log(x))) #define geometric mean
      gsd <- function(x) exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))
      gse <- function(x) (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) /sqrt(length(x))

      for (l in 1:length(d.neff$scan.no)) {
        df <- d.neff[1:l,]

        neff <- (sum(df$Basepeak.Ions) * sum(df$Isotopolog.Ions))/ (sum(df$Basepeak.Ions) + sum(df$Isotopolog.Ions))
        df.out$neff[l] <- neff

        shotnoise.neff <- neff^-0.5
        df.out$shotnoise.neff[l] <- shotnoise.neff

        shotnoise <- ((1/sum(df$Isotopolog.Ions) + (1/sum(df$Basepeak.Ions))))^0.5
        df.out$shotnoise[l] <- shotnoise

        rse <- gse(d.neff[1:l,]$Ratio) / gmean(d.neff[1:l,]$Ratio)
        df.out$rse[l] <- rse

      }


      df.out <- df.out %>% na.omit() %>% select(-shotnoise, -shotnoise.neff)

      df.out <- df.out %>% tidyr::gather(key, value, -scans, -neff)

      return(df.out)

    }



    data.SN.2plot <- reactive ({ calc.shotnoise(data.SN.filtered())
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






    # Plot: Shot noise --------------------------------------------------------

    output$plot_SN <- renderPlotly({


      ggplotly(

        ggplot(data = data.SN.2plot(), aes(x= neff, y=  value, color=key)) + #scale_color_brewer(palette="Dark2") +

          geom_point(size=0.8, alpha=0.5) +
          geom_abline(intercept = 0, slope = -0.5, col="blue") +
          labs(y="RSE",
               x="effective number of ions")  +
          theme(legend.position="none")+

          annotate(geom = 'text', label = 'shot noise \nlimit', color="blue", x=0.05*median(data.SN.2plot()$neff),y=median(data.SN.2plot()$value))+ #temp: better/ more elegant way to position label?
          scale_x_continuous(trans = log10_trans(), breaks = c(10000, 100000, 1000000,10000000, 100000000), labels=c("1E4","1E5", "1E6", "1E7", "1E8")) +
          scale_y_continuous(trans = log10_trans(), breaks = c(0.1, 0.05, 0.01,  0.002, 0.001, 0.0005,0.0002, 0.0001), labels=c("100 permil","50 permil", "10 permil", "2 permil" , "1 permil", "0.5 permil", "0.2 permil", "0.1 permil")),


        #dynamicTicks = TRUE #change ticks on zoom
      ) %>%

        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar

        config( #options on how to save an image
          toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 600,
            scale = 4,
            filename = paste("shotnoise", input$INPUT_SN_Isotopolog, input$INPUT_SN_Basepeak, sep="_")

          )
        )
    })






    # Function: remove.rare(df, INPUT_Rare) -----------------------------------



    remove.rare <- function(df, INPUT_Rare){ #temp check functions correctly in different tabs

      validate(
        need(INPUT_Rare, 'Missing input INPUT_Rare'),
        need(INPUT_Rare <=100, 'Check input INPUT_Rare is smaller than 100%')
      )

      remove.df <- df %>%
        group_by(filename) %>%
        mutate(n.scans = max(scan.no) - min(scan.no)) %>%
        group_by(filename, compound, isotopolog) %>%
        mutate(i.scans = length(scan.no)) %>%
        filter(i.scans < INPUT_Rare/100 * n.scans) %>% # input
        select(-n.scans, -i.scans) %>% droplevels() %>% as.data.frame()


      df <- anti_join(df, remove.df)

      return(df)

    }



    # Function: remove.ticxit(df, INPUT_TICxIT) -------------------------------



    remove.ticxit <- function(df, INPUT_TICxIT){

      validate(
        need(INPUT_TICxIT, 'Missing input INPUT_TICxIT'),
        need(INPUT_TICxIT <=10, 'Check input INPUT_TICxIT is smaller than 10%')
      )


      df <- df %>% group_by(filename) %>% mutate(TICxIT = tic * it.ms) %>% #edit 28-Feb-2022 enable multiple dual inlet files
        filter( TICxIT > quantile(TICxIT, INPUT_TICxIT/100) & TICxIT < quantile(TICxIT, 1- INPUT_TICxIT/100)) %>%
        select(-TICxIT)

      return(df)
    }



    # Function: remove.satellitePeaks(df) -------------------------------------


    remove.satellitePeaks <- function(df){

      df <- df %>%
        group_by(filename, compound, scan.no, isotopolog) %>%
        filter(ions.incremental == max(ions.incremental))

      return(df)

    }


    # Function remove.columns(df); Remove columns in dual inlet that are not necessary (after merging with annotations!) -------------------------------------------


    # remove.columns <- function(df){
    #   df <- df %>% select(filename,
    #                       Sample.Name,
    #                       compound,
    #                       Injection,
    #                       scan.no,
    #                       time.min,
    #                       isotopolog,
    #                       ions.incremental)
    #   return(df)
    # }



    # Function: merge.di.annotations(df.in, dual.inlet.anno) ------------------



    merge.di.annotations <- function(df.in, dual.inlet.anno){

      #temp; Add validations later for Dual Inlet input parameters. Are provided values reasonable.

      df.data <- df.in
      df.data$Sample.Name <- "NA" #normal NA caused issues!
      df.data$Injection <- 0L  #normal NA caused issues! L makes the zero an integer, not double
      #
      for (i in 1:nrow(dual.inlet.anno)) {

        sample <- dual.inlet.anno$Injection[i]
        injection.no <- dual.inlet.anno$Injection.Number[i]
        #
        time.start <- dual.inlet.anno$Start.Time[i]
        time.end <- dual.inlet.anno$End.Time[i]
        #
        df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$Sample.Name <- sample
        df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$Injection <- injection.no

        #return(df.data) #would be returned for function!
      }
      #

      df<- df.data  %>% filter(Injection >0) %>% filter(Sample.Name != "NA")

      df$Sample.Name <- as.factor(df$Sample.Name)
      df$compound <- as.factor(df$compound)
      df$isotopolog <- as.factor(df$isotopolog)

      return(df)

    }




    # Function to assign Base Peak --------------------------------------------


    # Dual Inlet
    assign.basepeak <- function(df, INPUT_Basepeak){

      validate(
        need(INPUT_Basepeak, 'Missing input for Basepeak'), #temp; check this works correctly
        need(INPUT_Basepeak %in% levels(df$isotopolog), 'Specified Basepeak was not found in dataframe'))

      # Annotation: Identify 'basepeak' for each scan

      df.sel <- df  %>%
        select(filename,
               Injection,
               compound,
               scan.no,
               isotopolog,
               ions.incremental) %>%
        group_by(filename, Injection, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%

        mutate(Basepeak = factor(
          paste(
            isotopolog, "is base peak", sep=" ")),
          Basepeak.Ions = ions.incremental) %>%
        select(filename,
               compound,
               Injection,
               scan.no,
               Basepeak,
               Basepeak.Ions) %>%
        as.data.frame()

      df.out <- merge(df.sel, df, all = TRUE)

      df.out <- df.out %>% filter(isotopolog!=INPUT_Basepeak) %>% droplevels()

      return(df.out)
    }


    #single analysis

    assign.basepeak.single <- function(df, INPUT_Basepeak){

      validate(
        need(INPUT_Basepeak, 'Missing input for Basepeak'), #temp; check this works correctly
        need(INPUT_Basepeak %in% levels(df$isotopolog), 'Specified Basepeak was not found in dataframe'))

      # Annotation: Identify 'basepeak' for each scan

      df.sel <- df  %>%
        select(filename,
               #Injection, #not present in single run analysis
               compound,
               scan.no,
               isotopolog,
               ions.incremental) %>%
        #group_by(filename, Injection, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%
        group_by(filename, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%
        mutate(Basepeak = factor(
          paste(
            isotopolog, "is base peak", sep=" ")),
          Basepeak.Ions = ions.incremental) %>%
        select(filename,
               compound,
               #Injection,
               scan.no,
               Basepeak,
               Basepeak.Ions) %>%
        as.data.frame()

      df.out <- merge(df.sel, df, all = TRUE)

      df.out <- df.out %>% filter(isotopolog!=INPUT_Basepeak) %>% droplevels()

      return(df.out)
    }





    # Functions to calculate stats --------------------------------------------


    #Define 'se' as a new function to calculate standard error
    se <- function(x) { sd(x) / sqrt(length(x)) }

    #NOTE: The formulate used to average ratios matters! Do not simply use arithmetic mean.

    calc.ratio <- function(peak1, peak2, ratio.method = c("mean", "sum", "median", "geometric.mean", "slope", "weighted.vector.sum") ) {

      #Define function for geometric mean
      gmean <- function(x) {exp(mean(log(x)))}
      #gsd <- function(x) {exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x))) } #geometric SD
      #gse <- function(x) {(exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) /sqrt(length(x))} #geometric SE

      slope <- function(x, y) {model <- lm(x ~ y+0, weights = x) #Note order of x and y to get correct slope!
      sl <- model$coefficients[[1]]
      sl}

      weighted.vector.sum <- function(x, y){

        df <- cbind(x,y)

        avg.ions <- ( sum(df[ ,1]) + sum(df[ ,2]) ) / length(df[ ,1])

        scan.ions <- (df[ ,1] + df[ ,2])

        weighted.x <- avg.ions/ scan.ions  * as.numeric(df[ ,1])
        weighted.y <- avg.ions/ scan.ions  * as.numeric(df[ ,2])
        ratio <- sum(weighted.x) / sum(weighted.y)
        ratio}

      if(ratio.method == "mean") { o <- mean(peak1/peak2); o } else {
        if(ratio.method == "slope") {o <- slope(peak1, peak2); o } else{
          if(ratio.method == "sum") {o <- sum(peak1)/sum(peak2); o } else{
            if(ratio.method == "geometric.mean") {o <- gmean(peak1/peak2); o } else{
              if(ratio.method == "weighted.vector.sum") {o <- weighted.vector.sum(peak1, peak2); o } else{
                if(ratio.method == "median") {o <- median(peak1/peak2); o } else{print("`ratio.method` has to be `mean`, `sum`, `median`, `geometric.mean`, `slope` or `weighted.vector.sum`")
                }
              }}}}}

    }


    ####

    ##Actual number crunching to generate Excel output for dual inlet

    calc.output <- function(df, INPUT_RatioMethod){

      df.stat <- df  %>%  group_by(filename,
                                   Injection,
                                   Sample.Name,
                                   compound,
                                   Basepeak,
                                   isotopolog) %>%

        mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

        mutate(Ratio.SEM = se(ions.incremental/Basepeak.Ions))      #For simplicity use basic standard error for all options

      df.stat <- df.stat %>% mutate(No.of.scans = length(Ratio),
                                    Mins.to.1mio = (1E6/sum(ions.incremental)) * ( max(time.min) - min(time.min)),
                                    Shot.Noise.permil = 1000 * (sqrt((sum(ions.incremental) + sum(Basepeak.Ions)) / (sum(ions.incremental) * sum(Basepeak.Ions)))),
                                    relSE.permil = 1000*(Ratio.SEM/Ratio)) %>%

        #Round values for output
        mutate(Ratio = round(Ratio, 8),
               Ratio.SEM = round(Ratio.SEM, 8),
               Shot.Noise.permil = round(Shot.Noise.permil, 3),
               relSE.permil = round(relSE.permil, 3),
               Mins.to.1mio = round(Mins.to.1mio, 2)) %>%

        select(filename,
               Injection,
               Sample.Name,
               Basepeak,
               compound,
               isotopolog,
               Ratio,
               Ratio.SEM,
               relSE.permil,
               Shot.Noise.permil,
               Mins.to.1mio,
               No.of.scans) %>%
        unique() %>% mutate(Injection= as.integer(Injection)) %>%
        arrange(filename, Injection, isotopolog)

      df.stat <- as.data.frame(df.stat)
      return(df.stat)
    }

    ##Actual number crunching to generate Excel output for single analysis

    calc.output.single <- function(df, INPUT_RatioMethod){

      df.stat <- df  %>%  group_by(filename,
                                   #Injection,
                                   Sample.Name,
                                   compound,
                                   Basepeak,
                                   isotopolog) %>%

        mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

        mutate(Ratio.SEM = se(ions.incremental/Basepeak.Ions))      #For simplicity use basic standard error for all options

      df.stat <- df.stat %>% mutate(No.of.scans = length(Ratio),
                                    Mins.to.1mio = (1E6/sum(ions.incremental)) * ( max(time.min) - min(time.min)),
                                    Shot.Noise.permil = 1000 * (sqrt((sum(ions.incremental) + sum(Basepeak.Ions)) / (sum(ions.incremental) * sum(Basepeak.Ions)))),
                                    relSE.permil = 1000*(Ratio.SEM/Ratio)) %>%

        #Round values for output
        mutate(Ratio = round(Ratio, 8),
               Ratio.SEM = round(Ratio.SEM, 8),
               Shot.Noise.permil = round(Shot.Noise.permil, 3),
               relSE.permil = round(relSE.permil, 3),
               Mins.to.1mio = round(Mins.to.1mio, 2)) %>%

        select(filename,
               # Injection,
               Sample.Name,
               Basepeak,
               compound,
               isotopolog,
               Ratio,
               Ratio.SEM,
               relSE.permil,
               Shot.Noise.permil,
               Mins.to.1mio,
               No.of.scans) %>%
        unique() %>% #mutate(Injection= as.integer(Injection)) %>%
        #arrange(filename, Injection, isotopolog)
        arrange(filename, isotopolog)

      df.stat <- as.data.frame(df.stat)
      return(df.stat)
    }


    ##Actual number crunching to generate Excel output for single analysis

    calc.output.flow <- function(df, INPUT_RatioMethod){

      df.stat <- df  %>%  group_by(filename,
                                   #Injection,
                                   #Sample.Name,
                                   compound,
                                   Basepeak,
                                   isotopolog) %>%

        mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

        mutate(Ratio.SEM = se(ions.incremental/Basepeak.Ions))      #For simplicity use basic standard error for all options

      df.stat <- df.stat %>% mutate(No.of.scans = length(Ratio),
                                    Mins.to.1mio = (1E6/sum(ions.incremental)) * ( max(time.min) - min(time.min)),
                                    Shot.Noise.permil = 1000 * (sqrt((sum(ions.incremental) + sum(Basepeak.Ions)) / (sum(ions.incremental) * sum(Basepeak.Ions)))),
                                    relSE.permil = 1000*(Ratio.SEM/Ratio)) %>%

        #Round values for output
        mutate(Ratio = round(Ratio, 8),
               Ratio.SEM = round(Ratio.SEM, 8),
               Shot.Noise.permil = round(Shot.Noise.permil, 3),
               relSE.permil = round(relSE.permil, 3),
               Mins.to.1mio = round(Mins.to.1mio, 2)) %>%

        select(filename,
               # Injection,
               #Sample.Name,
               Basepeak,
               compound,
               isotopolog,
               Ratio,
               Ratio.SEM,
               relSE.permil,
               Shot.Noise.permil,
               Mins.to.1mio,
               No.of.scans) %>%
        unique() %>% #mutate(Injection= as.integer(Injection)) %>%
        #arrange(filename, Injection, isotopolog)
        arrange(filename, isotopolog)

      df.stat <- as.data.frame(df.stat)
      return(df.stat)
    }


    # Function: make.xls(); save the summary statistics table as Excel --------


    make.xls <- function(df.stat, params){ wb <- createWorkbook()
    tabs <- levels(df.stat$isotopolog)

    for (i in tabs) {
      addWorksheet(wb, i)
      writeData(wb, i, df.stat[df.stat$isotopolog==i, ])
    }

    addWorksheet(wb, "IsoXL Parameters")
    writeData(wb, "IsoXL Parameters" , params)

    return(wb)

    }




    make.xls.onetab <- function(df.stat, params){ wb <- createWorkbook()
    tabs <- levels(df.stat$filename)

    for (i in tabs) {
      addWorksheet(wb, i)
      writeData(wb, i, df.stat[df.stat$filename==i, ])
    }

    addWorksheet(wb, "IsoXL Parameters")
    writeData(wb, "IsoXL Parameters" , params)

    return(wb)

    }



    # Plot output: Dual Inlet --------------------------------------------------------


    output$plot <- renderPlotly({


      ggplotly(

        ggplot(data = data.output(), aes(x = Injection, y = Ratio, color=isotopolog)) + scale_color_brewer(palette="Dark2") +

          geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
                            ymax=(Ratio+Ratio.SEM)),
                        width=0.7,
                        size=0.3,
                        color="black",
                        position=position_dodge(.9))       +        geom_point(size=3, shape=10) + facet_wrap(~ filename, ncol=1), # temp: could be interesting option: + facet_grid(isotopolog~.),

        dynamicTicks = TRUE #change ticks on zoom
      ) %>%

        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar

        config( #options on how to save an image
          toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 600,
            scale = 4,
            filename = paste("dual_inlet", input$upload_Dual_Inlet$name, sep="_")
          )
        )

      #plot_ly(data.output(), x = ~Injection, y = ~Ratio, type = 'scatter', mode = 'markers', color=factor(data.output$isotopolog()))
    })

    # Plot output: Single Analysis--------------------------------------------------------


    output$plot.single <- renderPlotly({


      ggplotly(

        ggplot(data = data.single_output(), aes(x = Shot.Noise.permil, y = relSE.permil, color=isotopolog)) + scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

          # geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
          #                   ymax=(Ratio+Ratio.SEM)),
          #               width=0.7,
          #               size=0.3,
          #               color="black",
          #               position=position_dodge(.9))       +
          geom_point(size=3, shape=10) +
          geom_abline(slope=1) +
          ylab(paste("relative SEM ", "[\u2030]")) + xlab(paste("shot noise limit", "[\u2030]")),

        dynamicTicks = TRUE #change ticks on zoom
      )%>%

        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar

        config( #options on how to save an image
          toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 600,
            scale = 4,
            filename = paste("single_analysis", input$upload_Single_Analysis$name, sep="_")
          )
        )


    })

    # Plot output: Flow Injection--------------------------------------------------------


    output$plot.flow <- renderPlotly({


      ggplotly(

        ggplot(data = data.flow_output(), aes(x = filename, y = Ratio, color=isotopolog)) + scale_color_brewer(palette="Dark2") + #temp: could be segments... for x axis

          geom_errorbar(aes(ymin=(Ratio-Ratio.SEM),
                            ymax=(Ratio+Ratio.SEM)),
                        width=0.7,
                        size=0.3,
                        color="black",
                        position=position_dodge(.9))       +         geom_point(size=3, shape=10) ,

        dynamicTicks = TRUE #change ticks on zoom
      )%>%

        config(displaylogo = FALSE,
               modeBarButtonsToRemove = c("zoomIn2d", "zoomOut2d", "lasso2d", "select2d", "hoverClosestCartesian","hoverCompareCartesian")) %>% # buttons https://plotly-r.com/control-modebar.html#control-modebar

        config( #options on how to save an image
          toImageButtonOptions = list(
            format = "png",
            width = 900,
            height = 600,
            scale = 4,
            filename = paste("flow_injection", input$upload_Flow_Injection$name, sep="_")
          )
        )

    })


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




    # Table output: Dual Inlet, show rows of isox file------------------------------------------------


    #test case
    # output$mytable = DT::renderDataTable({
    #
    # DT::datatable(
    #   mtcars,
    #   data.dual_inlet_filtered(),
    #   options = list(columnDefs = list(list(
    #     targets = 0:7, visible = FALSE
    #   )), buttons = c('colvis'), dom = 'Bfrtip'),
    #   extensions = 'Buttons'
    # )
    # })

    output$mytable = DT::renderDataTable({
      DT::datatable( data.dual_inlet_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder', 'Select'),

                 options = list(
                   #   autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scroll bar to see enture table

                   #   columnDefs = list(list(
                   #
                   #        targets = c(2L), visible = FALSE #hide some columns
                   # )),

                   dom = 'Bfrtip',

                   # buttons = list(
                   #   list(extend = c("colvis","excel"),
                   #        exportOptions = list(columns = ":visible"))
                   # ),
                   buttons = list(list(extend = 'colvis'),
                                  list(extend = 'copy', exportOptions = list(columns = ':visible')),
                                  list(extend = 'excel', exportOptions = list(columns = ':visible')),
                                  list(extend = 'pdf', exportOptions = list(columns = ':visible'))
                   ), #temp donwload only visible columns

                   #   buttons = c('colvis','copy',  'excel', 'pdf'),
                   # exportOptions = list(columns = ":visible"),
                   colReorder = TRUE#, #columns can be reordered,
                   # selection = list(target = "column")#,
                   # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
                   #
                 )
      )
      #How to export only visible columns?
      # buttons = list(
      #   list(extend = "excel", exportOptions = list(columns = ":visible"))
      # )



      #https://datatables.net/reference/option/columns.visible

    })



    # Table output: Dual Inlet, summary statistics ----------------------------


    output$summary <- DT::renderDataTable({

      datatable( data.output(), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder'),
                 options = list(
                   # autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scrollbar when table is not visible
                   columnDefs = list(list(
                     targets = c(7L, 10L), visible = FALSE
                   )),
                   dom = 'Bfrtip',
                   buttons = list(list(extend = 'colvis'),
                                  list(extend = 'copy', exportOptions = list(columns = ':visible')),
                                  list(extend = 'excel', exportOptions = list(columns = ':visible')),
                                  list(extend = 'pdf', exportOptions = list(columns = ':visible'))
                   ), #temp donwload only visible columns; validate more thoroughly and implement in all tables; define common sytles in a function?
                   colReorder = TRUE #columns can be reordered
                 )
      )

    })



    # Download output: Dual Inlet Excel file ----------------------------------


    output$download_Dual_Inlet_xlsx <- downloadHandler(
      filename = function(){  paste(tools::file_path_sans_ext(input$upload_Dual_Inlet$name), "_IsoXL.xlsx", sep="")},
      content = function(file) {
        saveWorkbook(data.xls(), file)

      }
    )


    # Table output: Single Analysis, summary statistics ----------------------------


    output$summary.single <- DT::renderDataTable({

      datatable( data.single_output(), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder'),
                 options = list(
                   # autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scrollbar when table is not visible
                   columnDefs = list(list(
                     targets = c(6L, 10L), visible = FALSE #hide some columns so the table is not as wide, can be activated using column button; remain in output
                   )),
                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE #columns can be reordered
                 )
      )

    })


    #####

    # Table output: Flow Analysis, summary statistics ----------------------------


    output$summary.flow <- DT::renderDataTable({

      datatable( data.flow_output(), rownames= FALSE , #temp, table with stat did not render
                 extensions = c('Buttons','ColReorder'),
                 options = list(
                   # autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scrollbar when table is not visible
                   columnDefs = list(list(
                     targets = c(6L, 9L), visible = FALSE #using these rows, the table does not display
                   )),
                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE #columns can be reordered
                 )
      )

    })




    # Table output: Flow Analysis show rows of isox file------------------------------------------------


    output$mytable.flow = DT::renderDataTable({

      datatable( data.flow_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder', 'Select'),

                 options = list(
                   #   autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scroll bar to see enture table

                   #   columnDefs = list(list(
                   #
                   #        targets = c(2L), visible = FALSE #hide some columns
                   # )),

                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE#, #columns can be reordered,
                   # selection = list(target = "column")#,
                   # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
                 )
      )
      #How to export only visible columns?
      # buttons = list(
      #   list(extend = "excel", exportOptions = list(columns = ":visible"))
      # )

      # DT::datatable(
      #   mtcars,
      #   options = list(columnDefs = list(list(
      #     targets = 0:7, visible = FALSE
      #   )), buttons = c('colvis'), dom = 'Bfrtip'),
      #   extensions = 'Buttons'
      # )


      #https://datatables.net/reference/option/columns.visible

    })


    # Download output: Single Analysis Excel file ----------------------------------


    output$download_Flow_xlsx <- downloadHandler(
      filename = function(){  paste(tools::file_path_sans_ext(input$upload_Flow_Injection$name), "_IsoXL.xlsx", sep="")},
      content = function(file) {
        saveWorkbook(data_flow.xls(), file)
      }
    )


    ####



    # Table output: Single Analysis show rows of isox file------------------------------------------------


    output$mytable.single = DT::renderDataTable({

      datatable( data.single_filtered() %>% mutate_if(is.numeric, round, digits=4), rownames= FALSE ,
                 extensions = c('Buttons','ColReorder', 'Select'),

                 options = list(
                   #   autoWidth = TRUE, #scroll bar to see enture table
                   scrollX = TRUE, #scroll bar to see enture table

                   #   columnDefs = list(list(
                   #
                   #        targets = c(2L), visible = FALSE #hide some columns
                   # )),

                   dom = 'Bfrtip',
                   buttons = c('colvis','copy',  'excel', 'pdf' ),
                   colReorder = TRUE#, #columns can be reordered,
                   # selection = list(target = "column")#,
                   # options = list(ordering = FALSE, searching = FALSE, pageLength = 5)
                 )
      )
      #How to export only visible columns?
      # buttons = list(
      #   list(extend = "excel", exportOptions = list(columns = ":visible"))
      # )

      # DT::datatable(
      #   mtcars,
      #   options = list(columnDefs = list(list(
      #     targets = 0:7, visible = FALSE
      #   )), buttons = c('colvis'), dom = 'Bfrtip'),
      #   extensions = 'Buttons'
      # )


      #https://datatables.net/reference/option/columns.visible

    })


    # Download output: Single Analysis Excel file ----------------------------------


    output$download_Single_xlsx <- downloadHandler(
      filename = function(){  paste(tools::file_path_sans_ext(input$upload_Single_Analysis$name), "_IsoXL.xlsx", sep="")},
      content = function(file) {
        saveWorkbook(data_single.xls(), file)
      }
    )


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


    # Output table: Make IsoX Input  ------------------------------------------

    #temporary section

    INPUT_formula.check <- reactive({
      run_check(INPUT_formula = input$INPUT_IsoX_sumformula)
    })

    #data(isotopes) not found 12-June-2022
     data(isotopes, package = "enviPat")#neeed for isopat function

    pattern <- reactive({

      validate(
        need(sum(as.data.frame(check_chemform(isotopes, chemforms= INPUT_formula.check(), get_list=TRUE))) <= 200, 'Sum formula has unusually many atoms (>200)')
      )

      data.out <- isopattern(
        isotopes=isotopes,
        chemforms = INPUT_formula.check(),
        threshold =  input$INPUT_IsoX_threshold,
        plotit = FALSE,
        charge = input$INPUT_IsoX_charge, #eg 1 or -1
        emass = 0.00054858,
        algo = 1,
        rel_to = 4,
        verbose = TRUE,
        return_iso_calc_amount = FALSE
      )


      validate(
        need(data.out[1] != "error", 'ERROR: cannot parse this sum formula with isopattern function') #example: NO00; not caught by check_chemform; still issues with P10; Si10 check validation/ code more
      ) #temp

      return(data.out)
    })




    out.isox <- reactive({ make.isox.input(pattern.list = pattern(), #temp!
                                           INPUT_compound = input$INPUT_IsoX_compound,
                                           INPUT_charge = input$INPUT_IsoX_charge,
                                           INPUT_tolerance= input$INPUT_IsoX_tolerance,
                                           INPUT_massOffset = input$INPUT_IsoX_massoffset/1000) #mDa!

    })





    # Preview isotopologs.tsv --------------------------------------------------



    output$isox = DT::renderDataTable({

      # validate(
      #   need(1>2, c("","Thanks for testing this feature. Hope to have it implemented soon."))
      # )
      #
      datatable( out.isox(), rownames= FALSE ,
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
                   #buttons = c('copy' )#,
                   #colReorder = TRUE#, #columns can be reordered,
                 )
      )



    })


    # Download isotopologs.tsv ------------------------------------------------

    output$download_IsoX <- downloadHandler(
      filename = function(){"isotopologs.tsv"},
      content = function(fname){
        write.table(out.isox(), fname, sep="\t", row.names =FALSE, quote = FALSE)
      }
    )


    })

}

