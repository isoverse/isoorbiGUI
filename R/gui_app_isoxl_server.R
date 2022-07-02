#' Orbitrap ShinyApp Server
#'
#' Generates the server part of the shiny app.
#' @param local whether running locally or as a server application (e.g. on shinyapps.io)
#' @param reset whether to reset settings if running in local mode (server mode always resets)
orbitrap_server <- function(local = FALSE, reset = FALSE) {

  shinyServer(function(input, output, session) {

    gui_message("info", "starting isoXL GUI")


    # settings server
    settings <- callModule(
      module_settings_server, "settings",
      save_settings = local,
      settings_file = reactive("orbitrap_gui_settings.rds")
    )

    # shotnoise server
    shot_noise <- callModule(module_isoxl_shot_noise_server, id = "shot_noise")

    #contact and info server
    info_in <- callModule(module_isoxl_info_server, id = "info")

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
               remove.ticxit( input = input$INPUT_TICxIT ) %>% #temp: validate correct functioning
               remove.satellitePeaks() %>%
               remove.rare(input = input$INPUT_Rare) )


    })





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

               remove.ticxit(input=input$INPUT_Flow_TICxIT) %>%
               remove.satellitePeaks() %>%
               remove.rare(input = input$INPUT_Flow_Rare) %>%

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

               remove.ticxit(input = input$INPUT_Single_TICxIT) %>%
               remove.satellitePeaks() %>%
               remove.rare(input = input$INPUT_Single_Rare) %>%

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

