#Caj: functions for shotnoise plot




assign_basepeak_shotnoise <- function(dataset, input) {
  df.out <- dataset %>%
    select(filename,
           scan.no,
           time.min,
           compound,
           isotopolog,
           ions.incremental) %>%
    filter(isotopolog %in% c(input$base_peak, input$isotopolog)) %>% #cross check with core logic filter function
    droplevels() %>%
    pivot_wider(names_from = isotopolog, values_from = ions.incremental) %>% na.omit() %>%

    rename(Basepeak.Ions = input$base_peak,
           Isotopolog.Ions = input$isotopolog)%>%
    mutate(Ratio = Isotopolog.Ions/Basepeak.Ions)

  return(df.out)
}



#SN calculation (specific for plot!)
calc_shotnoise <- function(dataset){

  df.out <- data.frame(
    scans = 1:length(dataset$scan.no),
    neff = 1:length(dataset$scan.no),
    rse = 1:length(dataset$scan.no),
    shotnoise = 1:length(dataset$scan.no),
    shotnoise.neff = 1:length(dataset$scan.no)
  )

 # message("info", str(df.out))
 #
  for (l in 1:length(dataset$scan.no)) {
    df <- dataset[1:l,]

    neff <-
      (sum(df$Basepeak.Ions) * sum(df$Isotopolog.Ions)) / (sum(df$Basepeak.Ions) + sum(df$Isotopolog.Ions))
    df.out$neff[l] <- neff

    shotnoise.neff <- neff ^ -0.5
    df.out$shotnoise.neff[l] <- shotnoise.neff

    shotnoise <-
      ((1 / sum(df$Isotopolog.Ions) + (1 / sum(df$Basepeak.Ions)))) ^ 0.5
    df.out$shotnoise[l] <- shotnoise

    rse <- gse(dataset[1:l,]$Ratio) / gmean(dataset[1:l,]$Ratio)
    df.out$rse[l] <- rse
  }
  message("info", str(df.out))
 return(df.out)
}



plot_shot_noise <- function(dataset, input) {

  dataset%>%
    ggplot2::ggplot() +
    ggplot2::aes(x = neff, y = value) +
    ggplot2::geom_line()
}


# Plot: Shot noise --------------------------------------------------------

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

