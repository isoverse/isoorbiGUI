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
    ggplot2::aes(x = neff, y = rse) +
    ggplot2::geom_line()
}

