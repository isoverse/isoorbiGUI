#Caj: insert here previous calc_SN function

calc_shotnoise <- function(dataset){

  df.out <- data.frame(
    scans = 1:length(dataset$scan.no),
    neff = 1:length(dataset$scan.no),
    rse = 1:length(dataset$scan.no),
    shotnoise = 1:length(dataset$scan.no),
    shotnoise.neff = 1:length(dataset$scan.no)
  )

  #Geometric mean, geometric SD and SE
  #Caj: these functions should eventually be placed in a separate location

  gmean <- function(x)
    exp(mean(log(x))) #define geometric mean
  gsd <-
    function(x)
      exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))
  gse <-
    function(x)
      (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) / sqrt(length(x))

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
}

plot_shot_noise <- function(dataset, input) {

  dataset %>%
    # isotopolog filter
    dplyr::filter(isotopolog == input$isotopolog) %>%
    # filename filter
    {
      if (!"all" %in% input$filename)
        dplyr::filter(., filename %in% input$filename)
      else
        .
    } %>%
    # compounds filter
    {
      if (!"all" %in% input$compound)
        dplyr::filter(., compound %in% input$compound)
      else
        .
    } %>%
    # timerange filter
    dplyr::filter(time.min >= input$timerange[1], time.min <= input$timerange[2]) %>%
    calc_shotnoise() %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = neff, y = rse) +
    ggplot2::geom_line()


}






