# functions specific to shot noise module


#' @title Assign base peak for shotnoise calculation
#' @description This function assigns a base peak to data for shotnoise calculation
#' @param data a dataframe output from `iso_simplify_isox_file()`
#' @param input the isotopocule assigned to be the base peak for ratio calculation
#' @return processed dataframe containing new columns `Basepeak.Ions`, `Isotopolog. Ions`, and `Ratio`
assign_basepeak_shotnoise <- function(data, input) {

#FIXME: what to do when beasepeak and signal are the same? add Input validation
  df.out <- data %>%
    filter(isotopolog %in% c(input$base_peak, input$isotopolog)) %>% #cross check with core logic filter function
    droplevels() %>%
    pivot_wider(names_from = isotopolog, values_from = ions.incremental) %>% na.omit() %>%

    rename(Basepeak.Ions = input$base_peak,
           Isotopolog.Ions = input$isotopolog)%>%
    mutate(Ratio = Isotopolog.Ions/Basepeak.Ions)

  return(df.out)
}


#' shot noise calculation (yields a vector!)
#' @title Shot noise calculation
#' @description This function takes a dataset and input[?]
#' @param dataset The dataset
#' @return processed dataframe, as vector
calc_shotnoise <- function(dataset){

  df.out <- data.frame(
    scans = 1:length(dataset$scan.no),
    neff = 1:length(dataset$scan.no),
    rse = 1:length(dataset$scan.no),
    shotnoise = 1:length(dataset$scan.no),
    shotnoise.neff = 1:length(dataset$scan.no)
  )

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

  df.out <- df.out %>% na.omit() %>% select(-shotnoise, -shotnoise.neff)

  df.out <- df.out %>% tidyr::pivot_longer(-c("scans", "neff"), names_to="key", values_to="value") #FIXME: improve nomenclature of columns

 return(df.out)
}


# Plot: Shot noise --------------------------------------------------------

#' plot shot noise
#' @title Assign basepeak for shotnoise
#' @description This function takes a dataset and input[?]
#' @param dataset The dataset
#' @param input The input
#' @return returns plot
plot_shot_noise <- function(dataset, input) {

  dataset %>%
    ggplot2::ggplot() +
    ggplot2::aes(x = neff, y = value, color = key) +
    ggplot2::geom_point(size = 0.8, alpha = 0.5) +
    geom_abline(intercept = 0,
                slope = -0.5,
                col = "blue") +
    labs(y = "RSE",
         x = "effective number of ions")  +
    theme(legend.position = "none") +

    annotate(
      geom = 'text',
      label = 'shot noise \nlimit',
      color = "blue",
      x = min(dataset$neff),
      y = 0.2*max(dataset$value)
    ) +

    scale_x_continuous(
      trans = log10_trans(),
      breaks = c( 10000, 100000, 1000000, 10000000, 100000000),
      labels = c("1E4", "1E5", "1E6", "1E7", "1E8")
    ) +
    scale_y_continuous(
      trans = log10_trans(),
      breaks = c(0.1, 0.05, 0.01,  0.002, 0.001, 0.0005, 0.0002, 0.0001),
      labels = c(
        "100 permil",
        "50 permil",
        "10 permil",
        "2 permil" ,
        "1 permil",
        "0.5 permil",
        "0.2 permil",
        "0.1 permil"
      )
      # ,
      # limits = c(0.0001, 0.01)
    )

}
