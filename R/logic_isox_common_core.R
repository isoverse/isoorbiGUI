# common core logic functions
#' Keep only columns that are useful for isotopocule ratio calculation
#' @export
iso_simplify_isox_file <- function(dataset) {
  df.out <- dataset %>% select(filename,
                               scan.no,
                               time.min,
                               compound,
                               isotopolog,
                               ions.incremental)
}

#' Remove isotopologs that are not consistently detected across scans
#' @export
remove.rare <- function(data, input){ #temp check functions correctly in different tabs

  # validate(
  #   need(input$rare, 'Missing input input$rare'),
  #   need(input$rare <=100, 'Check input input$rare is smaller than 100%')
  # )

  remove.df <- data %>%
    group_by(filename) %>%
    mutate(n.scans = max(scan.no) - min(scan.no)) %>% #implement a more general solution here
    group_by(filename, compound, isotopolog) %>%
    mutate(i.scans = length(scan.no)) %>%
    filter(i.scans < input$rare/100 * n.scans) %>% # => update selection in GUI?, add message?
    select(-n.scans, -i.scans) %>% droplevels() %>% as.data.frame()


  df <- anti_join(data, remove.df)

  return(df)

}



#' Remove minor signals (e.g., satellite peaks) that were reported by IsoX
#' @export
remove.satellitePeaks <- function(df){

  df <- df %>%
    group_by(filename, compound, scan.no, isotopolog) %>%
    filter(ions.incremental == max(ions.incremental))

  return(df)

}

#' Remove extreme scans based on TIC x Injection time
#' @export
remove.ticxit <- function(df, input){

  validate(
    need(input$extreme, 'Missing input input$extreme'),
    need(input$extreme <=10, 'Check input input$extreme is smaller than 10%')
  )


  df <- df %>% group_by(filename) %>% mutate(TICxIT = tic * it.ms) %>% #edit 28-Feb-2022 enable multiple dual inlet files
    filter( TICxIT > quantile(TICxIT, input$extreme/100) & TICxIT < quantile(TICxIT, 1- input$extreme/100)) %>%
    select(-TICxIT)

  return(df)
}


#' A basic filter (should be generalized for most use cases)
#' @export
iso_filter_isox_file <- function(dataset, input) {
  df.out <- dataset %>%
    # isotopolog filter
    dplyr::filter(isotopolog %in% c(input$isotopolog,input$base_peak)) %>% #maybe this line should be moved out of the core function!
    # file name filter
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
    dplyr::filter(time.min >= input$timerange[1], time.min <= input$timerange[2])


  return(df.out)
}


# Functions to calculate stats --------------------------------------------

#' isox data manipulation

#' Define 'se' as a new function to calculate standard error
#' @export
se <- function(x) { sd(x) / sqrt(length(x)) }

#' Define 'gmean' to calculate geometic mean
#' @export
gmean <- function(x)
  exp(mean(log(x))) #define geometric mean

gsd <-
  function(x)
    exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))
gse <-
  function(x)
    (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) / sqrt(length(x))

slope <-
  function(x, y) {
    model <-
      lm(x ~ y + 0, weights = x) #Note order of x and y to get correct slope!
    sl <- model$coefficients[[1]]
    sl
  }

weighted.vector.sum <- function(x, y) {
  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])
  ratio <- sum(weighted.x) / sum(weighted.y)
  ratio
}

#' Ratio calculation
#' NOTE: The formula used to average ratios matters! Do not simply use arithmetic mean.
#' @export
calc.ratio <- function(peak1, peak2, ratio.method = c("mean", "sum", "median", "geometric.mean", "slope", "weighted.vector.sum") ) {

  if(ratio.method == "mean") { o <- mean(peak1/peak2); o } else {
    if(ratio.method == "slope") {o <- slope(peak1, peak2); o } else{
      if(ratio.method == "sum") {o <- sum(peak1)/sum(peak2); o } else{
        if(ratio.method == "geometric.mean") {o <- gmean(peak1/peak2); o } else{
          if(ratio.method == "weighted.vector.sum") {o <- weighted.vector.sum(peak1, peak2); o } else{
            if(ratio.method == "median") {o <- median(peak1/peak2); o } else{print("`ratio.method` has to be `mean`, `sum`, `median`, `geometric.mean`, `slope` or `weighted.vector.sum`")
            }
          }}}}}

}




#' Save the summary statistics table as Excel
#' @export
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

#add a tab to record parameters
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


