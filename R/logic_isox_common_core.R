#' common core logic functions
#' @export

#Keep only columns that are useful for isotopocule ratio calculation
iso_simplify_isox_file <- function(dataset) {
  df.out <- dataset %>% select(filename,
                               scan.no,
                               time.min,
                               compound,
                               isotopolog,
                               ions.incremental)
}

# Function: remove.rare(df, input$rare) -----------------------------------



remove.rare <- function(df, input){ #temp check functions correctly in different tabs

  # validate(
  #   need(input$rare, 'Missing input input$rare'),
  #   need(input$rare <=100, 'Check input input$rare is smaller than 100%')
  # )

  remove.df <- df %>%
    group_by(filename) %>%
    mutate(n.scans = max(scan.no) - min(scan.no)) %>%
    group_by(filename, compound, isotopolog) %>%
    mutate(i.scans = length(scan.no)) %>%
    filter(i.scans < input$rare/100 * n.scans) %>% # input
    select(-n.scans, -i.scans) %>% droplevels() %>% as.data.frame()


  df <- anti_join(df, remove.df)

  return(df)

}



# Function: remove.satellitePeaks(df) -------------------------------------


remove.satellitePeaks <- function(df){

  df <- df %>%
    group_by(filename, compound, scan.no, isotopolog) %>%
    filter(ions.incremental == max(ions.incremental))

  return(df)

}

# Function: remove.ticxit(df, input$extreme) -------------------------------

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





#Basic filter, should be generalized for all modules
iso_filter_isox_file <- function(dataset, input) {
  df.out <- dataset %>%
    # isotopolog filter
    dplyr::filter(isotopolog %in% c(input$isotopolog,input$base_peak)) %>% #maybe this line should be moved out of the core function!
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
    dplyr::filter(time.min >= input$timerange[1], time.min <= input$timerange[2])


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


