# common core logic functions

#' @title iso_simplify_isox_file
#' @description Keep only columns that are useful for isotopocule ratio calculation
#' @param dataset The file.tsv that is to be simplified
#' @return Returns simplified IsoX file with the columns: 'filename', 'scan.no', 'time.min', 'compound', 'isotopocule', 'ions.incremental', 'tic', 'it.ms'.
#' @export
# iso_simplify_isox_file <- function(dataset) {
#   df.out <- dataset %>% dplyr::select(filename,
#                                scan.no,
#                                time.min,
#                                compound,
#                                isotopocule,
#                                ions.incremental,
#                                tic, #ok to keep always?
#                                it.ms) #ok to keep always?
# }

#FIXME-LEIF: complete @param input
#' @title remove.rare
#' @description Remove isotopocules that are not consistently detected across scans
#' @param data Simplified IsoX file to be processed
#' @param input ...
#' @return Returns dataframe with inconsistent isotopocules removed
#' @export
# remove.rare <- function(data, input){ #temp check functions correctly in different tabs
#
#   # validate(
#   #   need(input$rare, 'Missing input input$rare'),
#   #   need(input$rare <=100, 'Check input input$rare is smaller than 100%')
#   # )
#
#   remove.df <- data %>%
#     dplyr::group_by(filename) %>%
#     dplyr::mutate(n.scans = max(scan.no) - min(scan.no)) %>% #FIXME: implement a more general solution here
#     dplyr::group_by(filename, compound, isotopocule) %>%
#     dplyr::mutate(i.scans = length(scan.no)) %>%
#     dplyr::filter(i.scans < input$rare/100 * n.scans) %>% # => update selection in GUI?, add message?
#     dplyr::select(-n.scans, -i.scans) %>% droplevels() %>% as.data.frame()
#
#
#   df <- anti_join(data, remove.df, by = c("filename", "scan.no", "time.min", "compound", "isotopocule", "ions.incremental", "tic", "it.ms")) #FIXME:More elegant/robust solution?
#
#   return(df)
#
# }

#FIXME-LEIF: complete and confirm
#' @title remove.satellitePeaks
#' @description Remove minor signals (e.g., satellite peaks) that were reported by IsoX
#' @param df Simplified IsoX file to have satellites removed from
#' @return IsoX file with satellites removed
#' @export
# remove.satellitePeaks <- function(df){
#
#   df <- df %>%
#     dplyr::group_by(filename, compound, scan.no, isotopocule) %>%
#     dplyr::filter(ions.incremental == max(ions.incremental))
#
#   return(df)
#
#}

#FIXME-LEIF: complete and confirm
#' @title Remove.ticxit
#' @description Remove extreme scans based on TIC x Injection time
#' @param df Simplified IsoX file to have 'ticxit' removed
#' @param input ???
#' @return Returns filtered IsoXfile based of input value
#' @export
# remove.ticxit <- function(df, input){
#
#   # validate(
#   #   need(input$extreme, 'Missing input input$extreme'),
#   #   need(input$extreme <=10, 'Check input input$extreme is smaller than 10%')
#   # )
#
#
#   df <- df %>% dplyr::group_by(filename) %>% dplyr::mutate(TICxIT = tic * it.ms) %>% #edit 28-Feb-2022 enable multiple dual inlet files
#     dplyr::filter( TICxIT > quantile(TICxIT, input$extreme/100) & TICxIT < quantile(TICxIT, 1- input$extreme/100)) %>%
#     dplyr::select(-TICxIT)
#
#   return(df)
#}

#FIXME-LEIF: complete and confirm
#' @title iso_filter_isox_file
#' @description A basic dplyr::filter(should be generalized for most use cases)
#' @param dataset The file.tsv to be filtered
#' @param input ???
#' @return Returns a filtered IsoX file
#' @export
iso_filter_isox_file <- function(dataset, input) {
  df.out <- dataset %>%
    # isotopocule filter
    dplyr::filter(.data$isotopocule %in% c(input$isotopocule,input$base_peak)) %>% #maybe this line should be moved out of the core function!
    # file name filter
    {
      if (!"all" %in% input$filename)
        dplyr::filter(., .data$filename %in% input$filename)
      else
        .
    } %>%
    # compounds filter
    {
      if (!"all" %in% input$compound)
        dplyr::filter(., .data$compound %in% input$compound)
      else
        .
    } %>%
    # timerange filter
    dplyr::filter(.data$time.min >= input$timerange[1], .data$time.min <= input$timerange[2])


  return(df.out)
}


# Functions to calculate stats --------------------------------------------

# isox data manipulation

#' @title se
#' @description Define 'se' as a new function to calculate standard error
#' @param x value to sompute standard error from
#' @return Returns value of standard error
#' @export
se <- function(x) { sd(x) / sqrt(length(x)) }

#' @title gmean
#' @description  Define 'gmean' to calculate geometic mean
#' @param x value to compute geometric mean from
#' @return The calculated geometric mean
#' @export
gmean <- function(x)
  exp(mean(log(x))) #define geometric mean

#' @title gsd
#' @description  Define 'gsd' to calculate geometic standard deviation
#' @param x value to compute geometric standard deviation from
#' @return The calculated geometric standard deviation
#' @export
gsd <-
  function(x)
    exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))

#' @title gse
#' @description  Define 'gse' to calculate geometic standard error
#' @param x value to compute geometric standard error from
#' @return The calculated geometric standard error
#' @export
gse <-
  function(x)
    (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) / sqrt(length(x))

#' @title slope
#' @description  Define 'slope' to calculate geometic mean
#' @param x ???
#' @param y ???
#' @return The slope
#' @export
slope <-
  function(x, y) {
    model <-
      lm(x ~ y + 0, weights = x) #Note order of x and y to get correct slope!
    sl <- model$coefficients[[1]]
    sl
  }

#' @title weighted.vector.sum
#' @description ???
#' @param x ???
#' @param y ???
#' @return ???
#' @export
weighted.vector.sum <- function(x, y) {
  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])
  ratio <- sum(weighted.x) / sum(weighted.y)
  ratio
}

#' @title calc.ratio
#' @description Ratio calculation between peaks of interest. NOTE: The formula used to average ratios matters! Do not simply use arithmetic mean.
#' @param peak1 Peak 1 of interest
#' @param peak2 Peak 2 of interest
#' @param ratio.method The desired method of computing ratio
#' @return calculated ratio between peak1 and peak2 with selected ratio method
#' @export
calc.ratio <- function(peak1, peak2, ratio.method = c("mean", "sum", "median", "geometric.mean", "slope", "weighted.vector.sum") ) {

  if(ratio.method == "mean") { o <- mean(peak1/peak2); o } else {
    if(ratio.method == "slope") {o <- slope(peak1, peak2); o } else{
      if(ratio.method == "sum") {o <- sum(peak1)/sum(peak2); o } else{
        if(ratio.method == "geometric.mean") {o <- gmean(peak1/peak2); o } else{
          if(ratio.method == "weighted.vector.sum") {o <- weighted.vector.sum(peak1, peak2); o } else{
            if(ratio.method == "median") {o <- stats::median(peak1/peak2); o } else{print("`ratio.method` has to be `mean`, `sum`, `median`, `geometric.mean`, `slope` or `weighted.vector.sum`")
            }
          }}}}}

}


#' @title make.xls
#' @description Save the summary statistics table as Excel
#' @param df.stat The IsoX file with statistics of interest
#' @param params IsoXL parameters
#' @return Returns statistics of interest as an excel file
#' @export
make.xls <- function(df.stat, params){ wb <- createWorkbook()
tabs <- levels(df.stat$isotopocule)

for (i in tabs) {
  addWorksheet(wb, i)
  writeData(wb, i, df.stat[df.stat$isotopocule==i, ])
}

addWorksheet(wb, "IsoXL Parameters")
writeData(wb, "IsoXL Parameters" , params)

return(wb)

}

#' @title make.xls.onetab
#' @description add a tab to record parameters
#' @param df.stat The IsoX file with statistics of interest
#' @param params IsoXL parameters
#' @return Return modified excel workbook
#' @export
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


