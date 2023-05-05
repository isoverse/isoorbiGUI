# common core logic functions




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




#' @title make.xls
#' @description Save the summary statistics table as Excel
#' @param df.stat The IsoX file with statistics of interest
#' @param params IsoXL parameters
#' @return Returns statistics of interest as an excel file
#' @export
make.xls <- function(df.stat, params){ wb <- createWorkbook()
tabs <- levels(df.stat$isotopocule)

for (i in tabs) {
  addWorksheet(wb, stringr::str_trunc(i, 31)) #excel worksheet can max have 31 characters
  writeData(wb, stringr::str_trunc(i, 31), df.stat[df.stat$isotopocule==i, ])
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
  addWorksheet(wb, stringr::str_trunc(i, 31)) #excel worksheet can max have 31 characters
  writeData(wb, stringr::str_trunc(i, 31), df.stat[df.stat$filename==i, ])
}

addWorksheet(wb, "IsoXL Parameters")
writeData(wb, "IsoXL Parameters" , params)

return(wb)

}


