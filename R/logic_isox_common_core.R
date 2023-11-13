# common core logic functions



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


