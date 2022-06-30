#' common core logic functions


#Keep only columns that are useful for isotopocule ratio calculation
iso_simplify_isox_file <- function(dataset) {
  df.out <- dataset %>% select(filename,
                               scan.no,
                               time.min,
                               compound,
                               isotopolog,
                               ions.incremental)
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
