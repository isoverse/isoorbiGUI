#' core logic functions of isoorbi

#Basic filter, needs to be generalized for all modules
iso_filter_isox_file <- function(dataset, input) {
  df.out <- dataset %>%
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
    # time range filter
    dplyr::filter(time.min >= input$timerange[1], time.min <= input$timerange[2])
  return(df.out)
}
