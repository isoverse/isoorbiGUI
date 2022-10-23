#' functions specific to dual inlet module

# Dual Inlet: define experiment -------------------------------------------
#' @title define.dual.inlet
#' @description This function defines the experiment and ensure input has required information
#' @param input Input IsoX file for dual inlet analysis
#' @return Returns annotated dataframe for dual inlet analysis
#' @export
define.dual.inlet <-
  function(input) {
    validate(
      need(input$reference, 'Missing input name of reference'),
      need(input$sample, 'Missing input name of sample'),
      need(input$blocks, 'Missing input Blocks'),
      need(input$idle.time, 'Missing input Idle Time'),
      need(input$block.time, 'Missing input Block Time'),
      need(input$startup.time, 'Missing input Startup Time'),

      need(
        input$idle.time < input$block.time,
        'Idle Time is longer than Block Time!'
      ),
      need(input$startup.time < 100, 'Startup time seems too long!'), #FIXME: obsolete with shinyvalidate?
      need(input$blocks < 100, 'Looks like too many blocks!'),
      need(input$blocks > 0, 'Need at lewast one block'),
      need(input$block.time < 100, 'Block Time appears too long'),
      need(input$segments < 10 , 'Looks like too many segments')
    )


    blocks <-
      rep(c(input$reference, input$sample), 1E3)[1:input$blocks] #limited to maximum 1,000 blocks
    blocks <- rep(blocks, each = input$segments)
    segment.time <-
      (input$block.time - input$idle.time) / input$segments
    start.time <-
      0:(input$blocks - 1) * input$block.time + input$idle.time #Define start times to integrate (in minutes)
    out.start <- c()

    for (i in 1:(length(start.time))) {
      a <-
        seq(start.time[i],
            start.time[i] + input$block.time - input$idle.time,
            segment.time)
      out.start  <- append(out.start, utils::head(a,-1))
    }
    start.time <- out.start + input$startup.time
    end.time <- start.time + segment.time

    #combine all information
    block.number <- 1:length(blocks)
    dual.inlet.annot <- data.frame(
      Block.Number = block.number,
      block = blocks,
      Start.Time = start.time,
      End.Time = end.time
    )
    return(dual.inlet.annot)
  }


# Function: merge.di.annotations(df.in, dual.inlet.anno) ------------------

#' @title merge.di.annotations
#' @description This function merges annotations between input data and dual inlet data
#' @param df.in Filtered dual inlet data
#' @param dual.inlet.anno Annotated dual inlet data
#' @return Returns dataframe that is merged from Filtered and annotated dual inlet data
#' @export
merge_di_annotations <- function(df.in, dual.inlet.anno){

  #temp; Add validations later for Dual Inlet input parameters. Are provided values reasonable.

  df.data <- df.in
  df.data$sample_name <- "NA" #normal NA caused issues!
  df.data$block <- 0L  #normal NA caused issues! L makes the zero an integer, not double
  #
  for (i in 1:nrow(dual.inlet.anno)) {

    sample <- dual.inlet.anno$block[i]
    block.no <- dual.inlet.anno$Block.Number[i]
    #
    time.start <- dual.inlet.anno$Start.Time[i]
    time.end <- dual.inlet.anno$End.Time[i]
    #
    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$sample_name <- sample
    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$block <- block.no

    #return(df.data) #would be returned for function!
  }
  #

  df<- df.data  %>% dplyr::filter(.data$block >0) %>% dplyr::filter(.data$sample_name != "NA")

  df$sample_name <- as.factor(df$sample_name)
  df$compound <- as.factor(df$compound)
  df$isotopocule <- as.factor(df$isotopocule)

  return(df)

}

