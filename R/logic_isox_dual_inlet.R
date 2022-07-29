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
      need(input$startup.time < 100, 'Startup time seems too long!'),
      need(input$blocks < 100, 'Looks like too many blocks!'),
      need(input$blocks > 0, 'Need at lewast one block'),
      need(input$block.time < 100, 'Block Time appears too long'),
      need(input$segments < 10 , 'Looks like too many segments')
    )


    injections <-
      rep(c(input$reference, input$sample), 1E3)[1:input$blocks] #limited to maximum 1,000 blocks
    injections <- rep(injections, each = input$segments)
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
      out.start  <- append(out.start, head(a,-1))
    }
    start.time <- out.start + input$startup.time
    end.time <- start.time + segment.time

    #combine all information
    injection.number <- 1:length(injections)
    dual.inlet.annot <- data.frame(
      Injection.Number = injection.number,
      Injection = injections,
      Start.Time = start.time,
      End.Time = end.time
    )
    return(dual.inlet.annot)
  }

# Function to assign Base Peak --------------------------------------------

# Dual Inlet
#' @title assign.basepeak
#' @description Assign basepeak for dual inlet analysis
#' @param df a dataframe output from `iso_simplify_isox_file()`
#' @param INPUT_Basepeak the isotopocule assigned to be the base peak for dual inlet analysis
#' @return Returns dataframe with assigned base peak
#' @export
assign.basepeak <- function(df, INPUT_Basepeak) {
  validate(
    need(INPUT_Basepeak, 'Missing input for Basepeak'),
    #temp; check this works correctly
    need(
      INPUT_Basepeak %in% levels(df$isotopolog),
      'Specified Basepeak was not found in dataframe'
    )
  )

  # Annotation: Identify 'basepeak' for each scan

  df.sel <- df  %>%
    select(filename,
           Injection,
           compound,
           scan.no,
           isotopolog,
           ions.incremental) %>%
    group_by(filename, Injection, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%

    mutate(Basepeak = factor(paste(isotopolog, "is base peak", sep = " ")),
           Basepeak.Ions = ions.incremental) %>%
    select(filename,
           compound,
           Injection,
           scan.no,
           Basepeak,
           Basepeak.Ions) %>%
    as.data.frame()

  df.out <- merge(df.sel, df, all = TRUE)

  df.out <-
    df.out %>% filter(isotopolog != INPUT_Basepeak) %>% droplevels()

  return(df.out)
}


##Actual number crunching to generate Excel output for dual inlet
#' @title calc.output
#' @description Logic for generating Excel output for Dual Inlet analysis
#' @param df a dataframe output from `iso_simplify_isox_file()`
#' @param INPUT_RatioMethod Method of computing isotopolog ratios
#' @return Returns dataframe with statisitic of interest for dual inlet analysis
#' @export
calc.output <- function(df, INPUT_RatioMethod) {
  df.stat <- df  %>%  group_by(filename,
                               Injection,
                               Sample.Name,
                               compound,
                               Basepeak,
                               isotopolog) %>%

    mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

    mutate(Ratio.SEM = se(ions.incremental / Basepeak.Ions))      #For simplicity use basic standard error for all options

  df.stat <- df.stat %>% mutate(
    No.of.scans = length(Ratio),
    Mins.to.1mio = (1E6 / sum(ions.incremental)) * (max(time.min) - min(time.min)),
    Shot.Noise.permil = 1000 * (sqrt((
      sum(ions.incremental) + sum(Basepeak.Ions)
    ) / (
      sum(ions.incremental) * sum(Basepeak.Ions)
    ))),
    relSE.permil = 1000 * (Ratio.SEM / Ratio)
  ) %>%

    #Round values for output
    mutate(
      Ratio = round(Ratio, 8),
      Ratio.SEM = round(Ratio.SEM, 8),
      Shot.Noise.permil = round(Shot.Noise.permil, 3),
      relSE.permil = round(relSE.permil, 3),
      Mins.to.1mio = round(Mins.to.1mio, 2)
    ) %>%

    select(
      filename,
      Injection,
      Sample.Name,
      Basepeak,
      compound,
      isotopolog,
      Ratio,
      Ratio.SEM,
      relSE.permil,
      Shot.Noise.permil,
      Mins.to.1mio,
      No.of.scans
    ) %>%
    unique() %>% mutate(Injection = as.integer(Injection)) %>%
    arrange(filename, Injection, isotopolog)

  df.stat <- as.data.frame(df.stat)
  return(df.stat)
}


# Function: merge.di.annotations(df.in, dual.inlet.anno) ------------------

merge.di.annotations <- function(df.in, dual.inlet.anno){

  #temp; Add validations later for Dual Inlet input parameters. Are provided values reasonable.

  df.data <- df.in
  df.data$Sample.Name <- "NA" #normal NA caused issues!
  df.data$Injection <- 0L  #normal NA caused issues! L makes the zero an integer, not double
  #
  for (i in 1:nrow(dual.inlet.anno)) {

    sample <- dual.inlet.anno$Injection[i]
    injection.no <- dual.inlet.anno$Injection.Number[i]
    #
    time.start <- dual.inlet.anno$Start.Time[i]
    time.end <- dual.inlet.anno$End.Time[i]
    #
    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$Sample.Name <- sample
    df.data[(df.data$time.min > time.start) & (df.data$time.min < time.end), ]$Injection <- injection.no

    #return(df.data) #would be returned for function!
  }
  #

  df<- df.data  %>% filter(Injection >0) %>% filter(Sample.Name != "NA")

  df$Sample.Name <- as.factor(df$Sample.Name)
  df$compound <- as.factor(df$compound)
  df$isotopolog <- as.factor(df$isotopolog)

  return(df)

}

