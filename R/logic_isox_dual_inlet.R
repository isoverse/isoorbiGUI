#' functions specific to dual inlet module

# Dual Inlet: define experiment -------------------------------------------

define.dual.inlet <-
  function(INPUT_Ref,
           INPUT_Sample,
           INPUT_Blocks,
           INPUT_Segments,
           INPUT_Idle.time,
           INPUT_Startup.time,
           INPUT_Block.time) {
    validate(
      need(input$INPUT_Ref, 'Missing input name of reference'),
      need(input$INPUT_Sample, 'Missing input name of sample'),
      need(input$INPUT_Blocks, 'Missing input Blocks'),
      need(input$INPUT_Idle.time, 'Missing input Idle Time'),
      need(input$INPUT_Block.time, 'Missing input Block Time'),
      need(input$INPUT_Startup.time, 'Missing input Startup Time'),

      need(
        input$INPUT_Idle.time < input$INPUT_Block.time,
        'Idle Time is longer than Block Time!'
      ),
      need(input$INPUT_Startup.time < 100, 'Startup time seems too long!'),
      need(input$INPUT_Blocks < 100, 'Looks like too many blocks!'),
      need(input$INPUT_Block.time < 100, 'Block Time appears too long'),
      need(input$INPUT_Segments < 10 , 'Looks like too many segments')
    )

    injections <-
      rep(c(INPUT_Ref, INPUT_Sample), 1E3)[1:INPUT_Blocks] #limited to maximum 1,000 blocks
    injections <- rep(injections, each = INPUT_Segments)
    segment.time <-
      (INPUT_Block.time - INPUT_Idle.time) / INPUT_Segments
    start.time <-
      0:(INPUT_Blocks - 1) * INPUT_Block.time + INPUT_Idle.time #Define start times to integrate (in minutes)
    out.start <- c()

    for (i in 1:(length(start.time))) {
      a <-
        seq(start.time[i],
            start.time[i] + INPUT_Block.time - INPUT_Idle.time,
            segment.time)
      out.start  <- append(out.start, head(a,-1))
    }
    start.time <- out.start + INPUT_Startup.time
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

