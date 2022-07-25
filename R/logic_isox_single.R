#' functions specific to single analysis module

# Function to assign Base Peak --------------------------------------------



#single analysis

assign.basepeak.single <- function(df, INPUT_Basepeak) {
  validate(
    need(INPUT_Basepeak, 'Missing input for Basepeak'),
    #temp; check this works correctly
    need(
      INPUT_Basepeak %in% levels(df$isotopolog),
      'Specified Basepeak was not found in dataframe'
    )
  )

  # Annotation: Identify 'base peak' for each scan

  df.sel <- df  %>%
    select(filename,
           #Injection, #not present in single run analysis
           compound,
           scan.no,
           isotopolog,
           ions.incremental) %>%
    #group_by(filename, Injection, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%
    group_by(filename, scan.no) %>% filter(isotopolog == INPUT_Basepeak) %>%
    mutate(Basepeak = factor(paste(isotopolog, "is base peak", sep = " ")),
           Basepeak.Ions = ions.incremental) %>%
    select(filename,
           compound,
           #Injection,
           scan.no,
           Basepeak,
           Basepeak.Ions) %>%
    as.data.frame()

  df.out <- merge(df.sel, df, all = TRUE)

  df.out <-
    df.out %>% filter(isotopolog != INPUT_Basepeak) %>% droplevels()

  return(df.out)
}



##Actual number crunching to generate Excel output for single analysis

calc.output.single <- function(df, INPUT_RatioMethod) {
  df.stat <- df  %>%  group_by(filename,
                               #Injection,
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
      # Injection,
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
    unique() %>% #mutate(Injection= as.integer(Injection)) %>%
    #arrange(filename, Injection, isotopolog)
    arrange(filename, isotopolog)

  df.stat <- as.data.frame(df.stat)
  return(df.stat)
}

# Plot: Single analysis --------------------------------------------------------


# plot_single <- function(dataset, input){
#
# }
