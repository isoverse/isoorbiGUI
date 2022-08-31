# Functions specific to single analysis module

# Function to assign Base Peak --------------------------------------------

#single analysis

#' @title Assign a base peak
#' @description `assign.basepeak.single` assigns one isotopocule in the data frame as the base peak
#' @details This is a function specific to the analysis of data from a single analysis experiment.
#' @param df A dataframe from a single analysis experiment. Needs to contain columns for filename, compound, scan.no, isotopolog, ions.incremental.
#' @param INPUT_Basepeak The isotopolog used as base peak in the ratio calculation by calc.ratio() function.
#' @returns Outputs the input dataframe plus two columns called `Basepeak` and `Basepeak.Ions`
#' @export
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
    dplyr::select(filename,
                  #Injection, #not present in single run analysis
                  compound,
                  scan.no,
                  isotopolog,
                  ions.incremental) %>%
    #dplyr::group_by(filename, Injection, scan.no) %>% dplyr::filter(isotopolog == INPUT_Basepeak) %>%
    dplyr::group_by(.data$filename, .data$scan.no) %>%
    dplyr::filter(isotopolog == INPUT_Basepeak) %>%
    dplyr::mutate(Basepeak = factor(.data$isotopolog),
                  Basepeak.Ions = .data$ions.incremental) %>%
    dplyr::select(.data$filename,
                  .data$compound,
                  #Injection,
                  .data$scan.no,
                  .data$Basepeak,
                  .data$Basepeak.Ions) %>%
    as.data.frame()

  df.out <- merge(df.sel, df, all = TRUE)

  df.out <-
    df.out %>% dplyr::filter(isotopolog != INPUT_Basepeak) %>% droplevels()

  return(df.out)
}



# Actual number crunching to generate Excel output for single analysis

#FIXME-LEIF: complete below
#' @title calc.output.single
#' @description Contains the logic to generate the Excel output for single analysis
#' @param df A dataframe from a single analysis experiment
#' @param INPUT_RatioMethod The method of computing ratio
#' @return Returns table containing 'filename', 'sample.name', 'Basepeak', 'Compound', 'Isotopolog', 'Ratio', 'relSE.permill', 'shot.noise.permill', 'mins.to.1mio'
#' @export
calc.output.single <- function(df, INPUT_RatioMethod) {
  df.stat <- df  %>%  dplyr::group_by(filename,
                                      Segment,
                                      Sample.Name,
                                      compound,
                                      Basepeak,
                                      isotopolog) %>%

    dplyr::mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

    dplyr::mutate(Ratio.SEM = se(ions.incremental / Basepeak.Ions))      #For simplicity use basic standard error for all options

  df.stat <- df.stat %>% dplyr::mutate(
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
    dplyr::mutate(
      Ratio = round(Ratio, 8),
      Ratio.SEM = round(Ratio.SEM, 8),
      Shot.Noise.permil = round(Shot.Noise.permil, 3),
      relSE.permil = round(relSE.permil, 3),
      Mins.to.1mio = round(Mins.to.1mio, 2)
    ) %>%

    dplyr::select(
      filename,
      Sample.Name,
      Segment,
      compound,
      Basepeak,
      isotopolog,
      Ratio,
      Ratio.SEM,
      relSE.permil,
      Shot.Noise.permil,
      No.of.scans,
      Mins.to.1mio
    ) %>%
    unique() %>% #dplyr::mutate(Injection= as.integer(Injection)) %>%
    #arrange(filename, Injection, isotopolog)
    arrange(filename, isotopolog)

  df.stat <- as.data.frame(df.stat)
  return(df.stat)
}

#FIXME-LEIF: Inquire below
# Plot: Single analysis --------------------------------------------------------


# plot_single <- function(dataset, input){
#
# }
