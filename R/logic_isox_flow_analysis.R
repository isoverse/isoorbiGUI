# functions specific to flow analysis module

#FIXME-LEIF: complete and cofirm
#' @title Calculate output of flow module
#' @description This function takes a dataframe and returns processed information for the flow module
#' @param df a dataframe output from `iso_simplify_isox_file()`
#' @param INPUT_RatioMethod The method for computing ratio, method is signifigant
#' @return A dataframe with statisitics of interest
#' @export
##Actual number crunching to generate Excel output for flow analysis
#FIXME: change calc.output.flow to calc.ratio.flow?
calc.output.flow <- function(df, INPUT_RatioMethod){

  df.stat <- df  %>%  dplyr::group_by(filename,
                               segment,
                               #sample_name,
                               compound,
                               Basepeak,
                               isotopolog) %>%

    dplyr::mutate(Ratio = calc.ratio(ions.incremental, Basepeak.Ions, ratio.method = INPUT_RatioMethod)) %>% #RATIO CALCULATION!

    dplyr::mutate(Ratio.SEM = se(ions.incremental/Basepeak.Ions))      #For simplicity use basic standard error for all options

  df.stat <- df.stat %>% dplyr::mutate(No.of.scans = length(Ratio),
                                Mins.to.1mio = (1E6/sum(ions.incremental)) * ( max(time.min) - min(time.min)),
                                Shot.Noise.permil = 1000 * (sqrt((sum(ions.incremental) + sum(Basepeak.Ions)) / (sum(ions.incremental) * sum(Basepeak.Ions)))),
                                relSE.permil = 1000*(Ratio.SEM/Ratio)) %>%

    #Round values for output
    dplyr::mutate(Ratio = round(Ratio, 8),
           Ratio.SEM = round(Ratio.SEM, 8),
           Shot.Noise.permil = round(Shot.Noise.permil, 3),
           relSE.permil = round(relSE.permil, 3),
           Mins.to.1mio = round(Mins.to.1mio, 2)) %>%

    dplyr::select(filename,
           segment,
           #sample_name,
           Basepeak,
           compound,
           isotopolog,
           Ratio,
           Ratio.SEM,
           relSE.permil,
           Shot.Noise.permil,
           Mins.to.1mio,
           No.of.scans) %>%
    unique() %>% #dplyr::mutate(Injection= as.integer(Injection)) %>%
    #arrange(filename, Injection, isotopolog)
    arrange(filename, isotopolog)

  df.stat <- as.data.frame(df.stat)
  return(df.stat)
}

