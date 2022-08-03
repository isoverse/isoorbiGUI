# functions specific to make_inputFile module

# check sum formula input -------------------------------------------------
#FIXME-LEIF: complete below
#' @title run_check
#' @description This function checks the sum formula provided by user to ensure it is a valid Chemical Formula
#' @param INPUT_formula Chemical formula for compound
#' @return Returns formula if valid, else error message if invalid formula
#' @export
run_check <- function(INPUT_formula) {
  for (i in INPUT_formula) {
    check <-
      check_chemform(
        isotopes,
        chemforms = INPUT_formula,
        get_sorted = FALSE,
        get_list = FALSE
      )

    return(check)

    # TEMP: This had caused a merging error end of July 2022
    # ifelse(check$warning == FALSE,
    #        return(check$new_formula),
    #        validate("Error in sum formula"))

  }


}



# make.isox.input -------------------------------------------------------------
#FIXME-LEIF: complete below
#' @title make.isox.input
#' @description This function creates the input file for the IsoX software
#' @param pattern.list Chemical formula of compound
#' @param INPUT_charge Charge of compound
#' @param INPUT_compound Given name for compound
#' @param INPUT_tolerance Tolerance [mDa]
#' @param INPUT_massOffset Offset of mass to charge readings m/z [mDA]
#' @return Returns a file.tsv containing columns '#Compound','Isotopolog','Mass', 'Tolerance[mmu]', 'Charge(z)'
#' @export
make.isox.input <- function(pattern.list, INPUT_charge, INPUT_compound, INPUT_tolerance, INPUT_massOffset){

  data(isotopes, package = "enviPat")

  out.list <- list()

  for(i in 1:length(pattern.list)){

    df <- as.data.frame(pattern.list[[i]])

    # select columns for isotopic substitutions
    #could add more isotope systems here later
    sel <- colnames(df) %in% c('m/z', '13C', '2H',  '15N', '17O', '18O', '33S', '34S', '36S', '37Cl', '11B', '81Br')


    df.sel <- df[, sel]
    df.sel$M0 <- 0
    df.sel[which.min(df.sel$'m/z'),]$M0 <- 1 #add M0 info; temp: error when there is no M0, min....! Look for a better way

    df.temp <- df.sel %>% tidyr::pivot_longer( -c("m/z"), names_to= "isotope", values_to= "count",) %>% filter(count>0)

    df.temp$isotopolog <- NA
    df.temp$temp.string <- NA

    for(j in 1:nrow(df.temp)){

      temp.string <- paste(replicate(df.temp[j,]$count, df.temp[j,]$isotope), collapse = "")


      df.temp$temp.string[j] <- temp.string
    }

    df.temp2 <- df.temp %>% select(-count)%>% tidyr::pivot_wider(names_from = "isotope", values_from="temp.string") #FIXME: improve nomenclature

    cols <- colnames(df.temp2)[-1] # columns to paste together

    df.temp2[is.na(df.temp2)] <- ""  # replace NA with blanks

    df.temp2$Isotopolog <- apply( df.temp2[ , cols ] , 1 , paste , collapse = "" ) # create a new column `x` with the three columns collapsed together

    # remove the unnecessary columns
    data <- df.temp2 %>% select("m/z", "Isotopolog" )

    # add colums charge and tolerance

    data$'#Compound' <- INPUT_compound[i] #temp: validate that all entered formulas are correct!
    data$'Tolerance [mmu]' <- INPUT_tolerance
    data$z <- abs(INPUT_charge[i]) #absolute value of charge #more than 1?
    data$mass <- data$'m/z'

    data <- data %>% select('#Compound', Isotopolog, mass, 'Tolerance [mmu]', z)


    out.list[[i]] <- data

  }

  df.out <- out.list[[1]]

  if(length(out.list) >1){

    for(i in 2:length(out.list)){

      add.compound <- as.data.frame(out.list[[i]])
      df.out <-rbind(df.out, add.compound)
    }
  }

  df.out$mass <- df.out$mass + INPUT_massOffset #offset from UI input

  df.out$mass <- format(round(df.out$mass, 5),nsmall = 5) #show last digit zero

  return(df.out)
}
