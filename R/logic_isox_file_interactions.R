#' read an isox file
#' @param filepath the path to the isox file
#' @export
iso_read_isox_file <- function(filepath) {
  # safety checks
  if (missing(filepath)) stop("no file path supplied", call. = FALSE)
  if (length(filepath) != 1) stop("can only read exactly 1 file at the time, supplied paths: ", length(filepath), call. = FALSE)
  if (!file.exists(filepath)) stop("this file does not exist: ", filepath, call. = FALSE)
  ext <- stringr::str_extract(basename(filepath), "\\.[^.]+$")
  if (is.na(ext) || ext != ".isox") stop("unrecognized file extension: ", ext, call. = FALSE)

  tryCatch(
    readr::read_tsv(
      filepath,
      col_types = list(
        filename = readr::col_factor(),
        scan.no = readr::col_integer(),
        time.min = readr::col_double(),
        compound = readr::col_factor(),
        isotopolog = readr::col_factor(),
        ions.incremental = readr::col_double(),
        tic = readr::col_double(),
        it.ms = readr::col_double()
      )
     ),
    warning = function(w) {
      stop("file format error: ", w$message, call. = FALSE)
    }
  )
}

#' isox data manipulation

#Caj: these functions for ratio calculation should eventually be placed in a separate location

#Define 'se' as a new function to calculate standard error
se <- function(x) { sd(x) / sqrt(length(x)) }

gmean <- function(x)
  exp(mean(log(x))) #define geometric mean
gsd <-
  function(x)
    exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))
gse <-
  function(x)
    (exp(mean(log(x)) + sd(log(x))) - exp(mean(log(x)))) / sqrt(length(x))

slope <-
  function(x, y) {
    model <-
      lm(x ~ y + 0, weights = x) #Note order of x and y to get correct slope!
    sl <- model$coefficients[[1]]
    sl
  }

weighted.vector.sum <- function(x, y) {
  df <- cbind(x, y)

  avg.ions <- (sum(df[, 1]) + sum(df[, 2])) / length(df[, 1])

  scan.ions <- (df[, 1] + df[, 2])

  weighted.x <- avg.ions / scan.ions  * as.numeric(df[, 1])
  weighted.y <- avg.ions / scan.ions  * as.numeric(df[, 2])
  ratio <- sum(weighted.x) / sum(weighted.y)
  ratio
}

#NOTE: The formulate used to average ratios matters! Do not simply use arithmetic mean.

calc.ratio <- function(peak1, peak2, ratio.method = c("mean", "sum", "median", "geometric.mean", "slope", "weighted.vector.sum") ) {

  if(ratio.method == "mean") { o <- mean(peak1/peak2); o } else {
    if(ratio.method == "slope") {o <- slope(peak1, peak2); o } else{
      if(ratio.method == "sum") {o <- sum(peak1)/sum(peak2); o } else{
        if(ratio.method == "geometric.mean") {o <- gmean(peak1/peak2); o } else{
          if(ratio.method == "weighted.vector.sum") {o <- weighted.vector.sum(peak1, peak2); o } else{
            if(ratio.method == "median") {o <- median(peak1/peak2); o } else{print("`ratio.method` has to be `mean`, `sum`, `median`, `geometric.mean`, `slope` or `weighted.vector.sum`")
            }
          }}}}}

}

