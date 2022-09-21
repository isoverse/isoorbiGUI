# functions for logic_isox_file_interactions.R

#' @title Read .isox file
#' @description Read an IsoX output file into a tibble
#' @param filepath Path to the .isox file
#' @return Return error, else read TSV file as columns 'filename', 'scan.no', 'time.min', 'compound', 'isotopolog', 'ions.incremental', 'tic', 'it.ms'
#'
#' @examples
#' foo <- data.frame(filename="foo.isox", scan.no=1, time.min=0.01, compound="NO3", isotopolog="15N", ions.incremental=123.456, tic=12345, it.ms=0.1)
#' write.table(foo, file = "foo.isox", sep = "\t", row.names = FALSE)
#' df <- iso_read_isox_file(filepath = "inst/extdata/testfile_DualInlet_small.isox")
#'
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


