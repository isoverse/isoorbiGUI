# file loading ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("file loading")

test_that("test that isox files can be read", {

  # test safety checks
  # expect_error(orbi_read_isox_file(c("one", "two")), "can only read.*1")
  # expect_error(orbi_read_isox_file("DNE"), "does not exist")
  # temp_file <- tempfile(fileext = ".wrong")
  # cat("empty", file = temp_file) # create the temp file
  # expect_error(orbi_read_isox_file(temp_file), "unrecognized")
  # unlink(temp_file) # destroy the temp file

  # corrupt files
  # expect_error(orbi_read_isox_file(file.path(base_dir, "test_files", "missing_column.isox")), "file format error")

  # test reading a file
 expect_true(is.tbl(df <- isoorbi::orbi_read_isox(file = system.file("extdata", "testfile_DualInlet_small.isox", package = "isoorbiGUI"))))
  expect_equal(names(df), c("filepath", "filename", "scan.no", "time.min", "compound", "isotopocule", "ions.incremental", "tic", "it.ms"))
  expect_equal(nrow(df), 5184)

})
