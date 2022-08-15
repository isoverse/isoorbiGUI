# core functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("core")

test_that("test the function iso_simplify_isox_file()", {


  # iso_simplify_isox_file

  # test safety checks
  expect_error(iso_simplify_isox_file(), "argument \"dataset\" is missing, with no default")

  # remove.rare

  # remove.satellitePeaks

  #remove.ticxit

  # iso_filter_isox_file

  # se
  expect_type(se(c(1,2,3)), "double")

  # gmean


  # gsd

  # gse

  # slope


  # weighted.vector.sum

  #calc.ratio

  # make.xls

  #make.xls.onetab


})
