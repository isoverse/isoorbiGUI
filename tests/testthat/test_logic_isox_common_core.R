# core functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("core")

test_that("test the function iso_simplify_isox_file()", {
  # iso_simplify_isox_file

  # test safety checks
  expect_error(iso_simplify_isox_file(), "argument \"dataset\" is missing, with no default")
})

  # remove.rare

  # remove.satellitePeaks

  # remove.ticxit

  # iso_filter_isox_file

  # se
test_that("se tests", {
  #success
  expect_type(se(c(1,2,3)), "double")
  #failure
})


  # gmean
test_that("gmean tests", {
  expect_type(gmean(c(4,5,6)), "double")
})


  # gsd
test_that("gsd tests", {
  expect_type(gsd(c(1,2,3)), "double")
})

  # gse
test_that("gse tests", {
  expect_type(gse(c(4,5,6)), "double")
})

  # slope


  # weighted.vector.sum

  #calc.ratio

  # make.xls

  #make.xls.onetab
