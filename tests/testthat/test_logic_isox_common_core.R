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
  expect_equal(se(c(1,2,3)), 0.5773503)
  expect_type(se(c(1,2,3)), "double")

  #failure
  #...
})


  # gmean
test_that("gmean tests", {
  #success
  expect_equal(gmean(c(1,2,3)), 1.817121)
  expect_type(gmean(c(4,5,6)), "double")

  #failure
  #...
})


  # gsd
test_that("gsd tests", {
  #success
  expect_type(gsd(c(1,2,3)), "double")

  #failure
  #...
})

  # gse
test_that("gse tests", {
  #success
  expect_type(gse(c(4,5,6)), "double")

  #failure
  #...
})

  # slope


  # weighted.vector.sum

  #calc.ratio

  # make.xls

  #make.xls.onetab
