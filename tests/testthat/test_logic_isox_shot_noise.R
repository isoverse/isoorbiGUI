# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("shot noise")

test_that("assign_basepeak_shotnoise tests",{
  #succes
  #...
  #failure
  expect_error(assign_basepeak_shotnoise(), "argument \"data\" is missing, with no default")
})

test_that("calc_shotnoise tests",{
  #succes
  df <- iso_read_isox_file(system.file("extdata", "testfile_DualInlet_small.isox", package = "isoorbi"))
  expect_true(is.tbl(calc_shotnoise(iso_simplify_isox_file(df))))
  #failure
  expect_error(calc_shotnoise(), "argument \"dataset\" is missing, with no default")
})

test_that("plot_shot_noise tests",{
  #succes
  #...
  #failure
  expect_error(plot_shot_noise(), "argument \"dataset\" is missing, with no default")
})
