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
  # ...
  #failure
  expect_error(calc_shotnoise(), "argument \"dataset\" is missing, with no default")
})

test_that("plot_shot_noise tests",{
  #succes
  #...
  #failure
  expect_error(plot_shot_noise(), "argument \"dataset\" is missing, with no default")
})
