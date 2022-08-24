# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("plot functions")

test_that("scatter_isox test",{
  #success
  #...
  #failure
  expect_error(scatter_isox(), "argument \"INPUT_Basepeak\" is missing, with no default")
})

test_that("plot_labeller test",{
  #success
  #...
  #failure
  expect_error(plot_labeller(), "argument \"INPUT_Basepeak\" is missing, with no default")
})

test_that("density_isox test",{
  #success
  #...
  #failure
  expect_error(density_isox(), "argument \"INPUT_Basepeak\" is missing, with no default")
})
