# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("plot functions")

test_that("scatter_isox test",{
  #success
  #...
  #failure
  #...
})

test_that("plot_labeller test",{
  #success
  #...
  #failure
  #...
})

test_that("density_isox test",{
  #success
  #...
  #failure
  #...
})
