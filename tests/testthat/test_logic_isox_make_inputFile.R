# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("make input file")

test_that("run_check tests",{
  #success
  #...
  #failure
  expect_error(run_check(), "argument \"INPUT_formula\" is missing, with no default")
})

test_that("make.isox.input tests",{
  #success
  #...
  #failure
  expect_error(make.isox.input(), "argument \"pattern.list\" is missing, with no default")
})
