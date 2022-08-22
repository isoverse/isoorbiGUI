# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("flow analysis")

test_that("calc.output.flow() tests",{
  #succes
  #...
  #failure
  expect_error(calc.output.flow(), "argument \"df\" is missing, with no default")
})
