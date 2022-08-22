# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("single logic")

test_that("assign.basepeak.single() tests",{
  #success
  #...
  #failure
  expect_error(assign.basepeak.single(), "argument \"INPUT_Basepeak\" is missing, with no default")
})

test_that("calc.output.single() tests",{
  #success
  #...
  #failure
  expect_error(calc.output.single(), "argument \"df\" is missing, with no default")
})
