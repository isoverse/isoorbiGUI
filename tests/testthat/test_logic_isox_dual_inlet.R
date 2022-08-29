# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("dual inlet")

test_that("define.dual.inlet test",{
  #success
  # ...
  #failure
  expect_error(define.dual.inlet(), "argument \"input\" is missing, with no default")
})

test_that("assign.basepeak tests",{
  #success
  #...
  #failure
  expect_error(assign.basepeak(), "argument \"INPUT_Basepeak\" is missing, with no default")
})

test_that("calc.output tests",{
  #success
  #...
  #failure
  expect_error(calc.output(), "argument \"df\" is missing, with no default")
})

test_that("merge.di.annotations tests",{
  #success
  #...
  #failure
  expect_error(calc.output(), "argument \"df\" is missing, with no default")
})
