# core functions ====

# make both interactive test runs and auto_testing possible with a dynamic base path to the testthat folder
base_dir <- if (interactive()) file.path("tests", "testthat") else "."

context("core")

test_that("test the function iso_simplify_isox_file()", {

  # test safety checks
  expect_error(iso_simplify_isox_file(), "argument \"dataset\" is missing, with no default")
  #doesnt fix: expect_error(iso_simplify_isox_file(), "no file path")
})
