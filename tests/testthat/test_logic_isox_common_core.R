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
test_that("test the function remove.rare", {
  #success
  #failure
  expect_error(remove.rare(), "argument \"data\" is missing, with no default")
})

  # remove.satellitePeaks
test_that("test the function remove.satellitePeaks", {
  #succcess
  #...
  #failure
  expect_error(remove.satellitePeaks(), "argument \"df\" is missing, with no default")
})

  # remove.ticxit
test_that("test the function remove.ticxit", {
  #success
  #failure
  expect_error(remove.ticxit(), "argument \"df\" is missing, with no default")
})

  # iso_filter_isox_file

  # se
test_that("se tests", {
  #success
  #expect_equal(se(c(1,2,3)), 0.5773503)
  expect_type(se(c(1,2,3)), "double")

  #failure
  expect_error(se(), "argument \"x\" is missing, with no default")
})

  # gmean
test_that("gmean tests", {
  #success
  expect_type(gmean(c(4,5,6)), "double")

  #failure
  expect_error(gmean(), "argument \"x\" is missing, with no default")
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
test_that("slope tests", {
  #success
  x <- c(0,1,2,3)
  y <- c(0,1,2,3)
  expect_type(slope(x,y), "double")
  expect_equal(slope(x,y), 1)

  #failure
  #...
})

  # weighted.vector.sum
test_that("weighted.vector.sum tests", {
  #success
  x<- c(2,4,6)
  y<- c(3,5,7)
  expect_type(weighted.vector.sum(x,y), "double")
  #failure
  expect_error(weighted.vector.sum(), "argument \"x\" is missing, with no default")
})

  # calc.ratio
test_that("calc.ratio tests", {
  #success
  #...
  #failure
  expect_error(calc.ratio(), "the condition has length > 1")
})

  # make.xls
test_that("make.xls tests", {
  #success
  #...
  #failure
  expect_error(make.xls(), "")
})

  # make.xls.onetab
test_that("make.xls.onetab", {
  #success
  #...
  #failure
  expect_error(make.xls.onetab(), "")
})
