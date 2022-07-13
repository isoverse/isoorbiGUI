context("reports")

test_that("test that reports can be generated", {

  expect_error(iso_create_orbitrap_data_report(), "implement")
  expect_error(iso_run_orbitrap_data_report(), "implement")

})
