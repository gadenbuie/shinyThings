context("test-dropdownButton")

library(shinytest)

test_that("dropdownButton module works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "dropdownButton", package = "shinyThings")
  expect_pass(testApp(appdir))
})
