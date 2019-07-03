context("test-buttonGroup")

library(shinytest)

test_that("buttonGroup() inputs works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "buttonGroup", package = "shinyThings")
  expect_pass(testApp(appdir))
})
