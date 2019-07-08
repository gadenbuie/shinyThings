context("test-radioSwitchButtons")

library(shinytest)

test_that("radioSwitchButtons() inputs works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "radioSwitchButtons", package = "shinyThings")
  expect_pass(testApp(appdir, compareImages = FALSE))
})
