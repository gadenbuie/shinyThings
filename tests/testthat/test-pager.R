context("test-pager")

library(shinytest)

test_that("pager module works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "pager", package = "shinyThings")
  expect_pass(testApp(appdir, compareImages = FALSE))
})
