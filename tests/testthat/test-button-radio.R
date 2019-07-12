context("test-radioSwitchButtons")

library(shinytest)

test_that("radioSwitchButtons() inputs works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "radioSwitchButtons", package = "shinyThings")
  expect_pass(testApp(appdir, compareImages = FALSE))
})

describe("radioSwitchButtons()", {

  it("errors when choices and choice_labels have different sizes", expect_error(
    radioSwitchButtons("test", choices = letters[1], choice_labels = LETTERS[1:3])
  ))

  it("errors when inputId has whitespace", expect_error(
    radioSwitchButtons("test this", choices = letters[1:2])
  ))

  it("warns when mutiple selections are given", expect_warning(
    radioSwitchButtons("test", choices = letters[1:3], selected = letters[1:2])
  ))

  it("warns when selected item isn't in the list of choices", expect_warning(
    radioSwitchButtons("test", choices = letters[1:3], selected = "g")
  ))

  it("uses choice names or choices directly", expect_equal(
    radioSwitchButtons("test", choices = letters[1:5]),
    radioSwitchButtons("test", choices = letters[1:5], choice_labels = letters[1:5])
  ))

})
