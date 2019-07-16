library(shinytest)

test_that("history module works", {
  skip_on_cran()
  skip_if_not(shinytest::dependenciesInstalled())

  appdir <- system.file("examples", "history", package = "shinyThings")
  expect_pass(testApp(appdir, compareImages = FALSE))
})

describe("undoHistoryUI", {

  it("adds class to parent container", {
    x <- undoHistoryUI("test", class = "special-class")
    expect_true(grepl("special-class", x[[2]]$attribs$class))
  })

  it("creates two buttons", {
    x <- undoHistoryUI("test")
    expect_equal(x[[2]]$children[[1]]$name, "button")
    expect_equal(x[[2]]$children[[2]]$name, "button")
  })

  it("requires character btn_class of length 1 or 2", {
    expect_error(undoHistoryUI("test", btn_class = 3))
    expect_error(undoHistoryUI("test", btn_class = rep("a", 3)))
  })

  it("applies btn_class to both buttons if length 1", {
    x <- undoHistoryUI("test", btn_class = "special")
    expected_class <- spaces("special", "action-button disabled")
    expect_equal(x[[2]]$children[[1]]$attribs$class, expected_class)
    expect_equal(x[[2]]$children[[2]]$attribs$class, expected_class)
  })

  it("applies btn_class separately if length 2", {
    x <- undoHistoryUI("test", btn_class = letters[1:2])
    expected_class <- function(x) spaces(x, "action-button disabled")
    expect_equal(x[[2]]$children[[1]]$attribs$class, expected_class("a"))
    expect_equal(x[[2]]$children[[2]]$attribs$class, expected_class("b"))
  })

})
