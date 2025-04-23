test_that("element_marquee() inserts aesthetics into the style", {

  elem <- element_marquee("serif", colour = "red", size = 6, margin = ggplot2::margin(1, 1, 1, 1, "bigpts"))
  elem <- element_grob.element_marquee(elem, "test", hjust = "left", vjust = "center", margin_x = TRUE, margin_y = TRUE)
  expect_equal(elem$text$family[1], "serif")
  expect_equal(elem$text$color[1], "red")
  expect_equal(elem$text$size[1], 6)
  expect_equal(elem$text$padding_bottom, c(1, 0))
})

test_that("element_marquee() observes customs around margin_x/margin_y", {

  elem <- element_marquee(margin = unit(c(1, 1, 1, 1), "bigpts"), hjust = 0.5, vjust = 0.5, angle = 0)

  width  <- function(x) convertWidth(grobWidth(x),   "bigpts", valueOnly = TRUE)
  height <- function(x) convertHeight(grobHeight(x), "bigpts", valueOnly = TRUE)

  x <- element_grob.element_marquee(elem, "test", margin_x = TRUE, margin_y = FALSE)
  expect_equal(height(x), 0)
  expect_gt(width(x), 0)

  x <- element_grob.element_marquee(elem, "test", margin_x = FALSE, margin_y = TRUE)
  expect_equal(width(x), 0)
  expect_gt(height(x), 0)

  x <- element_grob.element_marquee(elem, "test", margin_x = TRUE, margin_y = TRUE)
  expect_gt(width(x), 0)
  expect_gt(height(x), 0)

  x <- element_grob.element_marquee(elem, "test", margin_x = FALSE, margin_y = FALSE)
  expect_gt(width(x), 0)
  expect_gt(height(x), 0)
})
