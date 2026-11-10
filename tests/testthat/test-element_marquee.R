test_that("element_marquee() inserts aesthetics into the style", {
  elem <- element_marquee(
    "serif",
    colour = "red",
    size = 6,
    margin = ggplot2::margin(1, 1, 1, 1, "bigpts")
  )
  elem <- element_grob.element_marquee(
    elem,
    "test",
    hjust = "left",
    vjust = "center",
    margin_x = TRUE,
    margin_y = TRUE
  )
  expect_equal(elem$text$family[1], "serif")
  expect_equal(elem$text$color[1], "red")
  expect_equal(elem$text$size[1], 6)
  expect_equal(elem$text$padding_bottom, c(1, 0))
})

test_that("element_marquee() observes customs around margin_x/margin_y", {

  elem <- element_marquee(margin = unit(c(1, 1, 1, 1), "bigpts"), hjust = 0.5, vjust = 0.5, angle = 0)

  png(tempfile())
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
  dev.off()
})

test_that("hjust controls multi-line alignment when style align is auto (#104)", {
  skip_if_not_installed("ragg")
  ragg::agg_png(tempfile(fileext = ".png"))
  grob <- element_grob.element_marquee(
    element_marquee(hjust = 0.5, vjust = 1, margin = ggplot2::margin(0, 0, 0, 0)),
    "line one\nlonger line two",
    x = 0.5,
    y = 0.5
  )
  expect_identical(grob$text$align[1], "center")

  grob <- element_grob.element_marquee(
    element_marquee(hjust = 1, vjust = 1, margin = ggplot2::margin(0, 0, 0, 0)),
    "line one\nlonger line two",
    x = 0.5,
    y = 0.5
  )
  expect_identical(grob$text$align[1], "right")

  # An explicit style alignment wins over hjust
  grob <- element_grob.element_marquee(
    element_marquee(
      hjust = 0.5,
      vjust = 1,
      style = classic_style(align = "left"),
      margin = ggplot2::margin(0, 0, 0, 0)
    ),
    "line one\nlonger line two",
    x = 0.5,
    y = 0.5
  )
  expect_identical(grob$text$align[1], "left")
  dev.off()
})

test_that("marquefy_theme() lets parent elements propagate style (#104)", {
  skip_if_not_installed("ggplot2", "4.0.0")
  theme <- marquefy_theme(ggplot2::theme_gray())

  # style and width are only set where needed so inheritance can fill them
  expect_null(theme$axis.text.x.bottom$style)
  expect_null(theme$axis.text.x.bottom$width)
  expect_equal(theme$text$width, NA)

  # A parent element's style reaches child elements
  theme <- theme +
    ggplot2::theme(
      axis.text = element_marquee(
        colour = "red",
        style = classic_style(align = "center")
      )
    )
  el <- ggplot2:::calc_element("axis.text.x", theme)
  expect_identical(el$colour, "red")
  expect_identical(el$style[[1]]$base$align, "center")
})

test_that("element_grob falls back to the style's base values", {
  skip_if_not_installed("ragg")
  ragg::agg_png(tempfile(fileext = ".png"))
  grob <- element_grob.element_marquee(
    element_marquee(
      style = classic_style(base_size = 20, body_font = "serif"),
      margin = ggplot2::margin(0, 0, 0, 0)
    ),
    "test",
    x = 0.5,
    y = 0.5,
    hjust = 0.5,
    vjust = 0.5
  )
  dev.off()
  expect_equal(grob$text$size[1], 20)
  expect_identical(grob$text$family[1], "serif")
  # lineheight comes from the style, not the hardcoded fallback of 1
  expected <- marquee_parse(
    "test",
    classic_style(base_size = 20, body_font = "serif")
  )$lineheight[1]
  expect_equal(grob$text$lineheight[1], expected)
})


test_that("element_marquee propagates to child theme elements (#97)", {
  skip_if_not_installed("ggplot2", "4.0.0")
  theme <- ggplot2::theme_gray() +
    ggplot2::theme(title = element_marquee(style = classic_style()))

  # plot.title takes on the class and style of the parent title element
  el <- ggplot2:::calc_element("plot.title", theme)
  expect_true(inherits(el, "element_marquee"))
  expect_s3_class(el$style, "marquee_style_set")

  # ... and renders markdown, including custom span colours
  p <- ggplot2::ggplot(mtcars) +
    ggplot2::geom_point(ggplot2::aes(mpg, disp)) +
    ggplot2::labs(title = "A {.red *marquee*} title") +
    ggplot2::theme_gray(base_size = 6) +
    ggplot2::theme(title = element_marquee())
  gt <- ggplot2::ggplotGrob(p)
  title_grob <- gt$grobs[[which(gt$layout$name == "title")]]
  expect_s3_class(title_grob, "marquee_grob")
  red <- title_grob$text$text == "marquee"
  expect_identical(title_grob$text$color[red], "red")
  expect_true(title_grob$text$italic[red])
})
