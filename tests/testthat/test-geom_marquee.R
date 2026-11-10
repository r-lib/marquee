test_that("geom_marquee inserts the aesthetics correctly", {
  p <- ggplot2::ggplot(mtcars) +
    geom_marquee(
      ggplot2::aes(disp, mpg, label = gear),
      family = "serif",
      size = 6,
      color = "red",
      fill = "blue",
      size.unit = "Pt"
    )
  png(tempfile())
  p <- ggplot2::ggplotGrob(p)
  dev.off()
  grob <- p$grobs[[which(p$layout$name == "panel")]]$children[[3]]

  expect_equal(grob$text$family[1], "serif")
  expect_equal(grob$text$size[1], 6, tolerance = 0.1)
  expect_equal(grob$text$color[1], "#FF0000")

  expect_s3_class(grob$text$background[[1]], "marquee_skip_inherit")
  expect_equal(as.vector(grob$text$background[[1]]), "blue")
  expect_equal(grob$text$background[[2]], NA_character_)
})

test_that("geom_marquee takes family, size, and colour from the theme (#106)", {
  skip_if_not_installed("ggplot2", "4.0.0")
  p <- ggplot2::ggplot(mtcars) +
    ggplot2::aes(disp, mpg) +
    ggplot2::annotate("marquee", x = 230, y = 20, label = "Some **text**") +
    ggplot2::theme_minimal(base_family = "serif")
  data <- ggplot2::ggplot_build(p)$data[[1]]

  expect_identical(data$family, "serif")

  # Resolved defaults match GeomText on the same theme
  p_text <- ggplot2::ggplot(mtcars) +
    ggplot2::aes(disp, mpg) +
    ggplot2::geom_text(label = "x") +
    ggplot2::theme_minimal(base_family = "serif")
  data_text <- ggplot2::ggplot_build(p_text)$data[[1]]
  expect_equal(data$size, data_text$size[1])
  expect_identical(data$colour, data_text$colour[1])
})
