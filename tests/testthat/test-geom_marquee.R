test_that("geom_marquee inserts the aesthetics correctly", {
  p <- ggplot2::ggplot(mtcars) +
    geom_marquee(ggplot2::aes(disp, mpg, label = gear), family = "serif", size = 6, color = "red", fill = "blue", size.unit = "Pt")
  p <- ggplot2::ggplotGrob(p)
  grob <- p$grobs[[which(p$layout$name == "panel")]]$children[[3]]

  expect_equal(grob$text$family[1], "serif")
  expect_equal(grob$text$size[1], 6, tolerance = 0.1)
  expect_equal(grob$text$color[1], "#FF0000")

  expect_s3_class(grob$text$background[[1]], "marquee_skip_inherit")
  expect_equal(as.vector(grob$text$background[[1]]), "blue")
  expect_equal(grob$text$background[[2]], NA_character_)
})
