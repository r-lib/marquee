skip_if_not_installed("ggplot2", "3.5.0")

test_that("guide_marquee can interpolate legend glyphs", {

  df <- data.frame(x = c("A", "B", "C"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, x, shape = x)) +
    ggplot2::geom_point() +
    ggplot2::scale_shape_discrete(
      name = "Glyph <<A>>, glyph <<2>> and <<C>>",
      guide = "marquee"
    )
  b <- ggplot2::ggplot_build(p)
  params <- b$plot$guides$get_params("shape")

  grob <- GuideMarquee$draw(ggplot2::theme_get(), params = params)$grobs[[1]]

  expect_equal(
    grob$text$text,
    c("", "Glyph ", NA, ", glyph ", NA, " and ", NA, "")
  )
  expect_equal(
    grob$images$path,
    c("GLYPH_A", "GLYPH_B", "GLYPH_C")
  )
  expect_true(
    all(vapply(grob$images$grobs, function(key) {
      !inherits(key, "missing_grob") && inherits(key, "gTree")
    }, logical(1)))
  )
})

test_that("guide_marquee can recolour text", {

  df <- data.frame(x = c("A", "B", "C"))
  p <- ggplot2::ggplot(df, ggplot2::aes(x, x, colour = x)) +
    ggplot2::geom_point() +
    ggplot2::scale_colour_manual(
      values = c("orchid", "limegreen", "gold"),
      name = "The quick !!2rown fox {.3 jumps} over the {.A lazy dog}",
      guide = "marquee"
    )
  b <- ggplot2::ggplot_build(p)
  params <- b$plot$guides$get_params("colour")

  grob <- GuideMarquee$draw(ggplot2::theme_get(), params = params)$grobs[[1]]
  expect_equal(
    grob$text$text,
    c("", "The quick ", "B", "rown fox ", "jumps", " over the ", "lazy dog", "")
  )
  expect_equal(
    grob$text$type,
    c("body", "p", "lab_b", "p", "lab_c", "p", "lab_a", "p")
  )
  expect_equal(
    grob$text$color,
    c("black", "black", "limegreen", "black", "gold", "black", "orchid", "black")
  )
})
