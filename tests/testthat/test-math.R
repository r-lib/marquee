test_that("math is tokenized during parsing", {
  text <- "Inline $x^2$ and display $$\\frac{a}{b}$$"

  parsed <- marquee_parse(text)
  expect_equal(parsed$type[parsed$text == "x^2"], "eq")
  expect_equal(parsed$type[parsed$text == "\\frac{a}{b}"], "eqd")
})

test_that("math rendering requires textshaping math support", {
  parsed <- marquee_parse("$x^2$")
  if ("shape_math" %in% getNamespaceExports("textshaping")) {
    expect_no_error(marquee_grob(parsed))
  } else {
    expect_error(marquee_grob(parsed), "shape_math")
  }
})

test_that("math grobs place glyphs, lines, and boxes", {
  skip_if_not("shape_math" %in% getNamespaceExports("textshaping"))
  skip_if_not_installed("ragg")

  ragg::agg_png(tempfile(fileext = ".png"))
  on.exit(dev.off())

  # Inline math
  grob <- marquee_grob("Euler: $e^{i\\pi} + 1 = 0$")
  grob <- makeContent.marquee_grob(makeContext.marquee_grob(grob))
  math_glyphs <- grob$shape[
    grob$shape$string_id == grob$maths$index,
  ]
  expect_gt(nrow(math_glyphs), 0)
  expect_true(all(grepl("Math", math_glyphs$font_path)))

  # Display math gets lines (fraction bar) and is centered by default
  grob <- marquee_grob(
    "$$\\frac{a}{b}$$",
    width = grid::unit(400, "bigpts")
  )
  expect_s3_class(grob, "marquee_precalculated_grob")
  expect_gt(nrow(grob$math_lines), 0)
  math_glyphs <- grob$shape[
    grob$shape$string_id == grob$maths$index,
  ]
  expect_gt(nrow(math_glyphs), 0)
  # Centered in the 400 bigpts wide grob
  left <- min(math_glyphs$x_offset)
  right <- max(math_glyphs$x_offset + math_glyphs$advance)
  expect_equal((left + right) / 2, 400 / 2, tolerance = 1)
})

test_that("malformed math falls back to literal text", {
  skip_if_not("shape_math" %in% getNamespaceExports("textshaping"))
  skip_if_not_installed("ragg")

  ragg::agg_png(tempfile(fileext = ".png"))
  on.exit(dev.off())

  expect_warning(
    grob <- marquee_grob("Good $x^2$ and bad $\\frac{a}{$"),
    "Failed to typeset"
  )
  expect_no_error(makeContent.marquee_grob(makeContext.marquee_grob(grob)))
})
