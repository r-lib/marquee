test_that("style checks it's inputs", {
  # Family
  expect_silent(s <- style(family = "sans"))
  expect_equal(s$family, "sans")
  expect_snapshot_error(style(family = letters))
  expect_snapshot_error(style(family = 1))

  # Weight
  expect_silent(s <- style(weight = "bold"))
  expect_equal(s$weight, 700L)
  expect_snapshot_error(style(weight = "A"))
  expect_snapshot_error(style(weight = TRUE))
  s <- style(weight = 400)
  expect_type(s$weight, "integer")

  # Italic
  expect_silent(s <- style(italic = TRUE))
  expect_equal(s$italic, TRUE)
  expect_snapshot_error(style(italic = "A"))
  expect_snapshot_error(style(italic = 1))

  # Width
  expect_silent(s <- style(width = "condensed"))
  expect_equal(s$width, 3L)
  expect_snapshot_error(style(width = "A"))
  expect_snapshot_error(style(width = TRUE))
  s <- style(width = 5)
  expect_type(s$width, "integer")

  # Features
  expect_silent(s <- style(features = systemfonts::font_feature()))
  expect_equal(s$features, systemfonts::font_feature())
  expect_snapshot_error(style(features = "A"))
  expect_snapshot_error(style(features = 1))

  # Size
  expect_silent(s <- style(size = 9))
  expect_equal(s$size, 9)
  expect_snapshot_error(style(size = "A"))
  expect_snapshot_error(style(size = TRUE))
  expect_silent(s <- style(size = em(9)))
  expect_equal(s$size, relative(9)) # em size gets converted
  expect_silent(s <- style(size = rem(9)))
  expect_equal(s$size, rem(9))
  expect_silent(s <- style(size = relative(9)))
  expect_equal(s$size, relative(9))

  # Color
  expect_silent(s <- style(color = "red"))
  expect_equal(s$color, "red")
  expect_snapshot_error(style(color = 1))
  expect_snapshot_error(style(color = TRUE))
  expect_silent(s <- style(color = NA))
  expect_type(s$color, "character")

  # Lineheight
  expect_silent(s <- style(lineheight = 1.6))
  expect_equal(s$lineheight, 1.6)
  expect_snapshot_error(style(lineheight = "A"))
  expect_snapshot_error(style(lineheight = TRUE))
  expect_silent(s <- style(lineheight = relative(0.5)))
  expect_equal(s$lineheight, relative(0.5))
  expect_snapshot_error(style(lineheight = em(1.2)))
  expect_snapshot_error(style(lineheight = rem(1.2)))

  # Align
  expect_silent(s <- style(align = "center"))
  expect_equal(s$align, "center")
  expect_snapshot_error(style(align = 1))
  expect_snapshot_error(style(align = TRUE))

  # Tracking
  expect_silent(s <- style(tracking = 100))
  expect_equal(s$tracking, 100)
  expect_snapshot_error(style(tracking = "A"))
  expect_snapshot_error(style(tracking = TRUE))
  expect_silent(s <- style(tracking = relative(0.5)))
  expect_equal(s$tracking, relative(0.5))
  expect_snapshot_error(style(tracking = em(1.2)))
  expect_snapshot_error(style(tracking = rem(1.2)))

  # Indent
  expect_silent(s <- style(indent = 9))
  expect_equal(s$indent, 9)
  expect_snapshot_error(style(indent = "A"))
  expect_snapshot_error(style(indent = TRUE))
  expect_silent(s <- style(indent = em(9)))
  expect_equal(s$indent, em(9))
  expect_silent(s <- style(indent = rem(9)))
  expect_equal(s$indent, rem(9))
  expect_silent(s <- style(indent = relative(9)))
  expect_equal(s$indent, relative(9))

  # Hanging
  expect_silent(s <- style(hanging = 9))
  expect_equal(s$hanging, 9)
  expect_snapshot_error(style(hanging = "A"))
  expect_snapshot_error(style(hanging = TRUE))
  expect_silent(s <- style(hanging = em(9)))
  expect_equal(s$hanging, em(9))
  expect_silent(s <- style(hanging = rem(9)))
  expect_equal(s$hanging, rem(9))
  expect_silent(s <- style(hanging = relative(9)))
  expect_equal(s$hanging, relative(9))

  # Margin
  expect_silent(s <- style(margin = trbl(1, relative(2), em(3), rem(4))))
  expect_equal(s$margin_top, 1)
  expect_equal(s$margin_right, relative(2))
  expect_equal(s$margin_bottom, em(3))
  expect_equal(s$margin_left, rem(4))
  expect_snapshot_error(style(margin = "A"))
  expect_snapshot_error(style(margin = TRUE))

  # Padding
  expect_silent(s <- style(padding = trbl(1, relative(2), em(3), rem(4))))
  expect_equal(s$padding_top, 1)
  expect_equal(s$padding_right, relative(2))
  expect_equal(s$padding_bottom, em(3))
  expect_equal(s$padding_left, rem(4))
  expect_snapshot_error(style(padding = "A"))
  expect_snapshot_error(style(padding = TRUE))

  # Background
  expect_silent(s <- style(background = "red"))
  expect_equal(s$background, "red")
  expect_snapshot_error(style(background = 1))
  expect_snapshot_error(style(background = TRUE))
  expect_silent(s <- style(background = NA))
  expect_type(s$background, "character")
  if ("linearGradient" %in% getNamespaceExports("grid")) {
    expect_silent(s <- style(background = linearGradient()))
  }

  # Border
  expect_silent(s <- style(border = "red"))
  expect_equal(s$border, "red")
  expect_snapshot_error(style(border = 1))
  expect_snapshot_error(style(border = TRUE))
  expect_silent(s <- style(border = NA))
  expect_type(s$border, "character")

  # Border size
  expect_silent(s <- style(border_size = trbl(1, relative(2), em(3), rem(4))))
  expect_equal(s$border_size_top, 1)
  expect_equal(s$border_size_right, relative(2))
  expect_equal(s$border_size_bottom, em(3))
  expect_equal(s$border_size_left, rem(4))
  expect_snapshot_error(style(border_size = "A"))
  expect_snapshot_error(style(border_size = TRUE))

  # Border radius
  expect_silent(s <- style(border_radius = 9))
  expect_equal(s$border_radius, 9)
  expect_snapshot_error(style(border_radius = "A"))
  expect_snapshot_error(style(border_radius = TRUE))
  expect_silent(s <- style(border_radius = em(9)))
  expect_equal(s$border_radius, em(9))
  expect_silent(s <- style(border_radius = rem(9)))
  expect_equal(s$border_radius, rem(9))
  expect_silent(s <- style(border_radius = relative(9)))
  expect_equal(s$border_radius, relative(9))

  # Bullets
  expect_silent(s <- style(bullets = letters))
  expect_equal(s$bullets, letters)
  expect_snapshot_error(style(bullets = 1))
  expect_snapshot_error(style(bullets = TRUE))

  # Underline
  expect_silent(s <- style(underline = TRUE))
  expect_equal(s$underline, TRUE)
  expect_snapshot_error(style(underline = "A"))
  expect_snapshot_error(style(underline = 1))

  # Strikethrough
  expect_silent(s <- style(strikethrough = TRUE))
  expect_equal(s$strikethrough, TRUE)
  expect_snapshot_error(style(strikethrough = "A"))
  expect_snapshot_error(style(strikethrough = 1))

  # Image aspect ratio
  expect_silent(s <- style(img_asp = 1.6))
  expect_equal(s$img_asp, 1.6)
  expect_snapshot_error(style(img_asp = "A"))
  expect_snapshot_error(style(img_asp = TRUE))
})

test_that("style() creates the right structure", {
  s <- style()
  expect_true(is_style(s))
  expect_s3_class(s, "marquee_style")
  expect_identical(names(s)[seq_len(3)], c("size", "background", "color"))
})

test_that("base_style() creates a complete style", {
  s <- base_style()
  expect_s3_class(s, "marquee_style")
  expect_false(any(vapply(s, is.null, logical(1))))
})
