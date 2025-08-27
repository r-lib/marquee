test_that("grobs gets correctly constructed", {
  grob1 <- marquee_grob(markdown_test, classic_style(lineheight = 1.1))
  expect_s3_class(grob1, "marquee_grob")
  expect_false(inherits(grob1, "marquee_precalculated_grob"))

  png(tempfile())
  grob2 <- marquee_grob(
    markdown_test,
    classic_style(lineheight = 1.1),
    width = unit(10, "cm")
  )
  dev.off()
  expect_s3_class(grob2, "marquee_grob")
  expect_s3_class(grob2, "marquee_precalculated_grob")

  expect_equal(grob1$text, grob2$text)

  expect_snapshot_error(marquee_grob("test", hjust = "venstre"))
  expect_snapshot_error(marquee_grob("test", hjust = TRUE))

  expect_snapshot_error(marquee_grob("test", vjust = "bund"))
  expect_snapshot_error(marquee_grob("test", hjust = TRUE))

  expect_s3_class(grob1$x, "unit")
  expect_s3_class(grob1$y, "unit")
  expect_s3_class(grob1$width, "unit")

  # Are tight lists handled
  expect_equal(
    grob1$text$margin_bottom[grob1$text$tight & grob1$text$type == "li"],
    c(1.2, 0)
  )

  # Are images handled
  expect_equal(grob1$images$index, c(12, 19))
  expect_equal(grob1$images$path, c("test", "testgrob"))
  expect_equal(grob1$images$inline, c(TRUE, FALSE))
  expect_equal(grob2$images$id, c(1, 1))

  # Are block info calculated
  expect_snapshot(grob1$blocks)

  # Are bullets placed
  skip_on_os("windows")
  skip_on_os("linux")
  expect_snapshot(grob1$bullets)
})

test_that("grob looks as it should (sadly too complex to test other way)", {
  grob1 <- marquee_grob(markdown_test, classic_style(lineheight = 1.1))

  skip_on_os("windows")
  skip_on_os("linux")
  file <- tempfile()
  ragg::agg_png(file, width = 500, height = 1000)
  grid.draw(grob1)
  dev.off()

  expect_snapshot_file(file, "marquee_grob.png")
})
