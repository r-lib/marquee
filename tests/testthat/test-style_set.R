test_that("style_set() does input checking", {
  expect_snapshot_error(style_set(body = style()))
  expect_snapshot_error(style_set(base = style()))
  expect_snapshot_error(style_set(base = letters))
  expect_snapshot_error(style_set(base = base_style(), style()))
})

test_that("style_set() constructs the right object", {
  ss <- style_set()
  expect_length(ss, 0)
  ss <- style_set(BASE = base_style(), bOdY = style())
  expect_named(ss[[1]], c("base", "body"))
  expect_s3_class(ss, "marquee_style_set")
  expect_true(is_style_set(ss))
})

test_that("modify_style() does correct modification", {
  ss <- style_set(base = base_style())
  ss <- modify_style(ss, "body", style(size = 9))
  expect_equal(ss[[1]]$body, style(size = 9))
  ss <- modify_style(ss, "body", size = 13)
  expect_equal(ss[[1]]$body, style(size = 13))
  ss <- modify_style(ss, "body", indent = 12, hanging = 2)
  expect_equal(ss[[1]]$body, style(size = 13, indent = 12, hanging = 2))
  ss <- modify_style(ss, "body", indent = 12, hanging = NULL)
  expect_equal(ss[[1]]$body, style(size = 13, indent = 12))

  ss <- modify_style(ss, "p", padding = trbl(1, 2, 3, 4))
  expect_equal(ss[[1]]$p, style(padding = trbl(1, 2, 3, 4)))

  expect_snapshot_error(modify_style(ss, "base", size = NULL))
  expect_snapshot_error(modify_style(ss, "base", style()))

  ss <- rep(style_set(base = base_style()), 3)
  ss <- modify_style(ss, "body", style())
  ss1 <- modify_style(ss, c("body", "p", "em"), size = 1:3)
  expect_equal(ss1[[1]]$body, style(size = 1))
  expect_equal(ss1[[2]]$p, style(size = 2))
  expect_equal(ss1[[3]]$em, style(size = 3))
  ss1 <- modify_style(ss1, "body", size = NULL)
  expect_equal(ss1[[1]]$body, style())
  expect_equal(ss1[[2]]$body, style())
  expect_equal(ss1[[3]]$body, style())

  expect_snapshot_error(modify_style(ss, "body", size = 1:2))
})

test_that("modify_style works on style objects", {
  s <- base_style()
  s <- modify_style(s, size = 6, color = "grey", padding = trbl(em(3), 0))
  expect_equal(s$size, 6)
  expect_equal(s$color, "grey")
  expect_equal(s$padding_top, em(3))
  expect_equal(s$padding_right, 0)
})

test_that("remove_style() works", {
  ss <- style_set(base = base_style(), body = style())

  expect_snapshot_error(remove_style(ss, "base"))

  expect_equal(remove_style(ss, "body"), style_set(base = base_style()))
})
