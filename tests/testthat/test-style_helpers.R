test_that("modifiers are constructed correctly", {
  x <- relative(1)
  expect_s3_class(x, "marquee_relative")
  expect_true(is_relative(x))
  expect_true(is_modifier(x))
  expect_type(x, "list")
  expect_equal(x[[1]], 1)

  x <- em(1)
  expect_s3_class(x, "marquee_em")
  expect_true(is_em(x))
  expect_true(is_modifier(x))
  expect_type(x, "list")
  expect_equal(x[[1]], 1)

  x <- rem(1)
  expect_s3_class(x, "marquee_rem")
  expect_true(is_rem(x))
  expect_true(is_modifier(x))
  expect_type(x, "list")
  expect_equal(x[[1]], 1)
})

test_that("trbl() constructs correct data", {
  expect_equal(trbl(1), trbl(1, 1, 1, 1))
  expect_equal(trbl(1, 2), trbl(1, 2, 1, 2))
  expect_equal(trbl(1, 2, 3), trbl(1, 2, 3, 2))

  expect_snapshot_error(trbl("a", 1, 1, 1))
  expect_snapshot_error(trbl(1, "a", 1, 1))
  expect_snapshot_error(trbl(1, 1, "a", 1))
  expect_snapshot_error(trbl(1, 1, 1, "a"))

  expect_silent(trbl(relative(1), 1, 1, 1))
  expect_silent(trbl(1, em(1), 1, 1))
  expect_silent(trbl(1, 1, rem(1), 1))

  box <- trbl(1, relative(1), em(1), rem(1))
  expect_s3_class(box, "marquee_trbl")
  expect_true(is_trbl(box))
  expect_equal(box[[1]], 1)
  expect_equal(box[[2]], relative(1))
  expect_equal(box[[3]], em(1))
  expect_equal(box[[4]], rem(1))
})

test_that("skip_inherit() works with different types", {
  expect_s3_class(skip_inherit(1), "marquee_skip_inherit")
  expect_s3_class(skip_inherit(relative(1)), "marquee_skip_inherit")
  expect_s3_class(skip_inherit(em(1)), "marquee_skip_inherit")
  expect_s3_class(skip_inherit(rem(1)), "marquee_skip_inherit")

  expect_s3_class(skip_inherit(relative(1)), "marquee_relative")
  expect_s3_class(skip_inherit(em(1)), "marquee_em")
  expect_s3_class(skip_inherit(rem(1)), "marquee_rem")

  box <- skip_inherit(trbl(1, 2, 3, 4))
  expect_s3_class(box[[1]], "marquee_skip_inherit")
  expect_s3_class(box[[2]], "marquee_skip_inherit")
  expect_s3_class(box[[3]], "marquee_skip_inherit")
  expect_s3_class(box[[4]], "marquee_skip_inherit")
})
