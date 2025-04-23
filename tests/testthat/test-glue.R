test_that("glue properly detects custom spans", {
  test <- "YES"
  span <- "custom"
  expect_equal(
    marquee_glue("This test passes? {.red {test}}!"),
    "This test passes? {.red YES}!"
  )
  expect_equal(
    marquee_glue("This test passes? {.{span} {test}}!"),
    "This test passes? {.custom YES}!"
  )
  expect_equal(
    marquee_glue("This test passes? {.red That is a {test}}!"),
    "This test passes? {.red That is a YES}!"
  )
  expect_equal(
    marquee_glue("This test passes? {.red }!"),
    "This test passes? {.red }!"
  )
  expect_snapshot_error(marquee_glue("This test passes? {.red{test}}!"))
  expect_snapshot_error(marquee_glue("This test passes? {.red}!"))
  expect_snapshot_error(marquee_glue("This test passes? {.{span}2 test}!"))

  bad_tag <- "tag 2"
  expect_snapshot_error(marquee_glue("This test passes? {.{bad_tag} test}!"))

  expect_equal(
    marquee_glue("This test {#e2a passes? {.red That is a {test}}!}"),
    "This test {#e2a passes? {.red That is a YES}!}"
  )
})
