test_that("parse works", {
  parsed <- marquee_parse(markdown_test)
  expect_s3_class(parsed, "marquee_parsed")
  expect_true(is_parsed(parsed))

  parsed_with_html <- marquee_parse(markdown_test, ignore_html = FALSE)
  expect_equal(parsed$text[-17], parsed_with_html$text[-17])
  expect_equal(parsed_with_html$text[17], " lines. <span>WHAT</span>")

  skip_on_os("windows")
  skip_on_os("linux")
  file <- tempfile()
  write.csv(
    parsed[, !names(parsed) %in% c("background", "features", "bullets")],
    file
  )
  expect_snapshot_file(file, "parsed.csv")
})
