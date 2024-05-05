test_that("parse works", {
  parsed <- marquee_parse(markdown_test)
  expect_s3_class(parsed, "marquee_parsed")
  expect_true(is_parsed(parsed))

  file <- tempfile()
  saveRDS(parsed, file)
  expect_snapshot_file(file, "parsed.RDS")

  parsed_with_html <- marquee_parse(markdown_test, ignore_html = FALSE)
  expect_equal(parsed$text[-17], parsed_with_html$text[-17])
  expect_equal(parsed_with_html$text[17], " lines. <span>WHAT</span>")
})
