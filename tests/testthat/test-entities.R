test_parsed_text <- function(input, output, index = 2, ...) {
  parsed <- marquee_parse(input, ...)
  expect_equal(parsed$text[index], output)
}

test_that("HTML entities work", {
  # test some commonly used symbols
  test_parsed_text("&gt;", ">")
  test_parsed_text("&lt;", "<")
  test_parsed_text("&amp;", "&")
  test_parsed_text("&copy;", "©")
  test_parsed_text("&trade;", "™")
  test_parsed_text("&nbsp;", "\u00A0")

  # test symbols outside the 16-bit Unicode code points
  test_parsed_text("&afr;", "\uD835\uDD1E")
  test_parsed_text("&bfr;", "\uD835\uDD1F")
  test_parsed_text("&cfr;", "\uD835\uDD20")
  
  # test entity at beginning of longer string
  test_parsed_text("&gt; abcd efgh", "> abcd efgh")
  # test entity at end of longer string
  test_parsed_text("abcd efgh &gt;", "abcd efgh >")
  
  # entities get parsed regardless of whether html is ignored or not
  test_parsed_text("&gt;<br>", ">", ignore_html = TRUE)
  test_parsed_text("&gt;<br>", "><br>", ignore_html = FALSE)
})
