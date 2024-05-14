test_that("images are picked up", {

  skip_if_not(getRversion() >= "4.1")

  # Depending on how we are testing the logo may reside in one of these locations
  logo <- system.file("man", "figures", "logo.png", package = "marquee")
  if (logo == "") logo <- system.file("help", "figures", "logo.png", package = "marquee")

  grob <- textGrob("test")
  gg <- ggplot2::ggplot()
  patch <- patchwork::wrap_plots(gg, gg)
  table <- gt::gt(mtcars)
  unknown <- "test"

  image_locs <- c(logo, "grob", "gg", "patch", "table", unknown)

  imgs <- images_as_grobs(image_locs)

  expect_type(imgs, "list")
  expect_s3_class(imgs[[1]], "rastergrob")
  expect_s3_class(imgs[[2]], "text")
  expect_s3_class(imgs[[3]], "gtable")
  expect_s3_class(imgs[[4]], "gtable")
  expect_s3_class(imgs[[6]], "missing_grob")
  if ("as_gtable" %in% getNamespaceExports("gt")) {
    expect_s3_class(imgs[[5]], "gtable")
  } else {
    expect_s3_class(imgs[[5]], "missing_grob")
  }
})
