test_that("images are picked up", {
  logo <- system.file("help", "figures", "logo.png", package = "marquee")
  grob <- textGrob("test")
  gg <- ggplot2::ggplot()
  patch <- patchwork::wrap_plots(gg, gg)
  unknown <- "test"

  image_locs <- c(logo, "grob", "gg", "patch", unknown)

  imgs <- images_as_grobs(image_locs)

  expect_type(imgs, "list")
  expect_s3_class(imgs[[1]], "rastergrob")
  expect_s3_class(imgs[[2]], "text")
  expect_s3_class(imgs[[3]], "gtable")
  expect_s3_class(imgs[[4]], "gtable")
  expect_s3_class(imgs[[5]], "missing_grob")
})
