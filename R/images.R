images_as_grobs <- function(paths, env = caller_env()) {
  is_file <- file.exists(paths)
  is_png <- is_file & grepl("\\w\\.png$", paths)
  is_jpeg <- is_file & grepl("\\w\\.jpe?g$", paths)
  lapply(seq_along(paths), function(i) {
    obj <- NULL
    if (is_png[i]) {
      obj <- try_fetch(
        rasterGrob(png::readPNG(paths[i], native = TRUE)),
        error = function(...) NULL
      )
    } else if (is_jpeg[i]) {
      obj <- try_fetch(
        rasterGrob(jpeg::readJPEG(paths[i], native = TRUE)),
        error = function(...) NULL
      )
    }
    if (is.null(obj)) {
      obj <- get0(paths[i], envir = env)
    }
    if (inherits(obj, "patchwork")) {
      check_installed("patchwork")
      obj <- patchwork::patchworkGrob(obj)
    }
    if (inherits(obj, "ggplot")) {
      check_installed("ggplot2")
      obj <- ggplot2::ggplotGrob(obj)
    }
    if (is.null(obj) || !is.grob(obj)) {
      obj <- missing_grob()
    }
    obj
  })
}

missing_grob <- function() {
  grobTree(
    segmentsGrob(
      x0 = c(0, 0),
      y0 = c(0, 1),
      x1 = c(1, 1),
      y1 = c(1, 0),
      gp = gpar(col = "red", lwd = 2)
    ),
    rectGrob(
      gp = gpar(col = "black", fill = NA, lwd = 4)
    ),
    vp = viewport(clip = rectGrob()),
    cl = "missing_grob"
  )
}
