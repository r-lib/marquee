images_as_grobs <- function(paths, env = caller_env()) {
  is_png <- grepl("\\w\\.png$", paths)
  is_jpeg <- grepl("\\w\\.jpe?g$", paths)
  is_svg <- grepl("\\w\\.svg$", paths)
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
    } else if (is_svg[i]) {
      check_installed("rsvg")
      svg <- charToRaw(paste0(trimws(readLines(paths[i])), collapse = ""))
      obj <- try_fetch(
        rsvg::rsvg_nativeraster(svg, width = 500),
        error = function(...) NULL
      )
      if (!is.null(obj)) {
        obj <- svg_grob(svg, ncol(obj) / nrow(obj))
      }
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

svg_grob <- function(path, asp = NULL, x = unit(0.5, "npc"), y = unit(0.5, "npc"),
                     just = "centre", hjust = NULL, vjust = NULL,
                     default.units = "npc", name = NULL, gp = gpar(), vp = NULL) {
  gTree(path = path, asp = asp, x = x, y = y, just = just, hjust = hjust, vjust = vjust,
        default.units = default.units, name = name, gp = gp, vp = vp, cl = "svg_grob")
}

#' @export
makeContent.svg_grob <- function(x) {
  width <- convertWidth(unit(1, "npc"), "inches", TRUE) * 300
  raster <- rsvg::rsvg_nativeraster(x$path, width = width)
  setChildren(x, gList(rasterGrob(raster, width = unit(1, "npc"))))
}

