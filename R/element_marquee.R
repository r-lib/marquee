#' ggplot2 theme element supporting marquee syntax
#'
#' This theme element is a drop-in replacement for `ggplot2::element_text()`. It
#' works by integrating the various style settings of the element into the base
#' style of the provided style set. If a margin is given, it is set on the body
#' tag with [skip_inherit()]. The default width is `NA` meaning that it will
#' span as long as the given text is, doing no line wrapping. You can set it to
#' any unit to make it fit within a specific width. However, this may not work
#' as expected with rotated text (you may get lucky). Note that you may see
#' small shifts in the visuals when going from `element_text()` to
#' `element_marquee()` as size reporting may differ between the two elements.
#'
#' @param family The font family of the base style
#' @param colour,color The font colour of the base style
#' @param size The font size of the base style
#' @param lineheight The lineheight of the base style
#' @param margin The margin for the body tag
#' @param style A style set to base the rendering on
#' @param width The maximum width of the text. See the description for some
#' caveats for this
#' @inheritParams ggplot2::element_text
#'
#' @export
#'
#' @examplesIf utils::packageVersion("base") > "4.3" && rlang::is_installed("ggplot2")
#' library(ggplot2)
#' p <- ggplot(mtcars) +
#'   geom_point(aes(mpg, disp)) +
#'   labs(title = "A {.red *marquee*} title\n* Look at this bullet list\n\n* great, huh?") +
#'   theme_gray(base_size = 6) +
#'   theme(title = element_marquee())
#'
#' plot(p)
#'
#' ggplot(mtcars) +
#'   geom_histogram(aes(x = mpg)) +
#'   labs(title =
#' "I put a plot in your title so you can plot while you title
#'
#' ![](p)
#'
#' What more could you _possibly_ want?") +
#'   theme(title = element_marquee())
#'
element_marquee <- function(family = NULL, colour = NULL, size = NULL, hjust = NULL,
                            vjust = NULL, angle = NULL, lineheight = NULL,
                            color = NULL, margin = NULL, style = NULL, width = NULL,
                            inherit.blank = FALSE) {
  if (!is.null(color))
    colour <- color
  n <- max(length(family), length(colour), length(size),
           length(hjust), length(vjust), length(angle), length(lineheight))
  if (n > 1) {
    cli::cli_warn(c("Vectorized input to {.fn element_text} is not officially supported.",
                    i = "Results may be unexpected or may change in future versions of ggplot2."))
  }
  structure(list(family = family, colour = colour, size = size, hjust = hjust,
                 vjust = vjust, angle = angle, lineheight = lineheight,
                 margin = margin, style = style, width = width,
                 inherit.blank = inherit.blank),
            class = c("element_marquee", "element_text", "element"))
}

element_grob.element_marquee <- function(element, label = "", x = NULL, y = NULL, family = NULL,
                                         colour = NULL, size = NULL, hjust = NULL, vjust = NULL,
                                         angle = NULL, lineheight = NULL, margin = NULL, margin_x = FALSE,
                                         margin_y = FALSE, style = NULL, width = NULL, ...) {
  if (is.null(label)) return(ggplot2::zeroGrob())
  style <- style %||% element$style %||% classic_style()
  style <- modify_style(style, "base",
    family = family %||% element$family %||% style$base$family,
    color = colour %||% element$colour %||% style$base$color,
    size = size %||% element$size %||% style$base$size,
    lineheight = lineheight %||% element$lineheight %||% style$base$lineheight
  )
  margin <- margin %||% element$margin
  if (!is.null(margin)) {
    pad <- skip_inherit(trbl(
      if (margin_y) margin[1] else 0,
      if (margin_x) margin[2] else 0,
      if (margin_y) margin[3] else 0,
      if (margin_x) margin[4] else 0
    ))
    style <- modify_style(style, "body", padding = pad)
  }
  just <- rotate_just(angle %||% element$angle, hjust %||% element$hjust, vjust %||% element$vjust)
  n <- max(length(x), length(y), 1)
  x <- x %||% rep(just$hjust, n)
  y <- y %||% rep(just$vjust, n)
  width <- width %||% element$width %||% NA
  angle <- angle %||% element$angle %||% 0
  marquee_grob(label, style, x = x, y = y, width = width,
               hjust = hjust %||% element$hjust, vjust = vjust %||% element$vjust, angle = angle)
}

on_load({
  on_package_load("ggplot2", registerS3method("element_grob", "element_marquee", element_grob.element_marquee, asNamespace("ggplot2")))
})

# Adaption of ggplot2:::rotate_just() to work with additional just keywords
rotate_just <- function (angle, hjust, vjust) {
  angle <- (angle %||% 0)%%360
  if (is.character(hjust)) {
    hjust <- switch(hjust, "left" = , "left-ink" = 0, "center" = , "center-ink" = 0.5, "right" = , "right-ink" = 1)
  }
  if (is.character(vjust)) {
    vjust <- switch(vjust, "bottom" = , "bottom-ink" = ,"last-line" = 0, "center" = , "center-ink" = 0.5, "top" = , "top-ink" = , "first-line" = 1)
  }
  size <- vctrs::vec_size_common(angle, hjust, vjust)
  angle <- vctrs::vec_recycle(angle, size)
  hjust <- vctrs::vec_recycle(hjust, size)
  vjust <- vctrs::vec_recycle(vjust, size)
  case <- findInterval(angle, c(0, 90, 180, 270, 360))
  hnew <- hjust
  vnew <- vjust
  is_case <- which(case == 2)
  hnew[is_case] <- 1 - vjust[is_case]
  vnew[is_case] <- hjust[is_case]
  is_case <- which(case == 3)
  hnew[is_case] <- 1 - hjust[is_case]
  vnew[is_case] <- 1 - vjust[is_case]
  is_case <- which(case == 4)
  hnew[is_case] <- vjust[is_case]
  vnew[is_case] <- 1 - hjust[is_case]
  list(hjust = hnew, vjust = vnew)
}
