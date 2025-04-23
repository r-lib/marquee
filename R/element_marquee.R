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
#' @param margin The margin for the body tag. As margins in `element_text()`
#' doesn't rotate along with `angle` we follow this behavior here as well so
#' that the right margin becomes the bottom margin when rotating the text 90
#' degrees and so forth.
#' @param style A style set to base the rendering on
#' @param width The maximum width of the text. See the description for some
#' caveats for this
#' @inheritParams ggplot2::element_text
#'
#' @return An `element_marquee` object that can be used in place of
#' `element_text` in ggplot2 theme specifications
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
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
    family = family %||% element$family %||% style$base$family %||% "",
    color = colour %||% element$colour %||% style$base$color %||% "black",
    size = size %||% element$size %||% style$base$size %||% 12,
    lineheight = lineheight %||% element$lineheight %||% style$base$lineheight %||% 1
  )
  margin <- margin %||% element$margin
  angle <- (angle %||% element$angle %||% 0) %% 360

  # Make sure margin doesn't rotate with element as that is how it works for element_text
  if (angle > 45 && angle <= 135) {
    margin <- margin[c(4, 1, 2, 3)]
    tmp <- margin_y
    margin_y <- margin_x
    margin_x <- tmp
  } else if (angle > 135 && angle <= 225) {
    margin <- margin[c(3, 4, 1, 2)]
  } else if (angle > 225 && angle <= 315) {
    margin <- margin[c(2, 3, 4, 1)]
    tmp <- margin_y
    margin_y <- margin_x
    margin_x <- tmp
  }

  vjust <- vjust %||% element$vjust
  if (all(as.numeric(margin)[c(1, 3)] == 0)) vjust <- ink(vjust)
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
  grob <- marquee_grob(
    label, style, force_body_margin = TRUE, x = x, y = y, width = width,
    hjust = hjust %||% element$hjust,
    vjust = vjust,
    angle = angle
  )
  if (xor(margin_x, margin_y)) {
    if (margin_x) grob$full_height <- unit(1, "null")
    if (margin_y) grob$full_width  <- unit(1, "null")
  }
  grob
}

on_load({
  on_package_load("ggplot2", registerS3method("element_grob", "element_marquee", element_grob.element_marquee, asNamespace("ggplot2")))
})

#' Convert all text elements in a theme to marquee elements
#'
#' While [element_marquee()] should behave similar to `ggplot2::element_text()`
#' when used on plain text (i.e. text without any markdown markup), the reality
#' can be different. This is because the text shaping engine used by marquee
#' ([textshaping::shape_text()]) may differ from the one used by the graphics
#' device (which is responsible for laying out text in `element_text()`).
#' Differences can range from slight differences in letter spacing to using a
#' different font altogether (this is because the font keywords `""`, `"sans"`,
#' `"serif"`, `"mono"`, and `"symbol"` may be mapped to different fonts
#' depending on the shaper). One way to handle this is to provide an explicit
#' font name for the elements, but alternatively you can use this function to
#' convert all text elements in a theme to [element_marquee()]
#'
#' @param theme A (complete) ggplot2 theme
#'
#' @return `theme` with all text elements substituted for marquee elements
#'
#' @export
#'
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' ggplot(mtcars) +
#'   geom_point(aes(disp, mpg)) +
#'   ggtitle("How about that") +
#'   marquefy_theme(theme_gray())
#'
marquefy_theme <- function(theme) {
  theme[] <- lapply(theme, function(elem) {
    if (!inherits(elem, "element_text")) return(elem)
    style <- classic_style()
    if (!is.null(elem$face)) {
      if (elem$face %in% c("italic", "bold.italic")) {
        style <- modify_style(style, "base", italic = TRUE)
      }
      if (elem$face %in% c("bold", "bold.italic")) {
        style <- modify_style(style, "base", weight = "bold")
      }
    }
    element_marquee(family = elem$family, colour = elem$colour, size = elem$size,
                    hjust = elem$hjust, vjust = elem$vjust, angle = elem$angle,
                    lineheight = elem$lineheight, margin = elem$margin,
                    style = style, width = NA,
                    inherit.blank = elem$inherit.blank)
  })
  theme
}

# Adaption of ggplot2:::rotate_just() to work with additional just keywords
rotate_just <- function (angle, hjust, vjust) {
  angle <- (angle %||% 0)%%360
  if (is.character(hjust)) {
    hjust <- switch(hjust, "left" = 0, "left-ink" = ink(0), "center" = 0.5, "center-ink" = ink(0.5), "right" = 1, "right-ink" = ink(1))
  }
  if (is.character(vjust)) {
    vjust <- switch(vjust, "bottom" = 0, "bottom-ink" = ink(0),"last-line" = 0, "center" = 0.5, "center-ink" = ink(0.5), "top" = 1, "top-ink" = ink(1), "first-line" = 1)
  }
  size <- vctrs::vec_size_common(angle, hjust, vjust)
  angle <- vctrs::vec_recycle(angle, size)
  hjust <- vctrs::vec_cast(vctrs::vec_recycle(hjust, size), ink())
  vjust <- vctrs::vec_cast(vctrs::vec_recycle(vjust, size), ink())
  case <- findInterval(angle, c(0, 90, 180, 270, 360))
  hnew <- as.numeric(hjust)
  vnew <- as.numeric(vjust)
  is_case <- which(case == 2)
  hnew[is_case] <- 1 - as.numeric(vjust[is_case])
  vnew[is_case] <- as.numeric(hjust[is_case])
  is_case <- which(case == 3)
  hnew[is_case] <- 1 - as.numeric(hjust[is_case])
  vnew[is_case] <- 1 - as.numeric(vjust[is_case])
  is_case <- which(case == 4)
  hnew[is_case] <- as.numeric(vjust[is_case])
  vnew[is_case] <- 1 - as.numeric(hjust[is_case])
  list(
    hjust = ink(hnew, ifelse(case == 3, vctrs::vec_data(hjust)$ink, vctrs::vec_data(vjust)$ink)),
    vjust = ink(vnew, ifelse(case == 3, vctrs::vec_data(vjust)$ink, vctrs::vec_data(hjust)$ink))
  )
}
