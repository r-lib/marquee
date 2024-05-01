#' Draw text formatted with marquee
#'
#' The geom is an extension of `geom_text()` and `geom_label()` that allows you
#' to draw richly formatted text in marquee-markdown format in your plot. For
#' plain text it is a near-drop-in replacement for the above geoms except some
#' sizing might be very slightly different. However, using this geom you are
#' able to access the much more powerful font settings available in marquee, so
#' even then it might make sense to opt for this geom.
#'
#' @inheritParams ggplot2::geom_text
#'
#' @details
#' Styling of the text is based on a style set with the exception that the
#' standard aesthetics such as family, size, colour, fill, etc. are recognized
#' and applied to the base tag style. The default style set ([classic_style])
#' can be changed using the style aesthetic which can take a vector of style
#' sets so that each text can rely on it's own style if needed. As with
#' [element_marquee()], the `fill` aesthetic is treated differently and not
#' applied to the base tag, but to the body tag as a [skip_inherit()] style so
#' as to not propagate the fill.
#'
#' Contrary to the standard text and label geoms, `geom_marquee()` takes a
#' `width` aesthetic that can be used to turn on soft wrapping of text. The
#' default value (`NA`) lets the text run as long as it want's (honoring hard
#' breaks), but setting this to something else will instruct marquee to use at
#' most that amount of space. You can use grid units to set it to an absolute
#' amount.
#'
#' @export
#'
#' @examplesIf utils::packageVersion("base") > "4.3" && rlang::is_installed("ggplot2")
#'
#' library(ggplot2)
#' # Standard use
#' p <- ggplot(mtcars, aes(wt, mpg))
#' p + geom_marquee(aes(label = rownames(mtcars)))
#'
#' # Make use of more powerful font features (note, result may depend on fonts
#' # installed on the system)
#' p + geom_marquee(
#'   aes(label = rownames(mtcars)),
#'   style = classic_style(weight = "thin", width = "condensed")
#' )
#'
#' # Turn on line wrapping
#' p + geom_marquee(aes(label = rownames(mtcars)), width = unit(2, "cm"))
#'
#' # Style like label
#' label_style <- modify_style(
#'   classic_style(),
#'   "body",
#'   padding = skip_inherit(trbl(4)),
#'   border = "black",
#'   border_size = skip_inherit(trbl(1)),
#'   border_radius = 3
#' )
#' p + geom_marquee(aes(label = rownames(mtcars), fill = gear), style = label_style)
#'
#' # Use markdown to style the text
#' red_bold_names <- sub("(\\w+)", "{.red **\\1**}", rownames(mtcars))
#' p + geom_marquee(aes(label = red_bold_names))
#'
geom_marquee <- function(mapping = NULL, data = NULL, stat = "identity",
                         position = "identity", ..., size.unit = "mm",
                         na.rm = FALSE, show.legend = NA, inherit.aes = TRUE) {
  check_installed("ggplot2")

  ggplot2::layer(
    data = data, mapping = mapping, stat = stat, geom = GeomMarquee$geom,
    position = position, show.legend = show.legend, inherit.aes = inherit.aes,
    params = list2(size.unit = size.unit, na.rm = na.rm, ...)
  )
}

#' @export
GeomMarquee <- new_environment(list(geom = NULL))

make_marquee_geom <- function() {
  GeomMarquee$geom <- ggplot2::ggproto(
    "GeomMarquee", ggplot2::Geom,

    required_aes = c("x", "y", "label"),

    default_aes = ggplot2::aes(
      colour = "black", fill = NA, size = 3.88, angle = 0,
      hjust = 0.5, vjust = 0.5, alpha = NA, family = "", lineheight = 1.2,
      style = classic_style(), width = NA
    ),

    draw_panel = function(data, panel_params, coord, na.rm = FALSE, size.unit = "mm") {
      lab <- data$label

      styles <- data$style
      if (!is_style_set(styles)) {
        stop_input_type(styles, "a marquee_style_set object", arg = "style")
      }
      check_character(data$family, arg = "family")
      size <- data$size * resolve_text_unit(size.unit) * 72.72 / 72
      check_numeric(size, arg = "size")
      check_numeric(data$lineheight)
      colour <- ggplot2::alpha(data$colour, data$alpha)
      check_character(colour, arg = "colour")
      if (!is.character(data$fill) &&
          !(is.logical(data$fill) && all(is.na(data$fill))) &&
          !all(vapply(data$fill, function(x) is.character(x) || inherits(x, "GridPattern"), logical(1)))) {
        stop_input_type(data$fill, "a character vector or a list of strings and patters", arg = "fill")
      }
      for (i in seq_along(styles)) {
        styles[[i]]$base$family <- data$family[[i]]
        styles[[i]]$base$size <- size[[i]]
        styles[[i]]$base$lineheight <- data$lineheight[[i]]
        styles[[i]]$base$color <- colour[[i]]
        if (!"body" %in% names(styles[[i]])) {
          styles[[i]]$body <- style()
        }
        styles[[i]]$body$background <- skip_inherit(data$fill[[i]])
      }

      data <- coord$transform(data, panel_params)

      data$vjust <- compute_just(data$vjust, data$y, data$x, data$angle)
      data$hjust <- compute_just(data$hjust, data$x, data$y, data$angle)

      marquee_grob(
        text = lab, style = styles,
        x = data$x, y = data$y, width = data$width,
        hjust = data$hjust, vjust = data$vjust,
        angle = data$angle, name = "geom_marquee"
      )
    },

    draw_key = ggplot2::draw_key_label
  )
}

on_load(on_package_load("ggplot2", {
 make_marquee_geom()
}))

combine_styles <- function(style, family, size, lineheight, color, background) {browser()
  style <- modify_style(style, "base", family = family, size = size, color = color, lineheight = lineheight)
  modify_style(style, "body", background = skip_inherit(background))
}

resolve_text_unit <- function(unit) {
  unit <- arg_match0(unit, c("mm", "pt", "cm", "in", "pc", "Pt"))
  switch(
    unit,
    "mm" = 2.845276,
    "cm" = 28.45276,
    "in" = 72.27,
    "pc" = 12,
    "Pt" = 72 / 72.27,
    1
  )
}

compute_just <- function (just, a = 0.5, b = a, angle = 0) {
  if (!is.character(just)) {
    return(just)
  }
  if (any(grepl("outward|inward", just))) {
    angle <- angle%%360
    angle <- ifelse(angle > 180, angle - 360, angle)
    angle <- ifelse(angle < -180, angle + 360, angle)
    rotated_forward <- grepl("outward|inward", just) & (angle > 45 & angle < 135)
    rotated_backwards <- grepl("outward|inward", just) & (angle < -45 & angle > -135)
    ab <- ifelse(rotated_forward | rotated_backwards, b, a)
    just_swap <- rotated_backwards | abs(angle) > 135
    inward <- (just == "inward" & !just_swap | just == "outward" & just_swap)
    just[inward] <- c("left", "center", "right")[just_dir(ab[inward])]
    outward <- (just == "outward" & !just_swap) | (just == "inward" & just_swap)
    just[outward] <- c("right", "center", "left")[just_dir(ab[outward])]
  }
  just
}

just_dir <- function (x, tol = 0.001) {
  out <- rep(2L, length(x))
  out[x < 0.5 - tol] <- 1L
  out[x > 0.5 + tol] <- 3L
  out
}
