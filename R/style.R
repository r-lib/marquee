#' Create a style specification for a single tag
#'
#' `style()` constructs a `marquee_style` object specifying the styling for a
#' single tag. The meaning of `NULL` is to inherit the value from the parent
#' element. It follows that top parent (the body element), must have values for
#' all it's options. The `body_style()` constructor is a convenient constructor
#' for a style with sensible defaults for all it's options.
#'
#' @param family The name of the font family to use
#' @param weight The font weight to use. Can either be a number (`0`, `100`,
#' `200`, `300`, `400`, `500`, `600`, `700`, `800`, or `900`) or a strings
#' (`"undefined"`, `"thin"`, `"ultralight"`, `"light"`, `"normal"`, `"medium"`,
#' `"semibold"`, `"bold"`, `"ultrabold"`, or `"heavy"`)
#' @param italic Should the font be slanted
#' @param width The font width to use. Can either be a number (``0`, `1`, `2`,
#' `3`, `4`, `5`, `6`, `7`, `8`, or `9`) or strings (`"undefined"`,
#' `"ultracondensed"`, `"extracondensed"`, `"condensed"`, `"semicondensed"`,
#' `"normal"`, `"semiexpanded"`, `"expanded"`, `"extraexpanded"`, or
#' `"ultraexpanded"`)
#' @param features A [font_feature][systemfonts::font_feature] object specifying
#' any OpenType font features to apply to the font
#' @param size The size of the font in points. Can be [relative()] or [em()] in
#' which case it is based on the parent font size (for `size` these are
#' equivalent) or [rem()] in which case it is based on the font size of the body
#' element.
#' @param color Is the color of the font
#' @param lineheight The spacing between subsequent lines relative to the font
#' size. Can be [relative()] in which case it is based on the parent lineheight.
#' @param align The alignment within the text. One of `"left"`, `"center"`,
#' `"right"`, `"justified-left"`, `"justified-center"`, `"justified-right"`, or
#' `"distributed"`
#' @param tracking Additional character spacing measured in 1/1000em. Can be
#' [relative()] in which case it is based on the parent tracking.
#' @param indent The indentation of the first line in a paragraph measured in
#' points. Can be [relative()] in which case it is based on the parent indent,
#' [em()] in which case it is based on the font size in this style, or [rem()]
#' in which case it is based on the font size of the body element.
#' @param hanging The indentation of all but the first line in a paragraph
#' measured in points. Can be [relative()] in which case it is based on the
#' parent hanging, [em()] in which case it is based on the font size in this
#' style, or [rem()] in which case it is based on the font size of the body
#' element.
#' @param margin The margin around the element, given as a call to [box()]
#' @param padding The padding around the element, given as a call to [box()]
#' @param background The color of the background fill. The background includes
#' the padding but not the margin. Can be a solid color or a gradient or pattern
#' made with [grid::linearGradient()]/[grid::radialGradient()]/[grid::pattern()]
#' @param border The color of the background stroke. The background includes
#' the padding but not the margin
#' @param border_size The line width of the background stroke, given as a call
#' to [box()]
#' @param border_radius The corner radius of the background, given in points
#' @param bullets A vector of strings to use for bullets in unordered lists.
#' `marquee_bullets` provides a selection
#' @param underline Should text be underlined
#' @param strikethrough Should text be strikethrough
#' @param img_asp The default aspect ratio for block level images if not
#' provided by the image itself
#'
#' @return A `marquee_style` object
#'
#' @export
#'
#' @examples
#' # A partial style
#' style(color = "red", underline = TRUE)
#'
#' # Full style
#' body_style()
#'
style <- function(family = NULL, weight = NULL, italic = NULL, width = NULL,
                  features = NULL, size = NULL, color = NULL, lineheight = NULL,
                  align = NULL, tracking = NULL, indent = NULL, hanging = NULL,
                  margin = NULL, padding = NULL, background = NULL, border = NULL,
                  border_size = NULL, border_radius = NULL, bullets = NULL,
                  underline = NULL, strikethrough = NULL, img_asp = NULL) {
  check_string(family, allow_null = TRUE)

  if (is.character(weight)) weight <- systemfonts::as_font_weight(weight)
  if (!is.null(weight) && length(weight) != 1) cli::cli_abort("{.arg weight} must be a scalar")
  if (!is.null(weight)) weight <- as.integer(weight)

  check_bool(italic, allow_null = TRUE)

  if (is.character(width)) width <- systemfonts::as_font_width(width)
  if (!is.null(width) && length(width) != 1) cli::cli_abort("{.arg width} must be a scalar")
  if (!is.null(width)) width <- as.integer(width)

  if (!is.null(features) && !inherits(features, "font_feature")) {
    stop_input_type(features, "a font_feature object", allow_null = TRUE)
  }

  if (inherits(size, "marquee_em")) size <- relative(size[[1]])
  if (!is_modifier(size)) check_number_decimal(size, allow_null = TRUE)

  check_string(color, allow_null = TRUE, allow_na = TRUE)
  if (!is.null(color) && is.na(color)) color <- NA_character_

  if (!is_relative(lineheight)) check_number_decimal(lineheight, allow_null = TRUE)

  check_string(align, allow_null = TRUE)

  if (!is_relative(tracking)) check_number_decimal(tracking, allow_null = TRUE)

  if (!is_modifier(indent)) check_number_decimal(indent, allow_null = TRUE)

  if (!is_modifier(hanging)) check_number_decimal(hanging, allow_null = TRUE)

  if (is.null(margin)) margin <- box()
  if (!is_box(margin)) stop_input_type(margin, "a marquee_box object", allow_null = TRUE)

  if (is.null(padding)) padding <- box()
  if (!is_box(padding)) stop_input_type(padding, "a marquee_box object", allow_null = TRUE)

  if (!inherits(background, "GridPattern")) {
    check_string(background, allow_null = TRUE, allow_na = TRUE)
    if (!is.null(background) && is.na(background)) background <- NA_character_
  }

  check_string(border, allow_null = TRUE, allow_na = TRUE)
  if (!is.null(border) && is.na(border)) border <- NA_character_

  if (is.null(border_size)) border_size <- box()
  if (!is_box(border_size)) stop_input_type(border_size, "a marquee_box object", allow_null = TRUE)

  if (!is_modifier(border_radius)) check_number_decimal(border_radius, allow_null = TRUE)

  check_character(bullets, allow_null = TRUE)

  check_bool(underline, allow_null = TRUE)

  check_bool(strikethrough, allow_null = TRUE)

  check_number_decimal(img_asp, allow_null = TRUE)

  structure(list(
      size = size, # Important this is the first. Required by compiled code
      background = background, # Important this is the second. Required by compiled code
      family = family,
      weight = weight,
      italic = italic,
      width = width,
      features = features,
      color = color,
      lineheight = lineheight,
      align = align,
      tracking = tracking,
      indent = indent,
      hanging = hanging,
      margin_top = margin[[1]],
      margin_right = margin[[2]],
      margin_bottom = margin[[3]],
      margin_left = margin[[4]],
      padding_top = padding[[1]],
      padding_right = padding[[2]],
      padding_bottom = padding[[3]],
      padding_left = padding[[4]],
      border = border,
      border_size_top = border_size[[1]],
      border_size_right = border_size[[2]],
      border_size_bottom = border_size[[3]],
      border_size_left = border_size[[4]],
      border_radius = border_radius,
      bullets = bullets,
      underline = underline,
      strikethrough = strikethrough,
      img_asp = img_asp
    ),
    class = "marquee_style"
  )
}

is_style <- function(x) inherits(x, "marquee_style")

#' @export
format.marquee_style <- function(x, ...) {
  defined_options <- x[!vapply(x, is.null, logical(1))]
  if (length(defined_options) == 0) return(character())
  options <- format(names(defined_options), width = max(nchar(names(defined_options))), justify = "right")
  paste0(options, ": ", vapply(defined_options, function(opt) {
    paste(format(opt, ...), collapse = ", ")
  }, character(1)))
}
#' @export
print.marquee_style <- function(x, ...) {
  options <- format(x, ...)
  if (length(options) == 0) {
    cat("An empty marquee style\n")
    return(invisible(NULL))
  }
  cat("A marquee style\n")
  cat(options, sep = "\n")
  return(invisible(NULL))
}
#' @export
str.marquee_style <- function(object, ...) {
  options <- format(object, ...)
  if (length(options) == 0) {
    cat("<empty marquee style>\n")
    return(invisible(NULL))
  }
  cat("<marquee style> ")
  cat(trimws(options), sep = ", ", append = TRUE)
  return(invisible(NULL))
}

#' @rdname style
#' @export
body_style <- function(family = "", weight = "normal", italic = FALSE,
                       width = "normal", features = systemfonts::font_feature(),
                       size = 12, color = "black", lineheight = 1.6,
                       align = "left", tracking = 0, indent = 0, hanging = 0,
                       margin = box(0, 0, rem(1)), padding = box(0), background = NA,
                       border = NA, border_size = box(0), border_radius = 0,
                       bullets = marquee_bullets, underline = FALSE,
                       strikethrough = FALSE, img_asp = 1.65) {
  style(
    family = family,
    weight = weight,
    italic = italic,
    width = width,
    features = features,
    size = size,
    color = color,
    lineheight = lineheight,
    align = align,
    tracking = tracking,
    indent = indent,
    hanging = hanging,
    margin = margin,
    padding = padding,
    background = background,
    border = border,
    border_size = border_size,
    border_radius = border_radius,
    bullets = bullets,
    underline = underline,
    strikethrough = strikethrough,
    img_asp = img_asp
  )
}
