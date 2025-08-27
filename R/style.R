#' Create a style specification for a single tag
#'
#' `style()` constructs a `marquee_style` object specifying the styling for a
#' single tag. The meaning of `NULL` is to inherit the value from the parent
#' element. It follows that top parent (the body element), must have values for
#' all it's options. The `base_style()` constructor is a convenient constructor
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
#' @param margin The margin around the element, given as a call to [trbl()].
#' Margin refers to the area outside the box that text is placed in. If the
#' element has a background, the margin area will not be colored.
#' @param padding The padding around the element, given as a call to [trbl()].
#' Padding refers to the distance between the text and the border of the box it
#' will be drawn in. If the element has a background, the padding area will be
#' colored.
#' @param background The color of the background fill. The background includes
#' the padding but not the margin. Can be a solid color or a gradient or pattern
#' made with `grid::linearGradient()`/`grid::radialGradient()`/`grid::pattern()`
#' @param border The color of the background stroke. The background includes
#' the padding but not the margin
#' @param border_size The line width of the background stroke, given as a call
#' to [trbl()]
#' @param border_radius The corner radius of the background, given in points
#' @param outline The color of the outline stroke.
#' @param outline_width The line width of the outline stroke.
#' @param outline_join The line join type for the outline. Either `"round"`,
#' `"mitre"`, or `"bevel"`.
#' @param outline_mitre The mitre limit (relative distance between inner and
#' outer corner at a join) if `outline_join = "mitre"`.
#' @param bullets A vector of strings to use for bullets in unordered lists.
#' `marquee_bullets` provides a selection
#' @param underline Should text be underlined
#' @param strikethrough Should text be strikethrough
#' @param baseline The baseline shift to apply to the text
#' @param img_asp The default aspect ratio for block level images if not
#' provided by the image itself
#' @param text_direction The directional flow of the text. Either `"auto"` to
#' let it be determined by the content of the text, or `"ltr"`/`"rtl"` to
#' hard-code it to either left-to-right or right-to-left. This setting will not
#' change the order of glyphs within a span of text, but rather whether
#' consequtive blocks of text are laid out left-to-right or right-to-left. It
#' also affects to which side indentation is applied as well as the meaning of
#' `"auto"`, and `"justified-auto"` aligment.
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
#' base_style()
#'
style <- function(
  family = NULL,
  weight = NULL,
  italic = NULL,
  width = NULL,
  features = NULL,
  size = NULL,
  color = NULL,
  lineheight = NULL,
  align = NULL,
  tracking = NULL,
  indent = NULL,
  hanging = NULL,
  margin = NULL,
  padding = NULL,
  background = NULL,
  border = NULL,
  border_size = NULL,
  border_radius = NULL,
  outline = NULL,
  outline_width = NULL,
  outline_join = NULL,
  outline_mitre = NULL,
  bullets = NULL,
  underline = NULL,
  strikethrough = NULL,
  baseline = NULL,
  img_asp = NULL,
  text_direction = NULL
) {
  check_string(family, allow_null = TRUE)

  if (is.character(weight)) {
    weight <- systemfonts::as_font_weight(weight)
  }
  check_number_whole(weight, allow_null = TRUE)
  if (!is.null(weight)) {
    weight <- as.integer(weight)
  }

  check_bool(italic, allow_null = TRUE)

  if (is.character(width)) {
    width <- systemfonts::as_font_width(width)
  }
  check_number_whole(width, allow_null = TRUE)
  if (!is.null(width)) {
    width <- as.integer(width)
  }

  if (!is.null(features) && !inherits(features, "font_feature")) {
    stop_input_type(features, "a font_feature object", allow_null = TRUE)
  }

  if (inherits(size, "marquee_em")) {
    size <- relative(size[[1]])
  }
  if (is.unit(size)) {
    size <- as_bigpts(size, width = FALSE)
  }
  if (!is_modifier(size)) {
    check_number_decimal(size, allow_null = TRUE)
  }

  check_string(color, allow_null = TRUE, allow_na = TRUE)
  if (!is.null(color) && is.na(color)) {
    color[] <- NA_character_
  }

  if (!is_relative(lineheight)) {
    check_number_decimal(lineheight, allow_null = TRUE)
  }

  check_string(align, allow_null = TRUE)

  if (!is_relative(tracking)) {
    check_number_decimal(tracking, allow_null = TRUE)
  }

  if (is.unit(indent)) {
    indent <- as_bigpts(indent)
  }
  if (!is_modifier(indent)) {
    check_number_decimal(indent, allow_null = TRUE)
  }

  if (is.unit(hanging)) {
    hanging <- as_bigpts(hanging)
  }
  if (!is_modifier(hanging)) {
    check_number_decimal(hanging, allow_null = TRUE)
  }

  if (is.null(margin)) {
    margin <- trbl()
  }
  if (!is_trbl(margin)) {
    stop_input_type(margin, "a marquee_trbl object", allow_null = TRUE)
  }

  if (is.null(padding)) {
    padding <- trbl()
  }
  if (!is_trbl(padding)) {
    stop_input_type(padding, "a marquee_trbl object", allow_null = TRUE)
  }

  if (!inherits(background, "GridPattern")) {
    check_string(unclass(background), allow_null = TRUE, allow_na = TRUE)
    if (!is.null(background) && is.na(background)) background[] <- NA_character_
  }

  check_string(border, allow_null = TRUE, allow_na = TRUE)
  if (!is.null(border) && is.na(border)) {
    border[] <- NA_character_
  }

  if (is.null(border_size)) {
    border_size <- trbl()
  }
  if (!is_trbl(border_size)) {
    stop_input_type(border_size, "a marquee_trbl object", allow_null = TRUE)
  }

  if (is.unit(border_radius)) {
    border_radius <- as_bigpts(border_radius)
  }
  if (!is_modifier(border_radius)) {
    check_number_decimal(border_radius, allow_null = TRUE)
  }

  check_string(outline, allow_null = TRUE, allow_na = TRUE)
  if (!is.null(outline) && is.na(outline)) {
    outline[] <- NA_character_
  }

  if (is.unit(outline_width)) {
    outline_width <- as_bigpts(outline_width)
  }

  check_string(outline_join, allow_null = TRUE)
  if (!is.null(outline_join)) {
    outline_join <- arg_match0(outline_join, c("round", "mitre", "bevel"))
  }

  check_number_decimal(
    outline_mitre,
    allow_null = TRUE,
    min = 1,
    allow_infinite = FALSE
  )

  check_character(bullets, allow_null = TRUE)

  check_bool(underline, allow_null = TRUE)

  check_bool(strikethrough, allow_null = TRUE)

  check_number_decimal(img_asp, allow_null = TRUE)

  check_string(text_direction, allow_null = TRUE)

  if (!is_modifier(baseline)) {
    check_number_decimal(baseline, allow_null = TRUE)
  }

  structure(
    list(
      size = size, # Important this is the first. Required by compiled code
      background = background, # Important this is the second. Required by compiled code
      color = color, # Important this is the third. Required by compiled code
      family = family,
      weight = weight,
      italic = italic,
      width = width,
      features = features,
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
      outline = outline,
      outline_width = outline_width,
      outline_join = outline_join,
      outline_mitre = outline_mitre,
      bullets = bullets,
      underline = underline,
      strikethrough = strikethrough,
      baseline = baseline,
      img_asp = img_asp,
      text_direction = text_direction
    ),
    class = "marquee_style"
  )
}

is_style <- function(x) inherits(x, "marquee_style")

#' @export
format.marquee_style <- function(x, ...) {
  defined_options <- x[!vapply(x, is.null, logical(1))]
  if (length(defined_options) == 0) {
    return(character())
  }
  options <- format(
    names(defined_options),
    width = max(nchar(names(defined_options))),
    justify = "right"
  )
  paste0(
    options,
    ": ",
    vapply(
      defined_options,
      function(opt) {
        paste(format(opt, ...), collapse = ", ")
      },
      character(1)
    )
  )
}
#' @export
print.marquee_style <- function(x, ...) {
  options <- format(x, ...)
  cat("<marquee_style[", length(options), "]>\n", sep = "")
  if (length(options) != 0) {
    cat(options, sep = "\n")
  }
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

#' @export
`$<-.marquee_style` <- function(x, name, value) {
  cli::cli_abort(
    "Setting style values using {.arg $}, {.arg []}, or {.arg [[]]} are not permitted. Please use {.fun modify_style}"
  )
}

#' @export
`[[<-.marquee_style` <- function(x, ..., value) {
  cli::cli_abort(
    "Setting style values using {.arg $}, {.arg []}, or {.arg [[]]} are not permitted. Please use {.fun modify_style}"
  )
}

#' @export
`[<-.marquee_style` <- function(x, ..., value) {
  cli::cli_abort(
    "Setting style values using {.arg $}, {.arg []}, or {.arg [[]]} are not permitted. Please use {.fun modify_style}"
  )
}

#' @rdname style
#' @export
base_style <- function(
  family = "",
  weight = "normal",
  italic = FALSE,
  width = "normal",
  features = systemfonts::font_feature(),
  size = 12,
  color = "black",
  lineheight = 1.6,
  align = "auto",
  tracking = 0,
  indent = 0,
  hanging = 0,
  margin = trbl(0, 0, rem(1)),
  padding = trbl(0),
  background = NA,
  border = NA,
  border_size = trbl(0),
  border_radius = 0,
  outline = NA,
  outline_width = 1,
  outline_join = "round",
  outline_mitre = 10,
  bullets = marquee_bullets,
  underline = FALSE,
  strikethrough = FALSE,
  baseline = 0,
  img_asp = 1.65,
  text_direction = "auto"
) {
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
    outline = outline,
    outline_width = outline_width,
    outline_join = outline_join,
    outline_mitre = outline_mitre,
    bullets = bullets,
    underline = underline,
    strikethrough = strikethrough,
    baseline = baseline,
    img_asp = img_asp,
    text_direction = text_direction
  )
}
