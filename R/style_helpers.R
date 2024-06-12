#' Helpers for defining styles
#'
#' marquee provides a small set of helpers for constructing the needed styles.
#' `relative()` specifies a numeric value as relative to the value of the parent
#' style by a certain factor, e.g. a font size of `relative(0.5)` would give a
#' style a font size half of it's parent. `em()` specify a numeric value as
#' relative to the font size of the current style. If the font size is `12`, and
#' indent is set to `em(2)`, then the indent will be equivalent to 24. `rem()`
#' works like `em()` but rather than using the font size of the current style it
#' uses the font size of the root style (which is the body element). `trbl()`
#' helps you construct styles that refers to sides of a rectangle (margin,
#' padding, and border size). The function names refers to the order of the
#' arguments (top, right, bottom, left). `skip_inherit()` tells the style
#' inheritance to ignore this value and look for the value one above in the
#' stack. `marquee_bullets` is just a character vector with 6 sensible bullet
#' glyphs for unordered lists.
#'
#' @param x A decimal number. If a vector is provided only the first element
#' will be used
#' @param top,right,bottom,left Values for the sides of the rectangles. Either
#' numbers or modifiers (relative, em, or rem)
#'
#' @return Objects of the relevant class
#'
#' @name style-helpers
#' @rdname style_helpers
#'
#' @examples
#' relative(0.35)
#'
#' em(2)
#'
#' rem(1.2)
#'
#' # Argument default means it recycles like CSS if fewer values are specified
#' trbl(6, em(1.5))
#'
#' skip_inherit("sans")
#'
#' marquee_bullets
#'
NULL

#' @rdname style_helpers
#' @export
relative <- function(x) structure(list(as.numeric(x)[1]), class = "marquee_relative")

is_relative <- function(x) inherits(x, "marquee_relative")

#' @export
format.marquee_relative <- function(x, ...) {
  paste0("relative(", format(x[[1]], ...), ")")
}

#' @export
print.marquee_relative <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(NULL)
}

#' @rdname style_helpers
#' @export
em <- function(x) structure(list(as.numeric(x)[1]), class = "marquee_em")

is_em <- function(x) inherits(x, "marquee_em")

#' @export
format.marquee_em <- function(x, ...) {
  paste0("em(", format(x[[1]], ...), ")")
}

#' @export
print.marquee_em <- print.marquee_relative

#' @rdname style_helpers
#' @export
rem <- function(x) structure(list(as.numeric(x)[1]), class = "marquee_rem")

is_rem <- function(x) inherits(x, "marquee_rem")

#' @export
format.marquee_rem <- function(x, ...) {
  paste0("rem(", format(x[[1]], ...), ")")
}

#' @export
print.marquee_rem <- print.marquee_relative

is_modifier <- function(x) inherits(x, c("marquee_relative", "marquee_em", "marquee_rem"))

#' @rdname style_helpers
#' @export
trbl <- function(top = NULL, right = top, bottom = top, left = right) {
  if (is.unit(top)) top <- convertHeight(top, "bigpts", TRUE)
  if (is.unit(right)) right <- convertWidth(right, "bigpts", TRUE)
  if (is.unit(bottom)) bottom <- convertHeight(bottom, "bigpts", TRUE)
  if (is.unit(left)) left <- convertWidth(left, "bigpts", TRUE)
  if (!is.null(top) && !is_modifier(top)) check_number_decimal(top, allow_null = TRUE)
  if (!is.null(right) && !is_modifier(right)) check_number_decimal(right, allow_null = TRUE)
  if (!is.null(bottom) && !is_modifier(bottom)) check_number_decimal(bottom)
  if (!is.null(left) && !is_modifier(left)) check_number_decimal(left, allow_null = TRUE)
  structure(list(top, right, bottom, left), class = "marquee_trbl")
}

is_trbl <- function(x) inherits(x, "marquee_trbl")

#' @export
format.marquee_trbl <- function(x, ...) {
  paste0(c("   top: ", " right: ", "bottom: ", "  left: "), vapply(x, format, character(1), ...))
}

#' @export
print.marquee_trbl <- function(x, ...) {
  cat("A marquee trbl\n")
  cat(format(x, ...), sep = "\n")
  invisible(NULL)
}

#' @rdname style_helpers
#' @export
skip_inherit <- function(x) {
  if (is_trbl(x)) x[] <- lapply(x[], skip_inherit)
  else class(x) <- c("marquee_skip_inherit", class(x))
  x
}

#' @export
format.marquee_skip_inherit <- function(x, ...) {
  str <- NextMethod()
  paste0(str, " (no inheritance)")
}

#' @export
`[[.marquee_skip_inherit` <- function(x, ..., value) {
  skip_inherit(NextMethod())
}

#' @export
`[.marquee_skip_inherit` <- function(x, ..., value) {
  skip_inherit(NextMethod())
}

#' @export
print.marquee_skip_inherit <- print.marquee_relative

#' @rdname style_helpers
#' @export
marquee_bullets <- c(
  "\u2022",
  "\u25E6",
  "\u25AA",
  "\u25AB",
  "\u2023",
  "\u2043"
)
