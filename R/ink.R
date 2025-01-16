#' Make justifications relative to the ink extent of the text
#'
#' Marquee measures the extent of the box around text with bearings, that is, the
#' height of the string "mean" is the same as the height of the string "median",
#' despite the latter having a "d" extending upwards. This makes it easier to
#' justification text irrespective of the glyphs used to render it. However, if
#' you want alignment to be relative to the "tight" box around the text (the
#' bounding box of where ink has been placed), you can use the `ink()` function
#' to inform marquee of your intend. In general the effect is often minuscule
#' for horizontal justifications but can have a big effect on vertical
#' justification depending on the presence of ascenders and descenders in the
#' rendered glyphs.
#'
#' @param x A string giving a valid justification or a numeric between 0 and 1
#' @param use_ink Should the values be relative to the ink extend. Will be
#' recycled to the length of `x`
#'
#' @return A `marquee_ink` vector
#'
#' @export
#'
#' @examples
#' # Plot to illustrate the difference in vertical alignment
#' library(grid)
#' grid.newpage()
#' grid.draw(
#'   marquee_grob(
#'     c("### Textbox justification (default)",
#'       "### Bounding box justification (using `ink()`)"),
#'     x = 0.5,
#'     y = c(0.95, 0.45),
#'     hjust = 0.5,
#'     width = NA
#'   )
#' )
#'
#' # Standard justification
#' grid.draw(
#'   marquee_grob(
#'     "mean",
#'     x = 0.5,
#'     y = 0.75,
#'     hjust = "right",
#'     vjust = 0.5,
#'     width = NA
#'   )
#' )
#' grid.draw(
#'   marquee_grob(
#'     "median",
#'     x = 0.5,
#'     y = 0.75,
#'     hjust = "left",
#'     vjust = 0.5,
#'     width = NA
#'   )
#' )
#'
#' # Justification using `ink()`
#' grid.draw(
#'   marquee_grob(
#'     "mean",
#'     x = 0.5,
#'     y = 0.25,
#'     hjust = "right",
#'     vjust = ink(0.5),
#'     width = NA
#'   )
#' )
#' grid.draw(
#'   marquee_grob(
#'     "median",
#'     x = 0.5,
#'     y = 0.25,
#'     hjust = "left",
#'     vjust = ink(0.5),
#'     width = NA
#'   )
#' )
#'
ink <- function(x = numeric(), use_ink = TRUE) {
  if (is.character(x)) {
    if (grepl("-ink$", x)) {
      x
    } else {
      paste0(x, "-ink")
    }
  } else if (!is_ink(x)) {
    vctrs::new_rcrd(list(val = x, ink = rep_along(x, use_ink)), class = "marquee_ink")
  } else {
    x
  }
}

is_ink <- function(x) inherits(x, "marquee_ink")

#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.marquee_ink.marquee_ink <- function(x, y, ...) ink()
#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.marquee_ink.double <- function(x, y, ...) ink()
#' @importFrom vctrs vec_ptype2
#' @export
vec_ptype2.double.marquee_ink <- function(x, y, ...) ink()

#' @importFrom vctrs vec_cast
#' @export
vec_cast.marquee_ink.marquee_ink <- function(x, to, ...) x
#' @importFrom vctrs vec_cast
#' @export
vec_cast.marquee_ink.double <- function(x, to, ...) ink(x, FALSE)
#' @importFrom vctrs vec_cast
#' @export
vec_cast.double.marquee_ink <- function(x, to, ...) vctrs::vec_data(x)$val

#' @export
format.marquee_ink <- function(x, ...) paste0(format(as.numeric(x), ...), ifelse(vctrs::vec_data(x)$ink, "-ink", ""))
