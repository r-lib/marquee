#' Classic styling for markdown
#'
#' This function facilitates construction of a complete style set based on the
#' classic look of an HTML rendered markdown document. It contains style
#' specifications for all the supported markdown elements as well as a `sub` and
#' `sup` style that can be used for subscripts and superscript respectively.
#' These are only accessible through custom spans (e.g. `H{.sub 2}O`) as
#' markdown doesn't provide a syntax for these formats.
#'
#' @param base_size The base font size for the text. All other sizing is based
#' on this
#' @param body_font The font family to use for body text
#' @param header_font The font family to use for headers
#' @param code_font The font family to use for code and code block text
#' @inheritDotParams base_style -family -size
#'
#' @return A style set object
#'
#' @export
#'
#' @examples
#' classic_style(16, "serif", "sans")
#'
classic_style <- function(base_size = 12, body_font = "", header_font = "",
                          code_font = "mono", ...) {
  base <- base_style(family = body_font, size = base_size, ...)
  style_set(
    base = base,
    body = style(),
    ul   = style(padding = trbl(0, 0, 0, em(2)), background = NA, border = NA),
    ol   = style(padding = trbl(0, 0, 0, em(2)), background = NA, border = NA),
    li   = style(padding = trbl(0), background = NA, border = NA),
    hr   = style(padding = trbl(0, 0, rem(1/8)), border = "#eeeeee", border_size = trbl(0, 0, rem(1/16))),
    h1   = style(family = header_font, size = relative(2.25), weight = "bold", lineheight = 1.2, margin = trbl(em(1), NULL, NULL, NULL), padding = trbl(0, 0, em(0.3)), border = "#eeeeee", border_size = trbl(0, 0, rem(1/16))),
    h2   = style(family = header_font, size = relative(1.75), weight = "bold", lineheight = 1.225, margin = trbl(em(1), NULL, NULL, NULL), padding = trbl(0, 0, em(0.3)), border = "#eeeeee", border_size = trbl(0, 0, rem(1/16))),
    h3   = style(family = header_font, size = relative(1.5), weight = "bold", lineheight = 1.43, margin = trbl(em(1), NULL, NULL, NULL)),
    h4   = style(family = header_font, size = relative(1.25), weight = "bold", lineheight = 1.4, margin = trbl(em(1), NULL, NULL, NULL)),
    h5   = style(family = header_font, weight = "bold", lineheight = 1.4, margin = trbl(em(1), NULL, NULL, NULL)),
    h6   = style(family = header_font, weight = "bold", lineheight = 1.4, margin = trbl(em(1), NULL, NULL, NULL), color = "#777777"),
    cb   = style(family = code_font, size = relative(0.85), lineheight = 1.45, padding = trbl(rem(1)), background = "#f7f7f7", border_radius = rem(3/16)),
    p    = style(padding = trbl(0), background = NA, border = NA),
    qb   = style(color = "#777777", padding = trbl(em(0.2), 0, em(0.2), em(1)), border = "#dddddd", border_size = trbl(0, 0, 0, rem(0.25))),
    em   = style(italic = TRUE),
    str  = style(weight = "bold"),
    a    = style(color = "#4078c0"),
    code = style(family = code_font, size = relative(0.85), background = "#0000000A", padding = trbl(em(0.2), em(0.1)), border_radius = rem(3/16)),
    u    = style(underline = TRUE),
    del  = style(strikethrough = TRUE),
    img  = style(align = "center", border = NA),
    sub  = style(size = em(0.5), baseline = em(-0.2)),
    sup  = style(size = em(0.5), baseline = em(1))
  )
}
