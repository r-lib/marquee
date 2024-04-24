#' Classic styling for markdown
#'
#' This function facilitates construction of a complete style set based on the
#' classic look of an HTML rendered markdown document.
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
    ul   = style(padding = box(0, 0, 0, em(2)), background = NA, border = NA),
    ol   = style(padding = box(0, 0, 0, em(2)), background = NA, border = NA),
    li   = style(padding = box(0), background = NA, border = NA),
    hr   = style(padding = box(0, 0, 2), border = "#eeeeee", border_size = box(0, 0, 1)),
    h1   = style(family = header_font, size = relative(2.25), weight = "bold", lineheight = 1.2, margin = box(em(1), NULL, NULL, NULL), padding = box(0, 0, em(0.3)), border = "#eeeeee", border_size = box(0, 0, 1)),
    h2   = style(family = header_font, size = relative(1.75), weight = "bold", lineheight = 1.225, margin = box(em(1), NULL, NULL, NULL), padding = box(0, 0, em(0.3)), border = "#eeeeee", border_size = box(0, 0, 1)),
    h3   = style(family = header_font, size = relative(1.5), weight = "bold", lineheight = 1.43, margin = box(em(1), NULL, NULL, NULL)),
    h4   = style(family = header_font, size = relative(1.25), weight = "bold", lineheight = 1.4, margin = box(em(1), NULL, NULL, NULL)),
    h5   = style(family = header_font, weight = "bold", lineheight = 1.4, margin = box(em(1), NULL, NULL, NULL)),
    h6   = style(family = header_font, weight = "bold", lineheight = 1.4, margin = box(em(1), NULL, NULL, NULL), color = "#777777"),
    cb   = style(family = code_font, size = relative(0.85), lineheight = 1.45, padding = box(16), background = "#f7f7f7", border_radius = 3),
    p    = style(padding = box(0), background = NA, border = NA),
    qb   = style(color = "#777777", padding = box(em(0.2), 0, em(0.2), em(1)), border = "#dddddd", border_size = box(0, 0, 0, 4)),
    em   = style(italic = TRUE),
    str  = style(weight = "bold"),
    a    = style(color = "#4078c0"),
    code = style(family = code_font, size = relative(0.85), background = "#0000000A", padding = box(em(0.2), 0), border_radius = 3),
    u    = style(underline = TRUE),
    del  = style(strikethrough = TRUE),
    img  = style(align = "center"),
  )
}
