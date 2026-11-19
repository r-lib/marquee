#' Vertical (tategaki) styling for markdown
#'
#' This function facilitates construction of a complete style set for
#' vertically typeset text, following traditional Japanese typesetting
#' conventions (tategaki): text flows in top-to-bottom columns, headings are
#' centered with space to either side, emphasis is given with emphasis marks
#' (kenten) rather than italics, unordered lists use `・` bullets, ordered
#' lists use kanji numerals (一、二、…), and code blocks are set horizontally
#' (yokogaki) as is customary in Japanese print.
#'
#' @param direction The writing direction: `"ttb"` for Japanese/Chinese style
#' typesetting (columns top-to-bottom, blocks ordered right-to-left) or
#' `"ttb-ltr"` for Mongolian style typesetting (columns top-to-bottom, blocks
#' ordered left-to-right)
#' @param base_size The base font size for the text. All other sizing is based
#' on this
#' @param body_font The font family to use for body text
#' @param header_font The font family to use for headers
#' @param code_font The font family to use for code and code block text
#' @param strong_font The font family to use for strong (`str`) spans. Strong
#' emphasis in Japanese typesetting is conventionally done by switching from
#' mincho to a gothic typeface rather than by bolding. If `NULL` (the
#' default), strong text is bolded
#' @param emphasis The emphasis mark (kenten) used for `em` spans. Set to `NA`
#' to use italics instead. Common marks to copy-paste: `"●"` (the most
#' common), `"○"`, `"◎"`, `"▲"`, `"△"`, `"■"`, `"□"`, `"★"`, `"☆"`, `"・"`,
#' `"﹅"` (sesame dot), and `"※"` (common in Korean texts)
#' @inheritDotParams base_style -family -size -text_direction -mixed
#'
#' @return A style set object
#'
#' @export
#'
#' @examples
#' vertical_style(16, "Hiragino Mincho ProN")
#'
vertical_style <- function(
  direction = c("ttb", "ttb-ltr"),
  base_size = 12,
  body_font = "",
  header_font = "",
  code_font = "mono",
  strong_font = NULL,
  emphasis = "●",
  ...
) {
  direction <- arg_match0(direction, c("ttb", "ttb-ltr"))
  ttb <- direction == "ttb"

  style <- classic_style(
    base_size = base_size,
    body_font = body_font,
    header_font = header_font,
    code_font = code_font,
    ...,
    ltr = !ttb
  )
  ## Base direction and shaping settings. Block spacing goes on the
  ## block-end side (left for ttb, right for ttb-ltr)
  style <- modify_style(
    style,
    "base",
    text_direction = direction,
    mixed = "tate",
    lineheight = 2,
    margin = trbl(0, if (ttb) 0 else em(1), 0, if (ttb) em(1) else 0)
  )
  ## Headings are centered, with space on the block axis (right/left in ttb)
  for (h in paste0("h", 1:6)) {
    style <- modify_style(
      style,
      h,
      align = "center",
      margin = trbl(0, em(1), 0, em(1)),
      padding = trbl(0),
      border = NA
    )
  }
  ## Lists are not indented as a whole. The marker hangs at the column head
  ## and the item text indents past it: one cell for ・, two for the kanji
  ## numeral + 、 marker (JLREQ's head-aligned list pattern)
  style <- modify_style(
    style,
    "ul",
    padding = trbl(0),
    bullets = "・",
    indent = em(1),
    hanging = em(1)
  )
  style <- modify_style(
    style,
    "ol",
    padding = trbl(0),
    bullets = "_、",
    indent = em(2),
    hanging = em(2)
  )
  ## Blockquotes indent from the inline start (top), with the same small
  ## inset on both block-axis sides as the horizontal style (em(0.2) on
  ## top and bottom there, i.e. left and right here)
  style <- modify_style(
    style,
    "qb",
    padding = trbl(em(1), em(0.2), em(0.2), em(0.2)),
    border_width = trbl(rem(0.25), 0, 0, 0)
  )
  ## A section break is a rule on the block-end side of the hr block
  style <- modify_style(
    style,
    "hr",
    padding = if (ttb) trbl(0, 0, 0, rem(1 / 8)) else trbl(0, rem(1 / 8), 0, 0),
    border_width = if (ttb) {
      trbl(0, 0, 0, rem(1 / 16))
    } else {
      trbl(0, rem(1 / 16), 0, 0)
    }
  )
  ## Strong emphasis switches to a gothic typeface rather than (synthetically)
  ## bolding the mincho body font, as is the Japanese convention. Falls back
  ## to bold when no strong font is given
  if (!is.null(strong_font)) {
    style <- modify_style(style, "str", family = strong_font, weight = "normal")
  }
  ## Emphasis uses kenten rather than italics
  style <- modify_style(style, "em", italic = FALSE, emphasis = emphasis)
  ## Code spans rotate, code blocks are set horizontally (yokogaki)
  style <- modify_style(style, "code", mixed = "rotate")
  style <- modify_style(style, "cb", text_direction = "ltr", align = "left")
  style
}
