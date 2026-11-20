# Vertical (tategaki) styling for markdown

This function facilitates construction of a complete style set for
vertically typeset text, following traditional Japanese typesetting
conventions (tategaki): text flows in top-to-bottom columns, headings
are centered with space to either side, emphasis is given with emphasis
marks (kenten) rather than italics, unordered lists use `・` bullets,
ordered lists use kanji numerals (一、二、…), and code blocks are set
horizontally (yokogaki) as is customary in Japanese print.

## Usage

``` r
vertical_style(
  direction = c("ttb", "ttb-ltr"),
  base_size = 12,
  body_font = "",
  header_font = "",
  code_font = "mono",
  strong_font = NULL,
  emphasis = "●",
  ...
)
```

## Arguments

- direction:

  The writing direction: `"ttb"` for Japanese/Chinese style typesetting
  (columns top-to-bottom, blocks ordered right-to-left) or `"ttb-ltr"`
  for Mongolian style typesetting (columns top-to-bottom, blocks ordered
  left-to-right)

- base_size:

  The base font size for the text. All other sizing is based on this

- body_font:

  The font family to use for body text

- header_font:

  The font family to use for headers

- code_font:

  The font family to use for code and code block text

- strong_font:

  The font family to use for strong (`str`) spans. Strong emphasis in
  Japanese typesetting is conventionally done by switching from mincho
  to a gothic typeface rather than by bolding. If `NULL` (the default),
  strong text is bolded

- emphasis:

  The emphasis mark (kenten) used for `em` spans. Set to `NA` to use
  italics instead. Common marks to copy-paste: `"●"` (the most common),
  `"○"`, `"◎"`, `"▲"`, `"△"`, `"■"`, `"□"`, `"★"`, `"☆"`, `"・"`, `"﹅"`
  (sesame dot), and `"※"` (common in Korean texts)

- ...:

  Arguments passed on to
  [`base_style`](https://marquee.r-lib.org/dev/reference/style.md)

  `weight`

  :   The font weight to use. Can either be a number (`0`, `100`, `200`,
      `300`, `400`, `500`, `600`, `700`, `800`, or `900`) or a strings
      (`"undefined"`, `"thin"`, `"ultralight"`, `"light"`, `"normal"`,
      `"medium"`, `"semibold"`, `"bold"`, `"ultrabold"`, or `"heavy"`)

  `italic`

  :   Should the font be slanted

  `width`

  :   The font width to use. Can either be a number (“0`, `1\`, \`2\`,
      \`3\`, \`4\`, \`5\`, \`6\`, \`7\`, \`8\`, or \`9\`) or strings
      (\`"undefined"\`, \`"ultracondensed"\`, \`"extracondensed"\`,
      \`"condensed"\`, \`"semicondensed"\`, \`"normal"\`,
      \`"semiexpanded"\`, \`"expanded"\`, \`"extraexpanded"\`, or
      \`"ultraexpanded"\`)

  `features`

  :   A
      [font_feature](https://systemfonts.r-lib.org/reference/font_feature.html)
      object specifying any OpenType font features to apply to the font

  `color`

  :   Is the color of the font

  `lineheight`

  :   The spacing between subsequent lines relative to the font size.
      Can be
      [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the parent lineheight.

  `align`

  :   The alignment within the text. One of `"left"`, `"center"`,
      `"right"`, `"justified-left"`, `"justified-center"`,
      `"justified-right"`, or `"distributed"`

  `tracking`

  :   Additional character spacing measured in 1/1000em. Can be
      [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the parent tracking.

  `indent`

  :   The indentation of the first line in a paragraph measured in
      points. Can be
      [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the parent indent,
      [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the font size in this style, or
      [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the font size of the body element.

  `hanging`

  :   The indentation of all but the first line in a paragraph measured
      in points. Can be
      [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the parent hanging,
      [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the font size in this style, or
      [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
      in which case it is based on the font size of the body element.

  `marker_width`

  :   How much space to reserve for list markers so they never overlap
      the first line: `"auto"` (the default) reserves the width of the
      widest marker in the list uniformly for all items, `"natural"`
      reserves each item's own marker width (ragged first lines), and a
      number or [`grid::unit()`](https://rdrr.io/r/grid/unit.html)
      reserves that fixed width uniformly (like a label length). The
      reservation raises `indent` and `hanging` as needed, so all lines
      stay aligned.

  `margin`

  :   The margin around the element, given as a call to
      [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md).
      Margin refers to the area outside the box that text is placed in.
      If the element has a background, the margin area will not be
      colored.

  `padding`

  :   The padding around the element, given as a call to
      [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md).
      Padding refers to the distance between the text and the border of
      the box it will be drawn in. If the element has a background, the
      padding area will be colored.

  `background`

  :   The color of the background fill. The background includes the
      padding but not the margin. Can be a solid color or a gradient or
      pattern made with
      [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html)/[`grid::radialGradient()`](https://rdrr.io/r/grid/patterns.html)/[`grid::pattern()`](https://rdrr.io/r/grid/patterns.html)

  `border`

  :   The color of the background stroke. The background includes the
      padding but not the margin

  `border_width`

  :   The line width of the background stroke, given as a call to
      [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)

  `border_type`

  :   The linetype of the background stroke, given as an an `lty`
      compatible value (See the *Line Type Specification* section in
      [par](https://rdrr.io/r/graphics/par.html))

  `border_radius`

  :   The corner radius of the background, given in points

  `outline`

  :   The color of the outline stroke.

  `outline_width`

  :   The line width of the outline stroke.

  `outline_type`

  :   The linetype of the outline stroke, given as an an `lty`
      compatible value (See the *Line Type Specification* section in
      [par](https://rdrr.io/r/graphics/par.html))

  `outline_join`

  :   The line join type for the outline. Either `"round"`, `"mitre"`,
      or `"bevel"`.

  `outline_mitre`

  :   The mitre limit (relative distance between inner and outer corner
      at a join) if `outline_join = "mitre"`.

  `bullets`

  :   A vector of strings to use for bullets in unordered lists.
      `marquee_bullets` provides a selection

  `underline`

  :   Should text be underlined

  `strikethrough`

  :   Should text be strikethrough

  `baseline`

  :   The baseline shift to apply to the text

  `img_asp`

  :   The default aspect ratio for block level images if not provided by
      the image itself

  `kinsoku`

  :   The strictness of CJK line break prohibition rules. One of
      `"strict"`, `"loose"`, or `"none"`.

  `kinsoku_style`

  :   How kinsoku violations are resolved when a line is forced past the
      last valid break point. One of `"hang"` (offending punctuation
      hangs past the line end), `"push-out"` (move to next line and
      expand spacing), or `"push-in"` (compress spacing to fit it on the
      line).

  `border_size`

  :   **\[deprecated\]** Use `border_width` instead

## Value

A style set object

## Examples

``` r
vertical_style(base_size = 16, body_font = "Hiragino Mincho ProN")
#> <marquee_style_set[1]>
#> [1] <base, body, ul, ol, li, hr, h1, h2, h3, h4, h5, h6, cb, p, qb, em, str, a, code, u, del, img, eq, eqd, sub, sup, out>
```
