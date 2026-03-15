# Classic styling for markdown

This function facilitates construction of a complete style set based on
the classic look of an HTML rendered markdown document. It contains
style specifications for all the supported markdown elements as well as
a `sub` and `sup` style that can be used for subscripts and superscript
respectively. These are only accessible through custom spans (e.g.
`H{.sub 2}O`) as markdown doesn't provide a syntax for these formats.

## Usage

``` r
classic_style(
  base_size = 12,
  body_font = "",
  header_font = "",
  code_font = "mono",
  ...,
  ltr = TRUE
)
```

## Arguments

- base_size:

  The base font size for the text. All other sizing is based on this

- body_font:

  The font family to use for body text

- header_font:

  The font family to use for headers

- code_font:

  The font family to use for code and code block text

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

  `text_direction`

  :   The directional flow of the text. Either `"auto"` to let it be
      determined by the content of the text, or `"ltr"`/`"rtl"` to
      hard-code it to either left-to-right or right-to-left. This
      setting will not change the order of glyphs within a span of text,
      but rather whether consequtive blocks of text are laid out
      left-to-right or right-to-left. It also affects to which side
      indentation is applied as well as the meaning of `"auto"`, and
      `"justified-auto"` aligment.

  `border_size`

  :   **\[deprecated\]** Use `border_width` instead

- ltr:

  Is the style intended for left-to-right text? This affects list
  indentation and citation border

## Value

A style set object

## Examples

``` r
classic_style(16, "serif", "sans")
#> <marquee_style_set[1]>
#> [1] <base, body, ul, ol, li, hr, h1, h2, h3, h4, h5, h6, cb, p, qb, em, str, a, code, u, del, img, sub, sup, out>
```
