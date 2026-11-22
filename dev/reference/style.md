# Create a style specification for a single tag

`style()` constructs a `marquee_style` object specifying the styling for
a single tag. The meaning of `NULL` is to inherit the value from the
parent element. It follows that top parent (the body element), must have
values for all it's options. The `base_style()` constructor is a
convenient constructor for a style with sensible defaults for all it's
options.

## Usage

``` r
style(
  family = NULL,
  weight = NULL,
  italic = NULL,
  width = NULL,
  features = NULL,
  variation = NULL,
  size = NULL,
  color = NULL,
  lineheight = NULL,
  align = NULL,
  tracking = NULL,
  indent = NULL,
  hanging = NULL,
  marker_width = NULL,
  margin = NULL,
  padding = NULL,
  background = NULL,
  border = NULL,
  border_width = NULL,
  border_type = NULL,
  border_radius = NULL,
  outline = NULL,
  outline_width = NULL,
  outline_type = NULL,
  outline_join = NULL,
  outline_mitre = NULL,
  bullets = NULL,
  underline = NULL,
  strikethrough = NULL,
  emphasis = NULL,
  baseline = NULL,
  img_asp = NULL,
  text_direction = NULL,
  mixed = NULL,
  kinsoku = NULL,
  kinsoku_style = NULL,
  border_size = deprecated()
)

base_style(
  family = "",
  weight = "normal",
  italic = FALSE,
  width = "normal",
  features = systemfonts::font_feature(),
  variation = systemfonts::font_variation(),
  size = 12,
  color = "black",
  lineheight = 1.6,
  align = "auto",
  tracking = 0,
  indent = 0,
  hanging = 0,
  marker_width = "auto",
  margin = trbl(0, 0, rem(1)),
  padding = trbl(0),
  background = NA,
  border = NA,
  border_width = trbl(0),
  border_type = "solid",
  border_radius = 0,
  outline = NA,
  outline_width = 1,
  outline_type = "solid",
  outline_join = "round",
  outline_mitre = 10,
  bullets = marquee_bullets,
  underline = FALSE,
  strikethrough = FALSE,
  emphasis = NA_character_,
  baseline = 0,
  img_asp = 1.65,
  text_direction = "auto",
  mixed = "rotate",
  kinsoku = "strict",
  kinsoku_style = "hang",
  border_size = deprecated()
)
```

## Arguments

- family:

  The name of the font family to use

- weight:

  The font weight to use. Can either be a number (`0`, `100`, `200`,
  `300`, `400`, `500`, `600`, `700`, `800`, or `900`) or a strings
  (`"undefined"`, `"thin"`, `"ultralight"`, `"light"`, `"normal"`,
  `"medium"`, `"semibold"`, `"bold"`, `"ultrabold"`, or `"heavy"`)

- italic:

  Should the font be slanted

- width:

  The font width to use. Can either be a number (“0`, `1\`, \`2\`,
  \`3\`, \`4\`, \`5\`, \`6\`, \`7\`, \`8\`, or \`9\`) or strings
  (\`"undefined"\`, \`"ultracondensed"\`, \`"extracondensed"\`,
  \`"condensed"\`, \`"semicondensed"\`, \`"normal"\`,
  \`"semiexpanded"\`, \`"expanded"\`, \`"extraexpanded"\`, or
  \`"ultraexpanded"\`)

- features:

  A
  [font_feature](https://systemfonts.r-lib.org/reference/font_feature.html)
  object specifying any OpenType font features to apply to the font

- variation:

  A
  [font_variation](https://systemfonts.r-lib.org/reference/font_variation.html)
  object specifying the coordinates on the variable font axes to use.
  Settings here take precedence over the corresponding `weight`,
  `italic`, and `width` settings during shaping and rendering

- size:

  The size of the font in points. Can be
  [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  or [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  in which case it is based on the parent font size (for `size` these
  are equivalent) or
  [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md) in
  which case it is based on the font size of the body element.

- color:

  Is the color of the font

- lineheight:

  The spacing between subsequent lines relative to the font size. Can be
  [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  in which case it is based on the parent lineheight.

- align:

  The alignment within the text. One of `"left"`, `"center"`, `"right"`,
  `"justified-left"`, `"justified-center"`, `"justified-right"`, or
  `"distributed"`

- tracking:

  Additional character spacing measured in 1/1000em. Can be
  [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  in which case it is based on the parent tracking.

- indent:

  The indentation of the first line in a paragraph measured in points.
  Can be
  [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  in which case it is based on the parent indent,
  [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md) in
  which case it is based on the font size in this style, or
  [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md) in
  which case it is based on the font size of the body element.

- hanging:

  The indentation of all but the first line in a paragraph measured in
  points. Can be
  [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  in which case it is based on the parent hanging,
  [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md) in
  which case it is based on the font size in this style, or
  [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md) in
  which case it is based on the font size of the body element.

- marker_width:

  How much space to reserve for list markers so they never overlap the
  first line: `"auto"` (the default) reserves the width of the widest
  marker in the list uniformly for all items, `"natural"` reserves each
  item's own marker width (ragged first lines), and a number or
  [`grid::unit()`](https://rdrr.io/r/grid/unit.html) reserves that fixed
  width uniformly (like a label length). The reservation raises `indent`
  and `hanging` as needed, so all lines stay aligned.

- margin:

  The margin around the element, given as a call to
  [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md).
  Margin refers to the area outside the box that text is placed in. If
  the element has a background, the margin area will not be colored.

- padding:

  The padding around the element, given as a call to
  [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md).
  Padding refers to the distance between the text and the border of the
  box it will be drawn in. If the element has a background, the padding
  area will be colored.

- background:

  The color of the background fill. The background includes the padding
  but not the margin. Can be a solid color or a gradient or pattern made
  with
  [`grid::linearGradient()`](https://rdrr.io/r/grid/patterns.html)/[`grid::radialGradient()`](https://rdrr.io/r/grid/patterns.html)/[`grid::pattern()`](https://rdrr.io/r/grid/patterns.html)

- border:

  The color of the background stroke. The background includes the
  padding but not the margin

- border_width:

  The line width of the background stroke, given as a call to
  [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)

- border_type:

  The linetype of the background stroke, given as an an `lty` compatible
  value (See the *Line Type Specification* section in
  [par](https://rdrr.io/r/graphics/par.html))

- border_radius:

  The corner radius of the background, given in points

- outline:

  The color of the outline stroke.

- outline_width:

  The line width of the outline stroke.

- outline_type:

  The linetype of the outline stroke, given as an an `lty` compatible
  value (See the *Line Type Specification* section in
  [par](https://rdrr.io/r/graphics/par.html))

- outline_join:

  The line join type for the outline. Either `"round"`, `"mitre"`, or
  `"bevel"`.

- outline_mitre:

  The mitre limit (relative distance between inner and outer corner at a
  join) if `outline_join = "mitre"`.

- bullets:

  A vector of strings to use for bullets in unordered lists.
  `marquee_bullets` provides a selection

- underline:

  Should text be underlined

- strikethrough:

  Should text be strikethrough

- emphasis:

  A single character used as an emphasis mark (kenten/boten), drawn
  beside every glyph in the span (to the right of the column in vertical
  text, above the line in horizontal text), or `NA` for no mark.
  Traditional Japanese typesetting uses emphasis marks instead of
  italics. Common marks to copy-paste: `"●"` (the most common), `"○"`,
  `"◎"`, `"▲"`, `"△"`, `"■"`, `"□"`, `"★"`, `"☆"`, `"・"`, `"﹅"`
  (sesame dot), and `"※"` (common in Korean texts)

- baseline:

  The baseline shift to apply to the text

- img_asp:

  The default aspect ratio for block level images if not provided by the
  image itself

- text_direction:

  The directional flow of the text. Either `"auto"` to let it be
  determined by the content of the text, or `"ltr"`/`"rtl"` to hard-code
  it to either left-to-right or right-to-left. Vertical text is
  requested with `"ttb"` (top-to-bottom columns, as used in e.g.
  Japanese and traditional Chinese typesetting) or `"ttb-ltr"`
  (top-to-bottom columns with left-to-right column and block order, as
  used in traditional Mongolian typesetting). This setting will not
  change the order of glyphs within a span of text, but rather how
  consequtive lines/columns of text are laid out. It also affects to
  which side indentation is applied as well as the meaning of `"auto"`,
  and `"justified-auto"` aligment.

- mixed:

  How horizontal-only runs (e.g. Latin script or digits) are set inside
  vertical text. One of `"rotate"` (set horizontally and rotate the run
  90 degrees), `"upright"` (set upright using the font's `vert`/`vrt2`
  glyph substitutions, falling back to `"rotate"`), `"tate"`
  (tate-chu-yoko: digit runs of up to 4 digits are set horizontally
  within the column), `"tate2"` or `"tate3"` (like `"tate"` but capping
  digit runs at 2 or 3 digits, respectively), `"combine"` (generalised
  tate-chu-yoko for any run of up to 4 characters), `"combine2"` or
  `"combine3"` (like `"combine"` but capping runs at 2 or 3 characters),
  or `"sideways"` (set the whole string horizontally and rotate it).
  Ignored for horizontal text.

- kinsoku:

  The strictness of CJK line break prohibition rules. One of `"strict"`,
  `"loose"`, or `"none"`.

- kinsoku_style:

  How kinsoku violations are resolved when a line is forced past the
  last valid break point. One of `"hang"` (offending punctuation hangs
  past the line end), `"push-out"` (move to next line and expand
  spacing), or `"push-in"` (compress spacing to fit it on the line).

- border_size:

  **\[deprecated\]** Use `border_width` instead

## Value

A `marquee_style` object

## Examples

``` r
# A partial style
style(color = "red", underline = TRUE)
#> <marquee_style[2]>
#>     color: red
#> underline: TRUE

# Full style
base_style()
#> <marquee_style[45]>
#>                size: 12
#>          background: NA
#>               color: black
#>              family: 
#>              weight: 400
#>              italic: FALSE
#>               width: 5
#>            features: <empty>
#>           variation: <empty>
#>          lineheight: 1.6
#>               align: auto
#>            tracking: 0
#>              indent: 0
#>             hanging: 0
#>        marker_width: auto
#>          margin_top: 0
#>        margin_right: 0
#>       margin_bottom: rem(1)
#>         margin_left: 0
#>         padding_top: 0
#>       padding_right: 0
#>      padding_bottom: 0
#>        padding_left: 0
#>              border: NA
#>    border_width_top: 0
#>  border_width_right: 0
#> border_width_bottom: 0
#>   border_width_left: 0
#>         border_type: solid
#>       border_radius: 0
#>             outline: NA
#>       outline_width: 1
#>        outline_type: solid
#>        outline_join: round
#>       outline_mitre: 10
#>             bullets: •, ◦, ▪, ▫, ‣, ⁃
#>           underline: FALSE
#>       strikethrough: FALSE
#>            emphasis: NA
#>            baseline: 0
#>             img_asp: 1.65
#>      text_direction: auto
#>               mixed: rotate
#>             kinsoku: strict
#>       kinsoku_style: hang
```
