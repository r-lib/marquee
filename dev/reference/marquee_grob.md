# Construct a grob rendering one or more markdown texts

This is the main function of marquee. It takes a vector of markdown
strings, parses them with the provided style, and returns a grob capable
of rendering the parsed text into rich text and (possibly) images. See
[`marquee_parse()`](https://marquee.r-lib.org/dev/reference/marquee_parse.md)
for more information about how markdown is parsed and see details below
for further information on how rendering proceeds.

## Usage

``` r
marquee_grob(
  text,
  style = classic_style(),
  ignore_html = TRUE,
  force_body_margin = FALSE,
  clip = "off",
  x = NULL,
  y = 1,
  width = "auto",
  height = "auto",
  default.units = "npc",
  hjust = NULL,
  vjust = "top",
  angle = 0,
  vp = NULL,
  name = NULL
)
```

## Arguments

- text:

  Either a character vector or a `marquee_parsed` object as created by
  [`marquee_parse()`](https://marquee.r-lib.org/dev/reference/marquee_parse.md)

- style:

  A style set such as
  [`classic_style()`](https://marquee.r-lib.org/dev/reference/classic_style.md)
  that defines how the text should be rendered

- ignore_html:

  Should HTML code be removed from the output

- force_body_margin:

  Should the body margin override margin collapsing calculations. See
  Details.

- clip:

  Should content that overflows a `width`/`height` constraint be clipped
  at the constraint box? Either `"off"` (never clip, the default),
  `"on"` (clip), or `"inherit"` (defer to the clipping state of the
  drawing context). Note that clipping also cuts intentional overflow
  such as emphasis marks and hanging punctuation.

- x, y:

  The location of the markdown text in the graphics. If numeric it will
  be converted to units using `default.units`. The default is the
  flow-start corner: `x = 0` for horizontal and ttb-ltr text, `x = 1`
  for ttb text, `y = 1` always.

- width, height:

  The width and height of each markdown text. If numeric they are
  converted to units using `default.units`. Both accept the same three
  kinds of values:

  - `"auto"` (the default): resolve per writing direction. The
    constraint axis (width for horizontal text, height for vertical
    text) takes the extent of the parent container; the block axis
    (height for horizontal, width for vertical) is sized by the content.
    This makes vertical text work out of the box, wrapping columns at
    the container height.

  - `NULL`: the extent of the parent container — a hard constraint.

  - `NA`: derive the size from the content (for width this avoids soft
    breaking of lines).

  Setting both to `NULL` thus constrains the content to the available
  space (clipping it if `clip = "on"`).

- default.units:

  A string giving the default units to apply to `x`, `y`, and `width`

- hjust:

  The horizontal justification of the markdown with respect to `x`. Can
  either be a numeric or one of `"left"`, `"left-ink"`, `"center"`,
  `"center-ink"`, `"right-ink"`, or `"right"`

- vjust:

  The vertical justification of the markdown with respect to `y`. Can
  either be a numeric or one of `"bottom"`, `"bottom-ink"`,
  `"last-line"`, `"center"`, `"center-ink"`, `"first-line"`,
  `"top-ink"`, `"top"`

- angle:

  The angle of rotation (in degrees) around `x` and `y`

- vp:

  An optional viewport to assign to the grob

- name:

  The name for the grob. If `NULL` a unique name will be generated

## Value

A grob of class `marquee`

## Rendering

marquee is first and foremost developed with the new 'glyph' rendering
features in 4.3.0 in mind. However, not all graphics devices supports
this, and while some might eventually do, it is quite concievable that
some never will. Because of this, marquee has a fallback where it will
render text as a mix of polygons and rasters (depending on the font in
use) if the device doesn't report 'glyphs' capabilities. The upside is
that it works (almost) everywhere, but the downside is that the fallback
is much slower and with poorer visual quality. Because of this it is
advisable to use a modern graphics device with glyphs support if at all
possible.

## Rendering style

The rendering more or less adheres to the styling provided by
[`marquee_parse()`](https://marquee.r-lib.org/dev/reference/marquee_parse.md),
but has some intricacies as detailed below:

**Tight lists**

If a list is tight, the bottom margin of each `li` tag will be set so
the spacing matches the lineheight. Further, the top margin will be set
to 0.

**Block images**

In markdown, image tags are span elements so they can be placed inline.
However, if an image tag is the only thing that is contained inside a p
tag marquee determines that it should be considered a block element. In
that case, the parent p element inherits the styling from the image
element so that the image can e.g. adhere to `align` properties, or
provide their own padding.

**Horizontal rulers**

These elements are rendered as an empty block. The standard style sets a
bottom border size and no size for the other sides.

**Margin collapsing**

Margin calculations follows the margin collapsing rules of HTML. Read
more about these at
[mdn](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_box_model/Mastering_margin_collapsing).
Margin collapsing means that elements with margin set to 0 might end up
with a margin. Specifically for the body element this can be a problem
if you want to enforce a tight box around your text. Because of this the
`force_body_margin` argument allows you to overwrite the margins for the
body element with the original values after collapsing has been
performed.

**Underline and strikethrough**

Underlines are placed according to the font specification. Strikethrough
are placed 0.3em above the baseline. The width of the line is set
according to the fonr specification for underline width, both for
underline and strikethrough. It inherits the color of the text.

**Spans with background**

Consecutive spans with the same background and border settings are
merged into a single rectangle. The padding of the span defines the size
of the background.

**Bullet position**

Bullets are placed, right-aligned, 0.25em to the left of the first line
in the li element if the text direction is ltr. For rtl text it is
placed, left-aligned, 0.25 em to the right of the first line.

**Border with border radius**

If borders are not the same on all sides they are drawn one by one. In
this case the border radius is ignored.

## Image rendering

The image tag can be used to place images. There are support for both
png, jpeg, and svg images. If the path instead names a grob, ggplot, or
patchwork object then this is rendered instead. If the file cannot be
read, if it d oesn't exist, or if the path names an object that is not a
grob, ggplot or patchwork, a placeholder is rendered in it's place
(black square with red cross).

**Image sizing**

There is no standard in markdown for specifying the size of images. By
default, block-level images fill the width of it's container and
maintain it's aspect ratio. Inline images have a default width of 0.65em
and a height matching the aspect ration.

However, if you wish to control sizing, you can instead provide the
image as a grob with a viewport with fixed dimensions, in which case
this will be used as long as the width doesn't exceed the width of the
container (in which case it will get downsized). If a rastergrob is
provided without absolute sizing, the aspect ratio will match the
raster, otherwise the aspect ratio will be taken from the styling of the
element (defaults to 1.65)

## Table rendering

While marquee does not support the extended table syntax for markdown it
does allow you to include tables in the output. It does so by supporting
gt objects as valid paths in image tags in the same way as ggplots etc.
This meeans that you can style your tables any way you wish and with the
full power of gt, which is much more flexible than the markdown table
syntax.

## Math rendering

Math expressions written with standard markdown math syntax are rendered
using the math shaper in textshaping
([`textshaping::shape_math()`](https://rdrr.io/pkg/textshaping/man/shape_math.html)).
This functionality is only available if the installed version of
textshaping provides `shape_math()`; if the text contains math
expressions and textshaping is too old, an error is raised informing you
of this. Both LaTeX and Typst math syntax are supported (see
[`textshaping::shape_math()`](https://rdrr.io/pkg/textshaping/man/shape_math.html)
for the full specification).

Inline math (`$expression$`) flows with the surrounding text, while
display math (`$$expression$$`) is typeset in display style. A display
math expression standing alone in a block behaves like a block-level
image: it can be aligned through its style (it is centered by default
with
[`classic_style()`](https://marquee.r-lib.org/dev/reference/classic_style.md)).
Math is typeset in the system math font using the size and color of the
surrounding text. A different math font can be selected by setting the
family of the `eq`/`eqd` styles (e.g.
`modify_style(classic_style(), "eq", family = "Latin Modern Math")`);
for best results use a font with an OpenType MATH table. Elements within
an expression can be colored individually with e.g.
`\textcolor{color}{content}`.

## Vertical typesetting

marquee supports vertical writing modes through the `text_direction`
style property: `"ttb"` sets text in top-to-bottom columns that progress
right-to-left (as in Japanese and traditional Chinese typesetting), and
`"ttb-ltr"` sets the same columns with left-to-right progression (the
Mongolian convention). The root writing mode is taken from the base
style; any tag can override it, and the resolved value cascades down the
block tree. A block whose direction differs from its parent's is laid
out in its own transposed coordinate system and positioned as a box in
the parent's flow, which makes it possible to mix directions freely —
e.g. a tategaki quotation in a horizontal document, or a yokogaki code
block inside vertical text.

In vertical mode both axes transpose: the constraint axis is the height
(columns wrap when they reach it) and the block axis runs horizontally.
Margins and padding remain physical
([`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)),
so in ttb mode margin collapsing happens between the right margin of the
left block and the left margin of the right block. Decorations follow
the conventions of vertical typesetting: bullets hang at the column
head, underlines are drawn as bōsen to the right of the column in ttb
(left in ttb-ltr), and emphasis marks (see the `emphasis` argument of
[`style()`](https://marquee.r-lib.org/dev/reference/style.md)) sit to
the right of each character. Horizontal-only runs (Latin script, digits)
inside vertical text are handled according to the `mixed` style
property, and CJK line breaking follows the `kinsoku`/`kinsoku_style`
properties.

The
[`vertical_style()`](https://marquee.r-lib.org/dev/reference/vertical_style.md)
constructor provides a ready-made style set with traditional defaults
(kenten emphasis, kanji list numbering, hanging bullets, horizontal code
blocks) for both directions.

A few limitations apply: stroked text (the `outline_*` style properties)
does not account for glyph rotation, bitmap glyphs (color emoji) are not
rotated in the fallback rendering path, and the `"first-line"`/
`"last-line"` justification anchors remain y-based for vertical text.

## Textbox justification

The justification options exceeds the classic ones provided by grid.
While numeric values are available as always, the number of possible
text values are larger. Horizontal justification add `"left-ink"`,
`"center-ink"`, and `"right-ink"` which uses the left-most and
right-most positioned glyph (or halfway between them) as anchors.
Vertical justification has the equivalent `"bottom-ink"`,
`"center-ink"`, and `"top-ink"` anchors, but also `"first-line"` and
`"last-line"` which sets the anchor at the baseline of the first or last
line respectively.
