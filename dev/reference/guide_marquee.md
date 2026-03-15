# Marquee subtitle guide

This legend appears similar to a subtitle and uses marquee syntax to
typeset the text and interpolate legend glyphs.

## Usage

``` r
guide_marquee(
  title = ggplot2::waiver(),
  style = marquee::style(background = NA),
  detect = FALSE,
  width = NULL,
  theme = NULL,
  position = "top",
  override.aes = list(),
  order = 1
)
```

## Arguments

- title:

  A single character string indicating the text to display. If `NULL`
  the title is not shown. If
  [`waiver()`](https://ggplot2.tidyverse.org/reference/waiver.html)
  (default), the name of the scale or the name specified in
  [`labs()`](https://ggplot2.tidyverse.org/reference/labs.html) is used
  for the tyle.

- style:

  Either a
  [style_set](https://marquee.r-lib.org/dev/reference/style_set.md) to
  override style sets inherited from the theme, or a
  [style](https://marquee.r-lib.org/dev/reference/style.md) for styling
  the labels specifically. For `colour` or `fill` scales, the `color`,
  `background` and `border` style properties are overridden when set as
  `NULL`, see examples.

- detect:

  Either `FALSE` to typeset entirely through syntax or `TRUE` to
  automatically detect labels and apply.

- width:

  The width of the textbox. If `NULL` it will take up the width of the
  panel, but due to issues with grid this might lead to an erroneous
  height calculation

- theme:

  A [`theme`](https://ggplot2.tidyverse.org/reference/theme.html) object
  to style the guide individually or differently from the plot's theme
  settings. The `theme` argument in the guide partially overrides, and
  is combined with, the plot's theme. Arguments that apply to a single
  legend are respected, most of which have the `legend`-prefix.
  Arguments that apply to combined legends (the legend box) are ignored,
  including `legend.position`, `legend.justification.*`,
  `legend.location` and `legend.box.*`.

- position:

  A character string indicating where the legend should be placed
  relative to the plot panels. One of "top", "right", "bottom", "left",
  or "inside".

- override.aes:

  A list specifying aesthetic parameters of the legend keys. See details
  and examples in
  [`?guide_legend`](https://ggplot2.tidyverse.org/reference/guide_legend.html).

- order:

  positive integer less than 99 that specifies the order of this guide
  among multiple guides. This controls the order in which multiple
  guides are displayed, not the contents of the guide itself. If 0
  (default), the order is determined by a secret algorithm.

## Value

A GuideMarquee object that can be passed to the
[`guides()`](https://ggplot2.tidyverse.org/reference/guides.html)
function or used as the `guide` argument in a scale.

## Text formatting

In addition to standard [marquee
syntax](https://marquee.r-lib.org/dev/reference/marquee_parse.md), there
is additional syntax to make building a guide easier. In the text below,
`n` marks the `n`-th break in the scale, `label` represents any of the
scale's labels and `foo` represents arbitrary text.

- `<<n>>` or `<<label>>` can be used to insert key glyphs into the text.

- `![](n)` or `![](label)` can also be used to insert key glyphs into
  the text.

- `{.n foo}` or `{.label foo}` applies the `style` argument to `foo`,
  including recoloring when the guide represents a `colour` or `fill`
  scale.

- `!!n` or `!!label` translates to `{.label label}` to insert the label
  verbatim with the application of the `style` argument.

## Examples

``` r
library(ggplot2)
# A standard plot
base <- ggplot(mpg, aes(displ, hwy)) +
  geom_point()

# Using key glyphs
base + aes(shape = drv) +
  scale_shape_discrete(
    # Same as using <<1>>, <<2>> and <<3>>,
    # or ![](1), ![](2) and ![](3)
    # or ![](4), ![](f) and ![](r)
    name = "Cars with four wheel <<4>>, forward <<f>> or reverse <<r>> drive.",
    guide = "marquee"
  )


# Recolouring text
base <- base +
  aes(colour = drv) +
  labs(
    colour = "Cars with {.4 four wheel}, {.f forward} or {.r reverse} drive."
  )
base + guides(colour = "marquee")


# Adjust display of labels
st <- style(weight = "bold", italic = TRUE, background = NA)
base + guides(colour = guide_marquee(style = st))


# Using background instead of text colour by setting it to NULL
st <- style(color = "black", background = NULL)
base + guides(colour = guide_marquee(style = st))


# Customising style of each label through style sets
# Note: tag names must be universal per `vctrs::vec_as_names` and
# prefixed with `lab_`.
st <- classic_style()
st <- modify_style(st, tag = "lab_f", background = NULL, color = "black")
st <- modify_style(st, tag = "lab_r", border_width = trbl(1),
                   color = "black", background = NA)
base + guides(colour = guide_marquee(style = st))


# Alternatively:
base + guides(colour = "marquee") +
  theme(plot.subtitle = element_marquee(style = st))


# Splicing in labels by number (!!2) or label (!!subcompact)
base + aes(colour = class) +
  labs(colour = "Cars including !!2 and !!subcompact vehicles") +
  guides(colour = "marquee")


# Using automatic detection
base + aes(colour = class) +
  labs(colour = "Cars including suv and minivan vehicles") +
  guides(colour = guide_marquee(detect = TRUE))
```
