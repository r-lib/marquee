# Convert all text elements in a theme to marquee elements

While
[`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
should behave similar to
[`ggplot2::element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
when used on plain text (i.e. text without any markdown markup), the
reality can be different. This is because the text shaping engine used
by marquee
([`textshaping::shape_text()`](https://rdrr.io/pkg/textshaping/man/shape_text.html))
may differ from the one used by the graphics device (which is
responsible for laying out text in
[`element_text()`](https://ggplot2.tidyverse.org/reference/element.html)).
Differences can range from slight differences in letter spacing to using
a different font altogether (this is because the font keywords `""`,
`"sans"`, `"serif"`, `"mono"`, and `"symbol"` may be mapped to different
fonts depending on the shaper). One way to handle this is to provide an
explicit font name for the elements, but alternatively you can use this
function to convert all text elements in a theme to
[`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)

## Usage

``` r
marquefy_theme(theme)
```

## Arguments

- theme:

  A (complete) ggplot2 theme

## Value

`theme` with all text elements substituted for marquee elements

## Examples

``` r
library(ggplot2)
ggplot(mtcars) +
  geom_point(aes(disp, mpg)) +
  ggtitle("How about that") +
  marquefy_theme(theme_gray())
```
