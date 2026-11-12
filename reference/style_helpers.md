# Helpers for defining styles

marquee provides a small set of helpers for constructing the needed
styles. `relative()` specifies a numeric value as relative to the value
of the parent style by a certain factor, e.g. a font size of
`relative(0.5)` would give a style a font size half of it's parent.
`em()` specify a numeric value as relative to the font size of the
current style. If the font size is `12`, and indent is set to `em(2)`,
then the indent will be equivalent to 24. `rem()` works like `em()` but
rather than using the font size of the current style it uses the font
size of the root style (which is the body element). `trbl()` helps you
construct styles that refers to sides of a rectangle (margin, padding,
and border size). The function names refers to the order of the
arguments (top, right, bottom, left). `skip_inherit()` tells the style
inheritance to ignore this value and look for the value one above in the
stack. `marquee_bullets` is just a character vector with 6 sensible
bullet glyphs for unordered lists.

## Usage

``` r
relative(x)

em(x)

rem(x)

trbl(top = NULL, right = top, bottom = top, left = right)

skip_inherit(x)

marquee_bullets
```

## Format

An object of class `character` of length 6.

## Arguments

- x:

  A decimal number. If a vector is provided only the first element will
  be used

- top, right, bottom, left:

  Values for the sides of the rectangles. Either numbers or modifiers
  (relative, em, or rem)

## Value

Objects of the relevant class

## Examples

``` r
relative(0.35)
#> relative(0.35) 

em(2)
#> em(2) 

rem(1.2)
#> rem(1.2) 

# Argument default means it recycles like CSS if fewer values are specified
trbl(6, em(1.5))
#> A marquee trbl
#>    top: 6
#>  right: em(1.5)
#> bottom: 6
#>   left: em(1.5)

skip_inherit("sans")
#> sans (no inheritance) 

marquee_bullets
#> [1] "•" "◦" "▪" "▫" "‣" "⁃"
```
