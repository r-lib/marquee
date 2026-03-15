# Package index

## Rendering

These functions all help render markdown in various ways

- [`marquee_grob()`](https://marquee.r-lib.org/dev/reference/marquee_grob.md)
  : Construct a grob rendering one or more markdown texts
- [`geom_marquee()`](https://marquee.r-lib.org/dev/reference/geom_marquee.md)
  : Draw text formatted with marquee
- [`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
  : ggplot2 theme element supporting marquee syntax
- [`marquefy_theme()`](https://marquee.r-lib.org/dev/reference/marquefy_theme.md)
  : Convert all text elements in a theme to marquee elements
- [`guide_marquee()`](https://marquee.r-lib.org/dev/reference/guide_marquee.md)
  : Marquee subtitle guide
- [`ink()`](https://marquee.r-lib.org/dev/reference/ink.md) : Make
  justifications relative to the ink extent of the text

## Styling

These functions helps you specify the visual style used during rendering

- [`style()`](https://marquee.r-lib.org/dev/reference/style.md)
  [`base_style()`](https://marquee.r-lib.org/dev/reference/style.md) :
  Create a style specification for a single tag
- [`style_set()`](https://marquee.r-lib.org/dev/reference/style_set.md)
  [`modify_style()`](https://marquee.r-lib.org/dev/reference/style_set.md)
  [`remove_style()`](https://marquee.r-lib.org/dev/reference/style_set.md)
  : Create or modify a style set that describes a full markdown text
- [`classic_style()`](https://marquee.r-lib.org/dev/reference/classic_style.md)
  : Classic styling for markdown
- [`relative()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  [`em()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  [`rem()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  [`trbl()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  [`skip_inherit()`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  [`marquee_bullets`](https://marquee.r-lib.org/dev/reference/style_helpers.md)
  : Helpers for defining styles

## Parsing

While often not needed, these functions helps you work with markdown
text before it is rendered

- [`marquee_parse()`](https://marquee.r-lib.org/dev/reference/marquee_parse.md)
  : Parse a text as marquee
- [`marquee_glue()`](https://marquee.r-lib.org/dev/reference/marquee_glue.md)
  [`marquee_glue_data()`](https://marquee.r-lib.org/dev/reference/marquee_glue.md)
  : Marquee-aware string interpolation
