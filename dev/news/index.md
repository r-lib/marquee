# Changelog

## marquee (development version)

## marquee 1.2.1

CRAN release: 2025-09-15

- Fixed a bug when calculating em() of a relative sized style
- Fixed a bug in outline glyphs
- Fixed a bug in `force_body_margin`
- Make sure that `width` is properly propagated in ´guide_marquee()\`

## marquee 1.2.0

CRAN release: 2025-09-05

- Better vectorisation of style_set and classic_style
- Renamed `border_size` to `border_width` for consistency
- Added `border_type` and `outline_type` styles to control the linetype
  of borders and outlines
- Underline and strikethrough now uses metrics from the font for width
  and position

## marquee 1.1.1

CRAN release: 2025-08-27

- Fixed a bug in
  [`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
  that resulted in wrong width calculation for rotated text
  ([\#69](https://github.com/r-lib/marquee/issues/69))
- If image URLs doesn’t indicate image format marquee will attempt to
  sniff it
- Fixes an S7 compatibility issue with merge_element
  ([\#83](https://github.com/r-lib/marquee/issues/83))
- marquee should no longer open a graphics device if none exists
  ([\#75](https://github.com/r-lib/marquee/issues/75))
- [`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
  now works with factor input and will fall back to
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  for expressions ([\#71](https://github.com/r-lib/marquee/issues/71))

## marquee 1.1.0

CRAN release: 2025-08-19

- Size of
  [`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
  is communicated similar to
  [`element_text()`](https://ggplot2.tidyverse.org/reference/element.html)
  ([\#57](https://github.com/r-lib/marquee/issues/57))
- You can now change the size by using a {.size …} shortcut, e.g. {.30
  BIG} to render `BIG` with font size 30
- Added functionality to add outline to text as well as adding the
  `.out` style. Outlines can be controlled with the `outline`,
  `outline_width`, `outline_join` and `outline_mitre` style settings
  ([\#60](https://github.com/r-lib/marquee/issues/60))
- Added support for PNG and JPEG files from URLs
  ([\#63](https://github.com/r-lib/marquee/issues/63))
- Fixed a bug in bullet placement when the bullet was the last in the
  document and contained multiple text spans
  ([\#54](https://github.com/r-lib/marquee/issues/54))
- Fixed a bug in nested unordered bullet lists where the calculated
  bullet would be wrong
  ([\#53](https://github.com/r-lib/marquee/issues/53))
- Fixed a bug when using ordered list
- Fixed various bugs in
  [`guide_marquee()`](https://marquee.r-lib.org/dev/reference/guide_marquee.md)

## marquee 1.0.0

CRAN release: 2025-01-20

- Fixed a bug in bullet placement that affected tight lists with
  multiple spans ([\#18](https://github.com/r-lib/marquee/issues/18))
- code spans gains a slight horizontal padding to let the background
  breathe a bit. Currently padding around spans doesn’t affect shaping
  (i.e. it doesn’t move text further from it’s neighbors).
- Better adherence to margin collapsing rules of CSS. Any background or
  border will now prevent further collapsing
- Add `force_body_margin` argument to enforce that the body margin is
  not influenced by collapsing (allowing you to turn it off completely).
  This setting is turned on for
  [`geom_marquee()`](https://marquee.r-lib.org/dev/reference/geom_marquee.md)
  and
  [`element_marquee()`](https://marquee.r-lib.org/dev/reference/element_marquee.md)
  ([\#23](https://github.com/r-lib/marquee/issues/23))
- Add support for rendering on graphics devices that doesn’t support the
  new `glyphs` capabilities
- Inline padding now reserves space on the left and right side during
  shaping if the inline tag has decoration (background or border)
- Added
  [`guide_marquee()`](https://marquee.r-lib.org/dev/reference/guide_marquee.md)
  to make allow legends with keys mixed into free text descriptions

## marquee 0.1.0

CRAN release: 2024-05-28

- Initial CRAN submission.
