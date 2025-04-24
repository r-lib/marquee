# marquee (development version)

* Size of `element_marquee()` is communicated similar to `element_text()` (#57)
* You can now change the size by using a {.size ...} shortcut, e.g. {.30 BIG} to
  render `BIG` with font size 30
* Added functionality to add outline to text as well as adding the `.out` style.
  Outlines can be controlled with the `outline`, `outline_width`, `outline_join`
  and `outline_mitre` style settings (#60)
* Added support for PNG and JPEG files from URLs (#63)
* Fixed a bug in bullet placement when the bullet was the last in the document
  and contained multiple text spans (#54)

# marquee 1.0.0

* Fixed a bug in bullet placement that affected tight lists with multiple spans
  (#18)
* code spans gains a slight horizontal padding to let the background breathe a
  bit. Currently padding around spans doesn't affect shaping (i.e. it doesn't
  move text further from it's neighbors).
* Better adherence to margin collapsing rules of CSS. Any background or border
  will now prevent further collapsing
* Add `force_body_margin` argument to enforce that the body margin is not
  influenced by collapsing (allowing you to turn it off completely). This
  setting is turned on for `geom_marquee()` and `element_marquee()` (#23)
* Add support for rendering on graphics devices that doesn't support the new
  `glyphs` capabilities
* Inline padding now reserves space on the left and right side during shaping if
  the inline tag has decoration (background or border)
* Added `guide_marquee()` to make allow legends with keys mixed into free text
  descriptions

# marquee 0.1.0

* Initial CRAN submission.
