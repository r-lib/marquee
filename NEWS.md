# marquee (development version)

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

# marquee 0.1.0

* Initial CRAN submission.
