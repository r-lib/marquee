# marquee (development version)

* Added support for vertical typesetting (tategaki). The `text_direction` style
  property now accepts `"ttb"` (top-to-bottom columns progressing
  right-to-left, as in Japanese and traditional Chinese typesetting) and
  `"ttb-ltr"` (top-to-bottom columns progressing left-to-right, the Mongolian
  convention). Blocks whose direction differs from their parent's are laid out
  in their own transposed coordinate system, so horizontal and vertical text
  can be mixed at any nesting level. Vertical text requires a version of
  textshaping with vertical shaping support
* New `mixed`, `kinsoku`, and `kinsoku_style` style properties control how
  horizontal-only runs are set inside vertical text (rotation, upright
  substitution, tate-chu-yoko, etc.) and how CJK line-break prohibitions are
  applied and resolved
* New `emphasis` style property sets an emphasis mark (kenten/boten) placed
  next to each character of a span — right of the column in ttb, left in
  ttb-ltr, above in horizontal text
* New `vertical_style()` constructor provides a style set with traditional
  tategaki defaults (kenten emphasis, kanji list numbering, hanging bullets,
  centered headings, horizontal code blocks)
* Ordered list bullets are now templates: the digit character selects the
  numbering system (arabic, Han, Roman, circled, parenthesized, and other
  Unicode digit systems), and `"_"` auto-detects a script-appropriate system
  from the list content. The default `"_."` keeps the previous behavior for
  Latin content
* `marquee_grob()` gains symmetric `width`/`height` semantics (`"auto"`,
  `NULL`, or `NA`) and a `clip` argument. The default `"auto"` resolves per
  writing direction so vertical text wraps at the available height and sizes
  itself horizontally from the content. The default `x`/`hjust` anchor at the
  flow-start corner (top-right for ttb)
* In vertical text, underlines are drawn as bōsen (right of the column in ttb,
  left in ttb-ltr), bullets hang at the column head, span backgrounds split
  per column, and inline math is rotated to match `mixed = "rotate"`
* `marquee_parse()` no longer crashes on style sets serialized by an older
  version of marquee that lack newer style properties; missing properties are
  treated as unset

# marquee 1.3.0

* Added support for math expressions written with standard markdown
  math syntax (`$expression$` for inline math and `$$expression$$` for display
  math). Rendering is done with the new math shaper in textshaping
  (`textshaping::shape_math()`) and requires a version of textshaping that
  provides it; an informative error is raised if the text contains math and
  the installed textshaping is too old

* Added support for custom block tags using fenced div syntax: a fence of
  three or more colons followed by a class (`::: {.name}`) wraps the enclosed
  content in a block with the given style tag. Fences of different lengths
  can be nested, allowing whole blocks to be styled with custom styles (#3)
* HTML entities now return the correct symbols (@clauswilke, #89)
* Fixed a regression where a vector of bullets passed to `classic_style()` or
  `modify_style()` was recycled into multiple style sets (#99)
* Fixed a crash (session abort) when using `guide_marquee()` together with
  `marquefy_theme()`, caused by an `NA` title width being used instead of the
  default width (#95)
* `geom_marquee()` now takes its default font family, size, and colour from
  the theme, like other text geoms do on ggplot2 >= 4.0.0 (#106)
* Fixed horizontal inline padding being applied twice to background and border
  rectangles of styled spans, so borders no longer spill into neighbouring
  words. One-sided padding now also correctly adds spacing (#93)
* Multi-line text in `element_marquee()` now follows the element's `hjust` for
  line alignment (like `element_text()`), unless the style sets an explicit
  alignment. Additionally, `marquefy_theme()` no longer sets `style` and
  `width` on every element, so a `theme()` call on a parent element (e.g.
  `axis.text`) propagates to its children (e.g. `axis.text.x`) (#104)
* `element_marquee()` now correctly falls back to the base family, colour,
  size, and lineheight of the provided style when these are not set on the
  element itself
* Documented that in `element_marquee()` the element margin replaces body
  padding from the style, and that paragraph spacing must be set on the
  paragraph style (e.g. via `modify_style(style, "p", ...)`) rather than
  through base-level padding (#100)
* `element_marquee()` is now an S7 subclass of `ggplot2::element_text` on
  ggplot2 >= 4.0.0, so setting it on a parent theme element (e.g. `title`)
  correctly propagates to child elements (e.g. `plot.title`) instead of
  rendering the raw markdown as plain text (#97)

# marquee 1.2.1

* Fixed a bug when calculating em() of a relative sized style
* Fixed a bug in outline glyphs
* Fixed a bug in `force_body_margin`
* Make sure that `width` is properly propagated in ´guide_marquee()`

# marquee 1.2.0

* Better vectorisation of style_set and classic_style
* Renamed `border_size` to `border_width` for consistency
* Added `border_type` and `outline_type` styles to control the linetype of
  borders and outlines
* Underline and strikethrough now uses metrics from the font for width and
  position

# marquee 1.1.1

* Fixed a bug in `element_marquee()` that resulted in wrong width calculation
  for rotated text (#69)
* If image URLs doesn't indicate image format marquee will attempt to sniff it
* Fixes an S7 compatibility issue with merge_element (#83)
* marquee should no longer open a graphics device if none exists (#75)
* `element_marquee()` now works with factor input and will fall back to
  `element_text()` for expressions (#71)

# marquee 1.1.0

* Size of `element_marquee()` is communicated similar to `element_text()` (#57)
* You can now change the size by using a {.size ...} shortcut, e.g. {.30 BIG} to
  render `BIG` with font size 30
* Added functionality to add outline to text as well as adding the `.out` style.
  Outlines can be controlled with the `outline`, `outline_width`, `outline_join`
  and `outline_mitre` style settings (#60)
* Added support for PNG and JPEG files from URLs (#63)
* Fixed a bug in bullet placement when the bullet was the last in the document
  and contained multiple text spans (#54)
* Fixed a bug in nested unordered bullet lists where the calculated bullet would
  be wrong (#53)
* Fixed a bug when using ordered list
* Fixed various bugs in `guide_marquee()`

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
