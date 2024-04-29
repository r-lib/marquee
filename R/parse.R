#' Parse a text as marquee
#'
#' marquee uses an extension of CommonMark with no support for HTML code (it is
#' rendered verbatim). The focus is to allow easy formatting of text for
#' graphics, rather than fully fledged typesetting. See *marquee syntax* for
#' more about the format.
#'
#' @param text A character string. The core quality of markdown is that any text
#' is valid markdown so there is no restrictions on the content
#' @param style A style set such as [classic_style()] that defines how the text
#' should be rendered
#'
#' @return A data frame describing the various tokens of the text and the style
#' to apply to them. The output is mainly meant for programmatic consumption
#' such as in [marquee_grob()]
#'
#' @details
#'
#' # marquee tags
#' marquee tokenizes the input text into blocks and spans. It recognises the
#' following tags:
#'
#' **Block tags**
#'
#' `body` is the parent tag of a markdown document. It never contains any text
#' itself, only other blocks.
#'
#' `ul` is an unordered list. It contains a number of `li` children
#'
#' `ol` is an ordered list. It contains a number of `li` children
#'
#' `li` is a list element. If the list is tight it contains text directly inside
#' of it. If not, text are placed inside child `p` blocks
#'
#' `hr` is a horizontal line, spanning the width of the parent block. For
#' styling, the bottom border size is used when rendering
#'
#' `h1`-`h6` are headings at different levels
#'
#' `cb` is a code block. Text inside code blocks are rendered verbatim, i.e. it
#' cannot contain any children
#'
#' `p` is a standard paragraph block. Text separated by two line-ends are
#' separated into separate paragraphs
#'
#' `qb` is a quote block. It may contain children
#'
#' **Span tags**
#'
#' `em` is an emphasized text span. Often this means italicizing the text, but
#' it is ultimately up to the renderer
#'
#' `str` is strong text, often rendered with bold text
#'
#' `a` is a link text. While marquee rendering doesn't allow for links, it can
#' still be rendered in a particular way
#'
#' `code` is text rendered as code. Often this uses a monospaced font. Text
#' inside this span is rendered verbatim
#'
#' `u` is text that should be underlined
#'
#' `del` is text that should have strikethrough
#'
#' *custom spans* is a marquee specific extension to the syntax that allows you
#' to make up tags on the fly. See the section on marquee syntax for more.
#'
#' # marquee syntax
#' marquee uses md4c which is a fully CommonMark compliant markdown parser.
#' CommonMark is an effort to create an internally coherent markdown
#' specification, something that was missing from the original markdown
#' description. If you are used to writing markdown, you are used to CommonMark.
#' Below is a list of notable additions or details about the specific way
#' marquee handles CommonMark
#'
#' **Underlines and strikethrough**
#'
#' While not part of the basic CommonMark spec, underline and strikethrough are
#' supported by marquee using `_` and `~` (e.g. `_underline this_` and `~this
#' was an error~`).
#'
#' **Images**
#'
#' Image tags (`![image title](path/to/image)`) are supported, but the title is
#' ignored. The path is returned as the token text.
#'
#' **HTML**
#'
#' HTML tags are ignored, i.e. they are rendered verbatim. This is not that
#' different from classic markdown rendering except that people often convert
#' markdown to HTML where these tags suddenly have meaning. They do not carry
#' any special significance when rendered with marquee
#'
#' **Custom tags**
#'
#' While markdown provides most of what is necessary for standard text markup,
#' there are situations, especially in visualisation, where we need something
#' more. Often users reach for inline HTML spans for that, but since HTML is
#' fully ignored in marquee this is not an option. Further, adding in HTML
#' decreases readability of the unformatted text a lot.
#'
#' With marquee you can create a custom span using the `{.tag <some text>}`
#' syntax, e.g. `{.sm small text}` to wrap "small text" in the `sm` tag. You can
#' alternatively use `{#tag <some text>}` for the same effect. The only
#' difference is that in the former syntax the `.` is stripped from the tag name,
#' whereas in the latter the `#` remains part of the name. See the Styling
#' section for the primal use of the latter syntax.
#'
#' # Styling
#' During parsing, each token is assigned a style based on the provided style
#' set. The styling is cascading, but without the intricacies of CSS. A child
#' element inherits the styling of it's parent for the options that are set to
#' `NULL` in the style matching the child tag. Any style element that are
#' [relative()] are computed based on the value of the parent style element.
#' [em()] elements are resolved based on the size element of the child style,
#' and [rem()] elements are resolved using the size element of the `body` style.
#' If a style is not provided for the tag, it fully inherits the style of it's
#' parent.
#'
#' **Automatic coloring**
#' Recognizing that the primary use for custom tags may be to change the color
#' of some text, marquee provides a shortcut for this. If a style is not found
#' for the tag in the provided style set, marquee will check if the tag matches
#' a valid color (i.e. a string from `grDevices::colors()`, or a valid hex
#' string, e.g. `#53f2a9`). If it is a valid color it will set this as the font
#' color of the style. This means that parsing "Color {.red this} red"
#' automatically sets the color of "this" to red, even if no style is provided
#' for the `red` tag. Likewise, parsing "Color {#00FF00 me} green" will
#' automatically set the color of "me" to #00FF00 (fully satuared green).
#'
#' # Additional parsing information
#' Apart from splitting the text up into tokens, `marquee_parse()` also provides
#' some additional information useful for rendering the output in the expected
#' way. The `id` column refers the tokens back to the original input text, the
#' `block` relates tokens together into blocks. Block elements increment the
#' block count when they are entered, and decrement it when they are excited.
#' The `type` column provides the type of the block. The `indentation` column
#' provides the node level in the tree. A child block will increase the
#' indentation for as long as it is active. `ol_index` provides the number
#' associated with the ordered list element. `tight` indicates whether the list
#' is tight (i.e. it was provided with no empty lines between list elements).
#' The `ends` column indicate until which row in the output the tag is active
#' (i.e. the tag is closed after the row indicated by the value in this column).
#'
#' @export
#'
#' @examples
#' marquee_parse("# Header of the example\nSome body text", classic_style())
#'
marquee_parse <- function(text, style) {
  check_character(text)
  if (!is_style_set(style)) {
    cli::cli_abort("{.arg style} must be a style set")
  }
  style <- vctrs::vec_recycle(style, length(text))
  parsed <- marquee_c(text, style)
  # Remove terminal line end from code blocks
  parsed$text[parsed$type == "cb"] <- sub("\n$", "", parsed$text[parsed$type == "cb"])
  class(parsed) <- c("marquee_parsed", class(parsed))
  parsed
}
is_parsed <- function(x) inherits(x, "marquee_parsed")
