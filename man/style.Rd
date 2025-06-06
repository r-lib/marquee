% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/style.R
\name{style}
\alias{style}
\alias{base_style}
\title{Create a style specification for a single tag}
\usage{
style(
  family = NULL,
  weight = NULL,
  italic = NULL,
  width = NULL,
  features = NULL,
  size = NULL,
  color = NULL,
  lineheight = NULL,
  align = NULL,
  tracking = NULL,
  indent = NULL,
  hanging = NULL,
  margin = NULL,
  padding = NULL,
  background = NULL,
  border = NULL,
  border_size = NULL,
  border_radius = NULL,
  outline = NULL,
  outline_width = NULL,
  outline_join = NULL,
  outline_mitre = NULL,
  bullets = NULL,
  underline = NULL,
  strikethrough = NULL,
  baseline = NULL,
  img_asp = NULL,
  text_direction = NULL
)

base_style(
  family = "",
  weight = "normal",
  italic = FALSE,
  width = "normal",
  features = systemfonts::font_feature(),
  size = 12,
  color = "black",
  lineheight = 1.6,
  align = "auto",
  tracking = 0,
  indent = 0,
  hanging = 0,
  margin = trbl(0, 0, rem(1)),
  padding = trbl(0),
  background = NA,
  border = NA,
  border_size = trbl(0),
  border_radius = 0,
  outline = NA,
  outline_width = 1,
  outline_join = "round",
  outline_mitre = 10,
  bullets = marquee_bullets,
  underline = FALSE,
  strikethrough = FALSE,
  baseline = 0,
  img_asp = 1.65,
  text_direction = "auto"
)
}
\arguments{
\item{family}{The name of the font family to use}

\item{weight}{The font weight to use. Can either be a number (\code{0}, \code{100},
\code{200}, \code{300}, \code{400}, \code{500}, \code{600}, \code{700}, \code{800}, or \code{900}) or a strings
(\code{"undefined"}, \code{"thin"}, \code{"ultralight"}, \code{"light"}, \code{"normal"}, \code{"medium"},
\code{"semibold"}, \code{"bold"}, \code{"ultrabold"}, or \code{"heavy"})}

\item{italic}{Should the font be slanted}

\item{width}{The font width to use. Can either be a number (``0\verb{, }1`, `2`,
`3`, `4`, `5`, `6`, `7`, `8`, or `9`) or strings (`"undefined"`,
`"ultracondensed"`, `"extracondensed"`, `"condensed"`, `"semicondensed"`,
`"normal"`, `"semiexpanded"`, `"expanded"`, `"extraexpanded"`, or
`"ultraexpanded"`)}

\item{features}{A \link[systemfonts:font_feature]{font_feature} object specifying
any OpenType font features to apply to the font}

\item{size}{The size of the font in points. Can be \code{\link[=relative]{relative()}} or \code{\link[=em]{em()}} in
which case it is based on the parent font size (for \code{size} these are
equivalent) or \code{\link[=rem]{rem()}} in which case it is based on the font size of the body
element.}

\item{color}{Is the color of the font}

\item{lineheight}{The spacing between subsequent lines relative to the font
size. Can be \code{\link[=relative]{relative()}} in which case it is based on the parent lineheight.}

\item{align}{The alignment within the text. One of \code{"left"}, \code{"center"},
\code{"right"}, \code{"justified-left"}, \code{"justified-center"}, \code{"justified-right"}, or
\code{"distributed"}}

\item{tracking}{Additional character spacing measured in 1/1000em. Can be
\code{\link[=relative]{relative()}} in which case it is based on the parent tracking.}

\item{indent}{The indentation of the first line in a paragraph measured in
points. Can be \code{\link[=relative]{relative()}} in which case it is based on the parent indent,
\code{\link[=em]{em()}} in which case it is based on the font size in this style, or \code{\link[=rem]{rem()}}
in which case it is based on the font size of the body element.}

\item{hanging}{The indentation of all but the first line in a paragraph
measured in points. Can be \code{\link[=relative]{relative()}} in which case it is based on the
parent hanging, \code{\link[=em]{em()}} in which case it is based on the font size in this
style, or \code{\link[=rem]{rem()}} in which case it is based on the font size of the body
element.}

\item{margin}{The margin around the element, given as a call to \code{\link[=trbl]{trbl()}}.
Margin refers to the area outside the box that text is placed in. If the
element has a background, the margin area will not be colored.}

\item{padding}{The padding around the element, given as a call to \code{\link[=trbl]{trbl()}}.
Padding refers to the distance between the text and the border of the box it
will be drawn in. If the element has a background, the padding area will be
colored.}

\item{background}{The color of the background fill. The background includes
the padding but not the margin. Can be a solid color or a gradient or pattern
made with \code{grid::linearGradient()}/\code{grid::radialGradient()}/\code{grid::pattern()}}

\item{border}{The color of the background stroke. The background includes
the padding but not the margin}

\item{border_size}{The line width of the background stroke, given as a call
to \code{\link[=trbl]{trbl()}}}

\item{border_radius}{The corner radius of the background, given in points}

\item{outline}{The color of the outline stroke.}

\item{outline_width}{The line width of the outline stroke.}

\item{outline_join}{The line join type for the outline. Either \code{"round"},
\code{"mitre"}, or \code{"bevel"}.}

\item{outline_mitre}{The mitre limit (relative distance between inner and
outer corner at a join) if \code{outline_join = "mitre"}.}

\item{bullets}{A vector of strings to use for bullets in unordered lists.
\code{marquee_bullets} provides a selection}

\item{underline}{Should text be underlined}

\item{strikethrough}{Should text be strikethrough}

\item{baseline}{The baseline shift to apply to the text}

\item{img_asp}{The default aspect ratio for block level images if not
provided by the image itself}

\item{text_direction}{The directional flow of the text. Either \code{"auto"} to
let it be determined by the content of the text, or \code{"ltr"}/\code{"rtl"} to
hard-code it to either left-to-right or right-to-left. This setting will not
change the order of glyphs within a span of text, but rather whether
consequtive blocks of text are laid out left-to-right or right-to-left. It
also affects to which side indentation is applied as well as the meaning of
\code{"auto"}, and \code{"justified-auto"} aligment.}
}
\value{
A \code{marquee_style} object
}
\description{
\code{style()} constructs a \code{marquee_style} object specifying the styling for a
single tag. The meaning of \code{NULL} is to inherit the value from the parent
element. It follows that top parent (the body element), must have values for
all it's options. The \code{base_style()} constructor is a convenient constructor
for a style with sensible defaults for all it's options.
}
\examples{
# A partial style
style(color = "red", underline = TRUE)

# Full style
base_style()

}
